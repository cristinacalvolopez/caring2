# Paso 3 · Redes ----------------------------------------------------------------------------

mod_redes_ui <- function(id) {
  ns <- NS(id)
  types <- names(net_types)
  div(class = "c-main", style = "max-width:1480px",
    div(class = "row g-4",
      div(class = "col-xl-3 col-lg-4",
        div(class = "c-card c-card-pad mb-3 c-analysis",
          tags$h2("¿Qué quieres ver?"),
          selectInput(ns("type"), NULL, selected = "factorial",
            choices = stats::setNames(types, vapply(net_types, function(x) x$titulo, ""))),
          uiOutput(ns("type_desc"))
        ),
        div(class = "c-card c-card-pad",
          uiOutput(ns("var_inputs")),
          uiOutput(ns("main_opts")),
          selectInput(ns("weight"), "Ponderar por", choices = c("Sin ponderar" = "")),
          bslib::accordion(open = FALSE, class = "mb-3",
            bslib::accordion_panel("Opciones avanzadas",
              textInput(ns("title"), "Título del gráfico"),
              textInput(ns("note"), "Nota al pie"),
              uiOutput(ns("adv_opts"))
            )
          ),
          actionButton(ns("go"), "Crear red", class = "btn-primary btn-lg-c w-100")
        )
      ),
      div(class = "col-xl-9 col-lg-8",
        div(class = "c-card",
          div(class = "d-flex flex-wrap justify-content-between align-items-center gap-2",
              style = "padding:14px 20px; border-bottom:1px solid #E3E3E3",
            div(tags$h2(style = "font-size:22px", textOutput(ns("net_title"), inline = TRUE)),
                div(class = "small-2", textOutput(ns("net_info"), inline = TRUE))),
            uiOutput(ns("net_actions"))),
          uiOutput(ns("net_view")))
      )
    )
  )
}

mod_redes_server <- function(id, ctx) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data <- ctx$data
    current <- reactiveValues(html = NULL, net = NULL, spec = NULL, title = NULL, error = NULL)

    labelled_choices <- function(d, vars) {
      if (!length(vars)) return(character())
      stats::setNames(vars, vapply(vars, function(v) {
        l <- var_label(d, v); if (l == v) v else paste0(l, " (", v, ")") }, ""))
    }

    observeEvent(data(), {
      nums <- numeric_vars(data())
      updateSelectInput(session, "weight", choices = c("Sin ponderar" = "", nums),
                        selected = if (isTRUE(input$weight %in% nums)) input$weight else "")
    })

    observeEvent(ctx$state$redes_preset, {
      pr <- ctx$state$redes_preset
      req(pr$type %in% names(net_types))
      updateSelectInput(session, "type", selected = pr$type)
      ctx$state$redes_preset <- NULL
    })

    output$type_desc <- renderUI({
      tagList(p(class = "c-hint", style = "margin:-4px 0 0", net_types[[input$type %||% "factorial"]]$desc),
              net_help_ui(input$type %||% "factorial"))
    })

    output$var_inputs <- renderUI({
      d <- data()
      t <- input$type %||% "factorial"
      cats <- labelled_choices(d, categorical_vars(d))
      nums <- labelled_choices(d, numeric_vars(d))
      txt <- labelled_choices(d, names(d)[vapply(d, is.character, logical(1))])
      prev <- isolate(input$vars)
      multi <- function(choices, label, help) tagList(
        selectizeInput(ns("vars"), label, choices = choices, multiple = TRUE,
                       selected = intersect(prev, choices),
                       options = list(plugins = list("remove_button"), placeholder = "Elige variables…")),
        p(class = "c-hint", style = "margin-top:-8px", help))
      todas <- labelled_choices(d, names(d))
      tiempos <- labelled_choices(d, names(d)[vapply(d, function(x) is.numeric(x) || inherits(x, c("Date", "POSIXct")), logical(1))])
      sel_keep <- function(id, choices, default = NULL) { v <- isolate(input[[id]]); if (isTRUE(v %in% choices)) v else default }
      # primera variable cuyo nombre se parece a alguno de los dados (nombre, inicio, fin, grupo)
      adivina <- function(choices, pat, default) {
        hit <- choices[grepl(pat, choices, ignore.case = TRUE)]
        if (length(hit)) hit[1] else default
      }
      switch(t,
        evolucion = tagList(
          p(class = "c-hint", "Cada caso es un periodo: por ejemplo, personas (nacimiento y muerte), cargos, legislaturas o contratos."),
          selectInput(ns("t_name"), "Nombre de cada caso", todas,
                      selected = sel_keep("t_name", todas, adivina(todas, "^(nombre|name|titulo|caso|persona)", todas[1]))),
          selectInput(ns("t_start"), "Inicio (año o fecha)", tiempos,
                      selected = sel_keep("t_start", tiempos, adivina(tiempos, "nacim|inicio|start|birth|desde|alta", tiempos[1]))),
          selectInput(ns("t_end"), "Fin (año o fecha)", tiempos,
                      selected = sel_keep("t_end", tiempos, adivina(tiempos, "muerte|fallec|fin|end|death|hasta|baja", tiempos[min(2, length(tiempos))]))),
          p(class = "c-hint", style = "margin-top:-8px", "Si falta el fin, el periodo llega hasta el último año de los datos."),
          selectInput(ns("t_group"), "Color según (opcional)", c("Ninguno" = "", cats, txt),
                      selected = sel_keep("t_group", c(cats, txt), adivina(cats, "escuela|grupo|group|tipo|partido|corriente", ""))),
          selectInput(ns("t_info"), "Texto al pulsar un caso (opcional)", c("Ninguno" = "", todas), selected = sel_keep("t_info", todas, ""))),
        trayectorias = tagList(
          textAreaInput(ns("model"), "Modelo", rows = 6, value = isolate(input$model) %||% "",
                        placeholder = "satisfaccion ~ edad + ingresos\nconfianza =~ p1 + p2 + p3"),
          div(class = "c-hint", style = "margin-top:-8px; margin-bottom:10px",
              tags$code("y ~ x1 + x2"), " regresión · ", tags$code("lat =~ a + b + c"), " variable latente · ",
              tags$code("a ~~ b"), " correlación. Usa los nombres de las variables."),
          tags$details(class = "c-mr-sug mb-3", open = if (!nzchar(isolate(input$model) %||% "")) NA,
            tags$summary(tags$strong("Construir el modelo paso a paso")),
            div(class = "mt-2",
              tags$strong(class = "small-2", "Regresión"),
              selectInput(ns("sb_dep"), "Variable explicada", c(nums, cats)),
              selectizeInput(ns("sb_indep"), "Variables explicativas", c(nums, cats), multiple = TRUE),
              actionButton(ns("sb_add_reg"), "Añadir regresión", class = "btn-light-c btn-sm mb-3"),
              tags$strong(class = "small-2", "Variable latente"),
              textInput(ns("sb_lat"), "Nombre", placeholder = "confianza"),
              selectizeInput(ns("sb_ind"), "Indicadores (tres o más)", c(nums, cats), multiple = TRUE),
              actionButton(ns("sb_add_lat"), "Añadir variable latente", class = "btn-light-c btn-sm mb-3"),
              tags$strong(class = "small-2", "Correlación"),
              div(class = "row g-1",
                div(class = "col-6", selectInput(ns("sb_c1"), NULL, c(nums, cats))),
                div(class = "col-6", selectInput(ns("sb_c2"), NULL, c(nums, cats)))),
              actionButton(ns("sb_add_cor"), "Añadir correlación", class = "btn-light-c btn-sm"))),
          p(class = "c-hint", "Las categóricas entran con sus códigos (1, 2, 3…): conviene que sean ordinales o dicotómicas.")),
        coincidencias = multi(cats, "Variables", "Categóricas. Cada categoría será un nodo. Las numéricas se pueden agrupar en tramos en Datos."),
        loglineal = multi(cats, "Variables", "Categóricas. Mejor pocas variables (3–6)."),
        correlaciones = multi(nums, "Variables", "Numéricas. Cada variable será un nodo."),
        factorial = {
          # Dos modelos distintos: correspondencias múltiples (cualitativas) y componentes principales (cuantitativas)
          ft <- input$f_type
          if (!isTRUE(ft %in% c("mca", "pca"))) ft <- if (length(cats) >= 2 || !length(nums)) "mca" else "pca"
          act <- if (ft == "mca") cats else nums
          todas_vars <- c(cats, nums)
          tagList(
            radioButtons(ns("f_type"), "Tipo de análisis", selected = ft,
              choiceValues = c("mca", "pca"),
              choiceNames = c("Correspondencias (variables cualitativas)",
                              "Factorial (variables cuantitativas)")),
            selectizeInput(ns("vars"), "Variables activas", choices = act, multiple = TRUE,
                           selected = intersect(prev, act),
                           options = list(plugins = list("remove_button"), placeholder = "Elige variables…")),
            p(class = "c-hint", style = "margin-top:-8px",
              if (ft == "mca") "Categóricas. Construyen los ejes. Las numéricas se pueden agrupar en tramos en Datos."
              else "Numéricas. Construyen los ejes."),
            selectizeInput(ns("passive"), "Variables pasivas (opcional)", choices = todas_vars, multiple = TRUE,
                           selected = intersect(isolate(input$passive), todas_vars),
                           options = list(plugins = list("remove_button"), placeholder = "Categóricas o numéricas…")),
            p(class = "c-hint", style = "margin-top:-8px",
              "No intervienen en los ejes: se describen sobre ellos (se ven al pulsar cada punto). Pueden ser categóricas o numéricas. Se excluyen los casos con datos perdidos en cualquiera de ellas."))
        },
        regresion = tagList(
          selectInput(ns("dep"), "Variable que quieres explicar", choices = c(cats, nums)),
          multi(c(cats, nums), "Variables explicativas", "Pueden ser categóricas o numéricas.")),
        multiple = {
          sets <- mrsets(d)
          mr <- if (length(sets)) stats::setNames(paste0("mr:", names(sets)), vapply(sets, mr_label, "")) else character()
          ch <- c(mr, if (length(txt)) stats::setNames(unname(txt), paste("Texto:", names(txt))))
          tagList(
            selectizeInput(ns("vars"), "Conjuntos de respuesta múltiple", choices = ch, multiple = TRUE,
                           selected = intersect(prev, ch),
                           options = list(plugins = list("remove_button"), placeholder = "Elige uno o varios conjuntos…")),
            if (!length(sets)) p(class = "c-hint", style = "margin-top:-8px",
              "Todavía no hay conjuntos. Créalos en el paso Datos con «+ Conjunto de respuesta múltiple»",
              if (length(txt)) " o elige una variable de texto con las opciones separadas." else "."),
            conditionalPanel("input.vars && input.vars.some(function(v){ return v.indexOf('mr:') !== 0; })", ns = ns,
              textInput(ns("sep"), "Separador de las opciones (variables de texto)", value = isolate(input$sep) %||% ";")),
            selectizeInput(ns("cross"), "Cruzar con (opcional)", choices = cats, multiple = TRUE,
                           selected = intersect(isolate(input$cross), cats),
                           options = list(plugins = list("remove_button"), placeholder = "Variables categóricas…")),
            p(class = "c-hint", style = "margin-top:-8px",
              "Las categorías de estas variables aparecen como nodos de otro color, para ver qué opciones marcan más unos grupos que otros."))
        }
      )
    })

    # Constructor del modelo de ecuaciones estructurales: añade líneas al texto del modelo
    add_model_line <- function(line) {
      cur <- trimws(input$model %||% "")
      updateTextAreaInput(session, "model", value = if (nzchar(cur)) paste0(cur, "\n", line) else line)
    }
    observeEvent(input$sb_add_reg, {
      ind <- setdiff(input$sb_indep, input$sb_dep)
      if (!length(ind)) return(showNotification("Elige al menos una variable explicativa distinta de la explicada.", type = "error"))
      add_model_line(paste(input$sb_dep, "~", paste(ind, collapse = " + ")))
    })
    observeEvent(input$sb_add_lat, {
      nm <- trimws(input$sb_lat %||% "")
      if (!grepl("^[A-Za-z][A-Za-z0-9_.]*$", nm)) return(showNotification("El nombre de la variable latente debe empezar por una letra y no llevar espacios.", type = "error"))
      if (nm %in% names(data())) return(showNotification("Ese nombre ya es el de una variable de los datos: elige otro.", type = "error"))
      if (length(input$sb_ind) < 2) return(showNotification("Elige al menos dos indicadores (mejor tres o más).", type = "error"))
      add_model_line(paste(nm, "=~", paste(input$sb_ind, collapse = " + ")))
    })
    observeEvent(input$sb_add_cor, {
      if (identical(input$sb_c1, input$sb_c2)) return(showNotification("Elige dos variables distintas.", type = "error"))
      add_model_line(paste(input$sb_c1, "~~", input$sb_c2))
    })

    # Opciones de cada tipo de análisis (net_options() dice cuáles corresponden a cada uno)
    keep <- function(id, default) { v <- isolate(input[[id]]); if (is.null(v)) default else v }

    output$main_opts <- renderUI({
      o <- net_options(input$type %||% "factorial")
      tagList(
        if ("procedure" %in% o) selectInput(ns("procedure"), "Unir dos categorías cuando…",
          stats::setNames(similarity_measures,
            ifelse(names(similarity_measures) == "Haberman (residuos ajustados)",
                   "coinciden más de lo esperado (Haberman)", names(similarity_measures))),
          selected = keep("procedure", "Haberman")),
        if ("method" %in% o) selectInput(ns("method"), "Tipo de correlación",
          c("Pearson (relación lineal)" = "pearson", "Spearman (por rangos)" = "spearman", "Kendall (por rangos, muestras pequeñas)" = "kendall"),
          selected = keep("method", "pearson")),
        if ("order" %in% o) radioButtons(ns("order"), "Orden del modelo log-lineal",
          c("2 · asociaciones entre pares" = 2, "3 · también entre tríos" = 3), selected = keep("order", 2)),
        if ("nclusters" %in% o) tagList(
          textInput(ns("nclusters"), "Número de conglomerados", value = keep("nclusters", "2"),
                    placeholder = "2"),
          p(class = "c-hint", style = "margin-top:-8px",
            "Un número (3), un rango (2:4), varios (3, 5) o ", tags$code("auto"),
            " para que se elija solo (también ", tags$code("auto, 2:6"),
            "). Con varios, el último colorea el gráfico. 0 = sin conglomerados.")),
        if ("clusters" %in% o) selectInput(ns("clusters"), "Método de conglomerados",
          c("K-medias" = "kmeans", "PAM (medoides)" = "pam", "Jerárquico" = "hclust"), selected = keep("clusters", "kmeans")),
        if ("signif" %in% o) checkboxInput(ns("signif"), "Mostrar solo relaciones significativas (p < 0,05)", value = keep("signif", TRUE)),
        if ("community" %in% o) selectInput(ns("community"), "Agrupar en comunidades", community_methods,
          selected = keep("community", "louvain")),
        if ("labels" %in% o) selectInput(ns("labels"), "Nombres de los nodos", node_label_styles,
          selected = keep("labels", "legible"))
      )
    })

    output$adv_opts <- renderUI({
      o <- net_options(input$type %||% "factorial")
      tagList(
        if ("bonferroni" %in% o) checkboxInput(ns("bonferroni"), "Corrección de Bonferroni (más exigente con muchas variables)",
                                               value = keep("bonferroni", FALSE)),
        if ("min_freq" %in% o) numericInput(ns("min_freq"), "Frecuencia mínima de una categoría (casos)",
                                            value = keep("min_freq", 0), min = 0),
        if ("degree" %in% o) numericInput(ns("degree"), "Ocultar nodos con menos de … enlaces", value = keep("degree", 0), min = 0),
        if ("layout" %in% o) selectInput(ns("layout"), "Disposición de los nodos", layout_methods, selected = keep("layout", "")),
        sliderInput(ns("cex"), "Tamaño del texto", min = 0.6, max = 2, value = keep("cex", 1), step = 0.1)
      )
    })


    # Solo se envían las opciones que corresponden al tipo de análisis elegido.
    spec <- reactive({
      t <- input$type %||% "factorial"
      o <- net_options(t)
      val <- function(k, v) if (k %in% o) v
      compact(list(type = t, vars = input$vars, dep = if (t == "regresion") input$dep,
           ftype = if (t == "factorial") input$f_type,
           passive = if (t == "factorial") setdiff(input$passive, input$vars),
           weight = input$weight, title = input$title, note = input$note, cex = input$cex,
           sep = if (t == "multiple") input$sep, cross = if (t == "multiple") input$cross,
           signif = if ("signif" %in% o) isTRUE(input$signif),
           procedure = val("procedure", input$procedure), method = val("method", input$method),
           community = val("community", input$community), layout = val("layout", input$layout),
           bonferroni = if ("bonferroni" %in% o) isTRUE(input$bonferroni),
           min_freq = val("min_freq", input$min_freq), degree = val("degree", input$degree),
           nclusters = val("nclusters", input$nclusters), clusters = val("clusters", input$clusters),
           labels = val("labels", input$labels),
           name = if (t == "evolucion") input$t_name, start = if (t == "evolucion") input$t_start,
           end = if (t == "evolucion") input$t_end, group = if (t == "evolucion") input$t_group,
           info = if (t == "evolucion") input$t_info, model = if (t == "trayectorias") input$model,
           order = if ("order" %in% o) as.integer(input$order %||% 2)))
    })

    observeEvent(input$go, {
      s <- spec()
      d <- data()
      if (!nzchar(s$title %||% "")) { s$title <- default_net_title(d, s); s$auto_title <- TRUE }
      current$error <- NULL
      res <- withProgress(message = "Calculando la red…", value = 0.3, tryCatch({
        net <- suppressWarnings(net_build(d, s))
        incProgress(0.4, message = "Dibujando…")
        f <- tempfile(fileext = ".html")
        net_render_html(net, f)
        list(net = net, html = f)
      }, error = function(e) e))
      if (inherits(res, "error")) {
        current$error <- error_es(res)
        current$html <- NULL
        return()
      }
      if (!is.null(current$html)) unlink(current$html)
      current$net <- res$net; current$html <- res$html; current$spec <- s; current$title <- s$title
      copia <- save_html_copy(res$html, s$title)
      if (!is.null(copia)) showNotification(paste("Red guardada también en", file.path(basename(dirname(copia)), basename(copia))),
                                            type = "message", duration = 4)
    })

    output$net_title <- renderText(current$title %||% "Red")
    output$net_info <- renderText({
      if (is.null(current$net)) return("Elige el tipo de análisis y las variables, y pulsa «Crear red».")
      sm <- net_summary(current$net)
      if (current$spec$type == "factorial") {
        return(paste0("Nube de puntos (factorial) · ", fmt_int(sm$nodes), " puntos"))
      }
      if (current$spec$type == "evolucion") return(paste0("Evolución temporal · ", fmt_int(sm$nodes), " periodos"))
      if (current$spec$type == "trayectorias") {
        return(paste0("Ecuaciones estructurales · ", sm$nodes, " variables · ", sm$links, " relaciones · ",
                      attr(current$net, "fit_text") %||% ""))
      }
      paste0(net_types[[current$spec$type]]$titulo,
             if (!is.na(sm$nodes)) paste0(" · ", sm$nodes, " nodos"),
             " · ", sm$links, if (sm$links == 1) " enlace" else " enlaces",
             if (isTRUE(current$spec$signif)) (if (sm$links == 1) " significativo" else " significativos") else "",
             if (sm$links == 0) ". Prueba a desmarcar «solo relaciones significativas» o a añadir variables." else "")
    })

    output$net_actions <- renderUI({
      req(current$html)
      div(class = "d-flex flex-wrap gap-2",
        actionButton(ns("about"), "Sobre tus datos", class = "btn-light-c c-about-btn"),
        actionButton(ns("fullscreen"), "Pantalla completa", class = "btn-light-c"),
        downloadButton(ns("dl_html"), "Descargar HTML", class = "btn-light-c", icon = NULL),
        actionButton(ns("show_code"), "Código R", class = "btn-light-c"),
        actionButton(ns("add"), "+ Añadir al informe", class = "btn-primary"))
    })

    output$net_view <- renderUI({
      if (!is.null(current$error)) {
        return(div(class = "c-empty", tags$h3("No se ha podido crear la red"), div(class = "c-msg-error", current$error)))
      }
      if (is.null(current$html)) {
        return(div(class = "c-empty", HTML(as.character(net_placeholder_svg())),
                   p("Aquí aparecerá la red interactiva.")))
      }
      html <- paste(readLines(current$html, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      tags$iframe(class = "c-net-frame", srcdoc = html, title = current$title)
    })

    observeEvent(input$about, {
      req(current$net)
      about_modal(tryCatch(explain_network(data(), current$spec, current$net),
                           error = function(e) p("No se ha podido preparar la explicación: ", error_es(e))))
    })

    observeEvent(input$fullscreen, {
      req(current$html)
      session$sendCustomMessage("caring-open-html",
        list(html = paste(readLines(current$html, warn = FALSE, encoding = "UTF-8"), collapse = "\n")))
    })

    output$dl_html <- downloadHandler(
      filename = function() paste0(safe_filename(current$title), ".html"),
      content = function(file) file.copy(current$html, file)
    )

    observeEvent(input$show_code, {
      req(current$spec)
      showModal(modalDialog(title = "Código R de esta red", size = "l", easyClose = TRUE,
        p(class = "c-hint", "Para reproducirla, ejecuta primero el script de preparación de datos (botón «Código R» de la barra del proyecto)."),
        tags$pre(class = "c-code", net_code(current$spec)), footer = modalButton("Cerrar")))
    })

    observeEvent(input$add, {
      req(current$net)
      if (!ctx$can_edit()) return(showNotification("Proyecto de solo lectura.", type = "error"))
      withProgress(message = "Guardando la red y su imagen…", value = 0.4, {
        png <- tempfile(fileext = ".png")
        r <- net_render(current$net, tempfile(fileext = ".html"), png)
        save_result(ctx$proj$dir, "red", current$title, data_hash = recipe_hash(ctx$proj$recipe),
                    list(spec = current$spec, summary = net_summary(current$net)),
                    png = r$png, html = current$html, source = "Redes")
        unlink(c(png, r$html))
      })
      ctx$results_changed()
      showNotification("Red añadida a los resultados del proyecto. La encontrarás en Informes.", type = "message")
    })
  })
}

default_net_title <- function(d, s) {
  base <- net_types[[s$type]]$titulo
  if (s$type == "evolucion") return(paste(base, "·", if (length(s$name) && s$name %in% names(d)) var_label(d, s$name) else ""))
  if (s$type == "trayectorias") return(base)
  if (s$type == "regresion" && length(s$dep) && nzchar(s$dep)) return(paste(base, "·", var_label(d, s$dep)))
  vars <- s$vars
  nombre <- function(v) if (startsWith(v, "mr:")) mr_label(mrsets(d)[[sub("^mr:", "", v)]] %||% list(name = v)) else var_label(d, v)
  if (s$type == "multiple" && length(s$cross)) {
    return(paste(base, "·", paste(vapply(vars, nombre, ""), collapse = ", "), "según",
                 paste(tolower(vapply(s$cross, function(v) var_label(d, v), "")), collapse = ", ")))
  }
  if (length(vars) && length(vars) <= 3) paste(base, "·", paste(vapply(vars, nombre, ""), collapse = ", "))
  else paste(base, "·", length(vars), "variables")
}

net_code <- function(s) {
  q <- function(x) paste0('"', x, '"')
  if (s$type == "evolucion") {
    return(paste(c("# Evolución temporal generada con CARING 2 (netCoin::timeCoin)", "library(netCoin)",
      "# 'datos' es el resultado del script de preparación",
      paste0("periodos <- datos[!is.na(datos[[", q(s$start), "]]), ]"),
      paste0("timeCoin(periodos, name = ", q(s$name), ", start = ", q(s$start), ", end = ", q(s$end),
             if (nzchar(s$group %||% "")) paste0(", group = ", q(s$group)), ", language = \"es\")")), collapse = "\n"))
  }
  if (s$type == "trayectorias") {
    return(paste(c("# Modelo de ecuaciones estructurales generado con CARING 2 (lavaan + netCoin)",
      "library(lavaan); library(netCoin)", "# 'datos' es el resultado del script de preparación",
      "modelo <- '", s$model %||% "", "'",
      paste0("ajuste <- sem(modelo, data = datos, missing = \"ml\"",
             if (nzchar(s$weight %||% "")) paste0(", sampling.weights = ", q(s$weight), ", estimator = \"MLR\""), ")"),
      "summary(ajuste, standardized = TRUE, fit.measures = TRUE)",
      "pathCoin(ajuste, language = \"es\")  # CARING dibuja además las variables latentes y sus indicadores"), collapse = "\n"))
  }
  fun <- c(coincidencias = "surCoin", correlaciones = "netCorr", factorial = "surScat",
           loglineal = "logCoin", regresion = "glmCoin", multiple = "allNet")[[s$type]]
  args <- compact(list(
    variables = if (length(s$vars)) paste0("c(", paste0('"', s$vars, '"', collapse = ", "), ")"),
    dependiente = if (length(s$dep) && nzchar(s$dep)) paste0('"', s$dep, '"'),
    weight = if (nzchar(s$weight %||% "")) paste0('"', s$weight, '"'),
    procedures = if (s$type %in% c("coincidencias", "multiple")) paste0('"', s$procedure %||% "Haberman", '"'),
    significativas = if (isTRUE(s$signif)) "maxL = 0.05" else "maxL = 0.5",
    community = if (nzchar(s$community %||% "")) paste0('"', s$community, '"'),
    main = if (nzchar(s$title %||% "")) paste0('"', s$title, '"'),
    language = '"es"'
  ))
  c(paste0("# Red generada con CARING 2 (netCoin::", fun, ")"),
    "library(netCoin)",
    "# 'datos' es el resultado del script de preparación",
    paste0("especificacion <- list(type = \"", s$type, "\"",
           paste0(",\n  ", names(args), " = ", unlist(args), collapse = ""), ")"),
    "# CARING traduce esta especificación en la llamada a netCoin:",
    "red <- caring2:::net_build(datos, list(",
    paste0("  type = \"", s$type, "\", vars = c(", paste0('"', s$vars, '"', collapse = ", "), ")",
           if (length(s$ftype)) paste0(", ftype = \"", s$ftype, "\""),
           if (length(s$passive)) paste0(", passive = c(", paste0('"', s$passive, '"', collapse = ", "), ")"),
           if (length(s$dep) && nzchar(s$dep)) paste0(", dep = \"", s$dep, "\""),
           if (nzchar(s$weight %||% "")) paste0(", weight = \"", s$weight, "\""),
           ", signif = ", isTRUE(s$signif),
           if (nzchar(s$community %||% "")) paste0(", community = \"", s$community, "\""), "))"),
    "plot(red)  # abre la red en el navegador") |> paste(collapse = "\n")
}
