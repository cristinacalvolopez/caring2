# Paso 3 · Redes ----------------------------------------------------------------------------

mod_redes_ui <- function(id) {
  ns <- NS(id)
  types <- names(net_types)
  div(class = "c-main", style = "max-width:1480px",
    div(class = "row g-4",
      div(class = "col-xl-3 col-lg-4",
        div(class = "c-card c-card-pad mb-3 c-analysis",
          tags$h2("¿Qué quieres ver?"),
          radioButtons(ns("type"), NULL, selected = "coincidencias",
            choiceValues = types,
            choiceNames = lapply(types, function(t) tagList(
              span(class = "a-title", net_types[[t]]$titulo),
              span(class = "a-desc", net_types[[t]]$desc))))
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

    output$var_inputs <- renderUI({
      d <- data()
      t <- input$type %||% "coincidencias"
      cats <- labelled_choices(d, categorical_vars(d))
      nums <- labelled_choices(d, numeric_vars(d))
      txt <- labelled_choices(d, names(d)[vapply(d, is.character, logical(1))])
      prev <- isolate(input$vars)
      multi <- function(choices, label, help) tagList(
        selectizeInput(ns("vars"), label, choices = choices, multiple = TRUE,
                       selected = intersect(prev, choices),
                       options = list(plugins = list("remove_button"), placeholder = "Elige variables…")),
        p(class = "c-hint", style = "margin-top:-8px", help))
      switch(t,
        coincidencias = multi(cats, "Variables", "Categóricas. Cada categoría será un nodo. Las numéricas se pueden agrupar en tramos en Datos."),
        loglineal = multi(cats, "Variables", "Categóricas. Mejor pocas variables (3–6)."),
        correlaciones = multi(nums, "Variables", "Numéricas. Cada variable será un nodo."),
        factorial = multi(c(cats, nums), "Variables", "Categóricas o numéricas."),
        regresion = tagList(
          selectInput(ns("dep"), "Variable que quieres explicar", choices = c(cats, nums)),
          multi(c(cats, nums), "Variables explicativas", "Pueden ser categóricas o numéricas.")),
        multiple = tagList(
          selectInput(ns("vars"), "Variable de respuesta múltiple", choices = txt),
          textInput(ns("sep"), "Separador de las opciones", value = ";"),
          p(class = "c-hint", style = "margin-top:-8px", "Una variable de texto con varias respuestas por caso, por ejemplo «deporte;cine;viajes»."))
      )
    })

    # Opciones de cada tipo de análisis (net_options() dice cuáles corresponden a cada uno)
    keep <- function(id, default) { v <- isolate(input[[id]]); if (is.null(v)) default else v }

    output$main_opts <- renderUI({
      o <- net_options(input$type %||% "coincidencias")
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
        if ("nclusters" %in% o) numericInput(ns("nclusters"), "Número de conglomerados (0 = ninguno)",
          value = keep("nclusters", 0), min = 0, max = 12),
        if ("clusters" %in% o) selectInput(ns("clusters"), "Método de conglomerados",
          c("K-medias" = "kmeans", "PAM (medoides)" = "pam", "Jerárquico" = "hclust"), selected = keep("clusters", "kmeans")),
        if ("signif" %in% o) checkboxInput(ns("signif"), "Mostrar solo relaciones significativas (p < 0,05)", value = keep("signif", TRUE)),
        if ("community" %in% o) selectInput(ns("community"), "Agrupar en comunidades", community_methods,
          selected = keep("community", "louvain"))
      )
    })

    output$adv_opts <- renderUI({
      o <- net_options(input$type %||% "coincidencias")
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
      t <- input$type %||% "coincidencias"
      o <- net_options(t)
      val <- function(k, v) if (k %in% o) v
      compact(list(type = t, vars = input$vars, dep = if (t == "regresion") input$dep,
           weight = input$weight, title = input$title, note = input$note, cex = input$cex,
           sep = if (t == "multiple") input$sep,
           signif = if ("signif" %in% o) isTRUE(input$signif),
           procedure = val("procedure", input$procedure), method = val("method", input$method),
           community = val("community", input$community), layout = val("layout", input$layout),
           bonferroni = if ("bonferroni" %in% o) isTRUE(input$bonferroni),
           min_freq = val("min_freq", input$min_freq), degree = val("degree", input$degree),
           nclusters = val("nclusters", input$nclusters), clusters = val("clusters", input$clusters),
           order = if ("order" %in% o) as.integer(input$order %||% 2)))
    })

    observeEvent(input$go, {
      s <- spec()
      d <- data()
      if (!nzchar(s$title %||% "")) s$title <- default_net_title(d, s)
      current$error <- NULL
      res <- withProgress(message = "Calculando la red…", value = 0.3, tryCatch({
        net <- suppressWarnings(net_build(d, s))
        incProgress(0.4, message = "Dibujando…")
        f <- tempfile(fileext = ".html")
        net_render_html(net, f)
        list(net = net, html = f)
      }, error = function(e) e))
      if (inherits(res, "error")) {
        current$error <- conditionMessage(res)
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
        return(paste0("Mapa factorial · ", fmt_int(sm$nodes), " puntos"))
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
        save_result(ctx$proj$dir, "red", current$title,
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
  if (s$type == "regresion" && length(s$dep) && nzchar(s$dep)) return(paste(base, "·", var_label(d, s$dep)))
  vars <- s$vars
  if (length(vars) && length(vars) <= 3) paste(base, "·", paste(vapply(vars, function(v) var_label(d, v), ""), collapse = ", "))
  else paste(base, "·", length(vars), "variables")
}

net_code <- function(s) {
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
           if (length(s$dep) && nzchar(s$dep)) paste0(", dep = \"", s$dep, "\""),
           if (nzchar(s$weight %||% "")) paste0(", weight = \"", s$weight, "\""),
           ", signif = ", isTRUE(s$signif),
           if (nzchar(s$community %||% "")) paste0(", community = \"", s$community, "\""), "))"),
    "plot(red)  # abre la red en el navegador") |> paste(collapse = "\n")
}
