# Paso 1 · Datos ------------------------------------------------------------------------

mod_datos_ui <- function(id) {
  ns <- NS(id)
  div(class = "c-main",
    div(class = "c-page-title",
      div(tags$h1("Datos"), div(class = "text-2", textOutput(ns("summary"), inline = TRUE))),
      seg_radio(ns("view"), c("Variables" = "variables", "Tabla de casos" = "casos", "Filtrar casos" = "filtro",
                              "Revisar" = "revisar"))
    ),
    conditionalPanel("input.view == 'variables'", ns = ns,
      div(class = "row g-4",
        div(class = "col-lg-7",
          div(class = "c-card",
            div(class = "d-flex flex-wrap gap-2 justify-content-between align-items-center",
                style = "padding:12px 16px; border-bottom:1px solid #E3E3E3",
              span(class = "c-hint", "Pulsa una variable para editarla."),
              div(class = "d-flex flex-wrap gap-2",
                actionButton(ns("new_mr"), "+ Conjunto de respuesta múltiple", class = "btn-outline-primary"),
                datos_tools_menu(ns),
                actionButton(ns("new_var"), "+ Nueva variable", class = "btn-primary"))),
            uiOutput(ns("label_notice")),
            div(style = "padding:4px 8px 8px", reactable::reactableOutput(ns("vars")))
          )
        ),
        div(class = "col-lg-5",
          # Columna flotante: acompaña al usuario mientras recorre la lista de variables
          div(class = "c-sticky-col",
            div(class = "c-card c-card-pad mb-4", uiOutput(ns("editor"))),
            div(class = "c-card c-card-pad mb-4", uiOutput(ns("mrsets_ui"))),
            div(class = "c-card c-card-pad c-history",
              div(class = "d-flex justify-content-between align-items-center mb-2",
                tags$h2("Historial"),
                downloadLink(ns("dl_script"), "Descargar script R")),
              uiOutput(ns("history"))))
        )
      )
    ),
    conditionalPanel("input.view == 'casos'", ns = ns,
      div(class = "c-card c-card-pad",
        uiOutput(ns("cases_note")),
        reactable::reactableOutput(ns("cases")))
    ),
    conditionalPanel("input.view == 'revisar'", ns = ns, datos_review_ui(ns)),
    conditionalPanel("input.view == 'filtro'", ns = ns,
      div(class = "row g-4",
        div(class = "col-lg-7",
          div(class = "c-card c-card-pad",
            tags$h2("Quedarse solo con algunos casos"),
            p(class = "c-hint", "Escribe una condición. Los casos que no la cumplan no se usarán en los análisis (el archivo original no se modifica)."),
            textInput(ns("filter_expr"), "Condición", placeholder = "edad >= 18 & genero == \"Mujer\""),
            uiOutput(ns("filter_preview")),
            actionButton(ns("filter_apply"), "Aplicar filtro", class = "btn-primary"),
            tags$hr(),
            formula_help())),
        div(class = "col-lg-5",
          div(class = "c-card c-card-pad",
            tags$h2("Filtros activos"),
            uiOutput(ns("filters"))))
      )
    )
  )
}

catlabels_ui <- function(ns, x) {
  lv <- levels(x)
  if (length(lv) > 60) return(p(class = "c-hint", "Esta variable tiene demasiadas categorías (", length(lv), ") para editarlas una a una. Recodifícala antes."))
  n <- tabulate(as.integer(x), nbins = length(lv))      # por posición: admite categorías vacías o NA
  tagList(
    p(class = "c-hint", "Cambia el texto de cada categoría. Si dos categorías quedan con la misma etiqueta, se unen en una."),
    div(class = "c-label-grid",
      lapply(seq_along(lv), function(i) div(class = "c-label-row",
        span(class = "small-2", fmt_int(n[i]), " casos"),
        textInput(ns(paste0("cat_", i)), NULL, value = if (is.na(lv[i])) "" else lv[i], width = "100%")))),
    actionButton(ns("save_cats"), "Guardar etiquetas", class = "btn-primary"))
}

formula_help <- function() {
  tags$details(
    tags$summary(class = "text-primary", style = "cursor:pointer", "Ayuda para escribir fórmulas y condiciones"),
    div(class = "c-hint", style = "margin-top:8px",
      tags$ul(
        tags$li(tags$code("edad >= 18"), " · comparaciones: ", tags$code("== != < > <= >=")),
        tags$li(tags$code("genero == \"Mujer\" & internet == \"Sí\""), " · y (", tags$code("&"), "), o (", tags$code("|"), "), no (", tags$code("!"), ")"),
        tags$li(tags$code("region %in% c(\"Norte\", \"Sur\")"), " · pertenece a una lista"),
        tags$li(tags$code("!is.na(ingresos)"), " · sin valor perdido"),
        tags$li(tags$code("log(ingresos)"), ", ", tags$code("sqrt()"), ", ", tags$code("round(x, 1)"), ", ", tags$code("abs()")),
        tags$li(tags$code("ifelse(edad < 30, \"Joven\", \"Adulto\")"), " · condicional"),
        tags$li(tags$code("rowMeans(cbind(p1, p2, p3))"), " · media de varias variables"),
        tags$li(tags$code("(x - mean(x)) / sd(x)"), " · tipificar (las medias ignoran los perdidos)")
      ),
      p("Los textos van entre comillas. Las categorías se escriben tal como aparecen en la lista.")
    )
  )
}

mod_datos_server <- function(id, ctx) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data <- ctx$data
    proj <- ctx$proj
    sel <- reactiveVal(NULL)
    datos_tools_server(input, output, session, ctx, sel)

    output$summary <- renderText({
      d <- data()
      paste0(proj$info$archivo, " · ", fmt_int(nrow(d)), " casos · ", ncol(d), " variables",
             if (length(proj$recipe)) paste0(" · ", length(proj$recipe), " cambios") else "")
    })

    # Lista de variables -----------------------------------------------------------
    vinfo <- reactive(var_info(data()))

    output$vars <- reactable::renderReactable({
      vi <- vinfo()
      s <- isolate(sel())
      vi$perdidos_txt <- fmt_pct(vi$perdidos)
      reactable::reactable(
        vi[c("nombre", "etiqueta", "tipo", "valores", "perdidos_txt")],
        selection = "single", searchable = TRUE, highlight = TRUE,
        onClick = reactable::JS(sprintf(
          "function(rowInfo) { rowInfo.toggleRowSelected(true); Shiny.setInputValue('%s', rowInfo.values.nombre, {priority: 'event'}); }",
          ns("pick"))),
        defaultSelected = if (!is.null(s) && s %in% vi$nombre) which(vi$nombre == s),
        pagination = nrow(vi) > 40, defaultPageSize = 40, compact = TRUE,
        language = reactable_es(),
        rowStyle = list(cursor = "pointer"),
        columns = list(
          .selection = reactable::colDef(show = FALSE),
          nombre = reactable::colDef(name = "Nombre", minWidth = 110,
                                     style = list(fontFamily = "ui-monospace, Menlo, monospace", fontSize = "13px")),
          etiqueta = reactable::colDef(name = "Etiqueta", minWidth = 150),
          tipo = reactable::colDef(name = "Tipo", minWidth = 95, cell = function(v) type_chip(v)),
          valores = reactable::colDef(name = "Valores", minWidth = 120),
          perdidos_txt = reactable::colDef(name = "Perdidos", minWidth = 80, align = "right",
            style = function(v, i) if (vi$perdidos[i] > 0.05) list(color = "#B3261E") else NULL)
        )
      )
    })

    observeEvent(input$pick, sel(input$pick))

    # Editor de variable -------------------------------------------------------------
    output$editor <- renderUI({
      v <- sel()
      d <- data()
      if (is.null(v) || !v %in% names(d)) {
        return(div(class = "c-empty", style = "padding:40px 10px",
          tags$h2("Editar variable"),
          p("Elige una variable de la lista para renombrarla, recodificarla, marcar perdidos o cambiar su tipo.")))
      }
      x <- d[[v]]
      ro <- !ctx$can_edit()
      tagList(
        span(class = "small-2", "Editar variable"),
        tags$h2(var_label(d, v), span(class = "small-2", style = "font-weight:400", paste0(" (", v, ")"))),
        if (ro) div(class = "alert alert-info", "Proyecto de solo lectura: no puedes hacer cambios."),
        div(class = "c-edit-tabs",
          bslib::navset_underline(id = ns("etab"),
            bslib::nav_panel("Renombrar", value = "nombre", div(style = "padding-top:14px",
              textInput(ns("new_name"), "Nombre (sin espacios)", value = v),
              textInput(ns("new_label"), "Etiqueta", value = attr(x, "label", exact = TRUE) %||% "",
                        placeholder = paste("Sin etiqueta · propuesta:", humanize_name(v))),
              div(class = "d-flex justify-content-between",
                actionButton(ns("save_name"), "Guardar", class = "btn-primary"),
                actionButton(ns("drop_var"), "Eliminar variable", class = "btn-danger-c")))),
            if (is.factor(x)) bslib::nav_panel("Categorías", value = "cats",
              div(style = "padding-top:14px", catlabels_ui(ns, x))),
            bslib::nav_panel(if (is.numeric(x)) "Tramos" else "Recodificar", value = "recode",
              div(style = "padding-top:14px", recode_ui(ns, d, v))),
            bslib::nav_panel("Perdidos", value = "missing",
              div(style = "padding-top:14px", missing_ui(ns, x))),
            bslib::nav_panel("Tipo", value = "type",
              div(style = "padding-top:14px", type_ui(ns, x)))
          )
        )
      )
    })

    output$recode_groups <- renderUI({
      v <- sel(); d <- data()
      req(v, v %in% names(d), is.factor(d[[v]]))
      k <- max(2, min(10, input$ngroups %||% 2))
      lv <- levels(d[[v]])
      div(class = "c-bucket",
        do.call(sortable::bucket_list, c(
          list(header = NULL, group_name = ns("bucket"), orientation = "vertical",
               sortable::add_rank_list("Sin agrupar", labels = lv, input_id = ns("grp_0"))),
          lapply(seq_len(k), function(i) sortable::add_rank_list(
            paste("Grupo", i), labels = NULL, input_id = ns(paste0("grp_", i)))))),
        div(class = "row g-2",
          lapply(seq_len(k), function(i) div(class = "col-6",
            textInput(ns(paste0("gname_", i)), paste("Nombre del grupo", i), value = paste("Grupo", i)))))
      )
    })

    output$cut_preview <- renderUI({
      v <- sel(); d <- data()
      req(v, is.numeric(d[[v]]))
      br <- parse_numbers(input$cuts)
      if (!length(br)) return(p(class = "c-hint", "Escribe al menos un punto de corte."))
      st <- tryCatch(apply_step(d, list(op = "cut", var = v, to = ".tmp", breaks = br)), error = function(e) NULL)
      if (is.null(st)) return(NULL)
      tb <- table(st$.tmp, useNA = "no")
      html_table(data.frame(Tramo = names(tb), Casos = fmt_int(as.integer(tb)), check.names = FALSE))
    })

    observeEvent(input$save_name, {
      v <- sel(); d <- data()
      ok <- TRUE
      nl <- trimws(input$new_label %||% "")
      if (!identical(nl, attr(d[[v]], "label", exact = TRUE) %||% "")) {
        ok <- ctx$add_step(list(op = "label", var = v, label = nl))
      }
      nn <- trimws(input$new_name %||% "")
      if (ok && nzchar(nn) && nn != v) {
        if (ctx$add_step(list(op = "rename", var = v, to = nn))) sel(nn)
      }
    })

    # Etiquetas de las categorías -----------------------------------------------------
    observeEvent(input$save_cats, {
      v <- sel(); d <- data()
      req(v, v %in% names(d), is.factor(d[[v]]))
      lv <- levels(d[[v]])
      nuevos <- vapply(seq_along(lv), function(i) trimws(input[[paste0("cat_", i)]] %||% lv[i] %||% ""), "")
      if (any(!nzchar(nuevos))) return(showNotification("Ninguna categoría puede quedar sin etiqueta.", type = "error"))
      if (anyNA(lv)) return(showNotification("Esta variable tiene una categoría «NA»: márcala antes como perdido en la pestaña Perdidos.", type = "error"))
      cambia <- nuevos != lv
      if (!any(cambia)) return(showNotification("No has cambiado ninguna etiqueta.", type = "message"))
      ctx$add_step(list(op = "catlabels", var = v, map = as.list(stats::setNames(nuevos[cambia], lv[cambia]))))
    })

    # Variables sin etiqueta: aviso y propuesta ---------------------------------------
    output$label_notice <- renderUI({
      faltan <- unlabelled_vars(data())
      if (!length(faltan)) return(NULL)
      div(class = "c-label-notice",
        span(HTML(paste0("<strong>", length(faltan), "</strong>",
             if (length(faltan) == 1) " variable no tiene etiqueta" else " variables no tienen etiqueta",
             ": se muestran con un nombre legible propuesto (<em>", htmltools::htmlEscape(humanize_name(faltan[1])), "</em>",
             if (length(faltan) > 1) ", …", ")."))),
        if (ctx$can_edit()) actionLink(ns("labels_open"), "Revisar y poner etiquetas"))
    })

    observeEvent(input$labels_open, {
      faltan <- unlabelled_vars(data())
      req(length(faltan))
      lista <- utils::head(faltan, 150)
      showModal(modalDialog(title = "Poner etiquetas a las variables", size = "l", easyClose = TRUE,
        p(class = "c-hint", "Te proponemos una etiqueta a partir del nombre de cada variable. Corrígela si hace falta; ",
          "deja vacía la casilla de las que no quieras etiquetar. Las etiquetas aparecen en tablas, gráficos y redes."),
        if (length(faltan) > length(lista)) p(class = "c-hint", "Se muestran las ", length(lista), " primeras de ", length(faltan), "."),
        div(class = "c-label-grid",
          lapply(seq_along(lista), function(i) div(class = "c-label-row",
            tags$code(lista[i]),
            textInput(ns(paste0("lab_", i)), NULL, value = humanize_name(lista[i]), width = "100%")))),
        footer = tagList(modalButton("Cancelar"), actionButton(ns("labels_ok"), "Guardar etiquetas", class = "btn-primary"))))
    })
    observeEvent(input$labels_ok, {
      lista <- utils::head(unlabelled_vars(data()), 150)
      labs <- vapply(seq_along(lista), function(i) trimws(input[[paste0("lab_", i)]] %||% ""), "")
      keep <- nzchar(labs)
      removeModal()
      if (!any(keep)) return()
      ctx$add_step(list(op = "labels", labels = as.list(stats::setNames(labs[keep], lista[keep]))))
    })

    observeEvent(input$drop_var, {
      showModal(modalDialog(title = "Eliminar variable", easyClose = TRUE,
        p("La variable «", sel(), "» dejará de estar disponible en los análisis. Puedes recuperarla quitando este paso del historial."),
        footer = tagList(modalButton("Cancelar"), actionButton(ns("drop_ok"), "Eliminar", class = "btn-danger-c"))))
    })
    observeEvent(input$drop_ok, {
      removeModal()
      if (ctx$add_step(list(op = "drop", vars = sel()))) sel(NULL)
    })

    observeEvent(input$do_recode, {
      v <- sel(); d <- data()
      to <- trimws(input$recode_name %||% "")
      if (is.numeric(d[[v]])) {
        br <- parse_numbers(input$cuts)
        if (!length(br)) return(showNotification("Escribe los puntos de corte separados por comas.", type = "error"))
        if (ctx$add_step(list(op = "cut", var = v, to = to, breaks = br,
                              label = paste(var_label(d, v), "(tramos)")))) sel(to)
      } else {
        k <- max(2, min(10, input$ngroups %||% 2))
        groups <- list()
        for (i in seq_len(k)) {
          items <- input[[paste0("grp_", i)]]
          if (length(items)) {
            nm <- trimws(input[[paste0("gname_", i)]] %||% paste("Grupo", i))
            groups <- c(groups, list(list(name = nm, from = unlist(items))))
          }
        }
        if (!length(groups)) return(showNotification("Arrastra al menos una categoría a un grupo.", type = "error"))
        if (anyDuplicated(vapply(groups, `[[`, "", "name"))) return(showNotification("Los grupos deben tener nombres distintos.", type = "error"))
        if (ctx$add_step(list(op = "recode", var = v, to = to, groups = groups,
                              others = input$others %||% "na", label = var_label(d, v)))) sel(to)
      }
    })

    observeEvent(input$do_missing, {
      v <- sel(); x <- data()[[v]]
      vals <- if (is.factor(x)) input$missing_levels else parse_numbers(input$missing_values)
      if (!length(vals)) return(showNotification("Elige al menos un valor.", type = "error"))
      ctx$add_step(list(op = "missing", var = v, values = vals))
    })

    observeEvent(input$do_type, {
      v <- sel(); x <- data()[[v]]
      if (identical(input$new_type, var_type(x))) return()
      ctx$add_step(list(op = "type", var = v, type = input$new_type))
    })

    observeEvent(input$do_levels, {
      v <- sel(); x <- data()[[v]]
      lv <- unlist(input$level_order)
      if (!length(lv) || identical(lv, levels(x))) return()
      ctx$add_step(list(op = "levels", var = v, levels = lv))
    })

    # Nueva variable ---------------------------------------------------------------------
    observeEvent(input$new_var, {
      if (!ctx$can_edit()) return(showNotification("Este proyecto es de solo lectura.", type = "error"))
      showModal(modalDialog(title = "Nueva variable", size = "l", easyClose = TRUE,
        div(class = "row",
          div(class = "col-md-6", textInput(ns("nv_name"), "Nombre", placeholder = "edad_decadas")),
          div(class = "col-md-6", textInput(ns("nv_label"), "Etiqueta (opcional)"))),
        textAreaInput(ns("nv_expr"), "Fórmula", rows = 2, placeholder = "round(edad / 10)"),
        div(class = "d-flex gap-2 align-items-center mb-2",
          selectizeInput(ns("nv_insert"), NULL, choices = c("Insertar variable…" = "", names(data())), width = "260px"),
          span(class = "c-hint", "o escribe su nombre directamente")),
        uiOutput(ns("nv_preview")),
        formula_help(),
        footer = tagList(modalButton("Cancelar"), actionButton(ns("nv_create"), "Crear variable", class = "btn-primary"))))
    })

    observeEvent(input$nv_insert, {
      req(nzchar(input$nv_insert))
      updateTextAreaInput(session, "nv_expr", value = paste0(input$nv_expr %||% "", input$nv_insert))
      updateSelectizeInput(session, "nv_insert", selected = "")
    })

    output$nv_preview <- renderUI({
      e <- trimws(input$nv_expr %||% "")
      if (!nzchar(e)) return(NULL)
      r <- tryCatch(eval_safe(e, data()), error = function(err) err)
      if (inherits(r, "error")) return(div(class = "c-msg-error", error_es(r)))
      if (length(r) != nrow(data()) && length(r) != 1) return(div(class = "c-msg-error", "La fórmula no da un valor por caso."))
      ej <- utils::head(r, 6)
      div(class = "c-stats mb-2", "Primeros valores: ", paste(format(ej, digits = 3), collapse = " · "),
          if (is.numeric(r)) paste0(" · media ", fmt_num(mean(r, na.rm = TRUE))))
    })

    observeEvent(input$nv_create, {
      nm <- trimws(input$nv_name %||% "")
      ok <- ctx$add_step(list(op = "compute", to = nm, expr = trimws(input$nv_expr %||% ""),
                              label = trimws(input$nv_label %||% "")))
      if (ok) { removeModal(); sel(nm) }
    })

    # Conjuntos de respuesta múltiple ----------------------------------------------------
    output$mrsets_ui <- renderUI({
      d <- data()
      sets <- mrsets(d)
      sug <- mr_suggest(d)
      tipo <- function(t) switch(t, dicotomias = "dicotomías", categorias = "categorías", "texto")
      tagList(
        tags$h2("Respuesta múltiple"),
        if (!length(sets)) p(class = "c-hint", "Agrupa en un conjunto las variables de una pregunta de respuesta múltiple ",
                             "para analizarlas juntas en Explorar y en Redes."),
        if (length(sets)) tags$ul(class = "list-unstyled mb-2", lapply(sets, function(s) tags$li(
          class = "d-flex justify-content-between align-items-start mb-2",
          div(tags$strong(mr_label(s)), span(class = "small-2", paste0(" · ", tipo(s$type), " · ", length(s$vars),
                                                                         if (length(s$vars) == 1) " variable" else " variables")),
              div(class = "small-2", paste(s$vars, collapse = ", "))),
          if (ctx$can_edit()) tags$a(href = "#", class = "small-2",
            onclick = sprintf("caringAction('%s', '%s', 'drop', event)", ns("mr_action"), s$name), "quitar")))),
        # Desplegable cerrado por defecto: solo se abre si la persona lo pulsa.
        if (length(sug) && ctx$can_edit()) tags$details(class = "c-mr-sug mt-2",
          tags$summary(tags$strong("Sugerencias"), paste0(" (", length(sug), ")"),
                       span(class = "small-2", " · variables con el mismo prefijo y las mismas categorías")),
          tags$ul(class = "list-unstyled mb-0 mt-2", lapply(seq_along(sug), function(i) {
            g <- sug[[i]]
            tags$li(class = "d-flex justify-content-between align-items-center gap-2 mt-1",
              span(tags$code(g$name), paste0(" · ", length(g$vars), " variables (", tipo(g$type),
                                             if (!is.null(g$value)) paste0(", cuenta «", g$value, "»"), ")")),
              tags$a(href = "#", onclick = sprintf("caringAction('%s', '%d', 'suggest', event)", ns("mr_action"), i), "crear"))
          })))
      )
    })

    mr_modal <- function(pre = list()) {
      d <- data()
      showModal(modalDialog(title = "Conjunto de respuesta múltiple", size = "l", easyClose = TRUE,
        p(class = "c-hint", "Agrupa las variables de una pregunta en la que se puede marcar más de una opción. ",
          "No se modifica ninguna variable: el conjunto queda en el historial y se puede quitar."),
        div(class = "row",
          div(class = "col-md-6", textInput(ns("mr_name"), "Nombre (sin espacios)", value = pre$name %||% "", placeholder = "aficiones")),
          div(class = "col-md-6", textInput(ns("mr_label"), "Etiqueta (opcional)", value = pre$label %||% "", placeholder = "Aficiones en el tiempo libre"))),
        radioButtons(ns("mr_type"), "¿Cómo están codificadas las respuestas?", mr_types, selected = pre$type %||% "dicotomias"),
        uiOutput(ns("mr_type_help")),
        selectizeInput(ns("mr_vars"), "Variables", choices = names(d), selected = pre$vars, multiple = TRUE,
                       options = list(plugins = list("remove_button", "drag_drop"), placeholder = "Elige las variables…")),
        uiOutput(ns("mr_extra")),
        uiOutput(ns("mr_preview")),
        footer = tagList(modalButton("Cancelar"), actionButton(ns("mr_create"), "Crear conjunto", class = "btn-primary"))))
      session$userData$mr_pre_value <- pre$value
    }

    observeEvent(input$new_mr, {
      if (!ctx$can_edit()) return(showNotification("Este proyecto es de solo lectura.", type = "error"))
      mr_modal()
    })

    observeEvent(input$mr_action, {
      a <- input$mr_action
      if (a$action == "drop") {
        ctx$add_step(list(op = "mrset_drop", name = a$id))
      } else if (a$action == "suggest") {
        g <- mr_suggest(data())[[as.integer(a$id)]]
        req(g)
        mr_modal(g)
      }
    })

    output$mr_type_help <- renderUI({
      p(class = "c-hint", style = "margin-top:-8px", switch(input$mr_type %||% "dicotomias",
        dicotomias = "Ejemplo: p5_deporte, p5_cine, p5_viajes… cada una con Sí/No o 1/0. Cada variable es una opción.",
        categorias = "Ejemplo: p5_1 (primera mención), p5_2 (segunda)… cada una con el código de la opción elegida.",
        texto = "Ejemplo: una variable con «deporte;cine;viajes». Elige una sola variable y el separador."))
    })

    observeEvent(input$mr_type, {
      d <- data()
      ch <- if (identical(input$mr_type, "texto")) names(d)[vapply(d, function(x) is.character(x) || is.factor(x), logical(1))]
            else names(d)[vapply(d, function(x) is.factor(x) || is.numeric(x), logical(1))]
      updateSelectizeInput(session, "mr_vars", choices = ch, selected = intersect(isolate(input$mr_vars), ch))
    }, ignoreInit = TRUE)

    output$mr_extra <- renderUI({
      t <- input$mr_type %||% "dicotomias"
      d <- data()
      if (t == "dicotomias") {
        vs <- intersect(input$mr_vars, names(d))
        vals <- unique(unlist(lapply(vs, function(v) if (is.factor(d[[v]])) levels(d[[v]]) else as.character(sort(unique(stats::na.omit(d[[v]])))))))
        if (!length(vals)) return(NULL)
        pre <- session$userData$mr_pre_value
        marcado <- c("Sí", "Si", "1", "Mencionado", "Marcado", "Seleccionado", "Yes", "TRUE")
        sel <- if (!is.null(pre) && pre %in% vals) pre else (vals[vals %in% marcado][1] %||% vals[1])
        if (is.na(sel)) sel <- vals[1]
        selectInput(ns("mr_value"), "Valor que cuenta como «marcado»", vals, selected = sel)
      } else if (t == "texto") {
        textInput(ns("mr_sep"), "Separador de las opciones", value = ";")
      }
    })

    mr_draft <- reactive({
      list(op = "mrset", name = trimws(input$mr_name %||% ""), label = trimws(input$mr_label %||% ""),
           type = input$mr_type %||% "dicotomias", vars = input$mr_vars,
           value = input$mr_value, sep = input$mr_sep %||% ";")
    })

    output$mr_preview <- renderUI({
      s <- mr_draft()
      if (!length(s$vars)) return(NULL)
      if (!nzchar(s$name)) s$name <- "vista_previa"
      x <- tryCatch(mr_add(data(), s), error = function(e) e)
      if (inherits(x, "error")) return(div(class = "c-msg-error", error_es(x)))
      set <- mrsets(x)[[s$name]]
      f <- mr_freq(x, set)
      vacias <- mr_empty_options(x, set)
      tab <- data.frame(Opción = f$opcion, n = fmt_int(round(f$n)), `% de casos` = fmt_pct(f$pct_casos), check.names = FALSE)
      tagList(
        if (length(vacias$opciones)) div(class = "alert alert-warning py-2",
          tags$strong(if (length(vacias$opciones) == 1) "Una opción no tiene ninguna respuesta marcada"
                      else paste(length(vacias$opciones), "opciones no tienen ninguna respuesta marcada")),
          if (length(vacias$detalle)) tagList(": ", paste(vacias$detalle, collapse = "; "),
            ". Comprueba que el valor elegido es el que significa «marcado» en todas las variables.")),
        div(tags$strong("Vista previa"), span(class = "small-2", paste0(" · ", fmt_int(round(attr(f, "casos"))), " casos con respuesta")),
            div(style = "max-height:260px; overflow:auto", html_table(tab))))
    })

    observeEvent(input$mr_create, {
      if (ctx$add_step(mr_draft())) { removeModal(); showNotification("Conjunto creado. Ya puedes usarlo en Explorar y en Redes.", type = "message") }
    })

    # Historial -------------------------------------------------------------------------
    output$history <- renderUI({
      r <- proj$recipe
      if (!length(r)) return(p(class = "c-hint", "Todavía no has hecho cambios. Los datos originales nunca se modifican: cada cambio queda aquí y se puede quitar."))
      tagList(
        tags$ol(lapply(seq_along(r), function(i) tags$li(
          step_label(r[[i]]), " ",
          if (ctx$can_edit()) tags$a(href = "#", class = "undo small-2",
            onclick = sprintf("caringAction('%s', %d, 'remove', event)", ns("hist"), i), "quitar")))),
        if (ctx$can_edit()) actionButton(ns("undo"), "Deshacer el último cambio", class = "btn-light-c mt-2")
      )
    })

    observeEvent(input$undo, {
      r <- proj$recipe
      if (length(r)) ctx$set_recipe(r[-length(r)])
    })

    observeEvent(input$hist, {
      i <- as.integer(input$hist$id)
      r <- proj$recipe
      req(i >= 1, i <= length(r))
      nr <- r[-i]
      test <- tryCatch(apply_recipe(proj$original, nr, proj$dir), error = function(e) e)
      if (inherits(test, "error")) {
        return(showNotification(paste("No se puede quitar este paso porque otros dependen de él:", error_es(test)),
                                type = "error", duration = 10))
      }
      ctx$set_recipe(nr)
    })

    output$dl_script <- downloadHandler(
      filename = function() paste0(safe_filename(proj$info$nombre), "_preparacion.R"),
      content = function(file) writeLines(recipe_to_code(proj$recipe, proj$info$archivo), file, useBytes = TRUE)
    )

    # Tabla de casos -----------------------------------------------------------------------
    output$cases_note <- renderUI({
      n <- nrow(data())
      if (n > 1000) p(class = "c-hint", "Se muestran los primeros 1.000 de ", fmt_int(n), " casos.")
    })
    output$cases <- reactable::renderReactable({
      d <- utils::head(data(), 1000)
      for (v in names(d)) if (is.factor(d[[v]])) d[[v]] <- as.character(d[[v]])
      reactable::reactable(d, searchable = TRUE, compact = TRUE, striped = TRUE, highlight = TRUE,
                           defaultPageSize = 15, resizable = TRUE, wrap = FALSE, language = reactable_es(),
                           defaultColDef = reactable::colDef(minWidth = 110,
                             na = "—", headerStyle = list(background = "#F5F5F5")))
    })

    # Filtros ------------------------------------------------------------------------------
    output$filter_preview <- renderUI({
      e <- trimws(input$filter_expr %||% "")
      if (!nzchar(e)) return(NULL)
      r <- tryCatch(eval_safe(e, data()), error = function(err) err)
      if (inherits(r, "error")) return(div(class = "c-msg-error mb-2", error_es(r)))
      if (!is.logical(r)) return(div(class = "c-msg-error mb-2", "La condición debe dar verdadero o falso."))
      div(class = "c-stats mb-3", "Se quedarían ", tags$strong(fmt_int(sum(r, na.rm = TRUE))), " de ",
          fmt_int(nrow(data())), " casos.")
    })

    observeEvent(input$filter_apply, {
      e <- trimws(input$filter_expr %||% "")
      if (!nzchar(e)) return()
      if (ctx$add_step(list(op = "filter", expr = e))) updateTextInput(session, "filter_expr", value = "")
    })

    output$filters <- renderUI({
      r <- proj$recipe
      idx <- which(vapply(r, function(s) s$op == "filter", logical(1)))
      if (!length(idx)) return(p(class = "c-hint", "No hay filtros: se usan todos los casos."))
      tags$ul(class = "list-unstyled", lapply(idx, function(i) tags$li(class = "d-flex justify-content-between mb-2",
        tags$code(r[[i]]$expr),
        if (ctx$can_edit()) tags$a(href = "#", onclick = sprintf("caringAction('%s', %d, 'remove', event)", ns("hist"), i), "quitar"))))
    })
  })
}

recode_ui <- function(ns, d, v) {
  x <- d[[v]]
  if (is.numeric(x)) {
    tagList(
      p(class = "c-hint", "Convierte la variable numérica en categorías (tramos). Escribe dónde empieza cada tramo."),
      textInput(ns("cuts"), "Puntos de corte", placeholder = "Por ejemplo: 35, 55"),
      uiOutput(ns("cut_preview")),
      textInput(ns("recode_name"), "Nombre de la nueva variable", value = paste0(v, "_tramos")),
      actionButton(ns("do_recode"), "Crear variable", class = "btn-primary w-100")
    )
  } else if (is.factor(x)) {
    tagList(
      p(class = "c-hint", "Arrastra las categorías a los grupos. Se crea una variable nueva y la original no se toca."),
      numericInput(ns("ngroups"), "Número de grupos", value = min(3, max(2, nlevels(x) - 1)), min = 2, max = 10, width = "160px"),
      uiOutput(ns("recode_groups")),
      radioButtons(ns("others"), "Las categorías sin agrupar…",
                   c("pasan a ser perdidos" = "na", "se mantienen como están" = "keep"), inline = TRUE),
      textInput(ns("recode_name"), "Nombre de la nueva variable", value = paste0(v, "_r")),
      actionButton(ns("do_recode"), "Crear variable", class = "btn-primary w-100")
    )
  } else {
    p(class = "c-hint", "Para recodificar una variable de texto, cámbiala antes a categórica en la pestaña Tipo.")
  }
}

missing_ui <- function(ns, x) {
  if (is.factor(x) && !nlevels(x)) {
    p(class = "c-hint", "Esta variable no tiene ninguna categoría con datos.")
  } else if (is.factor(x)) {
    tb <- table(x)
    tagList(
      p(class = "c-hint", "Marca las categorías que en realidad son no-respuesta (No sabe, No contesta…)."),
      checkboxGroupInput(ns("missing_levels"), NULL,
                         choiceNames = paste0(names(tb), " (", as.integer(tb), ")"), choiceValues = names(tb)),
      actionButton(ns("do_missing"), "Marcar como perdidos", class = "btn-primary w-100")
    )
  } else if (is.numeric(x)) {
    u <- sort(unique(stats::na.omit(x)))
    sospechosos <- u[u %in% c(-9, -8, -1, 8, 9, 97, 98, 99, 997, 998, 999, 9998, 9999, 99999)]
    tagList(
      p(class = "c-hint", "Escribe los códigos que significan «sin respuesta», separados por comas."),
      if (length(sospechosos)) div(class = "c-stats mb-2", "Posibles códigos de no-respuesta en los datos: ",
                                   paste(sospechosos, collapse = ", ")),
      textInput(ns("missing_values"), "Valores", value = paste(sospechosos, collapse = ", ")),
      actionButton(ns("do_missing"), "Marcar como perdidos", class = "btn-primary w-100")
    )
  } else {
    p(class = "c-hint", "Cambia primero el tipo de la variable.")
  }
}

type_ui <- function(ns, x) {
  tipo <- var_type(x)
  tagList(
    radioButtons(ns("new_type"), "Tipo de variable",
      c("Categórica (sin orden)" = "categórica", "Ordinal (categorías con orden)" = "ordinal",
        "Numérica" = "numérica", "Texto" = "texto"), selected = if (tipo == "fecha") "texto" else tipo),
    actionButton(ns("do_type"), "Cambiar tipo", class = "btn-primary w-100 mb-3"),
    if (is.factor(x)) tagList(
      tags$hr(),
      tags$strong("Orden de las categorías"),
      p(class = "c-hint", "Arrastra para ordenarlas. Este orden se usa en tablas y gráficos."),
      div(class = "c-bucket", sortable::rank_list(text = NULL, labels = levels(x), input_id = ns("level_order"))),
      actionButton(ns("do_levels"), "Guardar orden", class = "btn-light-c w-100")
    )
  )
}

parse_numbers <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) return(numeric())
  p <- trimws(strsplit(x, "[;,]\\s*|\\s+")[[1]])
  p <- gsub(",", ".", p)
  n <- suppressWarnings(as.numeric(p[nzchar(p)]))
  n[!is.na(n)]
}

reactable_es <- function() {
  reactable::reactableLang(
    searchPlaceholder = "Buscar…", noData = "Sin resultados", pageInfo = "{rowStart}–{rowEnd} de {rows}",
    pagePrevious = "‹ Anterior", pageNext = "Siguiente ›", pageSizeOptions = "Mostrar {rows}")
}
