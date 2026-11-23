# Paso 2 · Explorar ------------------------------------------------------------------------

mod_explorar_ui <- function(id) {
  ns <- NS(id)
  div(class = "c-main",
    div(class = "c-page-title",
      div(tags$h1("Explorar"),
          div(class = "text-2", "Elige las variables: CARING propone el gráfico, la tabla y la prueba adecuados.")),
      seg_radio(ns("mode"), c("Una variable" = "one", "Dos variables" = "two", "Tres variables" = "three"), selected = "one")
    ),
    div(class = "row g-4",
      div(class = "col-lg-3",
        div(class = "c-card c-card-pad",
          selectInput(ns("v1"), "Variable", choices = NULL),
          conditionalPanel("input.mode != 'one'", ns = ns,
            selectInput(ns("v2"), "Variable en columnas", choices = NULL),
            actionButton(ns("swap"), HTML("&#8646; Intercambiar filas y columnas"), class = "btn-light-c w-100 mb-3")),
          conditionalPanel("input.mode == 'three'", ns = ns,
            selectInput(ns("v3"), "Variable de control (una tabla por cada categoría)", choices = NULL)),
          uiOutput(ns("options")),
          selectInput(ns("weight"), "Ponderar por", choices = c("Sin ponderar" = "")))
      ),
      div(class = "col-lg-9",
        div(class = "c-card c-card-pad mb-4",
          div(class = "d-flex flex-wrap justify-content-between align-items-start gap-2 mb-2",
            div(tags$h2(textOutput(ns("title"), inline = TRUE)),
                div(class = "small-2", textOutput(ns("subtitle"), inline = TRUE))),
            div(class = "d-flex gap-2 align-items-center flex-wrap",
              div(class = "form-check form-switch c-switch",
                tags$input(class = "form-check-input", type = "checkbox", role = "switch", id = ns("interactive")),
                tags$label(class = "form-check-label", `for` = ns("interactive"), "Interactivo")),
              downloadButton(ns("dl_png"), "PNG", class = "btn-light-c", icon = NULL),
              downloadButton(ns("dl_html"), "HTML interactivo", class = "btn-light-c", icon = NULL),
              actionButton(ns("add_plot"), "+ Añadir al informe", class = "btn-primary"))),
          conditionalPanel("!input.interactive", ns = ns, plotOutput(ns("plot"), height = "460px")),
          conditionalPanel("input.interactive", ns = ns, plotly::plotlyOutput(ns("plotly"), height = "460px"))),
        div(class = "c-card c-card-pad",
          div(class = "d-flex flex-wrap justify-content-between align-items-center gap-2 mb-2",
            tags$h2("Tabla"),
            div(class = "d-flex gap-2",
              downloadButton(ns("dl_csv"), "CSV", class = "btn-light-c", icon = NULL),
              actionButton(ns("add_table"), "+ Añadir al informe", class = "btn-light-c"))),
          uiOutput(ns("stats")),
          uiOutput(ns("table")))
      )
    )
  )
}

mod_explorar_server <- function(id, ctx) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data <- ctx$data

    var_choices <- function(d) {
      ch <- names(d)
      names(ch) <- vapply(ch, function(v) {
        l <- var_label(d, v)
        if (l == v) v else paste0(l, " (", v, ")")
      }, "")
      ch <- ch[!vapply(d, function(x) inherits(x, c("Date", "POSIXct")), logical(1))]
      sets <- mrsets(d)
      if (length(sets)) {
        mr <- stats::setNames(paste0("mr:", names(sets)), paste0("Conjunto: ", vapply(sets, mr_label, "")))
        ch <- c(mr, ch)
      }
      ch
    }
    valid_choice <- function(d, v) !is.null(v) && (v %in% names(d) || (startsWith(v, "mr:") && sub("^mr:", "", v) %in% names(mrsets(d))))

    observeEvent(data(), {
      d <- data()
      ch <- var_choices(d)
      vars_only <- ch[!startsWith(ch, "mr:")]
      keep1 <- if (isTRUE(input$v1 %in% ch)) input$v1 else vars_only[min(2, length(vars_only))]
      keep2 <- if (isTRUE(input$v2 %in% ch)) input$v2 else vars_only[min(3, length(vars_only))]
      updateSelectInput(session, "v1", choices = ch, selected = keep1)
      updateSelectInput(session, "v2", choices = ch, selected = keep2)
      cats <- var_choices(d)[var_choices(d) %in% categorical_vars(d)]
      keep3 <- if (isTRUE(input$v3 %in% cats)) input$v3 else cats[min(1, length(cats))]
      updateSelectInput(session, "v3", choices = cats, selected = keep3)
      nums <- numeric_vars(d)
      wsel <- if (isTRUE(input$weight %in% nums)) input$weight else ""
      updateSelectInput(session, "weight", choices = c("Sin ponderar" = "", nums), selected = wsel)
    })

    # Tipo de combinación (una categórica, dos numéricas...) para ofrecer solo los gráficos que tienen sentido
    kind <- reactive({
      d <- data()
      req(valid_choice(d, input$v1))
      if (input$mode == "two") { req(valid_choice(d, input$v2)); chart_kind(d, input$v1, input$v2) }
      else if (input$mode == "three") {
        req(valid_choice(d, input$v2))
        k <- chart_kind(d, input$v1, input$v2)
        if (k %in% c("catcat", "numcat")) paste0(k, "3") else k
      }
      else chart_kind(d, input$v1)
    })

    # Nombres de los selectores: en dos variables, una va en filas y otra en columnas
    observeEvent(input$mode, {
      updateSelectInput(session, "v1", label = if (input$mode != "one") "Variable en filas" else "Variable")
    })

    observeEvent(input$swap, {
      a <- input$v1; b <- input$v2
      req(a, b)
      updateSelectInput(session, "v1", selected = b)
      updateSelectInput(session, "v2", selected = a)
    })

    output$options <- renderUI({
      k <- kind()
      ch <- chart_choices(k)
      tagList(
        if (length(ch)) selectInput(ns("chart"), "Tipo de gráfico", ch,
                                    selected = if (isTRUE(isolate(input$chart) %in% ch)) isolate(input$chart) else ch[1]),
        if (k == "mrcat") checkboxInput(ns("counts"), "Mostrar también los recuentos (n) en la tabla", value = isTRUE(isolate(input$counts))),
        if (k %in% c("catcat", "catcat3")) tagList(
          radioButtons(ns("pct"), "Porcentajes",
            c("Verticales (cada columna suma 100 %)" = "col", "Horizontales (cada fila suma 100 %)" = "row",
              "Sobre el total" = "total"),
            selected = isolate(input$pct) %||% "col"),
          checkboxInput(ns("counts"), "Mostrar también los recuentos (n) en la tabla", value = isTRUE(isolate(input$counts)))))
    })

    result <- reactive({
      d <- data()
      req(valid_choice(d, input$v1))
      if (input$mode != "one") req(valid_choice(d, input$v2))
      if (input$mode == "three") req(input$v3 %in% names(d))
      tryCatch(explore_from_spec(d, explore_spec()), error = function(e) list(error = error_es(e)))
    })

    output$title <- renderText({ r <- result(); r$title %||% "" })
    output$subtitle <- renderText({ r <- result(); r$subtitle %||% "" })

    output$plot <- renderPlot({
      r <- result()
      validate(need(is.null(r$error), r$error))
      validate(need(!is.null(r$plot), "Esta variable no tiene gráfico."))
      r$plot
    }, res = 110)

    output$plotly <- plotly::renderPlotly({
      req(isTRUE(input$interactive))
      r <- result()
      validate(need(is.null(r$error), r$error))
      validate(need(!is.null(r$plot), "Esta variable no tiene gráfico."))
      to_plotly(r)
    })

    output$dl_html <- downloadHandler(
      filename = function() paste0(safe_filename(result()$title), ".html"),
      content = function(file) {
        r <- result()
        if (is.null(r$plot)) stop("Este resultado no tiene gráfico.")
        save_plotly_html(to_plotly(r), file, title = r$title)
      }
    )

    output$stats <- renderUI({
      r <- result()
      if (!is.null(r$error)) return(div(class = "c-msg-error", r$error))
      if (nzchar(r$stats %||% "")) div(class = "c-stats mb-3", r$stats)
    })

    output$table <- renderUI({
      r <- result()
      req(is.null(r$error))
      dir_note <- if (input$mode == "two" && identical(kind(), "mrcat")) {
        "↓ % de casos de cada grupo: las columnas suman más de 100 % porque se puede marcar más de una opción."
      } else if (input$mode != "one" && kind() %in% c("catcat", "catcat3")) {
        switch(input$pct %||% "col",
          col = "↓ Porcentajes verticales: cada columna suma 100 % (fila Total).",
          row = "→ Porcentajes horizontales: cada fila suma 100 % (columna Total).",
          "Porcentajes sobre el total: todas las casillas juntas suman 100 %.")
      }
      tagList(
        if (!is.null(dir_note)) div(class = "c-dir-note", dir_note),
        div(style = "overflow-x:auto", html_table(r$table, total_last = identical(r$table[[1]][nrow(r$table)], "Total"))))
    })

    explore_spec <- function() {
      list(mode = input$mode, v1 = input$v1, v2 = if (input$mode != "one") input$v2,
           v3 = if (input$mode == "three") input$v3,
           weight = if (nzchar(input$weight %||% "")) input$weight, pct = input$pct %||% "col",
           chart = input$chart, counts = isTRUE(input$counts))
    }

    add_result <- function(kind) {
      if (!ctx$can_edit()) return(showNotification("Proyecto de solo lectura.", type = "error"))
      r <- result()
      if (!is.null(r$error)) return()
      png <- tempfile(fileext = ".png")
      on.exit(unlink(png))
      if (kind == "grafico") {
        if (is.null(r$plot)) return(showNotification("Este resultado no tiene gráfico.", type = "error"))
        save_plot_png(r$plot, png)
      } else {
        save_table_png(r$table, r$title, png)
      }
      # Se guarda la receta del resultado (variables y opciones), no el objeto gráfico:
      # ocupa unos pocos KB y permite volver a dibujarlo (p. ej., con otros colores).
      obj <- list(table = r$table, subtitle = r$subtitle, stats = r$stats, spec = explore_spec())
      save_result(ctx$proj$dir, kind, r$title, obj, png = png, source = "Explorar")
      ctx$results_changed()
      showNotification(paste(if (kind == "grafico") "Gráfico" else "Tabla", "añadido a los resultados del proyecto. Lo encontrarás en Informes."),
                       type = "message")
    }
    observeEvent(input$add_plot, add_result("grafico"))
    observeEvent(input$add_table, add_result("tabla"))

    output$dl_png <- downloadHandler(
      filename = function() paste0(safe_filename(result()$title), ".png"),
      content = function(file) save_plot_png(result()$plot, file)
    )
    output$dl_csv <- downloadHandler(
      filename = function() paste0(safe_filename(result()$title), ".csv"),
      content = function(file) utils::write.csv(result()$table, file, row.names = FALSE, fileEncoding = "UTF-8")
    )
  })
}
