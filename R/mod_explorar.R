# Paso 2 · Explorar ------------------------------------------------------------------------

mod_explorar_ui <- function(id) {
  ns <- NS(id)
  div(class = "c-main",
    div(class = "c-page-title",
      div(tags$h1("Explorar"),
          div(class = "text-2", "Elige una o dos variables: CARING propone el gráfico, la tabla y la prueba adecuados.")),
      seg_radio(ns("mode"), c("Una variable" = "one", "Dos variables" = "two"), selected = "one")
    ),
    div(class = "row g-4",
      div(class = "col-lg-3",
        div(class = "c-card c-card-pad",
          selectInput(ns("v1"), "Variable", choices = NULL),
          conditionalPanel("input.mode == 'two'", ns = ns,
            selectInput(ns("v2"), "Variable en columnas", choices = NULL),
            actionButton(ns("swap"), HTML("&#8646; Intercambiar filas y columnas"), class = "btn-light-c w-100 mb-3")),
          uiOutput(ns("options")),
          selectInput(ns("weight"), "Ponderar por", choices = c("Sin ponderar" = "")))
      ),
      div(class = "col-lg-9",
        div(class = "c-card c-card-pad mb-4",
          div(class = "d-flex flex-wrap justify-content-between align-items-start gap-2 mb-2",
            div(tags$h2(textOutput(ns("title"), inline = TRUE)),
                div(class = "small-2", textOutput(ns("subtitle"), inline = TRUE))),
            div(class = "d-flex gap-2",
              downloadButton(ns("dl_png"), "PNG", class = "btn-light-c", icon = NULL),
              actionButton(ns("add_plot"), "+ Añadir al informe", class = "btn-primary"))),
          plotOutput(ns("plot"), height = "460px")),
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
      ch[!vapply(d, function(x) inherits(x, c("Date", "POSIXct")), logical(1))]
    }

    observeEvent(data(), {
      d <- data()
      ch <- var_choices(d)
      keep1 <- if (isTRUE(input$v1 %in% ch)) input$v1 else ch[min(2, length(ch))]
      keep2 <- if (isTRUE(input$v2 %in% ch)) input$v2 else ch[min(3, length(ch))]
      updateSelectInput(session, "v1", choices = ch, selected = keep1)
      updateSelectInput(session, "v2", choices = ch, selected = keep2)
      nums <- numeric_vars(d)
      wsel <- if (isTRUE(input$weight %in% nums)) input$weight else ""
      updateSelectInput(session, "weight", choices = c("Sin ponderar" = "", nums), selected = wsel)
    })

    # Tipo de combinación (una categórica, dos numéricas...) para ofrecer solo los gráficos que tienen sentido
    kind <- reactive({
      d <- data()
      req(input$v1 %in% names(d))
      if (input$mode == "two") { req(input$v2 %in% names(d)); chart_kind(d, input$v1, input$v2) }
      else chart_kind(d, input$v1)
    })

    # Nombres de los selectores: en dos variables, una va en filas y otra en columnas
    observeEvent(input$mode, {
      updateSelectInput(session, "v1", label = if (input$mode == "two") "Variable en filas" else "Variable")
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
        if (k == "catcat") tagList(
          radioButtons(ns("pct"), "Porcentajes",
            c("Verticales (cada columna suma 100 %)" = "col", "Horizontales (cada fila suma 100 %)" = "row",
              "Sobre el total" = "total"),
            selected = isolate(input$pct) %||% "col"),
          checkboxInput(ns("counts"), "Mostrar también los recuentos (n) en la tabla", value = isTRUE(isolate(input$counts)))))
    })

    result <- reactive({
      d <- data()
      req(input$v1 %in% names(d))
      w <- if (nzchar(input$weight %||% "")) input$weight
      tryCatch({
        if (input$mode == "two") {
          req(input$v2 %in% names(d))
          if (input$v1 == input$v2) stop("Elige dos variables distintas.")
          explore_two(d, input$v1, input$v2, weight = w, pct = input$pct %||% "col", chart = input$chart,
                      counts = isTRUE(input$counts))
        } else {
          explore_one(d, input$v1, weight = w, chart = input$chart)
        }
      }, error = function(e) list(error = conditionMessage(e)))
    })

    output$title <- renderText({ r <- result(); r$title %||% "" })
    output$subtitle <- renderText({ r <- result(); r$subtitle %||% "" })

    output$plot <- renderPlot({
      r <- result()
      validate(need(is.null(r$error), r$error))
      validate(need(!is.null(r$plot), "Esta variable no tiene gráfico."))
      r$plot
    }, res = 110)

    output$stats <- renderUI({
      r <- result()
      if (!is.null(r$error)) return(div(class = "c-msg-error", r$error))
      if (nzchar(r$stats %||% "")) div(class = "c-stats mb-3", r$stats)
    })

    output$table <- renderUI({
      r <- result()
      req(is.null(r$error))
      dir_note <- if (input$mode == "two" && identical(kind(), "catcat")) {
        switch(input$pct %||% "col",
          col = "↓ Porcentajes verticales: cada columna suma 100 % (fila Total).",
          row = "→ Porcentajes horizontales: cada fila suma 100 % (columna Total).",
          "Porcentajes sobre el total: todas las casillas juntas suman 100 %.")
      }
      tagList(
        if (!is.null(dir_note)) div(class = "c-dir-note", dir_note),
        div(style = "overflow-x:auto", html_table(r$table, total_last = identical(r$table[[1]][nrow(r$table)], "Total"))))
    })

    add_result <- function(kind) {
      if (!ctx$can_edit()) return(showNotification("Proyecto de solo lectura.", type = "error"))
      r <- result()
      if (!is.null(r$error)) return()
      png <- tempfile(fileext = ".png")
      on.exit(unlink(png))
      if (kind == "grafico") {
        if (is.null(r$plot)) return(showNotification("Este resultado no tiene gráfico.", type = "error"))
        save_plot_png(r$plot, png)
        obj <- list(plot = r$plot, subtitle = r$subtitle, stats = r$stats)
      } else {
        save_table_png(r$table, r$title, png)
        obj <- list(table = r$table, subtitle = r$subtitle, stats = r$stats)
      }
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
