# Paso 4 · Informes y diapositivas ------------------------------------------------------------

mod_informes_ui <- function(id) {
  ns <- NS(id)
  div(class = "c-main", style = "max-width:1600px",
    div(class = "c-page-title",
      div(tags$h1("Informes"), div(class = "text-2", "Monta diapositivas o un informe con tus resultados. Todo se guarda automáticamente.")),
      div(class = "d-flex flex-wrap gap-2 align-items-end",
        div(style = "min-width:240px", selectInput(ns("doc"), "Documento", choices = NULL)),
        div(class = "dropdown mb-3",
          tags$button(class = "btn btn-light-c dropdown-toggle", type = "button", `data-bs-toggle` = "dropdown", "Nuevo"),
          tags$ul(class = "dropdown-menu",
            tags$li(actionLink(ns("new_slides"), "Diapositivas (16:9)", class = "dropdown-item")),
            tags$li(actionLink(ns("new_report"), "Informe (A4)", class = "dropdown-item")))),
        div(class = "dropdown mb-3",
          tags$button(class = "btn btn-light-c dropdown-toggle", type = "button", `data-bs-toggle` = "dropdown", "Documento"),
          tags$ul(class = "dropdown-menu",
            tags$li(actionLink(ns("rename_doc"), "Renombrar", class = "dropdown-item")),
            tags$li(actionLink(ns("delete_doc"), "Borrar", class = "dropdown-item text-danger")))),
        actionButton(ns("present"), "Presentar", class = "btn-light-c mb-3"),
        div(class = "dropdown mb-3",
          tags$button(class = "btn btn-primary dropdown-toggle", type = "button", `data-bs-toggle` = "dropdown", "Exportar"),
          tags$ul(class = "dropdown-menu dropdown-menu-end",
            tags$li(downloadLink(ns("dl_pptx"), "PowerPoint (.pptx)", class = "dropdown-item")),
            tags$li(downloadLink(ns("dl_docx"), "Word (.docx)", class = "dropdown-item")),
            tags$li(downloadLink(ns("dl_pdf"), "PDF", class = "dropdown-item")),
            tags$li(downloadLink(ns("dl_html"), "HTML con redes interactivas", class = "dropdown-item"))))
      )
    ),
    div(style = "position:absolute; left:-9999px; width:1px; height:1px; overflow:hidden",
        fileInput(ns("img_upload"), NULL, accept = c(".png", ".jpg", ".jpeg", ".gif", ".svg", ".webp"))),
    uiOutput(ns("empty")),
    div(id = ns("editor"), class = "ed")
  )
}

mod_informes_server <- function(id, ctx) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    proj <- ctx$proj
    docs_version <- reactiveVal(0)
    current <- reactiveVal(NULL)
    want_doc <- reactiveVal(NULL)

    docs <- reactive({
      docs_version(); proj$version
      req(proj$dir)
      list_documents(proj$dir)
    })

    observeEvent(docs(), {
      d <- docs()
      sel <- isolate(want_doc()) %||% isolate(input$doc)
      want_doc(NULL)
      if (!isTRUE(sel %in% d$id)) sel <- if (nrow(d)) d$id[1] else NULL
      ch <- if (nrow(d)) stats::setNames(d$id, paste0(d$title, ifelse(d$type == "report", " · informe", " · diapositivas"))) else character()
      updateSelectInput(session, "doc", choices = ch, selected = sel)
    })

    output$empty <- renderUI({
      if (nrow(docs())) return(NULL)
      div(class = "c-card c-empty",
        tags$h2("Todavía no hay documentos"),
        p("Crea unas diapositivas o un informe. Después añade los gráficos, tablas y redes que hayas guardado en Explorar y Redes."),
        div(class = "d-flex justify-content-center gap-2",
          actionButton(ns("new_slides2"), "Nuevas diapositivas", class = "btn-primary"),
          actionButton(ns("new_report2"), "Nuevo informe", class = "btn-light-c")))
    })

    templates_for <- function(type) {
      keys <- if (type == "report") c("portada", "apartado", "blanca") else c("portada", "contenido", "dos_columnas", "seccion", "blanca")
      stats::setNames(lapply(keys, function(k) page_template(k, type)), keys)
    }

    assets <- reactive({
      proj$results_version
      req(proj$dir)
      editor_assets(proj$dir, list_results(proj$dir))
    })

    mount <- function() {
      id <- input$doc
      if (is.null(id) || !nzchar(id)) { session$sendCustomMessage("caring-editor-load", list(el = "__none__")); return() }
      doc <- tryCatch(load_document(proj$dir, id), error = function(e) NULL)
      req(doc)
      current(doc)
      session$sendCustomMessage("caring-editor-load", list(
        el = ns("editor"), inputId = ns("doc_json"), deleteResultId = ns("del_result"),
        imageInput = ns("img_upload"), doc = doc, assets = assets(), templates = templates_for(doc$type)))
    }

    # Se monta el editor al entrar en el paso Informes o al cambiar de documento.
    observeEvent(list(state_step = ctx$state$step, input$doc, proj$id), {
      if (identical(ctx$state$step, "informes")) mount()
    }, ignoreInit = FALSE)

    observeEvent(proj$results_version, {
      if (identical(ctx$state$step, "informes")) session$sendCustomMessage("caring-editor-assets", list(assets = assets()))
    }, ignoreInit = TRUE)

    observeEvent(input$doc_json, {
      if (!ctx$can_edit()) return()
      doc <- tryCatch(jsonlite::fromJSON(input$doc_json, simplifyVector = FALSE), error = function(e) NULL)
      if (is.null(doc) || is.null(doc$id) || !grepl("^[a-f0-9]+$", doc$id) || !is.list(doc$pages)) return()
      save_document(proj$dir, doc)
      current(doc)
      touch_project(ctx$con, proj$id)
      proj$saved <- Sys.time()
    })

    create_doc <- function(type) {
      if (!ctx$can_edit()) return(showNotification("Proyecto de solo lectura.", type = "error"))
      n <- nrow(docs()) + 1
      title <- if (type == "report") paste("Informe", n) else paste("Presentación", n)
      if (n == 1) title <- proj$info$nombre
      doc <- new_document(title, type)
      save_document(proj$dir, doc)
      want_doc(doc$id)
      docs_version(docs_version() + 1)
    }
    observeEvent(input$new_slides, create_doc("slides"))
    observeEvent(input$new_slides2, create_doc("slides"))
    observeEvent(input$new_report, create_doc("report"))
    observeEvent(input$new_report2, create_doc("report"))

    observeEvent(input$rename_doc, {
      req(current())
      showModal(modalDialog(title = "Renombrar documento", easyClose = TRUE,
        textInput(ns("doc_title"), "Título", value = current()$title),
        footer = tagList(modalButton("Cancelar"), actionButton(ns("rename_ok"), "Guardar", class = "btn-primary"))))
    })
    observeEvent(input$rename_ok, {
      session$sendCustomMessage("caring-editor-flush", list())
      doc <- load_document(proj$dir, current()$id)
      doc$title <- trimws(input$doc_title)
      save_document(proj$dir, doc)
      removeModal()
      docs_version(docs_version() + 1)
    })

    observeEvent(input$delete_doc, {
      req(current())
      showModal(modalDialog(title = "Borrar documento", easyClose = TRUE,
        p("Se borrará «", current()$title, "». Los resultados del proyecto no se borran."),
        footer = tagList(modalButton("Cancelar"), actionButton(ns("delete_ok"), "Borrar", class = "btn-danger-c"))))
    })
    observeEvent(input$delete_ok, {
      delete_document(proj$dir, current()$id)
      current(NULL)
      removeModal()
      docs_version(docs_version() + 1)
      session$onFlushed(function() isolate(mount()), once = TRUE)
    })

    observeEvent(input$img_upload, {
      f <- input$img_upload
      safely({
        src <- save_image(proj$dir, f$datapath, f$name)
        file <- image_file(proj$dir, src)
        session$sendCustomMessage("caring-editor-add-image", list(src = src, uri = data_uri(file, mime_of(file))))
      })
    })

    observeEvent(input$del_result, {
      session$userData$del_result <- input$del_result$id
      showModal(modalDialog(title = "Borrar resultado", easyClose = TRUE,
        p("El resultado desaparecerá de la lista y de los documentos donde lo hayas usado."),
        footer = tagList(modalButton("Cancelar"), actionButton(ns("del_result_ok"), "Borrar", class = "btn-danger-c"))))
    })
    observeEvent(input$del_result_ok, {
      delete_result(proj$dir, session$userData$del_result)
      removeModal()
      ctx$results_changed()
    })

    latest <- function() {
      doc <- current()
      validate(need(!is.null(doc), "No hay ningún documento abierto."))
      tryCatch(load_document(proj$dir, doc$id), error = function(e) doc)
    }
    fname <- function(ext) function() paste0(safe_filename(current()$title %||% "documento"), ".", ext)

    output$dl_pptx <- downloadHandler(fname("pptx"), function(file) {
      withProgress(message = "Generando PowerPoint…", export_pptx(proj$dir, latest(), file))
    })
    output$dl_docx <- downloadHandler(fname("docx"), function(file) {
      withProgress(message = "Generando Word…", export_docx(proj$dir, latest(), file))
    })
    output$dl_pdf <- downloadHandler(fname("pdf"), function(file) {
      withProgress(message = "Generando PDF…", export_pdf(proj$dir, latest(), file))
    })
    output$dl_html <- downloadHandler(fname("html"), function(file) export_html(proj$dir, latest(), file))
    for (o in c("dl_pptx", "dl_docx", "dl_pdf", "dl_html")) outputOptions(output, o, suspendWhenHidden = FALSE)

    observeEvent(input$present, {
      req(current())
      f <- tempfile(fileext = ".html")
      on.exit(unlink(f))
      export_html(proj$dir, latest(), f, present = TRUE)
      session$sendCustomMessage("caring-open-html", list(html = paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")))
    })
  })
}
