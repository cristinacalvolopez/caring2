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
            tags$li(actionLink(ns("public_link"), "Enlace público (solo lectura)", class = "dropdown-item")),
            tags$li(tags$hr(class = "dropdown-divider")),
            tags$li(actionLink(ns("delete_doc"), "Mover a la papelera", class = "dropdown-item text-danger")),
            tags$li(actionLink(ns("doc_trash"), "Papelera de documentos", class = "dropdown-item")))),
        actionButton(ns("style"), "Estilo", class = "btn-light-c mb-3"),
        actionButton(ns("present"), "Presentar", class = "btn-light-c mb-3"),
        div(class = "dropdown mb-3",
          tags$button(class = "btn btn-primary dropdown-toggle", type = "button", `data-bs-toggle` = "dropdown", "Exportar"),
          tags$ul(class = "dropdown-menu dropdown-menu-end",
            tags$li(downloadLink(ns("dl_pptx"), "PowerPoint (.pptx)", class = "dropdown-item")),
            tags$li(downloadLink(ns("dl_docx"), "Word (.docx)", class = "dropdown-item")),
            tags$li(downloadLink(ns("dl_pdf"), "PDF", class = "dropdown-item")),
            tags$li(downloadLink(ns("dl_html"), "HTML con redes interactivas", class = "dropdown-item")),
            tags$li(tags$hr(class = "dropdown-divider")),
            tags$li(tags$h6(class = "dropdown-header", "Solo las tablas")),
            tags$li(downloadLink(ns("dl_tab_xlsx"), "Tablas a Excel (.xlsx)", class = "dropdown-item")),
            tags$li(downloadLink(ns("dl_tab_docx"), "Tablas a Word (.docx)", class = "dropdown-item"))))
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
    last_theme <- reactiveVal("clasico")     # estilo para los documentos nuevos
    pending_theme <- reactiveVal(NULL)       # estilo a aplicar en cuanto llegue el documento del editor

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

    templates_for <- function(type, theme) {
      keys <- if (type == "report") c("portada", "apartado", "blanca") else c("portada", "contenido", "dos_columnas", "seccion", "blanca")
      stats::setNames(lapply(keys, function(k) page_template(k, type, theme = theme)), keys)
    }

    assets <- reactive({
      proj$results_version
      req(proj$dir)
      editor_assets(proj$dir, list_results(proj$dir))
    })

    # Antes de mostrar los resultados, se ponen al día con los datos actuales.
    refresh <- function() {
      req(proj$dir)
      h <- recipe_hash(proj$recipe)
      pend <- Filter(function(r) !identical(r$data_hash, h), list_results(proj$dir))
      if (!length(pend)) return(invisible(FALSE))
      d <- isolate(ctx$data())
      withProgress(message = "Actualizando los resultados con los cambios de los datos…", value = 0, {
        k <- 0
        refresh_results(proj$dir, d, h, progress = function(t) { k <<- k + 1; incProgress(1 / length(pend), detail = t) })
      })
      proj$results_version <- proj$results_version + 1
      invisible(TRUE)
    }

    mount <- function() {
      refresh()
      if (isTRUE(tryCatch(ensure_palette_variants(proj$dir, isolate(ctx$data())), error = function(e) 0) > 0)) {
        proj$results_version <- proj$results_version + 1
      }
      id <- input$doc
      if (is.null(id) || !nzchar(id)) { session$sendCustomMessage("caring-editor-load", list(el = "__none__")); return() }
      doc <- if (file.exists(file.path(proj$dir, "informes", paste0(id, ".json")))) {
        tryCatch(load_document(proj$dir, id), error = function(e) NULL)
      }
      if (is.null(doc)) {                      # documento borrado o movido a la papelera
        current(NULL)
        session$sendCustomMessage("caring-editor-load", list(el = "__none__"))
        return(invisible())
      }
      current(doc)
      th <- doc$theme %||% "clasico"
      session$sendCustomMessage("caring-editor-load", list(
        el = ns("editor"), inputId = ns("doc_json"), deleteResultId = ns("del_result"),
        imageInput = ns("img_upload"), doc = doc, assets = assets(),
        templates = templates_for(doc$type, th), theme = theme_for_editor(th, doc$type),
        palettes = lapply(names(chart_palettes), function(k) {
          p <- chart_palettes[[k]]; tc <- table_colors(k)
          list(nombre = p$nombre, colors = p$colors, head = tc$head, head_text = tc$head_text)
        }) |> stats::setNames(names(chart_palettes))))
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
      # un documento que está en la papelera no se vuelve a guardar (el editor puede enviar su última copia)
      if (file.exists(file.path(doc_trash_dir(proj$dir), paste0(doc$id, ".json")))) return()
      th <- pending_theme()
      if (!is.null(th)) {
        pending_theme(NULL)
        doc <- apply_theme(doc, th)
        save_document(proj$dir, doc)
        current(doc)
        mount()
        showNotification(paste0("Estilo «", caring_themes[[th]]$nombre, "» aplicado. Puedes deshacerlo eligiendo otro estilo."),
                         type = "message")
      } else {
        save_document(proj$dir, doc)
        current(doc)
      }
      # si se ha elegido otra paleta para algún gráfico, se crea esa versión y se envía al editor
      nuevas <- tryCatch(ensure_palette_variants(proj$dir, isolate(ctx$data())), error = function(e) 0)
      if (nuevas > 0) proj$results_version <- proj$results_version + 1
      touch_project(ctx$con, proj$id)
      proj$saved <- Sys.time()
    })

    # Galería de estilos
    observeEvent(input$style, {
      doc <- current()
      type <- doc$type %||% "slides"
      actual <- doc$theme %||% last_theme()
      cards <- lapply(names(caring_themes), function(k) {
        th <- caring_themes[[k]]
        tags$button(type = "button", class = paste("c-theme-card", if (k == actual) "on"),
          onclick = sprintf("caringAction('%s', '%s', 'theme', event)", ns("pick_theme"), k),
          div(class = "c-theme-prev", HTML(theme_preview_html(k, type))),
          div(class = "c-theme-name", th$nombre, if (k == actual) span(class = "c-tag", "actual")),
          div(class = "c-theme-desc", th$desc))
      })
      showModal(modalDialog(title = "Elige un estilo", size = "xl", easyClose = TRUE,
        p(class = "c-hint", if (is.null(doc)) "Se usará en los documentos que crees a partir de ahora."
          else "Se aplica a todo el documento abierto: cambian colores y adornos, el contenido se mantiene. También se usará en los documentos nuevos."),
        div(class = "c-theme-grid", cards),
        footer = modalButton("Cerrar")))
    })

    observeEvent(input$pick_theme, {
      th <- input$pick_theme$id
      req(th %in% names(caring_themes))
      last_theme(th)
      removeModal()
      if (is.null(current())) return(showNotification(paste0("Los documentos nuevos usarán el estilo «", caring_themes[[th]]$nombre, "»."), type = "message"))
      if (!ctx$can_edit()) return(showNotification("Proyecto de solo lectura.", type = "error"))
      pending_theme(th)
      session$sendCustomMessage("caring-editor-flush", list())
    })

    create_doc <- function(type) {
      if (!ctx$can_edit()) return(showNotification("Proyecto de solo lectura.", type = "error"))
      n <- nrow(docs()) + 1
      title <- if (type == "report") paste("Informe", n) else paste("Presentación", n)
      if (n == 1) title <- proj$info$nombre
      doc <- new_document(title, type, theme = last_theme())
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
      showModal(modalDialog(title = "Mover a la papelera", easyClose = TRUE,
        p("«", current()$title, "» irá a la papelera de documentos, donde podrás recuperarlo durante ", papelera_dias,
          " días. Sus enlaces públicos dejarán de funcionar. Los resultados del proyecto no se borran."),
        footer = tagList(modalButton("Cancelar"), actionButton(ns("delete_ok"), "Mover a la papelera", class = "btn-danger-c"))))
    })
    observeEvent(input$delete_ok, {
      if (!ctx$can_edit()) return(removeModal())
      revoke_doc_links(ctx$con, proj$id, current()$id)
      trash_document(proj$dir, current()$id)
      current(NULL)
      removeModal()
      docs_version(docs_version() + 1)
      session$onFlushed(function() try(isolate(mount()), silent = TRUE), once = TRUE)
    })

    # Papelera de documentos -----------------------------------------------------------------
    observeEvent(input$doc_trash, {
      t <- list_trash_documents(proj$dir)
      act <- function(id, a) sprintf("caringAction('%s', '%s', '%s', event)", ns("dtrash"), id, a)
      showModal(modalDialog(title = "Papelera de documentos", easyClose = TRUE, size = "l",
        if (!nrow(t)) p(class = "c-hint", "No hay documentos en la papelera.")
        else tagList(
          p(class = "c-hint", "Los documentos borrados se guardan ", papelera_dias, " días."),
          div(class = "c-trash-list", lapply(seq_len(nrow(t)), function(i) div(class = "c-trash-item",
            div(tags$strong(t$title[i]), div(class = "small-2", paste0(if (t$type[i] == "report") "Informe" else "Diapositivas",
                " · borrado ", time_ago(t$borrado[i]), " · quedan ", t$quedan[i], " días"))),
            if (ctx$can_edit()) div(class = "d-flex gap-2",
              tags$button(class = "btn btn-outline-primary btn-sm", onclick = act(t$id[i], "restore"), "Recuperar"),
              tags$button(class = "btn btn-danger-c btn-sm", onclick = act(t$id[i], "purge"), "Borrar del todo")))))),
        footer = modalButton("Cerrar")))
    })
    observeEvent(input$dtrash, {
      a <- input$dtrash
      if (!ctx$can_edit() || !grepl("^[a-f0-9]+$", a$id %||% "")) return()
      if (a$action == "restore") {
        restore_document(proj$dir, a$id)
        want_doc(a$id)
        showNotification("Documento recuperado.", type = "message")
      } else {
        purge_document(proj$dir, a$id)
      }
      removeModal()
      docs_version(docs_version() + 1)
      if (a$action == "restore") session$onFlushed(function() try(isolate(mount()), silent = TRUE), once = TRUE)
    })

    # Enlace público de solo lectura ------------------------------------------------------------
    link_modal <- function(nuevo = NULL) {
      doc <- current()
      req(doc)
      links <- list_public_links(ctx$con, proj$id, doc$id)
      showModal(modalDialog(title = paste("Enlace público ·", doc$title), easyClose = TRUE, size = "l",
        p(class = "c-hint", "Cualquiera que tenga el enlace podrá ver este documento tal como está ahora (y con los cambios que hagas después), ",
          "sin cuenta y sin poder modificar nada. Los datos del proyecto no se muestran. Puedes anular el enlace cuando quieras."),
        if (!is.null(nuevo)) div(class = "c-link-box",
          tags$label(`for` = ns("link_url"), "Enlace (cópialo ahora: por seguridad no se vuelve a mostrar)"),
          tags$input(id = ns("link_url"), class = "form-control", readonly = NA, value = nuevo, onclick = "this.select()"),
          tags$button(class = "btn btn-primary btn-sm mt-2", type = "button",
                      onclick = sprintf("var i=document.getElementById('%s'); i.select(); try{navigator.clipboard.writeText(i.value)}catch(e){document.execCommand('copy')}; this.textContent='Copiado';", ns("link_url")),
                      "Copiar enlace")),
        tags$h3(class = "mt-3", style = "font-size:16px", "Enlaces activos"),
        if (!nrow(links)) p(class = "c-hint", "Ninguno.")
        else div(class = "c-trash-list", lapply(seq_len(nrow(links)), function(i) div(class = "c-trash-item",
          span(paste("Creado", time_ago(links$creado[i]))),
          tags$button(class = "btn btn-danger-c btn-sm",
                      onclick = sprintf("caringAction('%s', '%s', 'revoke', event)", ns("link_act"), substr(links$token_hash[i], 1, 16)),
                      "Anular")))),
        footer = tagList(modalButton("Cerrar"),
                         if (ctx$can_edit()) actionButton(ns("link_new"), "Crear un enlace nuevo", class = "btn-primary"))))
    }
    observeEvent(input$public_link, {
      req(current())
      if (identical(ctx$state$user$rol, "invitado")) {
        return(showNotification("Crea una cuenta para compartir enlaces públicos (los proyectos de invitado se borran a las 24 horas).", type = "error"))
      }
      link_modal()
    })
    observeEvent(input$link_new, {
      req(current(), ctx$can_edit())
      session$sendCustomMessage("caring-editor-flush", list())
      token <- create_public_link(ctx$con, proj$id, current()$id, ctx$state$user$id)
      audit(ctx$con, ctx$state$user$id, "enlace_publico", paste(proj$id, current()$id))
      link_modal(public_link_url(ctx$base_url(), token))
    })
    observeEvent(input$link_act, {
      req(ctx$can_edit())
      links <- list_public_links(ctx$con, proj$id, current()$id)
      h <- links$token_hash[startsWith(links$token_hash, input$link_act$id %||% "-")]
      if (length(h) == 1) revoke_public_link(ctx$con, h, proj$id)
      link_modal()
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
    output$dl_html <- downloadHandler(fname("html"), function(file) {
      doc <- latest()
      export_html(proj$dir, doc, file)
      save_html_copy(file, doc$title)
    })
    output$dl_tab_xlsx <- downloadHandler(function() paste0(safe_filename(current()$title), "_tablas.xlsx"),
                                          function(file) export_tables_xlsx(proj$dir, latest(), file))
    output$dl_tab_docx <- downloadHandler(function() paste0(safe_filename(current()$title), "_tablas.docx"),
                                          function(file) export_tables_docx(proj$dir, latest(), file))
    for (o in c("dl_pptx", "dl_docx", "dl_pdf", "dl_html", "dl_tab_xlsx", "dl_tab_docx")) outputOptions(output, o, suspendWhenHidden = FALSE)

    observeEvent(input$present, {
      req(current())
      f <- tempfile(fileext = ".html")
      on.exit(unlink(f))
      doc <- latest()
      export_html(proj$dir, doc, f, present = TRUE)
      save_html_copy(f, paste(doc$title, "(presentación)"))
      session$sendCustomMessage("caring-open-html", list(html = paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")))
    })
  })
}
