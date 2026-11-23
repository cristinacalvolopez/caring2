# Proyecto abierto: barra de pasos + los cuatro módulos -------------------------------------

mod_project_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "c-projectbar",
      div(style = "min-width:200px",
        div(class = "pname", textOutput(ns("name"), inline = TRUE)),
        div(class = "small-2", textOutput(ns("status"), inline = TRUE))),
      uiOutput(ns("steps"), inline = TRUE),
      div(class = "d-flex gap-2 flex-wrap",
        actionButton(ns("code"), "Código R", class = "btn-light-c"),
        div(class = "dropdown",
          tags$button(class = "btn btn-light-c dropdown-toggle", type = "button", `data-bs-toggle` = "dropdown",
                      "Descargar datos"),
          tags$ul(class = "dropdown-menu",
            tags$li(downloadLink(ns("dl_sav"), "SPSS (.sav)", class = "dropdown-item")),
            tags$li(downloadLink(ns("dl_dta"), "Stata (.dta)", class = "dropdown-item")),
            tags$li(downloadLink(ns("dl_rds"), "R (.rds)", class = "dropdown-item")),
            tags$li(downloadLink(ns("dl_csv"), "CSV (.csv)", class = "dropdown-item")),
            tags$li(tags$hr(class = "dropdown-divider")),
            tags$li(downloadLink(ns("dl_project"), "Proyecto completo (.caring)", class = "dropdown-item",
                                 title = "Datos, cambios, resultados e informes, para abrirlo en otro CARING")))),
        uiOutput(ns("comments_btn"), inline = TRUE),
        uiOutput(ns("share_btn"), inline = TRUE))
    ),
    bslib::navset_hidden(id = ns("panels"),
      bslib::nav_panel_hidden("datos", mod_datos_ui(ns("datos"))),
      bslib::nav_panel_hidden("explorar", mod_explorar_ui(ns("explorar"))),
      bslib::nav_panel_hidden("redes", mod_redes_ui(ns("redes"))),
      bslib::nav_panel_hidden("informes", mod_informes_ui(ns("informes")))
    )
  )
}

mod_project_server <- function(id, con, state, base_url = reactive("")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    proj <- reactiveValues(id = NULL, dir = NULL, permiso = NULL, info = NULL,
                           original = NULL, recipe = list(), version = 0, saved = NULL,
                           results_version = 0)

    # Carga del proyecto cuando cambia state$project
    observeEvent(state$project, {
      pid <- state$project
      req(pid, state$user)
      perm <- project_access(con, state$user$id, pid)
      if (is.null(perm)) { showNotification("No tienes acceso a este proyecto.", type = "error"); return() }
      dir <- project_path(con, pid)
      d <- tryCatch(withProgress(message = "Abriendo el proyecto…", value = 0.5,
                                 read_any(project_original(dir))),
                    error = function(e) { notify_error(e); NULL })
      req(d)
      proj$id <- pid; proj$dir <- dir; proj$permiso <- perm
      proj$info <- get_project(con, pid)
      proj$original <- d
      proj$recipe <- read_recipe(dir)
      proj$saved <- Sys.time()
      proj$version <- proj$version + 1
      proj$results_version <- proj$results_version + 1
      state$step <- state$step_on_open %||% "datos"
      bslib::nav_select("panels", state$step)
    }, ignoreNULL = TRUE)

    # Datos de trabajo = original + receta
    data <- reactive({
      proj$version
      req(proj$original)
      tryCatch(apply_recipe(proj$original, proj$recipe, proj$dir), error = function(e) {
        notify_error(paste("Error al aplicar los cambios:", error_es(e)))
        proj$original
      })
    })

    # Añade un paso a la receta (comprueba permisos y que el paso funciona)
    add_step <- function(step) {
      if (!can_edit(proj$permiso)) { showNotification("Este proyecto es de solo lectura.", type = "error"); return(FALSE) }
      test <- tryCatch(apply_step(data(), step, proj$dir), error = function(e) e)
      if (inherits(test, "error")) { notify_error(test); return(FALSE) }
      proj$recipe <- c(proj$recipe, list(step))
      persist()
      TRUE
    }
    set_recipe <- function(recipe) {
      if (!can_edit(proj$permiso)) return(FALSE)
      proj$recipe <- recipe
      persist()
      TRUE
    }
    persist <- function() {
      write_recipe(proj$dir, proj$recipe)
      d <- data_try()
      db_run(con, "UPDATE projects SET n_casos = ?, n_vars = ? WHERE id = ?", list(nrow(d), ncol(d), proj$id))
      touch_project(con, proj$id)
      proj$saved <- Sys.time()
      proj$version <- proj$version + 1
    }
    data_try <- function() tryCatch(apply_recipe(proj$original, proj$recipe, proj$dir), error = function(e) proj$original)
    results_changed <- function() {
      touch_project(con, proj$id)
      proj$results_version <- proj$results_version + 1
      proj$saved <- Sys.time()
    }

    ctx <- list(con = con, state = state, proj = proj, data = data, add_step = add_step, base_url = base_url,
                set_recipe = set_recipe, results_changed = results_changed,
                can_edit = reactive(can_edit(proj$permiso)))

    mod_datos_server("datos", ctx)
    mod_explorar_server("explorar", ctx)
    mod_redes_server("redes", ctx)
    mod_informes_server("informes", ctx)

    # Barra del proyecto
    output$name <- renderText({ proj$version; proj$info$nombre %||% "" })
    output$status <- renderText({
      invalidateLater(30000)
      req(proj$saved)
      ro <- if (!can_edit(proj$permiso)) "Solo lectura · " else ""
      d <- data()
      paste0(ro, fmt_int(nrow(d)), " casos · ", ncol(d), " variables · guardado ",
             time_ago(format(proj$saved, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))
    })
    output$steps <- renderUI(steps_nav(state$step %||% "datos", ns("step")))
    output$share_btn <- renderUI({
      if (identical(proj$permiso, "propietario") && !identical(state$user$rol, "invitado"))
        actionButton(ns("share"), "Compartir", class = "btn-light-c")
    })

    observeEvent(input$step, {
      state$step <- input$step
      bslib::nav_select("panels", input$step)
    })
    observeEvent(state$goto_step, {
      req(state$goto_step)
      state$step <- state$goto_step
      bslib::nav_select("panels", state$goto_step)
      state$goto_step <- NULL
    })

    observeEvent(input$share, state$share_project <- list(id = proj$id, t = Sys.time()))

    # Comentarios (solo en proyectos compartidos) ------------------------------------------------
    comments_v <- reactiveVal(0)
    output$comments_btn <- renderUI({
      comments_v(); proj$version
      req(proj$id, state$user)
      invalidateLater(60000)
      if (!project_is_shared(con, proj$id)) return(NULL)
      n <- unread_comments(con, proj$id, state$user$id)
      actionButton(ns("comments"), tagList("Comentarios", if (n > 0) span(class = "c-badge", `aria-label` = paste(n, "sin leer"), n)),
                   class = "btn-light-c")
    })
    comments_modal <- function() {
      cs <- list_comments(con, proj$id)
      owner <- identical(proj$permiso, "propietario")
      showModal(modalDialog(title = "Comentarios del proyecto", easyClose = TRUE, size = "l",
        p(class = "c-hint", "Los ven todas las personas con acceso al proyecto."),
        div(class = "c-comments",
          if (!nrow(cs)) p(class = "c-hint", "Todavía no hay comentarios.")
          else lapply(seq_len(nrow(cs)), function(i) div(class = paste("c-comment", if (identical(cs$user_id[i], state$user$id)) "mine"),
            div(class = "c-comment-head", tags$strong(cs$autor[i]), span(class = "small-2", time_ago(cs$creado[i])),
              if (owner || identical(cs$user_id[i], state$user$id))
                tags$button(class = "btn-link-c small-2", type = "button", `aria-label` = "Borrar comentario",
                            onclick = sprintf("caringAction('%s', '%s', 'del', event)", ns("comment_act"), cs$id[i]), "Borrar")),
            div(class = "c-comment-text", cs$texto[i])))),
        textAreaInput(ns("comment_text"), "Nuevo comentario", rows = 3, width = "100%",
                      placeholder = "Por ejemplo: he recodificado la edad en tramos; revisad la red de la diapositiva 3."),
        footer = tagList(modalButton("Cerrar"), actionButton(ns("comment_send"), "Publicar", class = "btn-primary"))))
      mark_comments_read(con, proj$id, state$user$id)
      comments_v(comments_v() + 1)
    }
    observeEvent(input$comments, comments_modal())
    observeEvent(input$comment_send, {
      req(project_access(con, state$user$id, proj$id))
      r <- add_comment(con, proj$id, state$user$id, input$comment_text)
      if (!r$ok) return(showNotification(r$error, type = "error"))
      comments_modal()
    })
    observeEvent(input$comment_act, {
      req(project_access(con, state$user$id, proj$id))
      delete_comment(con, input$comment_act$id, state$user$id, is_owner = identical(proj$permiso, "propietario"))
      comments_modal()
    })

    observeEvent(input$code, {
      code <- recipe_to_code(proj$recipe, proj$info$archivo)
      showModal(modalDialog(title = "Código R de la preparación de datos", size = "l", easyClose = TRUE,
        p(class = "c-hint", "Este script reproduce todos los cambios que has hecho sobre los datos."),
        tags$pre(class = "c-code", code),
        footer = tagList(downloadButton(ns("download_code"), "Descargar script", class = "btn-primary", icon = NULL),
                         modalButton("Cerrar"))))
    })

    output$download_code <- downloadHandler(
      filename = function() paste0(safe_filename(proj$info$nombre), "_preparacion.R"),
      content = function(file) writeLines(recipe_to_code(proj$recipe, proj$info$archivo), file, useBytes = TRUE)
    )

    for (fmt in c("sav", "dta", "rds", "csv")) local({
      f <- fmt
      output[[paste0("dl_", f)]] <- downloadHandler(
        filename = function() paste0(safe_filename(proj$info$nombre), ".", f),
        content = function(file) write_any(data(), file, f)
      )
      outputOptions(output, paste0("dl_", f), suspendWhenHidden = FALSE)
    })

    output$dl_project <- downloadHandler(
      filename = function() paste0(safe_filename(proj$info$nombre), ".caring"),
      content = function(file) withProgress(message = "Preparando el proyecto…", value = 0.5, export_project(con, proj$id, file))
    )
    outputOptions(output, "dl_project", suspendWhenHidden = FALSE)

    invisible(proj)
  })
}

# Quita tildes y eñes sin depender de iconv (que varía entre sistemas).
strip_accents <- function(x) {
  x <- enc2utf8(as.character(x))
  chartr("áéíóúàèìòùâêîôûäëïöüñçÁÉÍÓÚÀÈÌÒÙÂÊÎÔÛÄËÏÖÜÑÇ",
         "aeiouaeiouaeiouaeiouncAEIOUAEIOUAEIOUAEIOUNC", x)
}

safe_filename <- function(x) {
  x <- strip_accents(x %||% "datos")
  x <- gsub("[^A-Za-z0-9_ -]", "", x)
  x <- gsub("\\s+", "_", trimws(x))
  if (is.na(x) || !nzchar(x)) "datos" else substr(x, 1, 80)
}

# Nombres válidos para SPSS/Stata: letras, números y _, empezando por letra.
ascii_names <- function(x) {
  x <- gsub("[^A-Za-z0-9_]", "_", strip_accents(x))
  x <- gsub("_+", "_", gsub("^_+|_+$", "", x))
  x[!nzchar(x)] <- "v"
  x <- ifelse(grepl("^[A-Za-z]", x), x, paste0("v", x))
  make.unique(substr(x, 1, 30), sep = "_")
}

# Exporta los datos de trabajo en el formato pedido conservando etiquetas.
write_any <- function(d, file, format = "sav") {
  to_haven <- function(d) {
    for (v in names(d)) {
      x <- d[[v]]
      lab <- attr(x, "label", exact = TRUE)
      if (is.factor(x)) {
        codes <- as.integer(x)
        x <- haven::labelled(codes, labels = stats::setNames(seq_along(levels(x)), levels(x)))
      }
      if (!is.null(lab)) attr(x, "label") <- lab
      d[[v]] <- x
    }
    names(d) <- ascii_names(names(d))
    d
  }
  switch(format,
    sav = haven::write_sav(to_haven(d), file),
    dta = haven::write_dta(to_haven(d), file),
    rds = saveRDS(d, file),
    csv = utils::write.csv(d, file, row.names = FALSE, fileEncoding = "UTF-8", na = ""),
    stop("Formato no admitido"))
  invisible(file)
}
