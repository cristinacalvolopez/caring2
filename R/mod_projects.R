# Panel "Mis proyectos" ------------------------------------------------------------------

mod_projects_ui <- function(id, user) {
  ns <- NS(id)
  guest <- identical(user$rol, "invitado")
  tagList(
    div(class = "c-hero",
      div(class = "inner",
        div(style = "flex:1 1 480px",
          tags$h1(if (guest) "Prueba CARING" else "¿Qué quieres analizar hoy?"),
          tags$p(if (guest) "Estás en modo invitado: puedes usar todo, pero tus proyectos se borran a las 24 horas. Crea una cuenta para conservarlos."
                 else "Sube tu base de datos tal como esté. La limpias, la exploras, creas redes de coincidencias y montas el informe, todo en el mismo proyecto.")),
        div(style = "display:flex; flex-direction:column; align-items:flex-start; gap:8px",
          actionButton(ns("new"), "+ Nuevo proyecto", class = "btn-white"),
          span(style = "font-size:14px", "Stata · SPSS · R (.rds, .RData) · CSV · Excel · SAS"))
      )
    ),
    div(class = "c-main overlap",
      div(class = "c-toolbar",
        div(style = "flex:1 1 260px", textInput(ns("search"), NULL, placeholder = "Buscar proyectos")),
        seg_radio(ns("filter"), c("Todos" = "todos", "Míos" = "mios", "Compartidos conmigo" = "compartidos")),
        div(style = "width:210px", selectInput(ns("sort"), NULL,
          c("Última modificación" = "mod", "Nombre" = "nombre", "Fecha de creación" = "creado")))
      ),
      uiOutput(ns("grid"))
    )
  )
}

mod_projects_server <- function(id, con, state, on_open) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0)

    projects <- reactive({
      refresh(); state$projects_version
      req(state$user)
      list_projects(con, state$user$id)
    })

    output$grid <- renderUI({
      p <- projects()
      q <- tolower(trimws(input$search %||% ""))
      if (nzchar(q)) p <- p[grepl(q, tolower(p$nombre), fixed = TRUE) | grepl(q, tolower(p$archivo), fixed = TRUE), ]
      f <- input$filter %||% "todos"
      if (f == "mios") p <- p[p$permiso == "propietario", ]
      if (f == "compartidos") p <- p[p$permiso != "propietario", ]
      p <- switch(input$sort %||% "mod",
        nombre = p[order(tolower(p$nombre)), ],
        creado = p[order(p$creado, decreasing = TRUE), ],
        p[order(p$modificado, decreasing = TRUE), ])
      cards <- lapply(seq_len(nrow(p)), function(i) project_card(ns, con, p[i, ]))
      tagList(
        div(class = "c-grid",
          tags$div(class = "c-new", role = "button", tabindex = "0",
                   onclick = sprintf("Shiny.setInputValue('%s', Date.now(), {priority:'event'})", ns("new2")),
            span(class = "plus", "+"), tags$strong(style = "font-size:18px", "Nuevo proyecto"),
            span(class = "small-2", "Sube un archivo o usa los datos de demostración")),
          cards),
        if (!nrow(p) && nzchar(q)) p(class = "c-hint", style = "margin-top:16px", "Ningún proyecto coincide con la búsqueda.")
      )
    })

    new_modal <- function() {
      showModal(modalDialog(
        title = "Nuevo proyecto", easyClose = TRUE, size = "m",
        div(`data-max-mb` = as.numeric(get_setting(con, "max_mb")),
          fileInput(ns("file"), "Archivo de datos",
                    accept = c(paste0(".", names(formatos_admitidos)), ".caring"),
                    buttonLabel = "Elegir…", placeholder = "Ningún archivo")),
        p(class = "c-hint", "SPSS (.sav, .por), Stata (.dta), R (.rds, .RData), CSV, TSV, Excel (.xlsx, .xls) y SAS (.sas7bdat), ",
          "hasta ", tags$strong(fmt_num(as.numeric(get_setting(con, "max_mb")), 0), " MB"), ". ",
          "No hace falta que los datos estén limpios: podrás recodificar y crear variables después. ",
          "También puedes abrir un proyecto exportado de CARING (.caring)."),
        textInput(ns("name"), "Nombre del proyecto (opcional)"),
        div(class = "c-divider", "o"),
        actionButton(ns("demo"), "Usar los datos de demostración", class = "btn-outline-primary w-100"),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("create"), "Crear proyecto", class = "btn-primary"))
      ))
    }
    observeEvent(input$new, new_modal())
    observeEvent(input$new2, new_modal())
    observeEvent(state$open_new_modal, { if (isTRUE(state$open_new_modal)) { state$open_new_modal <- FALSE; new_modal() } })

    create <- function(path, file_name, nombre) {
      r <- if (identical(tolower(tools::file_ext(file_name)), "caring")) {
        withProgress(message = "Importando el proyecto…", value = 0.5, import_project(con, state$user, path, nombre))
      } else {
        withProgress(message = "Leyendo los datos…", value = 0.5, create_project(con, state$user, path, file_name, nombre))
      }
      if (!r$ok) { showNotification(r$error, type = "error", duration = 10); return() }
      removeModal()
      refresh(refresh() + 1)
      on_open(r$id)
    }

    observeEvent(input$create, {
      f <- input$file
      if (is.null(f)) { showNotification("Elige un archivo primero.", type = "error"); return() }
      nm <- trimws(input$name %||% "")
      create(f$datapath, f$name, if (nzchar(nm)) nm)
    })

    observeEvent(input$demo, {
      nm <- trimws(input$name %||% "")
      create(ext_file("demo_encuesta.sav"), "demo_encuesta.sav", if (nzchar(nm)) nm else "Encuesta de demostración")
    })

    observeEvent(input$card, {
      a <- input$card
      pid <- a$id
      perm <- project_access(con, state$user$id, pid)
      if (is.null(perm)) return()
      switch(a$action,
        open = on_open(pid),
        rename = {
          if (perm != "propietario") return(showNotification("Solo el propietario puede renombrar.", type = "error"))
          showModal(modalDialog(title = "Renombrar proyecto", easyClose = TRUE,
            textInput(ns("rename_value"), "Nombre", value = get_project(con, pid)$nombre),
            footer = tagList(modalButton("Cancelar"), actionButton(ns("rename_ok"), "Guardar", class = "btn-primary"))))
          session$userData$target <- pid
        },
        duplicate = {
          safely({
            duplicate_project(con, pid, state$user)
            refresh(refresh() + 1)
            showNotification("Proyecto duplicado.", type = "message")
          })
        },
        share = {
          session$userData$target <- pid
          state$share_project <- list(id = pid, t = Sys.time())
        },
        delete = {
          if (perm != "propietario") return(showNotification("Solo el propietario puede borrar el proyecto.", type = "error"))
          session$userData$target <- pid
          showModal(modalDialog(title = "Borrar proyecto", easyClose = TRUE,
            p("Se borrarán los datos, redes, resultados e informes de «", get_project(con, pid)$nombre, "». No se puede deshacer."),
            footer = tagList(modalButton("Cancelar"), actionButton(ns("delete_ok"), "Borrar", class = "btn-danger-c"))))
        },
        leave = {
          unshare_project(con, pid, state$user$id)
          refresh(refresh() + 1)
        }
      )
    })

    observeEvent(input$rename_ok, {
      nm <- trimws(input$rename_value %||% "")
      if (!nzchar(nm)) return()
      rename_project(con, session$userData$target, nm)
      removeModal()
      refresh(refresh() + 1)
    })

    observeEvent(input$delete_ok, {
      safely(delete_project(con, session$userData$target))
      removeModal()
      refresh(refresh() + 1)
    })
  })
}

project_card <- function(ns, con, p) {
  dir <- tryCatch(project_path(con, p$id), error = function(e) NULL)
  thumb <- if (!is.null(dir)) project_thumbnail(dir)
  n_res <- if (!is.null(dir)) length(list.files(file.path(dir, "resultados"), pattern = "\\.rds$")) else 0
  n_inf <- if (!is.null(dir)) length(list.files(file.path(dir, "informes"), pattern = "\\.json$")) else 0
  act <- function(a) sprintf("caringAction('%s', '%s', '%s', null); return false;", ns("card"), p$id, a)
  owner <- p$permiso == "propietario"
  tags$article(class = "c-project", tabindex = "0",
    onclick = sprintf("if (!event.target.closest('.dropdown')) caringAction('%s', '%s', 'open', null);", ns("card"), p$id),
    onkeydown = sprintf("if (event.key === 'Enter' && event.target === this) caringAction('%s', '%s', 'open', null);", ns("card"), p$id),
    `aria-label` = paste("Abrir", p$nombre),
    div(class = "thumb",
      if (!is.null(thumb)) tags$img(src = data_uri(thumb, "image/png"), alt = "")
      else net_placeholder_svg()),
    div(class = "dropdown", style = "position:absolute; right:8px; top:136px",
      tags$button(class = "menu-btn", style = "position:static", type = "button", `data-bs-toggle` = "dropdown",
                  `aria-label` = "Más opciones", HTML("&#8943;")),
      tags$ul(class = "dropdown-menu dropdown-menu-end",
        tags$li(tags$a(class = "dropdown-item", href = "#", onclick = act("open"), "Abrir")),
        if (owner) tags$li(tags$a(class = "dropdown-item", href = "#", onclick = act("rename"), "Renombrar")),
        tags$li(tags$a(class = "dropdown-item", href = "#", onclick = act("duplicate"), "Duplicar")),
        if (owner) tags$li(tags$a(class = "dropdown-item", href = "#", onclick = act("share"), "Compartir")),
        if (owner) tags$li(tags$a(class = "dropdown-item text-danger", href = "#", onclick = act("delete"), "Borrar"))
        else tags$li(tags$a(class = "dropdown-item", href = "#", onclick = act("leave"), "Dejar de ver"))
      )
    ),
    div(class = "body",
      tags$h3(p$nombre),
      span(class = "meta", paste0(p$archivo, " · ", fmt_int(p$n_casos), " casos · ", p$n_vars, " var.")),
      div(style = "margin-top:6px",
        if (n_res) span(class = "c-tag", n_res, if (n_res == 1) " resultado" else " resultados"),
        if (n_inf) span(class = "c-tag", n_inf, if (n_inf == 1) " informe" else " informes"),
        if (!owner) span(class = "c-tag", if (p$permiso == "editor") "Puedes editar" else "Solo lectura")),
      div(style = "flex-grow:1"),
      div(class = "foot", span(paste("Editado", time_ago(p$modificado))),
          span(if (owner) "Tú" else p$propietario))
    )
  )
}
