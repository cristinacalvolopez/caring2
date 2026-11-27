# Aplicación ------------------------------------------------------------------------------

.caring <- new.env()

get_con <- function() {
  if (is.null(.caring$con) || !DBI::dbIsValid(.caring$con)) .caring$con <- db_connect()
  .caring$con
}

caring_theme <- function() {
  bslib::bs_theme(
    version = 5, primary = "#236CDB", secondary = "#6B6B6B", danger = "#B3261E", success = "#1E6B34",
    "body-bg" = "#F5F5F5", "body-color" = "#000000", "border-radius" = "5px",
    base_font = bslib::font_collection("Roboto", "Arial", "sans-serif"),
    heading_font = bslib::font_collection("Roboto", "Arial", "sans-serif"),
    "font-size-base" = "0.95rem"
  )
}

#' Arranca CARING 2.
#' @param port puerto (por defecto, uno libre)
#' @param launch.browser abrir el navegador
#' @param ... otros argumentos para shiny::runApp
#' @export
run_app <- function(port = getOption("shiny.port"), launch.browser = interactive(), host = "127.0.0.1", ...) {
  shiny::runApp(caring_app(), port = port, launch.browser = launch.browser, host = host, ...)
}

caring_app <- function() {
  # Mensajes de R en español cuando R tiene la traducción (el resto los traduce error_es()).
  Sys.setenv(LANGUAGE = "es")
  cfg <- caring_config()
  dir.create(cfg$storage, recursive = TRUE, showWarnings = FALSE)
  shiny::addResourcePath("caring", app_file("www"))
  con <- get_con()
  apply_upload_limit(con)
  try(cleanup_guests(con), silent = TRUE)
  shiny::onStop(function() {
    if (!is.null(.caring$con) && DBI::dbIsValid(.caring$con)) DBI::dbDisconnect(.caring$con)
    .caring$con <- NULL
  })
  shiny::shinyApp(app_ui, app_server)
}

# Añade una versión a la URL de estilos y scripts para que el navegador no use una
# copia antigua guardada cuando se actualiza la aplicación.
asset_url <- function(file) {
  path <- app_file("www", file)
  v <- if (file.exists(path)) substr(digest::digest(file = path, algo = "md5"), 1, 8) else "0"
  paste0("caring/", file, "?v=", v)
}

app_ui <- function(request) {
  bslib::page(
    title = "CARING · Coincidencias en encuestas",
    theme = caring_theme(),
    lang = "es",
    tags$head(
      tags$link(rel = "stylesheet", href = "caring/fonts/roboto.css"),
      tags$link(rel = "stylesheet", href = asset_url("caring.css")),
      tags$link(rel = "icon", href = "data:,"),
      tags$script(src = asset_url("caring.js")),
      tags$script(src = asset_url("editor.js")),
      # dependencias que se usan dentro de uiOutput: se cargan desde el principio
      htmltools::findDependencies(list(
        reactable::reactableOutput("__dep1"),
        sortable::rank_list(text = NULL, labels = "x", input_id = "__dep3"),
        selectizeInput("__dep4", NULL, choices = "x")))
    ),
    uiOutput("page")
  )
}

app_server <- function(input, output, session) {
  con <- get_con()
  state <- reactiveValues(user = NULL, token = NULL, page = "loading", project = NULL, step = "datos",
                          notice = NULL, reset_token = NULL, share_project = NULL, projects_version = 0,
                          login_version = 0, open_new_modal = FALSE)

  base_url <- reactive({
    cd <- session$clientData
    port <- if (nzchar(cd$url_port %||% "") && !cd$url_port %in% c("80", "443")) paste0(":", cd$url_port) else ""
    paste0(cd$url_protocol, "//", cd$url_hostname, port, cd$url_pathname)
  })

  # Limpieza periódica de invitados (como mucho una vez por hora)
  if (is.null(.caring$last_cleanup) || difftime(Sys.time(), .caring$last_cleanup, units = "mins") > 60) {
    .caring$last_cleanup <- Sys.time()
    try(cleanup_guests(con), silent = TRUE)
    try(purge_old_trash(con), silent = TRUE)
  }

  # Enlace público de un informe o presentación (?ver=…): se muestra sin pedir cuenta ni cookie
  observe({
    q <- parseQueryString(session$clientData$url_search %||% "")
    if (is.null(q$ver)) return()
    pub <- tryCatch(resolve_public_link(con, q$ver), error = function(e) NULL)
    isolate({
      state$public <- pub %||% list(error = TRUE)
      state$page <- "public"
    })
  })

  # Arranque: enlaces de verificación / recuperación y sesión guardada en la cookie
  # Plan B: si el navegador no envía el token en 4 s, se muestra el acceso igualmente.
  auth_wait <- reactiveVal(TRUE)
  observe({
    if (isTRUE(isolate(auth_wait()))) {
      invalidateLater(4000)
      if (!is.null(isolate(state$user)) || !identical(isolate(state$page), "loading")) return(auth_wait(FALSE))
      if (identical(isolate(state$page), "public")) return(auth_wait(FALSE))
      if (isTRUE(isolate(session$userData$waited))) {
        auth_wait(FALSE)
        state$page <- "login"
      }
      session$userData$waited <- TRUE
    }
  })

  observeEvent(input$auth_token, {
    q <- parseQueryString(session$clientData$url_search)
    if (!is.null(q$ver) || identical(state$page, "public")) return()
    if (!is.null(q$verificar)) {
      ok <- verify_email(con, q$verificar)
      state$notice <- list(ok = ok, text = if (ok) "Correo confirmado. Ya puedes iniciar sesión." else "El enlace de confirmación no es válido o ya se usó.")
      session$sendCustomMessage("caring-clean-url", list())
    }
    if (!is.null(q$recuperar)) {
      state$reset_token <- q$recuperar
      session$sendCustomMessage("caring-clean-url", list())
    }
    u <- if (is.null(q$recuperar)) session_user(con, input$auth_token)
    if (!is.null(u)) {
      state$user <- u
      state$token <- input$auth_token
      state$page <- "projects"
    } else {
      state$page <- "login"
    }
  }, once = TRUE)

  on_login <- function(user, remember) {
    auth_wait(FALSE)
    days <- if (remember) 30 else 1
    token <- create_session(con, user$id, days)
    session$sendCustomMessage("caring-set-cookie",
      list(token = token, days = if (remember) days, secure = caring_config()$secure_cookies))
    state$user <- user
    state$token <- token
    state$notice <- NULL
    state$page <- "projects"
    if (identical(user$rol, "invitado")) state$open_new_modal <- TRUE
  }

  do_logout <- function() {
    removeModal()
    delete_session(con, state$token)
    session$sendCustomMessage("caring-clear-cookie", list())
    state$user <- NULL; state$token <- NULL; state$project <- NULL
    state$page <- "login"
  }

  open_project <- function(pid) {
    state$project <- NULL
    state$project <- pid
    state$step_on_open <- "datos"
    state$page <- "project"
  }

  mod_login_server("login", con, state, base_url, on_login)
  mod_projects_server("projects", con, state, open_project)
  mod_project_server("project", con, state, base_url)
  mod_admin_server("admin", con, state, base_url)
  mod_account_server("account", con, state, do_logout)

  # Página -----------------------------------------------------------------------------
  output$page <- renderUI({
    page <- state$page
    state$login_version
    if (page == "loading") return(div(class = "c-empty", style = "padding-top:30vh", "Cargando CARING…"))
    if (page == "public") return(public_page(state$public))
    if (page == "login" || is.null(state$user)) {
      return(mod_login_ui("login", notice = state$notice, reset_token = state$reset_token,
                          registro_abierto = get_setting(con, "registro_abierto")))
    }
    user <- isolate(state$user)
    active <- switch(page, admin = "admin", "projects")
    body <- switch(page,
      projects = mod_projects_ui("projects", user),
      project = mod_project_ui("project"),
      admin = if (identical(user$rol, "admin")) mod_admin_ui("admin") else div(class = "c-main", "Sin permiso."),
      account = mod_account_ui("account", user))
    div(style = "min-height:100vh; display:flex; flex-direction:column",
      skip_link(),
      topbar(user, active),
      tags$main(id = "main", tabindex = "-1", style = "flex-grow:1; outline:none", body),
      footer_bar())
  })

  # Cambios de usuario (nombre, registro de invitado) que afectan a la barra
  observeEvent(state$user, {
    if (!is.null(state$user) && state$page %in% c("projects", "account")) {
      p <- state$page
      state$page <- "loading"
      state$page <- p
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Navegación ----------------------------------------------------------------------------
  observeEvent(input$nav_home, if (!is.null(state$user)) state$page <- "projects")
  observeEvent(input$nav_projects, state$page <- "projects")
  observeEvent(input$nav_admin, state$page <- "admin")
  observeEvent(input$nav_account, state$page <- "account")
  observeEvent(input$nav_register, state$page <- "account")
  observeEvent(input$nav_logout, {
    if (identical(state$user$rol, "invitado")) {
      showModal(modalDialog(title = "Salir del modo invitado", easyClose = TRUE,
        p("Si sales ahora perderás tus proyectos de invitado. Para conservarlos, crea una cuenta."),
        footer = tagList(actionButton("logout_register", "Crear cuenta", class = "btn-primary"),
                         actionButton("logout_ok", "Salir igualmente", class = "btn-danger-c"))))
    } else do_logout()
  })
  observeEvent(input$logout_register, { removeModal(); state$page <- "account" })
  observeEvent(input$logout_ok, {
    removeModal()
    uid <- state$user$id
    do_logout()
    delete_user(con, uid)
  })

  # Compartir ------------------------------------------------------------------------------
  observeEvent(state$share_project, {
    pid <- state$share_project$id
    req(pid, identical(project_access(con, state$user$id, pid), "propietario"))
    if (identical(state$user$rol, "invitado")) {
      return(showNotification("Crea una cuenta para poder compartir proyectos.", type = "error"))
    }
    session$userData$share_pid <- pid
    showModal(modalDialog(title = paste("Compartir «", get_project(con, pid)$nombre, "»"), easyClose = TRUE,
      textInput("share_email", "Correo de la persona", placeholder = "nombre@ejemplo.com"),
      div(class = "d-flex gap-2 align-items-end",
        div(style = "flex:1", selectInput("share_perm", "Permiso", c("Lector" = "lector", "Editor" = "editor"))),
        actionButton("share_add", "Añadir", class = "btn-primary mb-3")),
      p(class = "c-hint", "Los lectores pueden ver y explorar el proyecto. Los editores también pueden cambiar los datos y los informes. La persona debe tener cuenta en CARING."),
      uiOutput("share_list"),
      footer = modalButton("Cerrar")))
  })
  share_v <- reactiveVal(0)
  output$share_list <- renderUI({
    share_v()
    pid <- session$userData$share_pid
    req(pid)
    m <- project_members(con, pid)
    if (!nrow(m)) return(p(class = "c-hint", "Todavía no lo has compartido con nadie."))
    tags$ul(class = "list-unstyled", lapply(seq_len(nrow(m)), function(i) tags$li(class = "d-flex justify-content-between mb-2",
      span(m$nombre[i], span(class = "small-2", paste0(" · ", m$email[i], " · ", m$permiso[i]))),
      tags$a(href = "#", onclick = sprintf("caringAction('share_remove', '%s', 'remove', event)", m$id[i]), "quitar"))))
  })
  observeEvent(input$share_add, {
    r <- share_project(con, session$userData$share_pid, input$share_email, input$share_perm)
    if (!r$ok) return(showNotification(r$error, type = "error"))
    audit(con, state$user$id, "proyecto_compartido", input$share_email)
    updateTextInput(session, "share_email", value = "")
    share_v(share_v() + 1)
  })
  observeEvent(input$share_remove, {
    unshare_project(con, session$userData$share_pid, input$share_remove$id)
    share_v(share_v() + 1)
  })

  # Ayuda y privacidad -----------------------------------------------------------------------
  observeEvent(input$show_privacy, {
    showModal(modalDialog(title = "Privacidad", easyClose = TRUE, privacy_text(), footer = modalButton("Cerrar")))
  })
  observeEvent(input$show_help, {
    showModal(modalDialog(title = "Cómo funciona CARING", size = "l", easyClose = TRUE, help_text(),
      footer = tagList(actionButton("tour_start", "Ver la visita guiada", class = "btn-outline-primary"), modalButton("Cerrar"))))
  })
  observeEvent(input$tour_start, {
    removeModal()
    donde <- if (identical(state$page, "project")) "proyecto" else "inicio"
    if (donde == "proyecto" && !identical(state$step, "datos")) {
      state$goto_step <- "datos"          # la visita empieza en el paso Datos
    }
    session$sendCustomMessage("caring-tour", list(steps = tour_steps(donde), key = donde, force = TRUE, delay = 600,
                                                  marks = if (donde == "proyecto") list("variables")))
  })
  # Primera vez: visita de la página de proyectos y, al abrir un proyecto, la del proyecto
  observeEvent(state$page, {
    if (identical(state$page, "projects") && !is.null(state$user) && !identical(state$user$rol, "invitado")) {
      session$sendCustomMessage("caring-tour", list(steps = tour_steps("inicio"), key = "inicio", force = FALSE, delay = 1200))
    }
  })
  observeEvent(state$project_loaded, {
    if (identical(state$step, "datos")) {
      # quien ya hizo la visita del proyecto ve, una vez, la de las novedades de Variables
      session$sendCustomMessage("caring-tour", list(steps = tour_steps("proyecto"), key = "proyecto", force = FALSE, delay = 1500, marks = list("variables"),
                                                    alt = list(steps = tour_steps("variables"), key = "variables")))
    }
  }, ignoreInit = TRUE)
}

help_text <- function() {
  tagList(
    tags$ol(
      tags$li(tags$strong("Datos. "), "Sube tu archivo (SPSS, Stata, R, CSV, Excel…). Renombra, recodifica, marca perdidos, crea variables o filtra casos. Cada cambio queda en el historial y se puede quitar; el archivo original nunca se modifica. ",
              "Si es un estudio del CIS, CARING te ofrece armonizar sus variables sociodemográficas (género, edad, ideología, estudios, clase…) para compararlo con otros estudios."),
      tags$li(tags$strong("Explorar. "), "Elige una variable para ver su distribución o dos para cruzarlas. CARING escoge el gráfico, la tabla y la prueba estadística."),
      tags$li(tags$strong("Redes. "), "Elige qué quieres ver (coincidencias, correlaciones, regresión…) y las variables. La red es interactiva: arrastra nodos, busca, filtra y exporta."),
      tags$li(tags$strong("Informes. "), "Con «+ Añadir al informe» guardas gráficos, tablas y redes. En Informes los colocas en diapositivas o en un informe y los exportas a PowerPoint, Word, PDF o HTML.")
    ),
    tags$h3(class = "c-help-h", "Organizar las variables"),
    tags$ul(
      tags$li(tags$strong("Elegir varias. "), "En la lista de Datos, marca las casillas: aparece una barra para añadirlas a un grupo, ocultarlas, renombrarlas, moverlas o eliminarlas."),
      tags$li(tags$strong("Grupos. "), "Reúne las variables que van juntas; una variable puede estar en varios grupos. Sirven para filtrar la lista y, en Explorar, Redes y las herramientas, para elegir un grupo entero de una vez. No cambian los datos."),
      tags$li(tags$strong("Ocultar. "), "Una variable oculta sigue en los datos, pero no aparece en las listas de los análisis. No es lo mismo que eliminarla."),
      tags$li(tags$strong("Renombrar y ordenar. "), "En Herramientas: renombra muchas variables con una expresión regular (con vista previa antes de aplicar) y ordénalas arrastrándolas, de la A a la Z, por grupos…"),
      tags$li("Todo queda en el historial, se puede quitar y aparece en el script de R.")),
    p(class = "c-hint", "Las redes se calculan con el paquete de R netCoin, del proyecto netCoin de la Universidad de Salamanca. ",
      "Más información en ", tags$a(href = "https://sociocav.usal.es/blog/nca/", target = "_blank", rel = "noopener", "sociocav.usal.es/blog/nca"),
      ". Dudas y sugerencias: ", tags$a(href = "mailto:gas@usal.es", "gas@usal.es"), ".")
  )
}

# Página pública (enlace de solo lectura de un informe o presentación)
public_page <- function(pub) {
  if (is.null(pub) || isTRUE(pub$error)) {
    return(tags$main(class = "c-empty", style = "padding-top:22vh",
      tags$img(src = "caring/img/caring-logo-azul.png", alt = "CARING", style = "height:36px"),
      tags$h1(style = "font-size:24px;margin-top:18px", "Este enlace no está disponible"),
      p("Puede que lo hayan anulado o que el documento se haya borrado. Pide un enlace nuevo a quien te lo envió.")))
  }
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f))
  slides <- !identical(pub$doc$type, "report")
  export_html(pub$dir, pub$doc, f, present = slides, interactive = TRUE)
  html <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  tags$main(class = "c-public",
    div(class = "c-public-bar",
      tags$img(src = "caring/img/caring-logo-blanco.png", alt = "CARING", style = "height:22px"),
      span(class = "c-public-title", pub$doc$title),
      span(class = "small-2", if (slides) "Presentación · usa las flechas o haz clic para avanzar" else "Informe · solo lectura")),
    # aislado de la aplicación: el documento no puede leer nada de la página que lo contiene
    tags$iframe(class = "c-public-frame", srcdoc = html, title = pub$doc$title, sandbox = "allow-scripts"))
}