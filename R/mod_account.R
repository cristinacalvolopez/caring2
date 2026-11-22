# Mi cuenta (y registro desde el modo invitado) ------------------------------------------------

mod_account_ui <- function(id, user) {
  ns <- NS(id)
  guest <- identical(user$rol, "invitado")
  div(class = "c-main", style = "max-width:900px",
    div(class = "c-page-title", tags$h1(if (guest) "Crea tu cuenta" else "Mi cuenta")),
    if (guest) div(class = "c-card c-card-pad",
      p("Al crear la cuenta conservas los proyectos que has hecho como invitado."),
      textInput(ns("g_name"), "Nombre"),
      textInput(ns("g_email"), "Correo electrónico"),
      passwordInput(ns("g_pw"), "Contraseña (mínimo 10 caracteres)"),
      passwordInput(ns("g_pw2"), "Repite la contraseña"),
      checkboxInput(ns("g_privacy"), HTML("He leído la <a href='#' onclick=\"Shiny.setInputValue('show_privacy', Date.now()); return false;\">política de privacidad</a>")),
      div(class = "c-msg-error", textOutput(ns("g_msg"))),
      actionButton(ns("g_create"), "Crear cuenta", class = "btn-primary btn-lg-c"))
    else tagList(
      div(class = "c-card c-card-pad mb-4",
        tags$h2("Datos personales"),
        textInput(ns("name"), "Nombre", value = user$nombre),
        p(class = "c-hint", "Correo: ", user$email),
        actionButton(ns("save_name"), "Guardar", class = "btn-primary")),
      div(class = "c-card c-card-pad mb-4",
        tags$h2("Cambiar contraseña"),
        passwordInput(ns("pw_old"), "Contraseña actual"),
        passwordInput(ns("pw_new"), "Contraseña nueva (mínimo 10 caracteres)"),
        passwordInput(ns("pw_new2"), "Repite la contraseña nueva"),
        div(class = "c-msg-error", textOutput(ns("pw_msg"))),
        actionButton(ns("save_pw"), "Cambiar contraseña", class = "btn-primary")),
      div(class = "c-card c-card-pad mb-4",
        tags$h2("Tus datos"),
        p(class = "c-hint", "Descarga todos tus proyectos (datos originales, historial, resultados e informes) en un archivo .zip."),
        downloadButton(ns("export_all"), "Descargar todo", class = "btn-light-c", icon = NULL)),
      div(class = "c-card c-card-pad",
        tags$h2(style = "color:#B3261E", "Borrar la cuenta"),
        p(class = "c-hint", "Se borrarán tu cuenta y todos tus proyectos. Los proyectos que otras personas compartieron contigo no se borran."),
        textInput(ns("del_confirm"), "Escribe tu correo para confirmar"),
        actionButton(ns("delete"), "Borrar mi cuenta", class = "btn-danger-c"))
    )
  )
}

mod_account_server <- function(id, con, state, on_logout) {
  moduleServer(id, function(input, output, session) {
    msg <- reactiveValues(g = "", pw = "")
    output$g_msg <- renderText(msg$g)
    output$pw_msg <- renderText(msg$pw)

    observeEvent(input$g_create, {
      if (!nzchar(trimws(input$g_name %||% ""))) { msg$g <- "Escribe tu nombre."; return() }
      if (!identical(input$g_pw, input$g_pw2)) { msg$g <- "Las contraseñas no coinciden."; return() }
      if (!isTRUE(input$g_privacy)) { msg$g <- "Tienes que aceptar la política de privacidad."; return() }
      if (!isTRUE(get_setting(con, "registro_abierto"))) { msg$g <- "El registro está cerrado."; return() }
      r <- convert_guest(con, state$user$id, input$g_email, input$g_name, input$g_pw)
      if (!r$ok) { msg$g <- r$error; return() }
      state$user <- get_user(con, state$user$id)
      state$page <- "projects"
      showNotification("Cuenta creada. Tus proyectos ya están guardados.", type = "message")
    })

    observeEvent(input$save_name, {
      nm <- trimws(input$name %||% "")
      if (!nzchar(nm)) return()
      db_run(con, "UPDATE users SET nombre = ? WHERE id = ?", list(nm, state$user$id))
      state$user <- get_user(con, state$user$id)
      showNotification("Nombre actualizado.", type = "message")
    })

    observeEvent(input$save_pw, {
      u <- get_user(con, state$user$id)
      if (!check_password(u$password_hash, input$pw_old %||% "")) { msg$pw <- "La contraseña actual no es correcta."; return() }
      if (!identical(input$pw_new, input$pw_new2)) { msg$pw <- "Las contraseñas nuevas no coinciden."; return() }
      p <- password_problem(input$pw_new)
      if (!is.null(p)) { msg$pw <- p; return() }
      db_run(con, "UPDATE users SET password_hash = ? WHERE id = ?", list(hash_password(input$pw_new), u$id))
      audit(con, u$id, "contraseña_cambiada")
      msg$pw <- ""
      showNotification("Contraseña cambiada.", type = "message")
    })

    output$export_all <- downloadHandler(
      filename = function() paste0("caring_mis_proyectos_", format(Sys.Date(), "%Y%m%d"), ".zip"),
      content = function(file) {
        src <- file.path(user_dir(state$user$id), "projects")
        tmp <- tempfile("export-"); dir.create(tmp)
        ps <- list_projects(con, state$user$id)
        ps <- ps[ps$permiso == "propietario", ]
        for (i in seq_len(nrow(ps))) {
          dst <- file.path(tmp, paste0(safe_filename(ps$nombre[i]), "_", substr(ps$id[i], 1, 6)))
          dir.create(dst)
          file.copy(list.files(file.path(src, ps$id[i]), full.names = TRUE), dst, recursive = TRUE)
        }
        old <- setwd(tmp); on.exit({ setwd(old); unlink(tmp, recursive = TRUE) })
        utils::zip(file, files = list.files(".", recursive = TRUE), flags = "-qr9X")
      })

    observeEvent(input$delete, {
      if (!identical(normalize_email(input$del_confirm %||% ""), state$user$email)) {
        return(showNotification("Escribe exactamente tu correo para confirmar.", type = "error"))
      }
      admins <- db_get(con, "SELECT COUNT(*) AS n FROM users WHERE rol = 'admin'")$n
      if (identical(state$user$rol, "admin") && admins <= 1) {
        return(showNotification("Eres la única persona administradora: nombra a otra antes de borrar tu cuenta.", type = "error"))
      }
      delete_user(con, state$user$id)
      on_logout()
      showNotification("Tu cuenta y tus datos se han borrado.", type = "message")
    })
  })
}
