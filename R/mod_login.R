# Página de acceso --------------------------------------------------------------------

mod_login_ui <- function(id, notice = NULL, reset_token = NULL, registro_abierto = TRUE) {
  ns <- NS(id)
  steps <- list(c("1", "Datos", "Carga y limpia"), c("2", "Explorar", "Tablas y gráficos"),
                c("3", "Redes", "Coincidencias"), c("4", "Informes", "Diapositivas"))
  div(class = "c-login",
    div(class = "left",
      div(logo_img(50)),
      div(style = "flex-grow:1; display:flex; flex-direction:column; justify-content:center; gap:20px; max-width:560px",
        tags$h1("Coincidencias en encuestas"),
        tags$p("Sube tu base de datos tal como esté, límpiala, explórala y conviértela en una red interactiva que muestra qué categorías tienden a coincidir."),
        tags$ol(class = "steps4", lapply(steps, function(s) tags$li(
          span(class = "n", s[1]), tags$strong(s[2]), span(style = "font-size:14px", s[3]))))
      ),
      span(style = "font-size:14px", "Proyecto netcoin · Universidad de Salamanca")
    ),
    div(class = "right",
      div(class = "box",
        if (!is.null(notice)) div(class = paste("alert", if (isTRUE(notice$ok)) "alert-success" else "alert-danger"), notice$text),
        if (!is.null(reset_token)) reset_form(ns) else tagList(
          bslib::navset_underline(id = ns("tab"),
            bslib::nav_panel("Iniciar sesión", value = "login",
              div(`data-enter-click` = ns("do_login"), style = "padding-top:20px",
                textInput(ns("email"), "Correo electrónico", placeholder = "nombre@ejemplo.com"),
                passwordInput(ns("password"), "Contraseña"),
                div(class = "d-flex justify-content-between align-items-center mb-3",
                  checkboxInput(ns("remember"), "Recordarme", value = TRUE),
                  actionLink(ns("go_forgot"), "¿Has olvidado la contraseña?")),
                div(class = "c-msg-error", textOutput(ns("login_msg"))),
                actionButton(ns("do_login"), "Entrar", class = "btn-primary btn-lg-c w-100")
              )
            ),
            bslib::nav_panel("Crear cuenta", value = "register",
              div(`data-enter-click` = ns("do_register"), style = "padding-top:20px",
                if (!isTRUE(registro_abierto)) div(class = "alert alert-warning",
                  "El registro de cuentas nuevas está cerrado en este momento. Puedes probar CARING sin cuenta."),
                textInput(ns("reg_name"), "Nombre"),
                textInput(ns("reg_email"), "Correo electrónico", placeholder = "nombre@ejemplo.com"),
                passwordInput(ns("reg_pw"), "Contraseña (mínimo 10 caracteres)"),
                passwordInput(ns("reg_pw2"), "Repite la contraseña"),
                checkboxInput(ns("reg_privacy"), HTML("He leído la <a href='#' onclick=\"Shiny.setInputValue('show_privacy', Date.now()); return false;\">política de privacidad</a>")),
                div(class = "c-msg-error", textOutput(ns("reg_msg"))),
                actionButton(ns("do_register"), "Crear cuenta", class = "btn-primary btn-lg-c w-100",
                             disabled = if (!isTRUE(registro_abierto)) "disabled")
              )
            ),
            bslib::nav_panel_hidden(value = "forgot",
              div(`data-enter-click` = ns("do_forgot"), style = "padding-top:20px",
                tags$h2(style = "font-size:22px", "Recuperar contraseña"),
                tags$p(class = "c-hint", "Te enviaremos un enlace para elegir una contraseña nueva."),
                textInput(ns("forgot_email"), "Correo electrónico"),
                div(class = "c-msg-ok", textOutput(ns("forgot_msg"))),
                actionButton(ns("do_forgot"), "Enviar enlace", class = "btn-primary btn-lg-c w-100"),
                div(style = "margin-top:12px", actionLink(ns("back_login"), "← Volver"))
              )
            )
          ),
          div(class = "c-divider", "o"),
          actionButton(ns("guest"), "Probar sin cuenta", class = "btn-outline-primary btn-lg-c w-100"),
          p(class = "small-2 text-center", style = "margin-top:8px",
            "Puedes usar todo CARING, pero el proyecto se borra a las 24 horas. Si después creas una cuenta, lo conservas.")
        ),
        p(class = "small-2", style = "margin-top:20px; border-top:1px solid #E3E3E3; padding-top:14px",
          "Tus datos solo los ves tú y las personas con las que compartas un proyecto. No se usan para nada más y puedes borrarlos cuando quieras. ",
          tags$a(href = "#", onclick = "Shiny.setInputValue('show_privacy', Date.now()); return false;", "Política de privacidad"))
      )
    )
  )
}

reset_form <- function(ns) {
  div(`data-enter-click` = ns("do_reset"),
    tags$h2(style = "font-size:22px", "Elige una contraseña nueva"),
    passwordInput(ns("reset_pw"), "Contraseña nueva (mínimo 10 caracteres)"),
    passwordInput(ns("reset_pw2"), "Repite la contraseña"),
    div(class = "c-msg-error", textOutput(ns("reset_msg"))),
    actionButton(ns("do_reset"), "Guardar contraseña", class = "btn-primary btn-lg-c w-100")
  )
}

# on_login(user, remember) lo proporciona el servidor principal.
mod_login_server <- function(id, con, state, base_url, on_login) {
  moduleServer(id, function(input, output, session) {
    msg <- reactiveValues(login = "", reg = "", forgot = "", reset = "")
    output$login_msg <- renderText(msg$login)
    output$reg_msg <- renderText(msg$reg)
    output$forgot_msg <- renderText(msg$forgot)
    output$reset_msg <- renderText(msg$reset)

    observeEvent(input$do_login, {
      r <- login_user(con, input$email %||% "", input$password %||% "")
      if (!r$ok) { msg$login <- r$error; return() }
      msg$login <- ""
      on_login(r$user, isTRUE(input$remember))
    })

    observeEvent(input$do_register, {
      if (!isTRUE(get_setting(con, "registro_abierto"))) { msg$reg <- "El registro está cerrado."; return() }
      if (!nzchar(trimws(input$reg_name %||% ""))) { msg$reg <- "Escribe tu nombre."; return() }
      if (!identical(input$reg_pw, input$reg_pw2)) { msg$reg <- "Las contraseñas no coinciden."; return() }
      if (!isTRUE(input$reg_privacy)) { msg$reg <- "Tienes que aceptar la política de privacidad."; return() }
      verify <- isTRUE(get_setting(con, "verificar_correo"))
      r <- create_user(con, input$reg_email, input$reg_name, input$reg_pw,
                       estado = if (verify) "pendiente" else "activo")
      if (!r$ok) { msg$reg <- r$error; return() }
      msg$reg <- ""
      if (verify) {
        send_verification(con, r$id, base_url())
        showModal(modalDialog(title = "Revisa tu correo", easyClose = TRUE, footer = modalButton("Entendido"),
          p("Te hemos enviado un enlace para confirmar tu cuenta. Ábrelo y después inicia sesión.")))
        bslib::nav_select("tab", "login")
      } else {
        on_login(get_user(con, r$id), TRUE)
      }
    })

    observeEvent(input$go_forgot, bslib::nav_select("tab", "forgot"))
    observeEvent(input$back_login, bslib::nav_select("tab", "login"))

    observeEvent(input$do_forgot, {
      if (!valid_email(normalize_email(input$forgot_email %||% ""))) { msg$forgot <- "Escribe un correo válido."; return() }
      request_password_reset(con, input$forgot_email, base_url())
      msg$forgot <- "Si hay una cuenta con ese correo, te hemos enviado un enlace. Caduca en 1 hora."
    })

    observeEvent(input$do_reset, {
      if (!identical(input$reset_pw, input$reset_pw2)) { msg$reset <- "Las contraseñas no coinciden."; return() }
      r <- reset_password(con, state$reset_token, input$reset_pw)
      if (!r$ok) { msg$reset <- r$error; return() }
      state$reset_token <- NULL
      state$notice <- list(ok = TRUE, text = "Contraseña cambiada. Ya puedes iniciar sesión.")
      state$login_version <- (state$login_version %||% 0) + 1
    })

    observeEvent(input$guest, {
      id <- create_guest(con)
      on_login(get_user(con, id), FALSE)
    })
  })
}
