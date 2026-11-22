# Administración de usuarios (solo rol admin) ---------------------------------------------------

mod_admin_ui <- function(id) {
  ns <- NS(id)
  div(class = "c-main",
    div(class = "c-page-title",
      div(tags$h1("Usuarios"), div(class = "text-2", "Solo visible para el equipo de administración.")),
      actionButton(ns("invite"), "+ Invitar usuario", class = "btn-primary")),
    uiOutput(ns("kpis")),
    div(class = "row g-4",
      div(class = "col-lg-8",
        div(class = "c-card c-card-pad",
          reactable::reactableOutput(ns("users")),
          uiOutput(ns("user_actions")))),
      div(class = "col-lg-4",
        div(class = "c-card c-card-pad",
          tags$h2("Ajustes de acceso"),
          uiOutput(ns("settings_form"))))
    )
  )
}

mod_admin_server <- function(id, con, state, base_url) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    v <- reactiveVal(0)
    is_admin <- reactive(identical(state$user$rol, "admin"))

    users <- reactive({ v(); state$page; req(is_admin()); list_users(con) })

    output$kpis <- renderUI({
      u <- users()
      act <- sum(!is.na(u$ultimo_acceso) & parse_utc(u$ultimo_acceso) > Sys.time() - 30 * 86400)
      shared <- db_get(con, "SELECT COUNT(DISTINCT project_id) AS n FROM project_members")$n
      guests <- db_get(con, "SELECT COUNT(*) AS n FROM users WHERE rol = 'invitado'")$n
      kp <- function(l, val, note) div(class = "c-kpi", div(class = "small-2", l), div(class = "v", val), div(class = "small-2", note))
      div(class = "c-kpis",
        kp("Usuarios registrados", fmt_int(nrow(u)), paste(guests, "invitados ahora")),
        kp("Activos (30 días)", fmt_int(act), "sin contar invitados"),
        kp("Proyectos guardados", fmt_int(sum(u$proyectos)), paste(shared, "compartidos")),
        kp("Espacio usado", fmt_bytes(sum(u$bytes)), paste("cuota por defecto", get_setting(con, "cuota_mb"), "MB")))
    })

    output$users <- reactable::renderReactable({
      u <- users()
      u$ultimo <- vapply(u$ultimo_acceso, function(x) if (is.na(x)) "Nunca" else time_ago(x), "")
      u$espacio <- vapply(u$bytes, fmt_bytes, "")
      u$rol_txt <- c(admin = "Administración", usuario = "Usuario")[u$rol]
      reactable::reactable(u[c("nombre", "email", "rol_txt", "proyectos", "espacio", "ultimo", "estado")],
        selection = "single", searchable = TRUE, highlight = TRUE, compact = TRUE,
        onClick = reactable::JS(sprintf(
          "function(rowInfo) { rowInfo.toggleRowSelected(true); Shiny.setInputValue('%s', rowInfo.index + 1, {priority: 'event'}); }",
          ns("pick"))),
        language = reactable_es(), defaultPageSize = 15,
        columns = list(
          .selection = reactable::colDef(show = FALSE),
          nombre = reactable::colDef(name = "Nombre"), email = reactable::colDef(name = "Correo", minWidth = 160),
          rol_txt = reactable::colDef(name = "Rol"), proyectos = reactable::colDef(name = "Proyectos", align = "right"),
          espacio = reactable::colDef(name = "Espacio", align = "right"), ultimo = reactable::colDef(name = "Último acceso"),
          estado = reactable::colDef(name = "Estado", cell = function(x) span(class = paste("c-chip",
            switch(x, activo = "ok", pendiente = "warn", "err")), x))))
    })

    picked <- reactiveVal(NULL)
    observeEvent(input$pick, picked(input$pick))
    observeEvent(v(), picked(NULL), ignoreInit = TRUE)
    selected <- reactive({
      i <- picked()
      u <- users()
      if (length(i) && i <= nrow(u)) as.list(u[i, ]) else NULL
    })

    output$user_actions <- renderUI({
      s <- selected()
      if (is.null(s)) return(p(class = "c-hint mt-3", "Selecciona un usuario para cambiar su rol, su estado o su cuota."))
      me <- identical(s$id, state$user$id)
      div(class = "mt-3 pt-3", style = "border-top:1px solid #E3E3E3",
        tags$h3(s$nombre, span(class = "small-2", paste0(" · ", s$email))),
        div(class = "row g-2 align-items-end",
          div(class = "col-sm-3", selectInput(ns("rol"), "Rol", c("Usuario" = "usuario", "Administración" = "admin"), selected = s$rol)),
          div(class = "col-sm-3", selectInput(ns("estado"), "Estado", c("Activo" = "activo", "Pendiente" = "pendiente", "Bloqueado" = "bloqueado"), selected = s$estado)),
          div(class = "col-sm-3", numericInput(ns("cuota"), "Cuota (MB)", value = if (is.na(s$cuota_mb)) get_setting(con, "cuota_mb") else s$cuota_mb, min = 0)),
          div(class = "col-sm-3", actionButton(ns("save_user"), "Guardar", class = "btn-primary w-100 mb-3"))),
        if (!me) actionButton(ns("delete_user"), "Borrar usuario y sus proyectos", class = "btn-danger-c"),
        if (me) p(class = "c-hint", "Esta es tu cuenta: no puedes borrarla ni quitarte el rol desde aquí."))
    })

    observeEvent(input$save_user, {
      s <- selected(); req(s, is_admin())
      me <- identical(s$id, state$user$id)
      update_user_admin(con, s$id, rol = if (me) NULL else input$rol, estado = if (me) NULL else input$estado,
                        cuota_mb = input$cuota)
      audit(con, state$user$id, "admin_usuario_modificado", s$email)
      v(v() + 1)
      showNotification("Usuario actualizado.", type = "message")
    })

    observeEvent(input$delete_user, {
      s <- selected(); req(s)
      showModal(modalDialog(title = "Borrar usuario", easyClose = TRUE,
        p("Se borrarán la cuenta de ", tags$strong(s$email), " y todos sus proyectos. No se puede deshacer."),
        footer = tagList(modalButton("Cancelar"), actionButton(ns("delete_ok"), "Borrar", class = "btn-danger-c"))))
    })
    observeEvent(input$delete_ok, {
      s <- selected(); req(s, is_admin(), !identical(s$id, state$user$id))
      delete_user(con, s$id)
      removeModal(); v(v() + 1)
    })

    observeEvent(input$invite, {
      showModal(modalDialog(title = "Invitar usuario", easyClose = TRUE,
        textInput(ns("inv_name"), "Nombre"), textInput(ns("inv_email"), "Correo"),
        selectInput(ns("inv_rol"), "Rol", c("Usuario" = "usuario", "Administración" = "admin")),
        p(class = "c-hint", "Recibirá un enlace para elegir su contraseña (caduca en 48 horas)."),
        footer = tagList(modalButton("Cancelar"), actionButton(ns("inv_ok"), "Enviar invitación", class = "btn-primary"))))
    })
    observeEvent(input$inv_ok, {
      req(is_admin())
      r <- create_user(con, input$inv_email, input$inv_name, random_token(), rol = input$inv_rol)
      if (!r$ok) return(showNotification(r$error, type = "error"))
      tk <- create_token(con, r$id, "recuperar", hours = 48)
      send_mail(normalize_email(input$inv_email), "Te han invitado a CARING",
                paste0("Hola ", input$inv_name, ",\n\nTe han creado una cuenta en CARING. Elige tu contraseña aquí (caduca en 48 horas):\n",
                       base_url(), "?recuperar=", tk, "\n"))
      audit(con, state$user$id, "admin_invitacion", input$inv_email)
      removeModal(); v(v() + 1)
      showNotification("Invitación enviada.", type = "message")
    })

    output$settings_form <- renderUI({
      v(); req(is_admin())
      s <- get_settings(con)
      tagList(
        checkboxInput(ns("s_registro"), "Registro abierto (cualquiera puede crear una cuenta)", s$registro_abierto),
        checkboxInput(ns("s_verificar"), "Verificar el correo al registrarse", s$verificar_correo),
        numericInput(ns("s_cuota"), "Cuota por usuario (MB)", s$cuota_mb, min = 10),
        numericInput(ns("s_invitados"), "Borrar proyectos de invitado tras (horas)", s$invitados_horas, min = 1),
        numericInput(ns("s_inactivas"), "Avisar de cuentas inactivas tras (años)", s$inactivas_anios, min = 1),
        actionButton(ns("save_settings"), "Guardar ajustes", class = "btn-primary w-100"),
        if (identical(caring_config()$mail, "console"))
          p(class = "c-hint mt-2", "Modo local: los correos no se envían, se guardan en ", tags$code("storage/correo/"), ".")
      )
    })
    observeEvent(input$save_settings, {
      req(is_admin())
      set_setting(con, "registro_abierto", isTRUE(input$s_registro))
      set_setting(con, "verificar_correo", isTRUE(input$s_verificar))
      set_setting(con, "cuota_mb", max(10, input$s_cuota %||% 2048))
      set_setting(con, "invitados_horas", max(1, input$s_invitados %||% 24))
      set_setting(con, "inactivas_anios", max(1, input$s_inactivas %||% 2))
      audit(con, state$user$id, "admin_ajustes")
      v(v() + 1)
      showNotification("Ajustes guardados.", type = "message")
    })
  })
}
