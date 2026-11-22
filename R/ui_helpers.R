# Piezas de interfaz comunes ----------------------------------------------------------

logo_img <- function(height = 40) {
  tags$img(src = "caring/img/caring-logo-blanco.png", alt = "caring", class = "logo",
           style = paste0("height:", height, "px;width:auto"))
}

initials <- function(name) {
  if (is.null(name) || is.na(name) || !nzchar(name)) return("?")
  p <- strsplit(trimws(name), "\\s+")[[1]]
  toupper(paste0(substr(p[1], 1, 1), if (length(p) > 1) substr(p[length(p)], 1, 1) else ""))
}

topbar <- function(user, active = "projects") {
  guest <- identical(user$rol, "invitado")
  tags$header(class = "c-topbar",
    actionLink("nav_home", logo_img(), `aria-label` = "caring, mis proyectos"),
    div(class = "c-nav",
      if (guest) span(class = "c-guest-badge", "Modo invitado · ",
                      actionLink("nav_register", "crea una cuenta para guardar")),
      actionLink("nav_projects", "Mis proyectos", class = paste("c-navlink", if (active == "projects") "active")),
      if (identical(user$rol, "admin"))
        actionLink("nav_admin", "Administración", class = paste("c-navlink", if (active == "admin") "active")),
      tags$a(href = "#", class = "c-navlink", onclick = "Shiny.setInputValue('show_help', Date.now()); return false;", "Ayuda"),
      div(class = "dropdown",
        tags$button(class = "c-avatar", type = "button", `data-bs-toggle` = "dropdown",
                    `aria-expanded` = "false", `aria-label` = "Mi cuenta",
                    if (guest) "?" else initials(user$nombre)),
        tags$ul(class = "dropdown-menu dropdown-menu-end",
          tags$li(tags$span(class = "dropdown-item-text fw-bold", user$nombre %||% "")),
          if (!guest) tags$li(tags$span(class = "dropdown-item-text small-2", user$email)),
          tags$li(tags$hr(class = "dropdown-divider")),
          tags$li(actionLink("nav_account", if (guest) "Crear cuenta" else "Mi cuenta", class = "dropdown-item")),
          tags$li(actionLink("nav_logout", "Cerrar sesión", class = "dropdown-item"))
        )
      )
    )
  )
}

footer_bar <- function() {
  tags$footer(class = "c-footer",
    span("© 2026 proyecto netCoin · Universidad de Salamanca"),
    span(tags$a(href = "https://sociocav.usal.es/blog/nca/", target = "_blank", rel = "noopener", "El proyecto"),
         " · ", tags$a(href = "mailto:gas@usal.es", "Contacto: gas@usal.es"),
         " · ", tags$a(href = "#", onclick = "Shiny.setInputValue('show_privacy', Date.now()); return false;",
                       "Privacidad"))
  )
}

step_defs <- c(datos = "Datos", explorar = "Explorar", redes = "Redes", informes = "Informes")

steps_nav <- function(active, input_id = "project-step") {
  items <- list()
  for (i in seq_along(step_defs)) {
    key <- names(step_defs)[i]
    if (i > 1) items <- c(items, list(span(class = "sep", `aria-hidden` = "true")))
    items <- c(items, list(tags$button(
      type = "button", class = paste("c-step", if (key == active) "active"),
      onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", input_id, key),
      span(class = "num", i), step_defs[[i]])))
  }
  tags$nav(class = "c-steps", `aria-label` = "Pasos del proyecto", items)
}

# Tabla HTML con el estilo de CARING.
html_table <- function(df, num_cols = NULL, total_last = FALSE) {
  if (is.null(df) || !nrow(df)) return(div(class = "c-hint", "Sin datos."))
  if (is.null(num_cols)) num_cols <- names(df)[-1]
  tags$table(class = "c-table",
    tags$thead(tags$tr(lapply(names(df), function(n) tags$th(class = if (n %in% num_cols) "num", n)))),
    tags$tbody(lapply(seq_len(nrow(df)), function(i) {
      tags$tr(class = if (total_last && i == nrow(df)) "total",
              lapply(names(df), function(n) tags$td(class = if (n %in% num_cols) "num", as.character(df[i, n]))))
    }))
  )
}

type_chip <- function(tipo) {
  cls <- switch(tipo, "categórica" = "cat", "ordinal" = "cat", "numérica" = "num", "txt")
  span(class = paste("c-chip", cls), tipo)
}

time_ago <- function(x) {
  t <- parse_utc(x)
  if (is.na(t)) return("")
  s <- as.numeric(difftime(Sys.time(), t, units = "secs"))
  if (s < 60) "hace un momento"
  else if (s < 3600) paste0("hace ", floor(s / 60), " min")
  else if (s < 86400) paste0("hace ", floor(s / 3600), " h")
  else if (s < 2 * 86400) "ayer"
  else if (s < 30 * 86400) paste0("hace ", floor(s / 86400), " días")
  else format(as.Date(t), "%d/%m/%Y")
}

fmt_bytes <- function(b) {
  if (is.na(b) || b < 1024^2) paste(fmt_num(b / 1024, 0), "KB")
  else if (b < 1024^3) paste(fmt_num(b / 1024^2, 1), "MB")
  else paste(fmt_num(b / 1024^3, 1), "GB")
}

# Notificación de error a partir de una condición.
notify_error <- function(e) {
  msg <- if (inherits(e, "condition")) conditionMessage(e) else as.character(e)
  showNotification(msg, type = "error", duration = 8)
}

# Ejecuta código mostrando el error al usuario en vez de romper la sesión.
safely <- function(expr) {
  tryCatch(expr, error = function(e) { notify_error(e); NULL })
}

seg_radio <- function(id, choices, selected = NULL, label = NULL) {
  div(class = "c-seg", radioButtons(id, label, choices = choices, selected = selected, inline = TRUE))
}

net_placeholder_svg <- function() {
  HTML('<svg role="img" aria-label="Sin vista previa" width="200" height="100" viewBox="0 0 200 100">
  <g stroke="#C6C6C6" stroke-width="1.5"><line x1="40" y1="30" x2="95" y2="55"/><line x1="95" y1="55" x2="150" y2="25"/>
  <line x1="95" y1="55" x2="70" y2="85"/><line x1="150" y1="25" x2="170" y2="75"/><line x1="40" y1="30" x2="70" y2="85"/></g>
  <circle cx="40" cy="30" r="9" fill="#1F77B4"/><circle cx="95" cy="55" r="13" fill="#FF7F0E"/>
  <circle cx="150" cy="25" r="8" fill="#2CA02C"/><circle cx="70" cy="85" r="7" fill="#1F77B4"/>
  <circle cx="170" cy="75" r="9" fill="#9467BD"/></svg>')
}

privacy_text <- function() {
  tagList(
    p("CARING guarda los archivos que subes y los resultados que generas solo para que puedas trabajar con ellos."),
    tags$ul(
      tags$li("Tus proyectos solo los ves tú y las personas con las que los compartas."),
      tags$li("El equipo de administración ve tu nombre, correo y el espacio que usas, pero no el contenido de tus datos."),
      tags$li("Los proyectos de invitado se borran automáticamente pasadas 24 horas."),
      tags$li("Puedes descargar o borrar tus proyectos y tu cuenta en cualquier momento desde «Mi cuenta»."),
      tags$li("No usamos analítica de terceros ni compartimos datos con nadie.")
    ),
    p(class = "small-2", "Si tus datos contienen información personal, eres responsable de tener la base legal para tratarlos (RGPD)."),
    p(class = "small-2", "Responsable: equipo CARING, proyecto netCoin (Universidad de Salamanca). Contacto: ",
      tags$a(href = "mailto:gas@usal.es", "gas@usal.es"), ".")
  )
}
