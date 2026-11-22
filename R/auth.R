# Usuarios, contraseñas y sesiones -------------------------------------------------

new_id <- function() uuid::UUIDgenerate()

random_token <- function() sodium::bin2hex(sodium::random(32))

hash_token <- function(token) digest::digest(token, algo = "sha256", serialize = FALSE)

hash_password <- function(pw) sodium::password_store(pw)

check_password <- function(hash, pw) {
  if (is.null(hash) || is.na(hash) || !nzchar(hash)) return(FALSE)
  isTRUE(tryCatch(sodium::password_verify(hash, pw), error = function(e) FALSE))
}

valid_email <- function(x) {
  is.character(x) && length(x) == 1 && grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", x, perl = TRUE)
}

password_problem <- function(pw) {
  if (!is.character(pw) || nchar(pw) < 10) return("La contraseña debe tener al menos 10 caracteres.")
  NULL
}

normalize_email <- function(x) tolower(trimws(x))

get_user <- function(con, id) {
  r <- db_get(con, "SELECT * FROM users WHERE id = ?", list(id))
  if (nrow(r)) as.list(r[1, ]) else NULL
}

get_user_by_email <- function(con, email) {
  r <- db_get(con, "SELECT * FROM users WHERE email = ?", list(normalize_email(email)))
  if (nrow(r)) as.list(r[1, ]) else NULL
}

# Crea un usuario. Devuelve list(ok, id | error).
create_user <- function(con, email, nombre, password, rol = "usuario", estado = "activo") {
  email <- normalize_email(email)
  if (!valid_email(email)) return(list(ok = FALSE, error = "El correo no es válido."))
  p <- password_problem(password)
  if (!is.null(p)) return(list(ok = FALSE, error = p))
  if (!is.null(get_user_by_email(con, email))) {
    return(list(ok = FALSE, error = "Ya existe una cuenta con ese correo."))
  }
  id <- new_id()
  db_run(con, "INSERT INTO users (id, email, nombre, password_hash, rol, estado, creado)
               VALUES (?, ?, ?, ?, ?, ?, ?)",
         list(id, email, trimws(nombre), hash_password(password), rol, estado, now_utc()))
  audit(con, id, "registro", email)
  list(ok = TRUE, id = id)
}

create_guest <- function(con) {
  id <- new_id()
  db_run(con, "INSERT INTO users (id, nombre, rol, estado, creado, ultimo_acceso)
               VALUES (?, 'Invitado', 'invitado', 'activo', ?, ?)",
         list(id, now_utc(), now_utc()))
  id
}

# Convierte una cuenta de invitado en cuenta propia: conserva sus proyectos.
convert_guest <- function(con, id, email, nombre, password, estado = "activo") {
  email <- normalize_email(email)
  if (!valid_email(email)) return(list(ok = FALSE, error = "El correo no es válido."))
  p <- password_problem(password)
  if (!is.null(p)) return(list(ok = FALSE, error = p))
  if (!is.null(get_user_by_email(con, email))) {
    return(list(ok = FALSE, error = "Ya existe una cuenta con ese correo."))
  }
  db_run(con, "UPDATE users SET email = ?, nombre = ?, password_hash = ?, rol = 'usuario', estado = ?
               WHERE id = ? AND rol = 'invitado'",
         list(email, trimws(nombre), hash_password(password), estado, id))
  audit(con, id, "registro_desde_invitado", email)
  list(ok = TRUE, id = id)
}

# Inicio de sesión con bloqueo temporal tras 5 intentos fallidos.
login_user <- function(con, email, password) {
  u <- get_user_by_email(con, email)
  fail <- list(ok = FALSE, error = "Correo o contraseña incorrectos.")
  if (is.null(u)) return(fail)
  if (!is.na(u$bloqueado_hasta) && parse_utc(u$bloqueado_hasta) > Sys.time()) {
    return(list(ok = FALSE, error = "Demasiados intentos fallidos. Prueba de nuevo en unos minutos."))
  }
  if (!check_password(u$password_hash, password)) {
    n <- u$intentos_fallidos + 1
    hasta <- if (n >= 5) now_utc(15 * 60) else NA_character_
    db_run(con, "UPDATE users SET intentos_fallidos = ?, bloqueado_hasta = ? WHERE id = ?",
           list(if (n >= 5) 0L else as.integer(n), hasta, u$id))
    audit(con, u$id, "login_fallido")
    return(fail)
  }
  if (u$estado == "bloqueado") {
    return(list(ok = FALSE, error = "Esta cuenta está bloqueada. Contacta con el equipo de CARING."))
  }
  if (u$estado == "pendiente") {
    return(list(ok = FALSE, error = "Tienes que verificar tu correo antes de entrar. Revisa tu bandeja de entrada."))
  }
  db_run(con, "UPDATE users SET intentos_fallidos = 0, bloqueado_hasta = NULL, ultimo_acceso = ? WHERE id = ?",
         list(now_utc(), u$id))
  audit(con, u$id, "login")
  list(ok = TRUE, user = get_user(con, u$id))
}

# Sesiones -----------------------------------------------------------------------

create_session <- function(con, user_id, days = 1) {
  token <- random_token()
  db_run(con, "INSERT INTO sessions (token_hash, user_id, creado, expira) VALUES (?, ?, ?, ?)",
         list(hash_token(token), user_id, now_utc(), now_utc(days * 86400)))
  token
}

session_user <- function(con, token) {
  if (is.null(token) || !nzchar(token)) return(NULL)
  r <- db_get(con, "SELECT user_id, expira FROM sessions WHERE token_hash = ?", list(hash_token(token)))
  if (!nrow(r) || parse_utc(r$expira[1]) < Sys.time()) return(NULL)
  u <- get_user(con, r$user_id[1])
  if (is.null(u) || u$estado == "bloqueado") return(NULL)
  db_run(con, "UPDATE users SET ultimo_acceso = ? WHERE id = ?", list(now_utc(), u$id))
  u
}

delete_session <- function(con, token) {
  if (!is.null(token) && nzchar(token)) {
    db_run(con, "DELETE FROM sessions WHERE token_hash = ?", list(hash_token(token)))
  }
}

# Tokens de un solo uso (verificar correo, recuperar contraseña) -----------------

create_token <- function(con, user_id, tipo, hours = 1) {
  token <- random_token()
  db_run(con, "INSERT INTO tokens (token_hash, tipo, user_id, expira) VALUES (?, ?, ?, ?)",
         list(hash_token(token), tipo, user_id, now_utc(hours * 3600)))
  token
}

use_token <- function(con, token, tipo) {
  r <- db_get(con, "SELECT * FROM tokens WHERE token_hash = ? AND tipo = ?", list(hash_token(token), tipo))
  if (!nrow(r) || !is.na(r$usado[1]) || parse_utc(r$expira[1]) < Sys.time()) return(NULL)
  db_run(con, "UPDATE tokens SET usado = ? WHERE token_hash = ?", list(now_utc(), hash_token(token)))
  r$user_id[1]
}

request_password_reset <- function(con, email, base_url = "") {
  u <- get_user_by_email(con, email)
  # Respuesta idéntica exista o no la cuenta, para no revelar qué correos están registrados.
  if (!is.null(u) && u$rol != "invitado") {
    tk <- create_token(con, u$id, "recuperar", hours = 1)
    send_mail(u$email, "CARING: recuperar contraseña",
              paste0("Hola ", u$nombre, ",\n\nPara elegir una contraseña nueva abre este enlace (caduca en 1 hora):\n",
                     base_url, "?recuperar=", tk, "\n\nSi no lo has pedido tú, ignora este mensaje."))
    audit(con, u$id, "recuperar_solicitado")
  }
  invisible(TRUE)
}

reset_password <- function(con, token, password) {
  p <- password_problem(password)
  if (!is.null(p)) return(list(ok = FALSE, error = p))
  uid <- use_token(con, token, "recuperar")
  if (is.null(uid)) return(list(ok = FALSE, error = "El enlace no es válido o ha caducado."))
  db_run(con, "UPDATE users SET password_hash = ?, intentos_fallidos = 0, bloqueado_hasta = NULL WHERE id = ?",
         list(hash_password(password), uid))
  db_run(con, "DELETE FROM sessions WHERE user_id = ?", list(uid))
  audit(con, uid, "contraseña_cambiada")
  list(ok = TRUE)
}

send_verification <- function(con, user_id, base_url = "") {
  u <- get_user(con, user_id)
  tk <- create_token(con, user_id, "verificar", hours = 48)
  send_mail(u$email, "CARING: confirma tu correo",
            paste0("Hola ", u$nombre, ",\n\nConfirma tu cuenta abriendo este enlace:\n",
                   base_url, "?verificar=", tk, "\n"))
}

verify_email <- function(con, token) {
  uid <- use_token(con, token, "verificar")
  if (is.null(uid)) return(FALSE)
  db_run(con, "UPDATE users SET estado = 'activo', verificado = ? WHERE id = ? AND estado = 'pendiente'",
         list(now_utc(), uid))
  TRUE
}

# Correo --------------------------------------------------------------------------
# En local los correos se guardan como archivos de texto en <storage>/correo y se
# muestran en la consola. En el servidor se cambiará por SMTP (CARING_MAIL=smtp).

send_mail <- function(to, subject, body) {
  cfg <- caring_config()
  if (cfg$mail == "console") {
    dir <- file.path(cfg$storage, "correo")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    f <- file.path(dir, paste0(format(Sys.time(), "%Y%m%d-%H%M%OS3"), ".txt"))
    writeLines(c(paste("Para:", to), paste("Asunto:", subject), "", body), f, useBytes = TRUE)
    message("[correo] ", subject, " -> ", to, " (", f, ")")
  } else {
    stop("Envío por SMTP aún no configurado (CARING_MAIL=", cfg$mail, ")")
  }
  invisible(TRUE)
}

list_users <- function(con) {
  db_get(con, "
    SELECT u.id, u.nombre, u.email, u.rol, u.estado, u.cuota_mb, u.creado, u.ultimo_acceso,
           COUNT(p.id) AS proyectos, COALESCE(SUM(p.bytes), 0) AS bytes
    FROM users u LEFT JOIN projects p ON p.owner_id = u.id
    WHERE u.rol != 'invitado'
    GROUP BY u.id
    ORDER BY u.creado DESC")
}

update_user_admin <- function(con, id, rol = NULL, estado = NULL, cuota_mb = NULL) {
  if (!is.null(rol)) db_run(con, "UPDATE users SET rol = ? WHERE id = ?", list(rol, id))
  if (!is.null(estado)) db_run(con, "UPDATE users SET estado = ? WHERE id = ?", list(estado, id))
  if (!is.null(cuota_mb)) db_run(con, "UPDATE users SET cuota_mb = ? WHERE id = ?", list(cuota_mb, id))
  if (identical(estado, "bloqueado")) db_run(con, "DELETE FROM sessions WHERE user_id = ?", list(id))
  invisible(TRUE)
}

delete_user <- function(con, id, storage = caring_config()$storage) {
  unlink(user_dir(id, storage), recursive = TRUE)
  db_run(con, "DELETE FROM users WHERE id = ?", list(id))
  audit(con, NA_character_, "usuario_borrado", id)
  invisible(TRUE)
}

# Borra invitados caducados y sus proyectos.
cleanup_guests <- function(con, horas = get_setting(con, "invitados_horas"),
                           storage = caring_config()$storage) {
  limite <- now_utc(-horas * 3600)
  old <- db_get(con, "SELECT id FROM users WHERE rol = 'invitado' AND COALESCE(ultimo_acceso, creado) < ?",
                list(limite))
  for (id in old$id) {
    unlink(user_dir(id, storage), recursive = TRUE)
    db_run(con, "DELETE FROM users WHERE id = ?", list(id))
  }
  db_run(con, "DELETE FROM sessions WHERE expira < ?", list(now_utc()))
  invisible(nrow(old))
}
