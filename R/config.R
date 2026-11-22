# Configuración ---------------------------------------------------------------
# Todo se controla con variables de entorno para que el mismo código funcione en
# local y en el servidor de la USAL.
#
#   CARING_STORAGE   carpeta de datos (por defecto ./storage)
#   CARING_DB        ruta del archivo SQLite (por defecto <storage>/caring.sqlite)
#   CARING_MAIL      "console" (por defecto: los correos se escriben en
#                    <storage>/correo/) o "smtp" (ver send_mail() en auth.R)
#   CARING_MAX_MB    tamaño máximo de subida en MB mientras no se cambie desde
#                    Administración (por defecto 200)
#   CARING_SECURE    "true" si la app se sirve por https (cookies Secure)

caring_config <- function() {
  storage <- Sys.getenv("CARING_STORAGE", file.path(getwd(), "storage"))
  list(
    storage = normalizePath(storage, mustWork = FALSE),
    db = Sys.getenv("CARING_DB", file.path(storage, "caring.sqlite")),
    mail = Sys.getenv("CARING_MAIL", "console"),
    max_mb = as.numeric(Sys.getenv("CARING_MAX_MB", "200")),
    secure_cookies = identical(tolower(Sys.getenv("CARING_SECURE", "false")), "true")
  )
}

# Ajustes que el administrador puede cambiar desde la app (tabla settings).
default_settings <- list(
  registro_abierto = TRUE,
  verificar_correo = FALSE,   # en local no hay servidor de correo
  cuota_mb = 2048,
  invitados_horas = 24,
  inactivas_anios = 2,
  max_mb = NULL               # tamaño máximo por archivo; si no se ha fijado, CARING_MAX_MB
)

app_file <- function(...) {
  p <- system.file("app", ..., package = "caring2")
  if (!nzchar(p)) p <- file.path(pkg_root(), "inst", "app", ...)
  p
}

ext_file <- function(...) {
  p <- system.file("extdata", ..., package = "caring2")
  if (!nzchar(p)) p <- file.path(pkg_root(), "inst", "extdata", ...)
  p
}

pkg_root <- function() {
  r <- getOption("caring2.root")
  if (is.null(r)) r <- getwd()
  r
}
