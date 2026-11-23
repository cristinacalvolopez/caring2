# Base de datos -----------------------------------------------------------------
# SQLite en local. Las consultas usan solo SQL estándar y parámetros (?), de modo
# que el paso a PostgreSQL consiste en cambiar db_connect().

db_connect <- function(path = caring_config()$db) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
  db_migrate(con)
  con
}

db_migrate <- function(con) {
  stmts <- c(
    "CREATE TABLE IF NOT EXISTS users (
       id TEXT PRIMARY KEY,
       email TEXT UNIQUE,
       nombre TEXT,
       password_hash TEXT,
       rol TEXT NOT NULL DEFAULT 'usuario',      -- invitado | usuario | admin
       estado TEXT NOT NULL DEFAULT 'activo',    -- activo | pendiente | bloqueado
       cuota_mb REAL,
       creado TEXT NOT NULL,
       verificado TEXT,
       ultimo_acceso TEXT,
       intentos_fallidos INTEGER NOT NULL DEFAULT 0,
       bloqueado_hasta TEXT)",
    "CREATE TABLE IF NOT EXISTS sessions (
       token_hash TEXT PRIMARY KEY,
       user_id TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
       creado TEXT NOT NULL,
       expira TEXT NOT NULL)",
    "CREATE TABLE IF NOT EXISTS tokens (
       token_hash TEXT PRIMARY KEY,
       tipo TEXT NOT NULL,                       -- verificar | recuperar
       user_id TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
       expira TEXT NOT NULL,
       usado TEXT)",
    "CREATE TABLE IF NOT EXISTS projects (
       id TEXT PRIMARY KEY,
       owner_id TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
       nombre TEXT NOT NULL,
       archivo TEXT,
       n_casos INTEGER,
       n_vars INTEGER,
       bytes REAL DEFAULT 0,
       creado TEXT NOT NULL,
       modificado TEXT NOT NULL)",
    "CREATE TABLE IF NOT EXISTS project_members (
       project_id TEXT NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
       user_id TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
       permiso TEXT NOT NULL,                    -- lector | editor
       PRIMARY KEY (project_id, user_id))",
    "CREATE TABLE IF NOT EXISTS settings (
       clave TEXT PRIMARY KEY,
       valor TEXT NOT NULL)",
    "CREATE TABLE IF NOT EXISTS audit_log (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       user_id TEXT,
       accion TEXT NOT NULL,
       detalle TEXT,
       fecha TEXT NOT NULL)",
    "CREATE TABLE IF NOT EXISTS comments (
       id TEXT PRIMARY KEY,
       project_id TEXT NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
       user_id TEXT REFERENCES users(id) ON DELETE SET NULL,
       texto TEXT NOT NULL,
       creado TEXT NOT NULL)",
    "CREATE TABLE IF NOT EXISTS comment_reads (
       project_id TEXT NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
       user_id TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
       visto TEXT NOT NULL,
       PRIMARY KEY (project_id, user_id))",
    "CREATE TABLE IF NOT EXISTS public_links (
       token_hash TEXT PRIMARY KEY,
       project_id TEXT NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
       doc_id TEXT NOT NULL,
       creado TEXT NOT NULL,
       creado_por TEXT REFERENCES users(id) ON DELETE SET NULL,
       revocado TEXT)"
  )
  for (s in stmts) DBI::dbExecute(con, s)
  # columnas añadidas después (bases de datos creadas con versiones anteriores)
  add_column(con, "projects", "borrado", "TEXT")       # fecha en que se movió a la papelera
  invisible(TRUE)
}

add_column <- function(con, table, column, type) {
  if (!column %in% DBI::dbListFields(con, table)) DBI::dbExecute(con, paste("ALTER TABLE", table, "ADD COLUMN", column, type))
}

now_utc <- function(offset_secs = 0) {
  format(Sys.time() + offset_secs, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

parse_utc <- function(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

db_get <- function(con, sql, params = list()) {
  if (length(params)) DBI::dbGetQuery(con, sql, params = params) else DBI::dbGetQuery(con, sql)
}

db_run <- function(con, sql, params = list()) {
  if (length(params)) DBI::dbExecute(con, sql, params = params) else DBI::dbExecute(con, sql)
}

audit <- function(con, user_id, accion, detalle = NA_character_) {
  db_run(con, "INSERT INTO audit_log (user_id, accion, detalle, fecha) VALUES (?, ?, ?, ?)",
         list(user_id %||% NA_character_, accion, as.character(detalle), now_utc()))
}

# Ajustes ----------------------------------------------------------------------

get_setting <- function(con, clave) {
  r <- db_get(con, "SELECT valor FROM settings WHERE clave = ?", list(clave))
  if (!nrow(r)) {
    if (clave == "max_mb") return(caring_config()$max_mb)
    return(default_settings[[clave]])
  }
  jsonlite::fromJSON(r$valor[1])
}

get_settings <- function(con) {
  s <- lapply(stats::setNames(names(default_settings), names(default_settings)), function(k) get_setting(con, k))
  s
}

# Aplica el tamaño máximo de subida (Shiny lo consulta en cada subida).
apply_upload_limit <- function(con) {
  mb <- as.numeric(get_setting(con, "max_mb"))
  if (!length(mb) || is.na(mb) || mb <= 0) mb <- 200
  options(shiny.maxRequestSize = mb * 1024^2)
  invisible(mb)
}

set_setting <- function(con, clave, valor) {
  db_run(con, "DELETE FROM settings WHERE clave = ?", list(clave))
  db_run(con, "INSERT INTO settings (clave, valor) VALUES (?, ?)",
         list(clave, as.character(jsonlite::toJSON(valor, auto_unbox = TRUE))))
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
