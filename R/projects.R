# Proyectos ---------------------------------------------------------------------
# Estructura en disco:
#   <storage>/users/<user_id>/projects/<project_id>/
#     original.<ext>      archivo subido, sin modificar
#     receta.rds          historial de operaciones
#     resultados/<id>.rds resultados guardados (gráficos, tablas, redes)
#     resultados/<id>.png vista previa
#     redes/<id>.html     red interactiva autocontenida
#     informes/<id>.json  documentos del editor de informes

user_dir <- function(user_id, storage = caring_config()$storage) {
  file.path(storage, "users", user_id)
}

project_path <- function(con, project_id, storage = caring_config()$storage) {
  p <- db_get(con, "SELECT owner_id FROM projects WHERE id = ?", list(project_id))
  if (!nrow(p)) stop("Proyecto no encontrado")
  file.path(user_dir(p$owner_id[1], storage), "projects", project_id)
}

dir_bytes <- function(path) {
  f <- list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  sum(file.size(f), na.rm = TRUE)
}

user_bytes <- function(con, user_id) {
  r <- db_get(con, "SELECT COALESCE(SUM(bytes), 0) AS b FROM projects WHERE owner_id = ?", list(user_id))
  r$b[1]
}

user_quota_mb <- function(con, user) {
  if (!is.null(user$cuota_mb) && !is.na(user$cuota_mb)) user$cuota_mb else get_setting(con, "cuota_mb")
}

# Crea un proyecto a partir de un archivo subido. Devuelve list(ok, id | error).
create_project <- function(con, user, file_path, file_name, nombre = NULL,
                           storage = caring_config()$storage) {
  ext <- file_ext(file_name)
  if (!ext %in% names(formatos_admitidos)) {
    return(list(ok = FALSE, error = paste0("Formato no admitido (.", ext, ").")))
  }
  max_mb <- as.numeric(get_setting(con, "max_mb"))
  if (isTRUE(file.size(file_path) > max_mb * 1024^2)) {
    return(list(ok = FALSE, error = paste0("El archivo pesa ", fmt_bytes(file.size(file_path)),
                                           " y el máximo permitido es ", fmt_num(max_mb, 0), " MB.")))
  }
  quota <- user_quota_mb(con, user) * 1024^2
  if (user_bytes(con, user$id) + file.size(file_path) > quota) {
    return(list(ok = FALSE, error = "No queda espacio en tu cuenta. Borra algún proyecto o pide más cuota."))
  }
  d <- tryCatch(read_any(file_path, ext), error = function(e) e)
  if (inherits(d, "error")) {
    return(list(ok = FALSE, error = paste("No se ha podido leer el archivo:", conditionMessage(d))))
  }
  id <- new_id()
  dir <- file.path(user_dir(user$id, storage), "projects", id)
  dir.create(file.path(dir, "resultados"), recursive = TRUE)
  dir.create(file.path(dir, "redes"))
  dir.create(file.path(dir, "informes"))
  file.copy(file_path, file.path(dir, paste0("original.", ext)))
  saveRDS(list(), file.path(dir, "receta.rds"))
  nombre <- nombre %||% tools::file_path_sans_ext(file_name)
  db_run(con, "INSERT INTO projects (id, owner_id, nombre, archivo, n_casos, n_vars, bytes, creado, modificado)
               VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
         list(id, user$id, nombre, file_name, nrow(d), ncol(d), dir_bytes(dir), now_utc(), now_utc()))
  audit(con, user$id, "proyecto_creado", id)
  list(ok = TRUE, id = id)
}

list_projects <- function(con, user_id) {
  own <- db_get(con, "
    SELECT p.*, 'propietario' AS permiso, u.nombre AS propietario
    FROM projects p JOIN users u ON u.id = p.owner_id
    WHERE p.owner_id = ?", list(user_id))
  shared <- db_get(con, "
    SELECT p.*, m.permiso AS permiso, u.nombre AS propietario
    FROM projects p JOIN project_members m ON m.project_id = p.id
    JOIN users u ON u.id = p.owner_id
    WHERE m.user_id = ?", list(user_id))
  r <- rbind(own, shared)
  r[order(r$modificado, decreasing = TRUE), , drop = FALSE]
}

# Permiso de un usuario sobre un proyecto: "propietario", "editor", "lector" o NULL.
project_access <- function(con, user_id, project_id) {
  p <- db_get(con, "SELECT owner_id FROM projects WHERE id = ?", list(project_id))
  if (!nrow(p)) return(NULL)
  if (p$owner_id[1] == user_id) return("propietario")
  m <- db_get(con, "SELECT permiso FROM project_members WHERE project_id = ? AND user_id = ?",
              list(project_id, user_id))
  if (nrow(m)) m$permiso[1] else NULL
}

can_edit <- function(permiso) isTRUE(permiso %in% c("propietario", "editor"))

get_project <- function(con, project_id) {
  r <- db_get(con, "SELECT * FROM projects WHERE id = ?", list(project_id))
  if (nrow(r)) as.list(r[1, ]) else NULL
}

touch_project <- function(con, project_id) {
  dir <- project_path(con, project_id)
  db_run(con, "UPDATE projects SET modificado = ?, bytes = ? WHERE id = ?",
         list(now_utc(), dir_bytes(dir), project_id))
}

rename_project <- function(con, project_id, nombre) {
  db_run(con, "UPDATE projects SET nombre = ?, modificado = ? WHERE id = ?",
         list(trimws(nombre), now_utc(), project_id))
}

delete_project <- function(con, project_id) {
  unlink(project_path(con, project_id), recursive = TRUE)
  db_run(con, "DELETE FROM projects WHERE id = ?", list(project_id))
}

duplicate_project <- function(con, project_id, user) {
  src <- project_path(con, project_id)
  p <- get_project(con, project_id)
  id <- new_id()
  dst <- file.path(user_dir(user$id), "projects", id)
  dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
  dir.create(dst)
  file.copy(list.files(src, full.names = TRUE), dst, recursive = TRUE)
  db_run(con, "INSERT INTO projects (id, owner_id, nombre, archivo, n_casos, n_vars, bytes, creado, modificado)
               VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
         list(id, user$id, paste(p$nombre, "(copia)"), p$archivo, p$n_casos, p$n_vars,
              dir_bytes(dst), now_utc(), now_utc()))
  id
}

share_project <- function(con, project_id, email, permiso = "lector") {
  u <- get_user_by_email(con, email)
  if (is.null(u) || u$rol == "invitado") {
    return(list(ok = FALSE, error = "No hay ninguna cuenta con ese correo. La persona debe registrarse primero."))
  }
  p <- get_project(con, project_id)
  if (u$id == p$owner_id) return(list(ok = FALSE, error = "Ese correo es el del propietario."))
  db_run(con, "DELETE FROM project_members WHERE project_id = ? AND user_id = ?", list(project_id, u$id))
  db_run(con, "INSERT INTO project_members (project_id, user_id, permiso) VALUES (?, ?, ?)",
         list(project_id, u$id, permiso))
  list(ok = TRUE)
}

project_members <- function(con, project_id) {
  db_get(con, "SELECT u.id, u.nombre, u.email, m.permiso FROM project_members m
               JOIN users u ON u.id = m.user_id WHERE m.project_id = ?", list(project_id))
}

unshare_project <- function(con, project_id, user_id) {
  db_run(con, "DELETE FROM project_members WHERE project_id = ? AND user_id = ?", list(project_id, user_id))
}

# Datos del proyecto -----------------------------------------------------------------

project_original <- function(dir) {
  f <- list.files(dir, pattern = "^original\\.", full.names = TRUE)
  if (!length(f)) stop("Falta el archivo original del proyecto")
  f[1]
}

read_recipe <- function(dir) {
  f <- file.path(dir, "receta.rds")
  if (file.exists(f)) readRDS(f) else list()
}

write_recipe <- function(dir, recipe) saveRDS(recipe, file.path(dir, "receta.rds"))

# Resultados guardados (para informes) --------------------------------------------------

save_result <- function(dir, kind, title, object, png = NULL, html = NULL, source = "") {
  id <- substr(gsub("-", "", new_id()), 1, 12)
  rdir <- file.path(dir, "resultados")
  dir.create(rdir, showWarnings = FALSE)
  res <- list(id = id, kind = kind, title = title, source = source, created = now_utc(),
              object = object, html = if (!is.null(html)) basename(html))
  if (!is.null(png) && file.exists(png)) file.copy(png, file.path(rdir, paste0(id, ".png")), overwrite = TRUE)
  if (!is.null(html) && file.exists(html)) file.copy(html, file.path(rdir, paste0(id, ".html")), overwrite = TRUE)
  saveRDS(res, file.path(rdir, paste0(id, ".rds")))
  id
}

list_results <- function(dir) {
  f <- list.files(file.path(dir, "resultados"), pattern = "\\.rds$", full.names = TRUE)
  if (!length(f)) return(list())
  r <- lapply(f, function(x) {
    o <- readRDS(x)
    o$object <- NULL
    o
  })
  r[order(vapply(r, `[[`, "", "created"), decreasing = TRUE)]
}

load_result <- function(dir, id) {
  f <- file.path(dir, "resultados", paste0(id, ".rds"))
  if (!grepl("^[a-f0-9]+$", id) || !file.exists(f)) return(NULL)
  readRDS(f)
}

delete_result <- function(dir, id) {
  if (!grepl("^[a-f0-9]+$", id)) return(invisible(FALSE))
  unlink(file.path(dir, "resultados", paste0(id, c(".rds", ".png", ".html"))))
}

project_thumbnail <- function(dir) {
  f <- list.files(file.path(dir, "resultados"), pattern = "\\.png$", full.names = TRUE)
  if (!length(f)) return(NULL)
  f[which.max(file.mtime(f))]
}
