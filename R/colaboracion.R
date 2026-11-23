# Colaboración (fase 6) ------------------------------------------------------------------------------
# Papelera de proyectos y documentos (30 días), comentarios en proyectos compartidos y
# enlaces públicos de solo lectura para informes y presentaciones.

papelera_dias <- 30

# Papelera de proyectos ---------------------------------------------------------------------------
# Un proyecto en la papelera no aparece en la lista ni se puede abrir (tampoco para las
# personas con las que se compartió); su propietario puede recuperarlo o borrarlo del todo.
trash_project <- function(con, project_id) {
  db_run(con, "UPDATE projects SET borrado = ? WHERE id = ?", list(now_utc(), project_id))
}

restore_project <- function(con, project_id) {
  db_run(con, "UPDATE projects SET borrado = NULL, modificado = ? WHERE id = ?", list(now_utc(), project_id))
}

list_trash <- function(con, user_id) {
  r <- db_get(con, "SELECT * FROM projects WHERE owner_id = ? AND borrado IS NOT NULL ORDER BY borrado DESC", list(user_id))
  r$quedan <- if (nrow(r)) pmax(0, ceiling(papelera_dias - as.numeric(difftime(Sys.time(), parse_utc(r$borrado), units = "days")))) else numeric()
  r
}

purge_old_trash <- function(con, dias = papelera_dias) {
  viejos <- db_get(con, "SELECT id FROM projects WHERE borrado IS NOT NULL AND borrado < ?", list(now_utc(-dias * 86400)))
  for (id in viejos$id) try(delete_project(con, id), silent = TRUE)
  invisible(nrow(viejos))
}

# Papelera de documentos (informes y presentaciones) ------------------------------------------------
doc_trash_dir <- function(dir) file.path(dir, "informes", "papelera")

trash_document <- function(dir, id) {
  f <- doc_path(dir, id)
  if (!file.exists(f)) return(invisible(FALSE))
  doc <- jsonlite::read_json(f, simplifyVector = FALSE)
  doc$borrado <- now_utc()
  dir.create(doc_trash_dir(dir), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(doc, file.path(doc_trash_dir(dir), basename(f)), auto_unbox = TRUE, null = "null", digits = NA)
  unlink(f)
  invisible(TRUE)
}

list_trash_documents <- function(dir, dias = papelera_dias) {
  f <- list.files(doc_trash_dir(dir), pattern = "^[a-f0-9]+\\.json$", full.names = TRUE)
  out <- lapply(f, function(x) {
    d <- tryCatch(jsonlite::read_json(x, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(d)) return(NULL)
    edad <- as.numeric(difftime(Sys.time(), parse_utc(d$borrado %||% now_utc()), units = "days"))
    if (edad > dias) { unlink(x); return(NULL) }                    # pasados 30 días se borra
    data.frame(id = d$id, title = d$title %||% "", type = d$type %||% "slides", borrado = d$borrado %||% "",
               quedan = max(0, ceiling(dias - edad)), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, Filter(Negate(is.null), out))
  if (is.null(out)) data.frame(id = character(), title = character(), type = character(), borrado = character(), quedan = numeric()) else out
}

restore_document <- function(dir, id) {
  if (!grepl("^[a-f0-9]+$", id)) return(invisible(FALSE))
  f <- file.path(doc_trash_dir(dir), paste0(id, ".json"))
  if (!file.exists(f)) return(invisible(FALSE))
  doc <- jsonlite::read_json(f, simplifyVector = FALSE)
  doc$borrado <- NULL
  save_document(dir, doc)
  unlink(f)
  invisible(TRUE)
}

purge_document <- function(dir, id) {
  if (grepl("^[a-f0-9]+$", id)) unlink(file.path(doc_trash_dir(dir), paste0(id, ".json")))
}

# Comentarios ------------------------------------------------------------------------------------------
add_comment <- function(con, project_id, user_id, texto) {
  texto <- trimws(texto %||% "")
  if (!nzchar(texto)) return(list(ok = FALSE, error = "Escribe el comentario."))
  if (nchar(texto) > 2000) return(list(ok = FALSE, error = "El comentario es demasiado largo (máximo 2.000 caracteres)."))
  id <- substr(gsub("-", "", new_id()), 1, 16)
  db_run(con, "INSERT INTO comments (id, project_id, user_id, texto, creado) VALUES (?, ?, ?, ?, ?)",
         list(id, project_id, user_id, texto, now_utc()))
  mark_comments_read(con, project_id, user_id)
  list(ok = TRUE, id = id)
}

list_comments <- function(con, project_id) {
  db_get(con, "SELECT c.id, c.user_id, c.texto, c.creado, COALESCE(u.nombre, 'Cuenta borrada') AS autor
               FROM comments c LEFT JOIN users u ON u.id = c.user_id
               WHERE c.project_id = ? ORDER BY c.creado", list(project_id))
}

# Cada persona borra sus comentarios; el propietario del proyecto, cualquiera.
delete_comment <- function(con, comment_id, user_id, is_owner = FALSE) {
  if (is_owner) db_run(con, "DELETE FROM comments WHERE id = ?", list(comment_id))
  else db_run(con, "DELETE FROM comments WHERE id = ? AND user_id = ?", list(comment_id, user_id))
}

mark_comments_read <- function(con, project_id, user_id) {
  db_run(con, "DELETE FROM comment_reads WHERE project_id = ? AND user_id = ?", list(project_id, user_id))
  db_run(con, "INSERT INTO comment_reads (project_id, user_id, visto) VALUES (?, ?, ?)", list(project_id, user_id, now_utc()))
}

unread_comments <- function(con, project_id, user_id) {
  v <- db_get(con, "SELECT visto FROM comment_reads WHERE project_id = ? AND user_id = ?", list(project_id, user_id))
  desde <- if (nrow(v)) v$visto[1] else "0000"
  db_get(con, "SELECT COUNT(*) AS n FROM comments WHERE project_id = ? AND creado > ? AND (user_id IS NULL OR user_id <> ?)",
         list(project_id, desde, user_id))$n[1]
}

project_is_shared <- function(con, project_id) {
  db_get(con, "SELECT COUNT(*) AS n FROM project_members WHERE project_id = ?", list(project_id))$n[1] > 0
}

# Enlaces públicos ---------------------------------------------------------------------------------------
# Solo de informes y presentaciones. El enlace lleva un código aleatorio que se guarda
# cifrado (como las sesiones): quien tenga el enlace ve el documento, sin cuenta y sin poder
# cambiar nada. Se puede anular en cualquier momento.
create_public_link <- function(con, project_id, doc_id, user_id) {
  token <- sodium::bin2hex(sodium::random(24))
  db_run(con, "INSERT INTO public_links (token_hash, project_id, doc_id, creado, creado_por) VALUES (?, ?, ?, ?, ?)",
         list(hash_token(token), project_id, doc_id, now_utc(), user_id))
  token
}

list_public_links <- function(con, project_id, doc_id) {
  db_get(con, "SELECT token_hash, creado, creado_por FROM public_links
               WHERE project_id = ? AND doc_id = ? AND revocado IS NULL ORDER BY creado DESC", list(project_id, doc_id))
}

revoke_public_link <- function(con, token_hash, project_id) {
  db_run(con, "UPDATE public_links SET revocado = ? WHERE token_hash = ? AND project_id = ?", list(now_utc(), token_hash, project_id))
}

revoke_doc_links <- function(con, project_id, doc_id) {
  db_run(con, "UPDATE public_links SET revocado = ? WHERE project_id = ? AND doc_id = ? AND revocado IS NULL",
         list(now_utc(), project_id, doc_id))
}

# Devuelve list(dir, doc, project) si el enlace es válido; NULL si no.
resolve_public_link <- function(con, token) {
  if (is.null(token) || !grepl("^[a-f0-9]{48}$", token)) return(NULL)
  l <- db_get(con, "SELECT l.project_id, l.doc_id FROM public_links l JOIN projects p ON p.id = l.project_id
                    WHERE l.token_hash = ? AND l.revocado IS NULL AND p.borrado IS NULL", list(hash_token(token)))
  if (!nrow(l)) return(NULL)
  dir <- project_path(con, l$project_id[1])
  doc <- tryCatch(load_document(dir, l$doc_id[1]), error = function(e) NULL)
  if (is.null(doc)) return(NULL)
  list(dir = dir, doc = doc, project = l$project_id[1])
}

public_link_url <- function(base_url, token) paste0(sub("[?#].*$", "", base_url), "?ver=", token)
