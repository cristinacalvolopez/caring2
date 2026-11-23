usuarios <- function(con) {
  a <- get_user(con, create_user(con, "a@ejemplo.com", "Ana", "contraseña-segura")$id)
  b <- get_user(con, create_user(con, "b@ejemplo.com", "Berta", "contraseña-segura")$id)
  list(a = a, b = b)
}

test_that("papelera de proyectos: se recupera y a los 30 días se borra", {
  con <- local_storage(); u <- usuarios(con)
  r <- create_project(con, u$a, demo_sav(), "demo.sav")
  share_project(con, r$id, "b@ejemplo.com", "editor")
  dir <- project_path(con, r$id)
  trash_project(con, r$id)
  expect_equal(nrow(list_projects(con, u$a$id)), 0)
  expect_equal(nrow(list_projects(con, u$b$id)), 0)              # tampoco lo ve quien lo compartía
  expect_null(project_access(con, u$a$id, r$id))
  t <- list_trash(con, u$a$id)
  expect_equal(t$id, r$id); expect_equal(t$quedan, 30)
  expect_equal(nrow(list_trash(con, u$b$id)), 0)
  expect_true(dir.exists(dir))
  restore_project(con, r$id)
  expect_equal(project_access(con, u$b$id, r$id), "editor")
  # pasados 30 días se elimina del todo
  trash_project(con, r$id)
  db_run(con, "UPDATE projects SET borrado = ? WHERE id = ?", list(now_utc(-31 * 86400), r$id))
  expect_equal(purge_old_trash(con), 1)
  expect_false(dir.exists(dir))
  expect_equal(nrow(db_get(con, "SELECT * FROM projects")), 0)
})

test_that("papelera de documentos", {
  dir <- withr::local_tempdir()
  doc <- new_document("Mi informe", "report"); save_document(dir, doc)
  trash_document(dir, doc$id)
  expect_equal(nrow(list_documents(dir)), 0)
  t <- list_trash_documents(dir)
  expect_equal(t$title, "Mi informe")
  restore_document(dir, doc$id)
  expect_equal(list_documents(dir)$id, doc$id)
  expect_null(load_document(dir, doc$id)$borrado)
  trash_document(dir, doc$id)
  f <- file.path(doc_trash_dir(dir), paste0(doc$id, ".json"))
  d <- jsonlite::read_json(f); d$borrado <- now_utc(-40 * 86400); jsonlite::write_json(d, f, auto_unbox = TRUE)
  expect_equal(nrow(list_trash_documents(dir)), 0)                 # caducado: se borra al listar
  expect_false(file.exists(f))
  expect_false(restore_document(dir, "../../x"))
})

test_that("comentarios en proyectos compartidos", {
  con <- local_storage(); u <- usuarios(con)
  r <- create_project(con, u$a, demo_sav(), "demo.sav")
  expect_false(project_is_shared(con, r$id))
  share_project(con, r$id, "b@ejemplo.com", "lector")
  expect_true(project_is_shared(con, r$id))
  expect_false(add_comment(con, r$id, u$a$id, "   ")$ok)
  expect_false(add_comment(con, r$id, u$a$id, strrep("x", 2001))$ok)
  c1 <- add_comment(con, r$id, u$a$id, "He recodificado la edad.")
  expect_true(c1$ok)
  expect_equal(unread_comments(con, r$id, u$a$id), 0)               # los propios no cuentan
  expect_equal(unread_comments(con, r$id, u$b$id), 1)
  mark_comments_read(con, r$id, u$b$id)
  expect_equal(unread_comments(con, r$id, u$b$id), 0)
  Sys.sleep(1.1)
  c2 <- add_comment(con, r$id, u$b$id, "¡Gracias!")
  expect_equal(unread_comments(con, r$id, u$a$id), 1)
  cs <- list_comments(con, r$id)
  expect_equal(cs$autor, c("Ana", "Berta"))
  delete_comment(con, c1$id, u$b$id, is_owner = FALSE)                # no puede borrar el de otra persona
  expect_equal(nrow(list_comments(con, r$id)), 2)
  delete_comment(con, c2$id, u$a$id, is_owner = TRUE)                 # la propietaria sí
  expect_equal(list_comments(con, r$id)$texto, "He recodificado la edad.")
})

test_that("enlaces públicos de informes y presentaciones", {
  con <- local_storage(); u <- usuarios(con)
  r <- create_project(con, u$a, demo_sav(), "demo.sav")
  dir <- project_path(con, r$id)
  doc <- new_document("Presentación pública", "slides"); save_document(dir, doc)
  tok <- create_public_link(con, r$id, doc$id, u$a$id)
  expect_match(tok, "^[a-f0-9]{48}$")
  expect_false(tok %in% db_get(con, "SELECT token_hash FROM public_links")$token_hash)   # se guarda cifrado
  p <- resolve_public_link(con, tok)
  expect_equal(p$doc$title, "Presentación pública")
  expect_null(resolve_public_link(con, "abc"))
  expect_null(resolve_public_link(con, strrep("0", 48)))
  expect_null(resolve_public_link(con, "' OR 1=1 --"))
  expect_match(public_link_url("https://caring.usal.es/app/?x=1", tok), paste0("^https://caring.usal.es/app/\\?ver=", tok, "$"))
  ui <- as.character(public_page(p))
  expect_match(ui, "Presentación pública"); expect_match(ui, 'sandbox="allow-scripts"')
  expect_match(as.character(public_page(NULL)), "no está disponible")
  # proyecto en la papelera o enlace anulado: deja de funcionar
  trash_project(con, r$id); expect_null(resolve_public_link(con, tok)); restore_project(con, r$id)
  expect_false(is.null(resolve_public_link(con, tok)))
  revoke_doc_links(con, r$id, doc$id)
  expect_null(resolve_public_link(con, tok))
  expect_equal(nrow(list_public_links(con, r$id, doc$id)), 0)
})

test_that("las bases de datos antiguas reciben la columna nueva", {
  f <- withr::local_tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), f)
  DBI::dbExecute(con, "CREATE TABLE users (id TEXT PRIMARY KEY, email TEXT, nombre TEXT, password_hash TEXT, rol TEXT, estado TEXT, cuota_mb REAL, creado TEXT, verificado TEXT, ultimo_acceso TEXT, intentos_fallidos INTEGER, bloqueado_hasta TEXT)")
  DBI::dbExecute(con, "CREATE TABLE projects (id TEXT PRIMARY KEY, owner_id TEXT, nombre TEXT, archivo TEXT, n_casos INTEGER, n_vars INTEGER, bytes REAL, creado TEXT, modificado TEXT)")
  db_migrate(con)
  expect_true("borrado" %in% DBI::dbListFields(con, "projects"))
  db_migrate(con)                                                     # repetirlo no falla
  DBI::dbDisconnect(con)
})
