test_that("crear, listar, compartir y borrar proyectos", {
  con <- local_storage()
  a <- get_user(con, create_user(con, "a@ejemplo.com", "A", "contraseña-segura")$id)
  b <- get_user(con, create_user(con, "b@ejemplo.com", "B", "contraseña-segura")$id)

  r <- create_project(con, a, demo_sav(), "demo_encuesta.sav")
  expect_true(r$ok)
  p <- get_project(con, r$id)
  expect_equal(p$n_casos, 1200)
  expect_equal(p$nombre, "demo_encuesta")
  dir <- project_path(con, r$id)
  expect_true(file.exists(file.path(dir, "original.sav")))

  expect_equal(project_access(con, a$id, r$id), "propietario")
  expect_null(project_access(con, b$id, r$id))
  expect_true(share_project(con, r$id, "b@ejemplo.com", "lector")$ok)
  expect_equal(project_access(con, b$id, r$id), "lector")
  expect_equal(nrow(list_projects(con, b$id)), 1)
  expect_false(share_project(con, r$id, "nadie@ejemplo.com")$ok)

  id2 <- duplicate_project(con, r$id, b)
  expect_equal(project_access(con, b$id, id2), "propietario")

  delete_project(con, r$id)
  expect_false(dir.exists(dir))
  expect_null(project_access(con, b$id, r$id))
})

test_that("cuota y formatos", {
  con <- local_storage()
  a <- get_user(con, create_user(con, "a@ejemplo.com", "A", "contraseña-segura")$id)
  update_user_admin(con, a$id, cuota_mb = 0.01)
  a <- get_user(con, a$id)
  expect_match(create_project(con, a, demo_sav(), "demo.sav")$error, "espacio")
  f <- tempfile(fileext = ".pdf"); writeLines("x", f)
  expect_match(create_project(con, a, f, "x.pdf")$error, "Formato")
})

test_that("resultados guardados", {
  dir <- tempfile(); dir.create(dir)
  id <- save_result(dir, "tabla", "Mi tabla", data.frame(x = 1))
  expect_length(list_results(dir), 1)
  expect_equal(load_result(dir, id)$title, "Mi tabla")
  expect_null(load_result(dir, "../../etc"))
  delete_result(dir, id)
  expect_length(list_results(dir), 0)
})

test_that("tamaño máximo por archivo ajustable desde administración", {
  con <- local_storage()
  withr::local_options(shiny.maxRequestSize = NULL)
  a <- get_user(con, create_user(con, "a@ejemplo.com", "A", "contraseña-segura")$id)
  expect_equal(get_setting(con, "max_mb"), 200)            # valor por defecto
  expect_equal(apply_upload_limit(con), 200)
  expect_equal(getOption("shiny.maxRequestSize"), 200 * 1024^2)
  set_setting(con, "max_mb", 0.01)                          # ~10 KB: el demo (72 KB) no cabe
  apply_upload_limit(con)
  expect_equal(getOption("shiny.maxRequestSize"), 0.01 * 1024^2)
  r <- create_project(con, a, demo_sav(), "demo.sav")
  expect_false(r$ok)
  expect_match(r$error, "máximo permitido")
  set_setting(con, "max_mb", 1000)
  expect_true(create_project(con, a, demo_sav(), "demo.sav")$ok)
  expect_equal(get_settings(con)$max_mb, 1000)
})

test_that("copias HTML en caring/: crea la carpeta, nunca borra ni sobrescribe", {
  root <- withr::local_tempdir()
  dir <- file.path(root, "caring")
  src <- tempfile(fileext = ".html"); writeLines("<html>red 1</html>", src)

  # la carpeta no existe: se crea
  f1 <- save_html_copy(src, "Coincidencias · 6 variables", dir = dir)
  expect_true(dir.exists(dir))
  expect_true(file.exists(f1))
  expect_match(basename(f1), "^[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9-]+_Coincidencias_6_variables\\.html$")

  # la carpeta ya existe con archivos ajenos: no se tocan
  ajeno <- file.path(dir, "no_tocar.txt"); writeLines("importante", ajeno)
  src2 <- tempfile(fileext = ".html"); writeLines("<html>red 2</html>", src2)
  f2 <- save_html_copy(src2, "Coincidencias · 6 variables", dir = dir)
  expect_equal(readLines(ajeno), "importante")
  expect_equal(readLines(f1), "<html>red 1</html>")         # la primera copia sigue intacta
  expect_false(identical(f1, f2))

  # mismo nombre en el mismo segundo: se añade _2, _3... sin sobrescribir
  objetivo <- file.path(dir, paste0(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), "_Repetido.html"))
  writeLines("original", objetivo)
  f3 <- save_html_copy(src2, "Repetido", dir = dir)
  expect_equal(readLines(objetivo), "original")
  expect_true(grepl("_Repetido(_[0-9]+)?\\.html$", basename(f3)))
  expect_false(identical(normalizePath(f3), normalizePath(objetivo)))

  # desactivado con CARING_HTML_DIR vacío
  expect_null(save_html_copy(src, "x", dir = ""))
  expect_length(list.files(dir), 5)                         # 3 copias + 2 archivos previos, todos intactos
})
