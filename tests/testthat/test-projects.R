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
