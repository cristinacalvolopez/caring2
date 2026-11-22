test_that("registro, login y sesiones", {
  con <- local_storage()
  r <- create_user(con, "Ana@Ejemplo.com ", "Ana", "contraseña-segura")
  expect_true(r$ok)
  expect_false(create_user(con, "ana@ejemplo.com", "Otra", "contraseña-segura")$ok)
  expect_false(create_user(con, "no-es-correo", "X", "contraseña-segura")$ok)
  expect_false(create_user(con, "b@ejemplo.com", "B", "corta")$ok)

  expect_false(login_user(con, "ana@ejemplo.com", "mala-contraseña")$ok)
  l <- login_user(con, "ana@ejemplo.com", "contraseña-segura")
  expect_true(l$ok)
  expect_equal(l$user$nombre, "Ana")

  tk <- create_session(con, l$user$id)
  expect_equal(session_user(con, tk)$id, l$user$id)
  delete_session(con, tk)
  expect_null(session_user(con, tk))
  expect_null(session_user(con, "token-inventado"))
})

test_that("bloqueo tras 5 intentos fallidos", {
  con <- local_storage()
  create_user(con, "c@ejemplo.com", "C", "contraseña-segura")
  for (i in 1:5) login_user(con, "c@ejemplo.com", "mal")
  r <- login_user(con, "c@ejemplo.com", "contraseña-segura")
  expect_false(r$ok)
  expect_match(r$error, "intentos")
})

test_that("cuentas bloqueadas y pendientes no entran", {
  con <- local_storage()
  id <- create_user(con, "d@ejemplo.com", "D", "contraseña-segura")$id
  update_user_admin(con, id, estado = "bloqueado")
  expect_match(login_user(con, "d@ejemplo.com", "contraseña-segura")$error, "bloqueada")
  update_user_admin(con, id, estado = "pendiente")
  expect_match(login_user(con, "d@ejemplo.com", "contraseña-segura")$error, "verificar")
  tk <- create_token(con, id, "verificar")
  expect_true(verify_email(con, tk))
  expect_false(verify_email(con, tk))  # un solo uso
  expect_true(login_user(con, "d@ejemplo.com", "contraseña-segura")$ok)
})

test_that("recuperar contraseña", {
  con <- local_storage()
  id <- create_user(con, "e@ejemplo.com", "E", "contraseña-segura")$id
  tk <- create_token(con, id, "recuperar")
  expect_false(reset_password(con, tk, "corta")$ok)
  expect_true(reset_password(con, tk, "otra-contraseña")$ok)
  expect_false(reset_password(con, tk, "otra-contraseña-2")$ok)
  expect_true(login_user(con, "e@ejemplo.com", "otra-contraseña")$ok)
})

test_that("invitado que se registra conserva su id", {
  con <- local_storage()
  g <- create_guest(con)
  r <- convert_guest(con, g, "f@ejemplo.com", "F", "contraseña-segura")
  expect_true(r$ok)
  u <- get_user(con, g)
  expect_equal(u$rol, "usuario")
  expect_equal(u$email, "f@ejemplo.com")
})

test_that("limpieza de invitados caducados", {
  con <- local_storage()
  g <- create_guest(con)
  db_run(con, "UPDATE users SET creado = ?, ultimo_acceso = ? WHERE id = ?",
         list(now_utc(-48 * 3600), now_utc(-48 * 3600), g))
  expect_equal(cleanup_guests(con, horas = 24), 1)
  expect_null(get_user(con, g))
})

test_that("ajustes", {
  con <- local_storage()
  expect_true(get_setting(con, "registro_abierto"))
  set_setting(con, "registro_abierto", FALSE)
  expect_false(get_setting(con, "registro_abierto"))
  set_setting(con, "cuota_mb", 500)
  expect_equal(get_settings(con)$cuota_mb, 500)
})

test_that("listado de usuarios para administración", {
  con <- local_storage()
  create_user(con, "a@ejemplo.com", "A", "contraseña-segura")
  create_guest(con)
  u <- list_users(con)
  expect_equal(nrow(u), 1)
  expect_true(all(c("proyectos", "bytes") %in% names(u)))
})
