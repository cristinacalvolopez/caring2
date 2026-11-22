# Cada test usa una carpeta de almacenamiento y una base de datos temporales.
local_storage <- function(env = parent.frame()) {
  dir <- withr::local_tempdir("caring-test-", .local_envir = env)
  withr::local_envvar(CARING_STORAGE = dir, CARING_DB = file.path(dir, "test.sqlite"),
                      .local_envir = env)
  con <- db_connect()
  withr::defer(DBI::dbDisconnect(con), envir = env)
  con
}

demo_sav <- function() {
  f <- system.file("extdata", "demo_encuesta.sav", package = "caring2")
  if (!nzchar(f)) f <- testthat::test_path("..", "..", "inst", "extdata", "demo_encuesta.sav")
  f
}
