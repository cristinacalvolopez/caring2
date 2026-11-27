# Un estudio mínimo con las etiquetas del CIS
cis_falso <- function(n = 30) {
  set.seed(1)
  d <- data.frame(
    ESTUDIO = haven::labelled(rep(3577, n), label = "Número de estudio"),
    SEXO = haven::labelled(sample(1:2, n, TRUE), c(Hombre = 1, Mujer = 2),
                           label = "Sexo de la persona entrevistada"),
    EDAD = haven::labelled(sample(18:90, n, TRUE), c(N.C. = 99), label = "Edad de la persona entrevistada"),
    ESCIDEOL = haven::labelled(sample(1:10, n, TRUE),
                               c(`Izquierda` = 1, `Derecha` = 10, N.S. = 98, N.C. = 99),
                               label = "Escala de autoubicación ideológica (1-10)"),
    P1 = haven::labelled(sample(1:3, n, TRUE), c(Sí = 1, No = 2, N.C. = 3), label = "Pregunta uno"))
  f <- tempfile(fileext = ".sav"); haven::write_sav(d, f)
  f
}

test_that("los archivos que no son del CIS no se detectan", {
  expect_null(es_cis(demo_sav(), "demo_encuesta.sav"))
  f <- tempfile(fileext = ".csv"); writeLines("a,b\n1,2", f)
  expect_null(es_cis(f, "x.csv"))
})

test_that("un estudio del CIS se armoniza como paso del historial", {
  f <- cis_falso()
  cis <- es_cis(f, "3577.sav")
  expect_equal(cis$estudio, 3577)
  expect_true(all(c("genero", "edad", "ideologia") %in% cis$conceptos))

  con <- local_storage()
  a <- get_user(con, create_user(con, "a@ejemplo.com", "A", "contraseña-segura")$id)
  r <- create_project(con, a, f, "3577.sav", cis = cis)
  expect_true(r$ok)
  expect_null(r$aviso)
  dir <- project_path(con, r$id)
  expect_true(file.exists(file.path(dir, "original.sav")))       # el original no se toca
  rec <- read_recipe(dir)
  expect_equal(rec[[1]]$op, "cis")
  orig <- read_any(project_original(dir))
  x <- apply_recipe(orig, rec, dir)
  expect_true(all(c("genero", "edad", "generacion", "ideologia", "clase") %in% names(x)))
  expect_true(all(c("SEXO", "EDAD_ORIG", "P1") %in% names(x)))     # las originales siguen (EDAD choca con edad)
  expect_false(anyDuplicated(tolower(names(x))) > 0)               # nada choca en SPSS
  expect_equal(names(x)[1:3], c("orden", "numentr", "genero"))
  expect_true(is.factor(x$genero))
  expect_match(step_label(rec[[1]]), "CIS \\(estudio 3577\\)")

  # se puede seguir trabajando encima y quitar el paso si nada depende de él
  rec2 <- c(rec, list(list(op = "filter", expr = "genero == \"Mujer\"")))
  expect_lt(nrow(apply_recipe(orig, rec2, dir)), nrow(orig))
  expect_error(apply_recipe(orig, rec2[-1], dir))
  expect_equal(ncol(apply_recipe(orig, list(), dir)), ncol(orig))

  # el script reproduce lo mismo
  code <- recipe_to_code(rec, "3577.sav")
  expect_match(code, "caring2::cis_armonizar(datos, \"3577.sav\", estudio = 3577)", fixed = TRUE)
  y <- cis_armonizar(read_any(f), f)
  expect_equal(names(y), names(x))

  # viaja en un .caring
  z <- tempfile(fileext = ".caring")
  export_project(con, r$id, z)
  imp <- import_project(con, a, z)
  expect_true(imp$ok)
  expect_equal(read_recipe(project_path(con, imp$id))[[1]]$op, "cis")

  # sin aceptar, sin paso
  r2 <- create_project(con, a, f, "3577.sav")
  expect_length(read_recipe(project_path(con, r2$id)), 0)
})

test_that("el paso del CIS tiene que ir antes de quitar casos", {
  f <- cis_falso()
  d <- read_any(f)
  expect_error(cis_armonizar(d[1:10, ], f), "antes de filtrar")
})

test_that("con un estudio real da lo mismo que sortCIS", {
  f <- "D:/Documentos/Archivos/Trabajo/Datos/CIS/sortCIS/datos/3577.sav"
  skip_if_not(file.exists(f))
  skip_if_not_installed("sortCIS")
  x <- cis_armonizar(read_any(f), f)
  utils::capture.output(s <- sortCIS::cis_sociodem(f, salida = "completo", escribir = FALSE, avisos = FALSE,
                                                   tabla = tempfile(fileext = ".tsv")))
  for (v in c("genero", "generacion", "edad", "ideologia", "religion", "estudios", "laboral", "profesional", "clase")) {
    expect_equal(as.character(x[[v]]), as.character(s[[v]]), info = v)
  }
  expect_true(all(c("escideol", "ESCIDEOL_ORIG") %in% names(x)))
  expect_false(anyDuplicated(tolower(names(x))) > 0)
})

test_that("un archivo que ya trae las armonizadas no las pierde", {
  f <- "D:/Documentos/Archivos/Trabajo/Datos/CIS/sortCIS/datos/CIS3567.sav"
  skip_if_not(file.exists(f))
  d <- read_any(f)
  x <- cis_armonizar(d, f)
  expect_true(all(c("orden", "genero", "clase", "genero_ORIG") %in% names(x)))
  expect_false(anyDuplicated(tolower(names(x))) > 0)
})
