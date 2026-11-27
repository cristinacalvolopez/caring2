test_that("SPSS: etiquetas, perdidos y tipos", {
  d <- read_any(demo_sav())
  expect_equal(nrow(d), 1200)
  expect_true(is.factor(d$genero))
  expect_equal(levels(d$genero), c("Mujer", "Hombre"))
  expect_equal(attr(d$genero, "label"), "Género")
  expect_true(is.numeric(d$edad))
  expect_true(is.numeric(d$ingresos))
  expect_false(any(d$ingresos == 99999, na.rm = TRUE))   # perdido de usuario -> NA
  expect_true(any(is.na(d$ingresos)))
  expect_true(is.numeric(d$ideologia))                   # etiquetas parciales -> numérica
  expect_true(is.character(d$aficiones))
})

test_that("CSV, Stata, R y Excel", {
  d0 <- read_any(demo_sav())
  tmp <- tempfile()
  dir.create(tmp)

  csv <- file.path(tmp, "d.csv")
  utils::write.csv(data.frame(a = c("x", "y", "x"), b = 1:3, `c d` = c("1", "2", NA), check.names = FALSE),
                   csv, row.names = FALSE)
  d <- read_any(csv)
  expect_true(is.factor(d$a))
  expect_true(is.numeric(d$b))
  expect_true("c_d" %in% names(d))

  latin <- file.path(tmp, "latin.csv")
  writeLines(iconv(c("región;n", "Castilla y León;1", "Cádiz;2"), "UTF-8", "latin1"), latin, useBytes = TRUE)
  d <- read_any(latin)
  expect_equal(names(d)[1], "región")
  expect_true("Cádiz" %in% as.character(d[[1]]))

  dta <- file.path(tmp, "d.dta")
  haven::write_dta(haven::read_sav(demo_sav())[, 1:5], dta)
  expect_true(is.factor(read_any(dta)$genero))

  rds <- file.path(tmp, "d.rds")
  saveRDS(d0[, 1:4], rds)
  expect_equal(ncol(read_any(rds)), 4)

  rda <- file.path(tmp, "d.RData")
  datos <- d0[, 1:3]
  save(datos, file = rda)
  expect_equal(ncol(read_any(rda)), 3)

  expect_error(read_any(file.path(tmp, "x.pdf")), "no admitido")
})

test_that("var_info resume las variables", {
  d <- read_any(demo_sav())
  vi <- var_info(d)
  expect_equal(nrow(vi), ncol(d))
  expect_equal(vi$tipo[vi$nombre == "genero"], "categórica")
  expect_equal(vi$tipo[vi$nombre == "edad"], "numérica")
})

test_that("un .sav con la fecha de cabecera mal escrita (como los del CIS) se lee igual y sin tocarlo", {
  f <- tempfile(fileext = ".sav")
  haven::write_sav(data.frame(a = haven::labelled(c(1, 2, 1), c(Sí = 1, No = 2)), b = c(3.5, 4, 5)), f)
  con <- file(f, "r+b"); seek(con, 92L, rw = "write"); writeBin(charToRaw("22 028 26"), con); close(con)
  d <- read_any(f)
  expect_equal(nrow(d), 3)
  expect_equal(levels(d$a), c("Sí", "No"))
  expect_equal(d$b, c(3.5, 4, 5))
  expect_equal(rawToChar(readBin(f, "raw", 101)[93:101]), "22 028 26")   # el archivo original no cambia
  # con la fecha bien escrita se lee directamente
  g <- tempfile(fileext = ".sav"); haven::write_sav(data.frame(x = 1:2), g)
  expect_equal(as.vector(read_sav_fecha(g)$x), c(1, 2))
})
