# Formatos, codificaciones (ñ, tildes) y casos difíciles de importación

esp <- data.frame(
  `año` = c(2019, 2020, 2021, 2022, 2023, 2024),
  `región` = c("Castilla y León", "Andalucía", "Cataluña", "Castilla y León", "Galicia", "Andalucía"),
  `opinión` = c("Muy buena", "Buena", "Mala", "Buena", "Muy mala", "Buena"),
  `niños` = c(0, 2, 1, 3, 0, 1),
  `¿Qué piensa?` = c("Sí", "No", "Sí", "NS/NC", "No", "Sí"),
  check.names = FALSE, stringsAsFactors = FALSE
)

expect_spanish <- function(d) {
  expect_true(all(c("año", "región", "opinión", "niños") %in% names(d)))
  expect_true("Castilla y León" %in% as.character(d$región))
  expect_true("Cataluña" %in% as.character(d$región))
  expect_true(is.factor(d$región))
  expect_true(is.numeric(d$niños))
  expect_true(any(grepl("Qué", names(d))))
  expect_true(all(validUTF8(names(d))))
  expect_true(all(validUTF8(levels(d$región))))
}

test_that("CSV en UTF-8, Latin-1, con BOM y TSV", {
  tmp <- withr::local_tempdir()
  f <- file.path(tmp, "utf8.csv")
  data.table::fwrite(esp, f)
  expect_spanish(read_any(f))

  f <- file.path(tmp, "latin1.csv")
  lines <- c(paste(names(esp), collapse = ","), apply(esp, 1, function(r) paste0('"', r, '"', collapse = ",")))
  writeLines(iconv(lines, "UTF-8", "latin1"), f, useBytes = TRUE)
  expect_false(validUTF8(paste(readLines(f, warn = FALSE), collapse = "")))
  expect_spanish(read_any(f))

  f <- file.path(tmp, "cp1252.csv")
  writeLines(iconv(c(lines, '"2025","Navarra","Buena","1","Sí – ¿quizá?"'), "UTF-8", "CP1252"), f, useBytes = TRUE)
  d <- read_any(f)
  expect_spanish(d)

  f <- file.path(tmp, "bom.csv")
  data.table::fwrite(esp, f, bom = TRUE)
  d <- read_any(f)
  expect_equal(names(d)[1], "año")
  expect_spanish(d)

  f <- file.path(tmp, "datos.tsv")
  data.table::fwrite(esp, f, sep = "\t")
  expect_spanish(read_any(f))

  f <- file.path(tmp, "datos.txt")
  data.table::fwrite(esp, f, sep = "|")
  expect_spanish(read_any(f))
})

test_that("CSV al estilo del Excel español: ; y coma decimal", {
  tmp <- withr::local_tempdir()
  f <- file.path(tmp, "excel_es.csv")
  writeLines(c("nombre;ingresos;peso;región",
               "Ana;1.234,50;0,85;León",
               "Íñigo;980,00;1,10;Cádiz",
               "Begoña;2.100,75;0,95;León"), f)
  d <- read_any(f)
  expect_equal(names(d), c("nombre", "ingresos", "peso", "región"))
  expect_true(is.numeric(d$peso))
  expect_equal(d$peso, c(0.85, 1.10, 0.95))
  expect_true(is.numeric(d$ingresos))
  expect_equal(d$ingresos, c(1234.5, 980, 2100.75))
  expect_true("Íñigo" %in% as.character(d$nombre))
})

test_that("Excel", {
  tmp <- withr::local_tempdir()
  f <- file.path(tmp, "datos.xlsx")
  writexl::write_xlsx(esp, f)
  expect_spanish(read_any(f))
})

test_that("SPSS y Stata (también Stata antiguo en Latin-1)", {
  tmp <- withr::local_tempdir()
  d <- esp
  d$región <- haven::labelled(c(1, 2, 3, 1, 4, 2), labels = c("Castilla y León" = 1, "Andalucía" = 2, "Cataluña" = 3, "Galicia" = 4),
                              label = "Comunidad autónoma")
  d$niños <- haven::labelled(c(0, 2, 1, 3, 0, 1), label = "Número de niños")
  names(d)[5] <- "que_piensa"
  f <- file.path(tmp, "d.sav")
  haven::write_sav(d, f)
  r <- read_any(f)
  expect_equal(attr(r$región, "label"), "Comunidad autónoma")
  expect_true("Cataluña" %in% levels(r$región))

  f <- file.path(tmp, "d.dta")
  names(d) <- c("anio", "region", "opinion", "ninos", "que_piensa")
  haven::write_dta(d, f)
  r <- read_any(f)
  expect_true("Castilla y León" %in% levels(r$region))

  f <- file.path(tmp, "d13.dta")
  haven::write_dta(d, f, version = 13)
  r <- read_any(f)
  expect_true(all(validUTF8(levels(r$region))))
  expect_true("Castilla y León" %in% levels(r$region))
})

test_that("R: rds y RData con factores ordenados y etiquetas", {
  tmp <- withr::local_tempdir()
  d <- esp
  d$opinión <- factor(d$opinión, levels = c("Muy mala", "Mala", "Buena", "Muy buena"), ordered = TRUE)
  attr(d$niños, "label") <- "Número de niños"
  f <- file.path(tmp, "d.rds"); saveRDS(d, f)
  r <- read_any(f)
  expect_true(is.ordered(r$opinión))
  expect_equal(levels(r$opinión), c("Muy mala", "Mala", "Buena", "Muy buena"))
  expect_equal(attr(r$niños, "label"), "Número de niños")
  f <- file.path(tmp, "d.RData"); encuesta <- d; otra <- 1:3; save(otra, encuesta, file = f)
  expect_spanish(read_any(f))
  tb <- tibble::as_tibble(d); f <- file.path(tmp, "tb.rds"); saveRDS(tb, f)
  expect_true(is.data.frame(read_any(f)))
})

test_that("casos difíciles", {
  tmp <- withr::local_tempdir()
  f <- file.path(tmp, "raro.csv")
  writeLines(c("id,id,,edad,vacía,fecha,mixta,miles,lógica",
               "1,a,x,30,,2024-01-15,12,1.200,TRUE",
               "2,b,y,,,2024-02-20,abc,3.500,FALSE",
               "3,c,x,45,,2024-03-05,7,12.000,TRUE"), f)
  d <- read_any(f)
  expect_equal(anyDuplicated(names(d)), 0)
  expect_true(all(nzchar(names(d))))
  expect_true(all(is.na(d$vacía)))
  expect_true(inherits(d$fecha, c("Date", "POSIXct", "IDate")))
  expect_false(is.numeric(d$mixta))
  expect_true(is.factor(d$lógica))
  vi <- var_info(d)
  expect_equal(nrow(vi), ncol(d))

  f <- file.path(tmp, "una.csv"); writeLines(c("x", "1", "2"), f)
  expect_equal(ncol(read_any(f)), 1)

  f <- file.path(tmp, "vacio.csv"); writeLines("a,b", f)
  expect_equal(nrow(read_any(f)), 0)
  expect_equal(nrow(var_info(read_any(f))), 2)
})

test_that("las operaciones funcionan con nombres y categorías con tildes", {
  d <- read_any({ f <- tempfile(fileext = ".csv"); data.table::fwrite(esp, f); f })
  r <- list(
    list(op = "recode", var = "región", to = "zona", others = "keep",
         groups = list(list(name = "Norte y centro", from = c("Castilla y León", "Galicia")))),
    list(op = "compute", to = "niños2", expr = "niños * 2"),
    list(op = "filter", expr = "región != \"Cataluña\""),
    list(op = "rename", var = "opinión", to = "valoración"))
  out <- apply_recipe(d, r)
  expect_true("Norte y centro" %in% levels(out$zona))
  expect_true("Andalucía" %in% levels(out$zona))
  expect_equal(out$niños2, out$niños * 2)
  expect_false("Cataluña" %in% as.character(out$región))
  expect_true("valoración" %in% names(out))
  code <- recipe_to_code(r)
  env <- new.env(); env$datos <- d
  eval(parse(text = sub("datos <- caring2::read_any\\([^)]*\\)", "", code), encoding = "UTF-8"), env)
  expect_equal(as.character(env$datos$zona), as.character(out$zona))

  r1 <- explore_two(out, "zona", "valoración")
  expect_s3_class(r1$plot, "ggplot")
  net <- net_build(out, list(type = "coincidencias", vars = c("zona", "valoración"), signif = FALSE))
  expect_true(inherits(net, "netCoin"))
  f <- tempfile(fileext = ".sav"); write_any(out, f, "sav")
  back <- read_any(f)
  expect_true("Norte y centro" %in% levels(back$zona))
  f <- tempfile(fileext = ".dta"); write_any(out, f, "dta"); expect_true(file.exists(f))
  f <- tempfile(fileext = ".csv"); write_any(out, f, "csv")
  expect_true("Andalucía" %in% as.character(read_any(f)$zona))
})
