d <- read_any(demo_sav())

test_that("se detectan los problemas de los datos", {
  dd <- rbind(d, d[1:3, ])
  expect_equal(nrow(apply_step(dd, list(op = "dedupe"))), nrow(d))
  expect_equal(nrow(apply_step(dd, list(op = "dedupe", by = "id"))), nrow(d))
  expect_true("duplicados" %in% vapply(data_problems(dd), `[[`, "", "tipo"))
  dd$vacia <- NA_real_
  dd$constante <- 1
  dd$p99 <- c(sample(1:5, nrow(dd) - 20, TRUE), rep(99, 20))
  dd$txt <- sample(c("a", "b", "c"), nrow(dd), TRUE)
  tipos <- vapply(data_problems(dd), `[[`, "", "tipo")
  expect_true(all(c("id_repetido", "vacia", "constante", "codigos", "texto_cat") %in% tipos))
  cod <- Filter(function(x) x$tipo == "codigos", data_problems(dd))[[1]]
  expect_equal(cod$values, 99)
  expect_equal(suspicious_codes(c(1:10, 99)), 99)
  expect_length(suspicious_codes(c(1:100)), 0)               # 99 forma parte de la escala
  expect_length(data_problems(d[0, ]), 0)
})

test_that("invertir escalas numéricas y categóricas", {
  r <- apply_step(d, list(op = "reverse", vars = c("ideologia", "satisfaccion"), limits = list(ideologia = list(min = 1, max = 10))))
  expect_equal(r$ideologia_inv, 11 - d$ideologia, ignore_attr = TRUE)
  expect_equal(levels(r$satisfaccion_inv), rev(levels(d$satisfaccion)))
  expect_equal(as.character(r$satisfaccion_inv), as.character(d$satisfaccion))
  expect_match(attr(r$ideologia_inv, "label"), "invertida")
  s <- apply_step(d, list(op = "reverse", vars = "ideologia", replace = TRUE))
  expect_equal(s$ideologia, min(d$ideologia, na.rm = TRUE) + max(d$ideologia, na.rm = TRUE) - d$ideologia, ignore_attr = TRUE)
  expect_error(apply_step(d, list(op = "reverse", vars = "aficiones")), "numéricas o categóricas")
})

test_that("recodificar varias variables a la vez", {
  dd <- d
  for (k in 1:3) dd[[paste0("q", k)]] <- factor(sample(c("Mucho", "Bastante", "Poco", "Nada"), nrow(d), TRUE),
                                                levels = c("Mucho", "Bastante", "Poco", "Nada"))
  r <- apply_step(dd, list(op = "recode_many", vars = paste0("q", 1:3), suffix = "_r",
                           groups = list(list(name = "Alto", from = c("Mucho", "Bastante")), list(name = "Bajo", from = c("Poco", "Nada")))))
  for (k in 1:3) {
    v <- paste0("q", k, "_r")
    expect_equal(levels(r[[v]]), c("Alto", "Bajo"))
    expect_equal(sum(r[[v]] == "Alto"), sum(dd[[paste0("q", k)]] %in% c("Mucho", "Bastante")))
  }
})

test_that("índices y alfa de Cronbach", {
  set.seed(3)
  f <- rnorm(500)
  m <- sapply(1:4, function(i) f + rnorm(500, sd = 0.8))
  a <- cronbach_alpha(m)
  # fórmula de referencia
  k <- 4; ref <- k / (k - 1) * (1 - sum(apply(m, 2, var)) / var(rowSums(m)))
  expect_equal(a$alpha, ref)
  expect_gt(a$alpha, 0.8)
  expect_equal(nrow(a$items), 4)
  m2 <- cbind(m, -m[, 1])
  b <- cronbach_alpha(m2)
  expect_lt(b$items$r_item_total[5], 0)                    # el ítem al revés se detecta
  expect_match(alpha_text(0.75), "aceptable")
  dd <- data.frame(a = c(1, 2, NA, 4), b = c(2, NA, NA, 4), c = c(3, 3, 3, NA))
  expect_equal(build_index(dd, c("a", "b", "c")), c(2, 2.5, NA, 4))
  expect_equal(build_index(dd, c("a", "b", "c"), "sum"), c(6, 7.5, NA, 12))
  i <- apply_step(d, list(op = "index", to = "ind", vars = c("confianza_ciencia", "satisfaccion"), label = "Mi índice"))
  expect_equal(attr(i$ind, "label"), "Mi índice")
  expect_error(apply_step(d, list(op = "index", to = "ind", vars = "edad")), "dos variables")
})

test_that("unir archivos: añadir casos y variables", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "anexos"))
  b <- d[1:40, c("id", "edad", "genero")]
  b$genero <- factor(as.character(b$genero), levels = c("Mujer", "Hombre", "Otro"))
  b$genero[1] <- "Otro"
  b$nueva <- factor(rep(c("x", "y"), 20))
  saveRDS(b, file.path(dir, "anexos", "aaaa1111bbbb2222.rds"))
  ap <- apply_step(d, list(op = "append", file = "aaaa1111bbbb2222.rds", origin = "origen", labels = c("A", "B")), dir)
  expect_equal(nrow(ap), nrow(d) + 40)
  expect_true("Otro" %in% levels(ap$genero))
  expect_equal(attr(ap$genero, "label"), attr(d$genero, "label"))
  expect_equal(as.vector(table(ap$origen)), c(nrow(d), 40))
  expect_true(all(is.na(ap$nueva[seq_len(nrow(d))])))
  jn <- apply_step(d, list(op = "join", file = "aaaa1111bbbb2222.rds", by = "id", vars = c("nueva", "edad")), dir)
  expect_equal(nrow(jn), nrow(d))
  expect_equal(sum(!is.na(jn$nueva)), 40)
  expect_true("edad_2" %in% names(jn))
  b2 <- rbind(b, b[1, ]); saveRDS(b2, file.path(dir, "anexos", "cccc1111bbbb2222.rds"))
  expect_error(apply_step(d, list(op = "join", file = "cccc1111bbbb2222.rds", by = "id", vars = "nueva"), dir), "se repite")
  expect_error(apply_step(d, list(op = "append", file = "../../etc/passwd"), dir), "no válido")
  expect_error(apply_step(d, list(op = "append", file = "aaaa1111bbbb2222.rds")), "carpeta del proyecto")
  # el código R generado reproduce la unión de variables
  code <- step_code(list(op = "join", file = "x", source = "otro.rds", by = "id", vars = "nueva"))
  expect_match(paste(code, collapse = "\n"), "merge\\(")
})

test_that("libro de códigos en Word y Excel", {
  cb <- codebook_df(d)
  expect_equal(nrow(cb), ncol(d))
  expect_match(cb$Valores[cb$Variable == "genero"], "Mujer")
  f1 <- withr::local_tempfile(fileext = ".docx"); write_codebook(d, f1, "docx", recipe = list(list(op = "dedupe")))
  f2 <- withr::local_tempfile(fileext = ".xlsx"); write_codebook(d, f2, "xlsx", recipe = list(list(op = "dedupe")))
  expect_gt(file.size(f1), 5000)
  x <- openxlsx::read.xlsx(f2, sheet = "Variables")
  expect_equal(nrow(x), ncol(d))
  expect_true("Cambios" %in% openxlsx::getSheetNames(f2))
})

test_that("exportar e importar un proyecto completo", {
  con <- local_storage()
  a <- get_user(con, create_user(con, "a@ejemplo.com", "A", "contraseña-segura")$id)
  b <- get_user(con, create_user(con, "b@ejemplo.com", "B", "contraseña-segura")$id)
  r <- create_project(con, a, demo_sav(), "demo_encuesta.sav", "Mi encuesta")
  dir <- project_path(con, r$id)
  ann <- save_annex(con, a, dir, demo_sav(), "otra.sav")
  expect_true(ann$ok)
  rec <- list(list(op = "label", var = "edad", label = "Años"),
              list(op = "append", file = ann$file, source = "otra.sav", origin = "origen"),
              list(op = "recode", var = "educacion", to = "educ2", others = "na",
                   groups = list(list(name = "Básica", from = c("Sin estudios", "Secundaria")))))
  write_recipe(dir, rec)
  dd <- apply_recipe(read_any(project_original(dir)), rec, dir)
  res <- explore_two(dd, "genero", "educ2")
  save_result(dir, "tabla", res$title, list(table = res$table, spec = list(mode = "two", v1 = "genero", v2 = "educ2")))
  doc <- new_document("Informe", "informe")
  doc$pages[[1]]$elements <- list(list(kind = "text", html = "<p onclick=\"alert(1)\">Hola</p><script>alert(2)</script>"))
  save_document(dir, doc)

  f <- withr::local_tempfile(fileext = ".caring")
  export_project(con, r$id, f)
  nombres <- zip::zip_list(f)$filename
  expect_true(all(c("proyecto.json", "original.sav", "receta.json") %in% nombres))
  expect_false(any(grepl("\\.rds$", nombres)))

  imp <- import_project(con, b, f)
  expect_true(imp$ok)
  p2 <- get_project(con, imp$id)
  expect_equal(p2$nombre, "Mi encuesta")
  expect_equal(p2$n_casos, 2400)
  dir2 <- project_path(con, imp$id)
  rec2 <- read_recipe(dir2)
  expect_equal(rec2, rec)
  d2 <- apply_recipe(read_any(project_original(dir2)), rec2, dir2)
  expect_equal(d2$educ2, dd$educ2)
  rs <- list_results(dir2)
  expect_length(rs, 1)
  expect_equal(load_result(dir2, rs[[1]]$id)$object$table, res$table)
  doc2 <- load_document(dir2, doc$id)
  txt <- doc2$pages[[1]]$elements[[1]]$html
  expect_false(grepl("script|onclick", txt))
  expect_match(txt, "Hola")
})

test_that("un proyecto manipulado no se importa", {
  con <- local_storage()
  a <- get_user(con, create_user(con, "a@ejemplo.com", "A", "contraseña-segura")$id)
  hacer <- function(extra = list(), receta = to_json_data(list())) {
    tmp <- withr::local_tempdir(.local_envir = parent.frame())
    writeLines(jsonlite::toJSON(list(formato = project_format, version = 1, nombre = "X", archivo = "x.sav"), auto_unbox = TRUE),
               file.path(tmp, "proyecto.json"))
    file.copy(demo_sav(), file.path(tmp, "original.sav"))
    writeLines(receta, file.path(tmp, "receta.json"))
    for (n in names(extra)) { dir.create(dirname(file.path(tmp, n)), recursive = TRUE, showWarnings = FALSE); writeLines(extra[[n]], file.path(tmp, n)) }
    f <- tempfile(fileext = ".caring")
    zip::zip(f, list.files(tmp, recursive = TRUE), root = tmp)
    f
  }
  expect_true(import_project(con, a, hacer())$ok)
  expect_match(import_project(con, a, hacer(list("receta.rds" = "x")))$error, "no admitidos")
  expect_match(import_project(con, a, hacer(list("informes/img/x.svg" = "<svg onload='x'/>")))$error, "no admitidos")
  expect_match(import_project(con, a, hacer(list("resultados/abc123.html" = "<script>")))$error, "no admitidos")
  mala <- to_json_data(list(list(op = "label", var = "edad", label = "x", f = function() system("echo hola"))))
  expect_match(import_project(con, a, hacer(receta = mala))$error, "historial")
  rara <- to_json_data(list(list(op = "inventada")))
  expect_match(import_project(con, a, hacer(receta = rara))$error, "no se puede aplicar")
  f <- tempfile(fileext = ".caring"); writeLines("no es un zip", f)
  expect_match(import_project(con, a, f)$error, "no es un proyecto")
  expect_equal(sanitize_html("<a href=\"javascript:alert(1)\">x</a>"), "<a href=\"#\">x</a>")
})
