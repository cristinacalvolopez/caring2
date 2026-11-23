d0 <- read_any(demo_sav())

doc_prueba <- function(dir, type = "slides") {
  x <- explore_from_spec(d0, list(mode = "two", v1 = "internet", v2 = "educacion"))
  png <- withr::local_tempfile(fileext = ".png", .local_envir = parent.frame()); save_plot_png(x$plot, png)
  g <- save_result(dir, "grafico", x$title, list(table = x$table, subtitle = x$subtitle, stats = x$stats), png = png)
  y <- explore_from_spec(d0, list(mode = "one", v1 = "educacion"))
  t <- save_result(dir, "tabla", y$title, list(table = y$table, subtitle = y$subtitle))
  doc <- new_document("Prueba", type)
  doc$pages[[1]]$elements <- c(doc$pages[[1]]$elements, list(
    list(id = "t1", kind = "text", x = 40, y = 380, w = 500, h = 60, style = list(size = 16, color = "#000000"),
         text = "Lo más importante: el 88 % usa internet",
         runs = list(list(t = "Lo más importante: ", m = NULL), list(t = "el 88 % usa internet", m = "#FFF176")))))
  doc$pages[[2]] <- list(id = "p2", bg = "#FFFFFF", elements = list(
    list(id = "r2", kind = "result", ref = t, x = 40, y = 300, w = 400, h = 200),
    list(id = "r1", kind = "result", ref = g, x = 40, y = 40, w = 400, h = 200),
    list(id = "r3", kind = "result", ref = g, x = 500, y = 40, w = 400, h = 200)))
  doc$pages[[3]] <- list(id = "p3", bg = "#135DCD", elements = list())
  list(doc = doc, g = g, t = t)
}

test_that("el subrayador se conserva en HTML, PowerPoint y Word", {
  dir <- withr::local_tempdir(); x <- doc_prueba(dir)
  f <- withr::local_tempfile(fileext = ".html"); export_html(dir, x$doc, f)
  h <- paste(readLines(f, warn = FALSE), collapse = "")
  expect_match(h, 'class="mk" style="background:#FFF176">el 88 % usa internet</span>')
  pp <- withr::local_tempfile(fileext = ".pptx"); export_pptx(dir, x$doc, pp)
  xml <- paste(readLines(unz(pp, "ppt/slides/slide1.xml"), warn = FALSE), collapse = "")
  expect_match(xml, "FFF176")
  dx <- withr::local_tempfile(fileext = ".docx"); export_docx(dir, x$doc, dx)
  xml <- paste(readLines(unz(dx, "word/document.xml"), warn = FALSE), collapse = "")
  expect_match(xml, "FFF176")
  # colores no válidos no pasan a la página
  e <- list(text = "x", runs = list(list(t = "x", m = "red;background:url(javascript:1)")))
  expect_false(grepl("javascript", text_html(e)))
  # texto sin subrayado: igual que antes
  expect_equal(text_html(list(text = "a\nb <c>")), "a<br>b &lt;c&gt;")
})

test_that("numeración automática de páginas", {
  dir <- withr::local_tempdir(); x <- doc_prueba(dir)
  expect_true(x$doc$page_numbers)
  f <- withr::local_tempfile(fileext = ".html"); export_html(dir, x$doc, f)
  h <- paste(readLines(f, warn = FALSE), collapse = "")
  expect_equal(lengths(regmatches(h, gregexpr('class="pnum', h))), 2)           # páginas 2 y 3, no la portada
  expect_match(h, 'class="pnum right" style="color:#FFFFFF">3<')                 # fondo oscuro: número blanco
  pp <- withr::local_tempfile(fileext = ".pptx"); export_pptx(dir, x$doc, pp)
  s <- officer::pptx_summary(officer::read_pptx(pp))
  expect_true("2" %in% s$text[s$slide_id == 2])
  expect_false("1" %in% s$text[s$slide_id == 1])
  r <- doc_prueba(withr::local_tempdir(), "report")
  dx <- withr::local_tempfile(fileext = ".docx"); export_docx(dir, r$doc, dx)
  foot <- paste(unlist(lapply(grep("footer", unzip(dx, list = TRUE)$Name, value = TRUE), function(n) readLines(unz(dx, n), warn = FALSE))), collapse = "")
  expect_match(foot, "PAGE")
  x$doc$page_numbers <- FALSE
  export_html(dir, x$doc, f)
  expect_false(grepl('class="pnum', paste(readLines(f, warn = FALSE), collapse = "")))
})

test_that("tablas del documento a Excel y Word", {
  dir <- withr::local_tempdir(); x <- doc_prueba(dir)
  tabs <- doc_tables(dir, x$doc)
  expect_length(tabs, 2)                                   # el gráfico repetido cuenta una vez
  expect_equal(tabs[[1]]$kind, "grafico")                  # orden de lectura: arriba primero
  f <- withr::local_tempfile(fileext = ".xlsx"); export_tables_xlsx(dir, x$doc, f)
  hojas <- openxlsx::getSheetNames(f)
  expect_length(hojas, 2)
  t1 <- openxlsx::read.xlsx(f, sheet = 1, startRow = 4, check.names = FALSE, sep.names = " ")
  expect_equal(names(t1), names(tabs[[1]]$table))
  w <- withr::local_tempfile(fileext = ".docx"); export_tables_docx(dir, x$doc, w)
  s <- officer::docx_summary(officer::read_docx(w))
  expect_true(any(grepl("Secundaria", s$text)))
  vacio <- new_document("Vacío", "slides")
  expect_error(export_tables_xlsx(dir, vacio, f), "no tiene tablas")
})
