test_that("documentos: crear, guardar, cargar", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "resultados"))
  doc <- new_document("Prueba con ñ y acentos: análisis", "slides")
  expect_length(doc$pages, 2)
  save_document(dir, doc)
  d2 <- load_document(dir, doc$id)
  expect_equal(d2$title, "Prueba con ñ y acentos: análisis")
  expect_equal(nrow(list_documents(dir)), 1)
  expect_error(doc_path(dir, "../x"), "no válido")
})

make_full_doc <- function(dir, type = "slides") {
  d <- read_any(demo_sav())
  r1 <- explore_two(d, "internet", "educacion")
  png <- tempfile(fileext = ".png"); save_plot_png(r1$plot, png)
  id_g <- save_result(dir, "grafico", r1$title, list(plot = r1$plot), png = png)
  png2 <- tempfile(fileext = ".png"); save_table_png(r1$table, r1$title, png2)
  id_t <- save_result(dir, "tabla", "Tabla de educación", list(table = r1$table), png = png2)
  net <- net_build(d, list(type = "coincidencias", vars = c("genero", "internet", "prensa")))
  h <- tempfile(fileext = ".html"); net_render_html(net, h)
  id_n <- save_result(dir, "red", "Red ñandú", list(), png = png, html = h)
  img <- save_image(dir, png, "foto.png")
  doc <- new_document("Informe de prueba — señales", type)
  p <- doc$pages[[length(doc$pages)]]
  p$elements <- c(p$elements, list(
    el("result", 40, 100, 400, 260, ref = id_g),
    el("result", 460, 100, 450, 200, ref = id_t),
    el("result", 40, 300, 300, 200, ref = id_n),
    el("image", 600, 320, 200, 150, src = img),
    el("text", 600, 480, 300, 40, text = "Año, niño, acción\nsegunda línea", style = txt_style(14, TRUE, "#B3261E", "center"))))
  doc$pages[[length(doc$pages)]] <- p
  save_document(dir, doc)
  doc
}

test_that("exportar a PowerPoint, Word, HTML y PDF", {
  dir <- withr::local_tempdir()
  for (sub in c("resultados", "informes")) dir.create(file.path(dir, sub))
  doc <- make_full_doc(dir, "slides")
  f <- tempfile(fileext = ".pptx"); export_pptx(dir, doc, f)
  pp <- officer::read_pptx(f)
  expect_equal(length(pp), length(doc$pages))
  expect_equal(round(officer::slide_size(pp)$width, 1), 13.3)
  txt <- officer::pptx_summary(pp)$text
  expect_true(any(grepl("Año, niño, acción", txt)))

  f <- tempfile(fileext = ".docx"); export_docx(dir, doc, f)
  dx <- officer::docx_summary(officer::read_docx(f))
  expect_true(any(grepl("señales", dx$text)))
  expect_true(any(grepl("niño", dx$text)))

  f <- tempfile(fileext = ".html"); export_html(dir, doc, f)
  html <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "niño", fixed = TRUE)
  expect_match(html, "openNet(", fixed = TRUE)
  f2 <- tempfile(fileext = ".html"); export_html(dir, doc, f2, present = TRUE)
  expect_match(paste(readLines(f2, warn = FALSE), collapse = ""), "ArrowRight")

  skip_if_not(nzchar(chromote::find_chrome() %||% ""), "Sin Chrome")
  f <- tempfile(fileext = ".pdf"); export_pdf(dir, doc, f)
  expect_gt(file.size(f), 10000)
  expect_equal(rawToChar(readBin(f, "raw", 4)), "%PDF")
})

test_that("informe A4 a Word", {
  dir <- withr::local_tempdir()
  for (sub in c("resultados", "informes")) dir.create(file.path(dir, sub))
  doc <- make_full_doc(dir, "report")
  f <- tempfile(fileext = ".docx"); export_docx(dir, doc, f)
  expect_gt(file.size(f), 5000)
  f <- tempfile(fileext = ".pptx"); export_pptx(dir, doc, f)
  expect_gt(file.size(f), 5000)
})
