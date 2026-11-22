# Genera inst/app/templates/caring_16x9.pptx: la plantilla por defecto de officer
# convertida a formato panorámico 16:9 (13,333 × 7,5 pulgadas).
# Ejecutar desde la raíz del repo: Rscript data-raw/plantilla_pptx.R

src <- system.file("template", "template.pptx", package = "officer")
tmp <- tempfile("pptx-")
dir.create(tmp)
utils::unzip(src, exdir = tmp)
pres <- file.path(tmp, "ppt", "presentation.xml")
x <- readLines(pres, warn = FALSE, encoding = "UTF-8")
x <- gsub('<p:sldSz cx="[0-9]+" cy="[0-9]+"[^/]*/>', '<p:sldSz cx="12192000" cy="6858000"/>', x)
writeLines(x, pres, useBytes = TRUE)
dir.create(file.path("inst", "app", "templates"), recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(file.path("inst", "app", "templates"))
dest <- file.path(out, "caring_16x9.pptx")
unlink(dest)
old <- setwd(tmp)
utils::zip(dest, files = list.files(".", recursive = TRUE, all.files = TRUE), flags = "-qX")
setwd(old)
p <- officer::read_pptx(dest)
message("Plantilla creada: ", dest, " (", paste(unlist(officer::slide_size(p)), collapse = " x "), " pulgadas)")
