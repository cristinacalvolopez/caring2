# Instala los paquetes que necesita CARING 2.
# Uso (desde la carpeta del proyecto):  Rscript instalar.R

paquetes <- c(
  "shiny", "bslib", "DBI", "RSQLite", "sodium", "jsonlite", "uuid", "digest",
  "haven", "readxl", "data.table", "netCoin", "ggplot2", "sortable", "reactable",
  "officer", "flextable", "base64enc", "webshot2", "chromote", "htmltools", "pkgload", "curl",
  # solo para las pruebas automáticas
  "testthat", "withr", "shinytest2", "writexl", "tibble"
)
faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan)) {
  message("Instalando: ", paste(faltan, collapse = ", "))
  install.packages(faltan, repos = "https://cloud.r-project.org")
} else {
  message("Todos los paquetes ya están instalados.")
}
if (is.null(chromote::find_chrome())) {
  message("Aviso: no se ha encontrado Google Chrome ni Chromium. CARING funciona igual, ",
          "pero sin exportar a PDF ni generar las imágenes de las redes para PowerPoint/Word.")
}
