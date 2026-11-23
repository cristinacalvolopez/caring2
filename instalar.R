# Instala los paquetes que necesita CARING 2.
#
# En tu ordenador (desde la carpeta del proyecto):
#     Rscript instalar.R
# En el servidor (solo lo necesario para la aplicación, sin los paquetes de pruebas):
#     Rscript instalar.R --servidor
#
# Se usa el repositorio configurado en R (options("repos")) y, si no hay ninguno,
# CRAN. En Linux conviene usar Posit Package Manager, que da paquetes ya compilados:
#     Rscript -e 'options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/noble/latest"))' ...
# (ver LEEME_ADMINISTRADOR.md en la carpeta de entrega).

servidor <- "--servidor" %in% commandArgs(trailingOnly = TRUE)

paquetes <- c(
  "shiny", "bslib", "DBI", "RSQLite", "sodium", "jsonlite", "uuid", "digest",
  "haven", "readxl", "data.table", "netCoin", "ggplot2", "sortable", "reactable",
  "officer", "flextable", "survey", "magick", "openxlsx", "zip", "base64enc", "webshot2", "chromote", "htmltools", "pkgload", "curl"
)
if (!servidor) {
  # solo para las pruebas automáticas
  paquetes <- c(paquetes, "testthat", "withr", "shinytest2", "writexl", "tibble")
}

repos <- getOption("repos")
if (is.null(repos) || !length(repos) || identical(unname(repos["CRAN"]), "@CRAN@")) {
  repos <- c(CRAN = Sys.getenv("CARING_REPOS", "https://cloud.r-project.org"))
}

faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan)) {
  message("Instalando: ", paste(faltan, collapse = ", "))
  install.packages(faltan, repos = repos)
  aun <- faltan[!vapply(faltan, requireNamespace, logical(1), quietly = TRUE)]
  if (length(aun)) stop("No se han podido instalar: ", paste(aun, collapse = ", "),
                        ". Revisa los mensajes anteriores (suelen faltar librerías del sistema).")
} else {
  message("Todos los paquetes ya están instalados.")
}
if (is.null(chromote::find_chrome())) {
  message("Aviso: no se ha encontrado Google Chrome ni Chromium. CARING funciona igual, ",
          "pero sin exportar a PDF ni generar las imágenes de las redes para PowerPoint/Word.")
}
