# Punto de entrada para shiny::runApp(), Shiny Server o Posit Connect.
# En local también puedes ejecutar:  Rscript -e 'caring2::run_app()'
if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) {
  pkgload::load_all(".", quiet = TRUE, export_all = TRUE)
  options(caring2.root = normalizePath("."))
} else {
  library(caring2)
}
caring_app()
