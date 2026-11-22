# ------------------------------------------------------------------------------
# Arranca CARING 2 en tu ordenador.
#
# Uso: abre este archivo y ejecútalo entero (en RStudio, botón "Source";
# en VS Code, Ctrl/Cmd + Mayús + S), o en la consola de R:
#     setwd("ruta/donde/esta/caring2")
#     source("iniciar.R")
#
# La primera vez instala los paquetes que falten (tarda unos minutos).
# Para parar la aplicación: tecla Esc en la consola de R.
# ------------------------------------------------------------------------------

# 1. Carpeta del proyecto: la de este archivo o, si no se detecta, la carpeta actual.
carpeta <- NULL
for (i in rev(seq_len(sys.nframe()))) {
  f <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
  if (is.character(f) && length(f) == 1) { carpeta <- dirname(normalizePath(f)); break }
}
if (is.null(carpeta) || !file.exists(file.path(carpeta, "DESCRIPTION"))) {
  carpeta <- getwd()
}
if (!file.exists(file.path(carpeta, "DESCRIPTION")) || !file.exists(file.path(carpeta, "app.R"))) {
  stop("No encuentro la carpeta de CARING 2. Sitúate en ella con setwd(\"ruta/donde/esta/caring2\") ",
       "y vuelve a ejecutar source(\"iniciar.R\").")
}
setwd(carpeta)
message("Carpeta de CARING 2: ", carpeta)

# 2. Paquetes que falten
source(file.path(carpeta, "instalar.R"), local = TRUE)

# 3. Cargar el código de la aplicación
pkgload::load_all(carpeta, quiet = TRUE)

# 4. Arrancar y abrir en el navegador del sistema (Chrome, Safari, Firefox…).
#    No se usa el panel interno de VS Code/RStudio: algunos bloquean partes de la app.
puerto <- getOption("shiny.port", 3838)
libre <- tryCatch({ srv <- httpuv::startServer("127.0.0.1", puerto, list()); httpuv::stopServer(srv); TRUE },
                  error = function(e) FALSE)
if (!libre) puerto <- httpuv::randomPort()
url <- paste0("http://127.0.0.1:", puerto)
abrir_navegador <- function(u) {
  if (identical(getOption("caring.no_browser"), TRUE)) return(invisible())
  sys <- Sys.info()[["sysname"]]
  ok <- tryCatch({
    if (sys == "Darwin") system2("open", u, wait = FALSE)
    else if (sys == "Windows") shell.exec(u)
    else system2("xdg-open", u, wait = FALSE, stdout = FALSE, stderr = FALSE)
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) utils::browseURL(u)
}
message("\n==============================================================")
message("  CARING 2 está arrancando en:  ", url)
message("  Si no se abre solo, copia esa dirección en Chrome o Safari.")
message("  Para pararlo: tecla Esc en esta consola.")
message("==============================================================\n")
shiny::runApp(carpeta, port = puerto, launch.browser = abrir_navegador)
