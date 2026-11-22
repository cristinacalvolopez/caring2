# ------------------------------------------------------------------------------
# Arranca CARING 2 en tu ordenador.
#
# Uso: abre este archivo y ejecútalo entero (en RStudio, botón "Source";
# en VS Code, Ctrl/Cmd + Mayús + S), o en la consola de R:
#     source("~/Desktop/GitHUb/caring2/iniciar.R")
#
# La primera vez instala los paquetes que falten (tarda unos minutos).
# Para parar la aplicación: tecla Esc en la consola de R.
# ------------------------------------------------------------------------------

# 1. Carpeta del proyecto: la de este archivo (o la ruta habitual si no se detecta).
carpeta <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL)
if (is.null(carpeta) || !file.exists(file.path(carpeta, "DESCRIPTION"))) {
  carpeta <- path.expand("~/Desktop/GitHUb/caring2")
}
if (!file.exists(file.path(carpeta, "DESCRIPTION"))) {
  stop("No encuentro la carpeta de CARING 2. Cambia la ruta en la línea 'carpeta <- ...'.")
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
