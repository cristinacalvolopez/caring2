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

# 4. Arrancar (se abre el navegador)
message("Arrancando CARING 2… (pulsa Esc para pararlo)")
shiny::runApp(carpeta, launch.browser = TRUE)
