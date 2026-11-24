# Ayuda guiada --------------------------------------------------------------------------------------
# «¿Cómo se lee?» de cada análisis (Explorar y Redes) y visita guiada de la aplicación.

ayuda_detalle <- function(titulo = "¿Cómo se lee?", ...) {
  tags$details(class = "c-howto", tags$summary(titulo), div(class = "c-howto-body", ...))
}

# Redes -------------------------------------------------------------------------------------------
# Una explicación breve y directa de cada análisis (sin desplegables ni listas).
net_help <- list(
  factorial = "Cada punto es un caso: los que responden de forma parecida quedan cerca. Los colores son grupos de casos parecidos.",
  coincidencias = "Cada nodo es una categoría y su tamaño indica cuántos casos la tienen. Una línea une dos categorías que aparecen juntas más de lo esperado por azar.",
  correlaciones = "Cada nodo es una variable. Una línea une dos variables correlacionadas; cuanto más gruesa, más fuerte es la relación.",
  loglineal = "Cada nodo es una categoría. Una línea une dos categorías asociadas entre sí, teniendo en cuenta el resto de variables del modelo.",
  regresion = "Los nodos son la variable que quieres explicar y sus variables explicativas. Una línea indica un efecto significativo; al pulsarla ves su tamaño y su signo.",
  multiple = "Cada nodo es una opción y su tamaño indica cuántas personas la marcan. Una línea une dos opciones que se marcan juntas más de lo esperado.",
  evolucion = "Cada barra es un caso, desde su inicio hasta su fin. Las barras que se solapan coinciden en el tiempo.",
  trayectorias = "Las flechas van de las causas a los efectos y su número indica la fuerza del efecto, entre −1 y 1. En verde, los efectos positivos; en rojo, los negativos."
)

net_help_ui <- function(type) {
  h <- net_help[[type]]
  if (is.null(h)) return(NULL)
  p(class = "c-net-help", h)
}

# Visita guiada ---------------------------------------------------------------------------------------
# Cada paso: selector CSS del elemento que se resalta, título y texto. Los pasos cuyo elemento no
# está en pantalla se saltan.
tour_steps <- function(where = "proyecto") {
  paso <- function(sel, titulo, texto) list(sel = sel, title = titulo, text = texto)
  if (identical(where, "inicio")) {
    return(list(
      paso(".c-new", "Nuevo proyecto", "Sube tu base de datos (SPSS, Stata, R, CSV, Excel…) o usa los datos de ejemplo. Cada proyecto guarda sus datos, cambios, resultados e informes."),
      paso("#projects-filter", "Tus proyectos", "Filtra los tuyos, los que te han compartido y la papelera, donde lo borrado se guarda 30 días."),
      paso("#projects-search", "Buscar", "Escribe parte del nombre del proyecto o del archivo."),
      paso("#nav_help", "Ayuda", "Aquí tienes la explicación de cada paso y puedes volver a ver esta visita.")))
  }
  list(
    paso(".c-steps", "Cuatro pasos", "Datos → Explorar → Redes → Informes. Puedes ir y volver cuando quieras: todo se guarda solo."),
    paso("#project-datos-vars", "Tus variables", "Pulsa una variable para cambiar su nombre, su etiqueta o sus categorías, recodificarla, marcar perdidos o cambiar su tipo."),
    paso("#project-datos-tools", "Herramientas", "Recodificar varias variables a la vez, invertir escalas, crear índices (con alfa de Cronbach), unir otro archivo y descargar el libro de códigos."),
    paso("#project-datos-view", "Revisar los datos", "En «Revisar» CARING busca casos repetidos, variables vacías o códigos como 99 que en realidad son «No sabe»."),
    paso(".c-history", "Historial", "Cada cambio queda aquí y se puede quitar. El archivo original nunca se modifica."),
    paso(".c-steps .c-step:nth-of-type(2)", "Explorar", "Elige una, dos o tres variables: CARING propone el gráfico, la tabla y la prueba estadística. Con «+ Añadir al informe» guardas el resultado."),
    paso(".c-steps .c-step:nth-of-type(3)", "Redes", "Coincidencias, correlaciones, nube de puntos, evolución temporal, ecuaciones estructurales… calculadas con netCoin."),
    paso(".c-steps .c-step:nth-of-type(4)", "Informes", "Monta diapositivas o un informe con tus resultados y descárgalo en PowerPoint, Word, PDF o HTML, o compártelo con un enlace."),
    paso("#project-code", "Código R", "El script de R que reproduce todos los cambios que has hecho en los datos."))
}
