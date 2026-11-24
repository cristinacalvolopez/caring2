# Ayuda guiada --------------------------------------------------------------------------------------
# «¿Cómo se lee?» de cada análisis (Explorar y Redes) y visita guiada de la aplicación.

ayuda_detalle <- function(titulo = "¿Cómo se lee?", ...) {
  tags$details(class = "c-howto", tags$summary(titulo), div(class = "c-howto-body", ...))
}

# Redes -------------------------------------------------------------------------------------------
net_help <- list(
  factorial = list(
    "Cada punto es un caso. Los casos que responden de forma parecida quedan cerca; los distintos, lejos.",
    "Los ejes resumen las variables activas: correspondencias si son cualitativas, componentes principales si son cuantitativas.",
    "Los colores son conglomerados: grupos de casos parecidos. Las variables pasivas no cambian la nube; se ven al pulsar cada punto."),
  coincidencias = list(
    "Cada nodo es una categoría (por ejemplo, «Mujer» o «Usa internet: Sí»); su tamaño, cuántos casos la tienen.",
    "Una línea une dos categorías que aparecen juntas más de lo esperado si fueran independientes (residuo de Haberman significativo).",
    "El color indica la variable de cada categoría; si eliges comunidades, se agrupan las categorías que tienden a darse juntas. Que no haya línea no significa que nunca coincidan, sino que no lo hacen más de lo esperado."),
  correlaciones = list(
    "Cada nodo es una variable numérica.",
    "Las líneas son correlaciones significativas, más gruesas cuanto más fuertes. Pulsa una línea para ver su valor: positivo si las variables aumentan juntas, negativo si una sube cuando la otra baja.",
    "Una correlación no implica causa: solo que las variables varían juntas."),
  loglineal = list(
    "Cada nodo es una categoría. Las líneas son asociaciones que se mantienen al tener en cuenta todas las demás variables del modelo.",
    "Es más exigente que la red de coincidencias: si una relación se explica por otra variable, desaparece.",
    "Con orden 3 se buscan también relaciones entre tríos de categorías."),
  regresion = list(
    "Los nodos son la variable que quieres explicar y las categorías o variables explicativas.",
    "Las líneas son efectos significativos sobre la variable explicada; pulsa una para ver el coeficiente y su signo (positivo: la aumenta; negativo: la reduce).",
    "Cada efecto se mide manteniendo constantes las demás variables explicativas."),
  multiple = list(
    "Cada nodo es una opción de una pregunta de respuesta múltiple; su tamaño, cuántas personas la marcan.",
    "Una línea une dos opciones que se marcan juntas más de lo esperado.",
    "Si cruzas con otra variable, sus categorías aparecen con otro color y se unen a las opciones que marcan más que el resto."),
  evolucion = list(
    "Cada barra es un caso, desde su inicio hasta su fin; las barras que se solapan coinciden en el tiempo.",
    "La banda de arriba resume todo el periodo; debajo, cada grupo por separado.",
    "El color agrupa los casos (por ejemplo, por escuela). Pulsa un caso para ver su información."),
  trayectorias = list(
    "Las flechas van de las causas supuestas a sus efectos. El número de cada flecha es el coeficiente estandarizado (β): entre −1 y 1, más lejos de 0 cuanto más fuerte.",
    "Verde: efecto positivo; rojo: negativo. Las variables latentes (círculos) se miden con sus indicadores.",
    "El ajuste del modelo es aceptable si el CFI es 0,90 o más y el RMSEA 0,08 o menos. R² es la parte de cada variable que explica el modelo.")
)

net_help_ui <- function(type) {
  h <- net_help[[type]]
  if (is.null(h)) return(NULL)
  ayuda_detalle(tags$ul(lapply(h, tags$li)))
}

# Explorar --------------------------------------------------------------------------------------------
explore_help <- function(kind) {
  txt <- switch(kind %||% "",
    cat = list("El gráfico y la tabla muestran cuántos casos hay en cada categoría.",
               "«% válido» no cuenta los perdidos; «%» sí. Con ponderación, los recuentos son ponderados."),
    num = list("La media es el valor promedio; la mediana deja la mitad de los casos por debajo y la mitad por encima.",
               "La desviación típica mide cuánto se alejan los casos de la media. Si la media y la mediana están lejos, la distribución es asimétrica."),
    catcat = list("Compara los porcentajes en el sentido indicado por la flecha: si las columnas suman 100 %, compara entre columnas a lo largo de cada fila.",
                  "χ² (ji cuadrado) contrasta si las dos variables son independientes: si p < 0,05, la relación es estadísticamente significativa.",
                  "La V de Cramér mide la fuerza de la relación: menos de 0,10 muy débil, hasta 0,30 débil, hasta 0,50 moderada y más, fuerte."),
    numcat = list("Compara la media (y la mediana) de la variable numérica en cada grupo.",
                  "El ANOVA contrasta si las medias son iguales: si p < 0,05, al menos un grupo es distinto.",
                  "η² es la parte de la variación que explican los grupos: 0,01 pequeña, 0,06 media, 0,14 grande."),
    numnum = list("Cada punto es un caso. La correlación de Pearson (r) mide la relación lineal, entre −1 y 1; la de Spearman (ρ), cualquier relación que siempre suba o siempre baje.",
                  "Si p < 0,05, la relación es estadísticamente significativa. Correlación no implica causa."),
    mr = list("En la respuesta múltiple cada caso puede marcar varias opciones.",
              "«% de casos» suma más de 100 %; «% de respuestas» reparte el total de marcas y suma 100 %."),
    mrcat = list("Para cada grupo, el porcentaje de casos que marca cada opción (las columnas suman más de 100 %).",
                 "Los asteriscos señalan las opciones que difieren entre grupos: * p < 0,05, ** p < 0,01, *** p < 0,001."),
    catcat3 = , numcat3 = list("Es la misma relación entre las dos primeras variables, calculada por separado en cada categoría de la variable de control.",
                               "Si la relación aparece en todos los grupos, no se debe a la variable de control; si cambia o desaparece en alguno, la variable de control la modifica."),
    NULL)
  if (is.null(txt)) return(NULL)
  ayuda_detalle(tags$ul(lapply(txt, tags$li)))
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
    paso("#project-datos-history", "Historial", "Cada cambio queda aquí y se puede quitar. El archivo original nunca se modifica."),
    paso(".c-steps .c-step:nth-of-type(2)", "Explorar", "Elige una, dos o tres variables: CARING propone el gráfico, la tabla y la prueba estadística. Con «+ Añadir al informe» guardas el resultado."),
    paso(".c-steps .c-step:nth-of-type(3)", "Redes", "Coincidencias, correlaciones, nube de puntos, evolución temporal, ecuaciones estructurales… calculadas con netCoin."),
    paso(".c-steps .c-step:nth-of-type(4)", "Informes", "Monta diapositivas o un informe con tus resultados y descárgalo en PowerPoint, Word, PDF o HTML, o compártelo con un enlace."),
    paso("#project-code", "Código R", "El script de R que reproduce todos los cambios que has hecho en los datos."))
}
