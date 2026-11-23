# Mensajes de error en español ------------------------------------------------------------
# Los mensajes propios de CARING ya están en español. Los que vienen de R o de otros
# paquetes suelen estar en inglés: se traducen los más habituales y, si no se reconocen,
# se explica en español y se deja el texto original como detalle técnico.

errores_conocidos <- c(
  "subscript out of bounds" = "Se ha pedido un elemento que no existe (índice fuera de los límites).",
  "subíndice fuera de +los límites" = "Se ha pedido un elemento que no existe (índice fuera de los límites).",
  "object '([^']+)' not found" = "No existe «\\1».",
  "could not find function \"([^\"]+)\"" = "La función «\\1» no está disponible.",
  "non-numeric argument to (binary operator|mathematical function)" = "Se ha intentado hacer una operación numérica con una variable que no es numérica.",
  "argument is not numeric or logical" = "La variable no es numérica.",
  "undefined columns selected" = "Alguna de las variables elegidas no existe.",
  "missing value where TRUE/FALSE needed" = "Hay valores perdidos donde se necesitaba un sí/no.",
  "argument is of length zero" = "Falta un valor necesario.",
  "replacement has (\\d+) rows?, data has (\\d+)" = "El resultado tiene \\1 filas y los datos \\2.",
  "arguments imply differing number of rows" = "Las variables no tienen el mismo número de casos.",
  "invalid 'type' \\(([^)]+)\\) of argument" = "Tipo de dato no válido (\\1).",
  "cannot open (the )?(file|connection)" = "No se ha podido abrir el archivo.",
  "cannot allocate vector of size (.+)" = "No hay memoria suficiente (hacían falta \\1).",
  "unused argument" = "Se ha pasado una opción que no se reconoce.",
  "Please specify a non-empty vector for `choices`" = "No hay opciones para elegir.",
  "contrasts can be applied only to factors with 2 or more levels" = "Alguna variable tiene una sola categoría con datos: hacen falta al menos dos.",
  "'x' must be numeric" = "La variable debe ser numérica.",
  "all entries of 'x' must be nonnegative and finite" = "Hay casillas negativas o infinitas en la tabla.",
  "at least two non-missing values are needed" = "Hacen falta al menos dos valores válidos.",
  "not enough (finite )?observations" = "No hay casos suficientes.",
  "NAs are not allowed in subscripted assignments" = "Hay valores perdidos donde no se admiten.",
  "incorrect number of dimensions" = "La tabla no tiene la forma esperada.",
  "invalid multibyte string" = "El archivo tiene caracteres con una codificación que no se reconoce (prueba a guardarlo como UTF-8).",
  "Timed out|timeout|timed out" = "La operación ha tardado demasiado y se ha cancelado.",
  "Chrome|chromote" = "No se ha encontrado Chrome o Chromium, que hace falta para crear imágenes y PDF."
)

parece_ingles <- function(msg) {
  grepl("\\b(the|of|is|are|not|must|be|argument|object|cannot|could|found|invalid|undefined|missing|length|unused|specify|please|error|should|only|need(ed)?|failed|unable|non|with|to)\\b",
        msg, ignore.case = TRUE, perl = TRUE) &&
    !grepl("[áéíóúñ¿¡]", msg) &&
    !grepl("\\b(el|la|los|las|de|del|que|una?|para|con|no se|hay|esta|este)\\b", msg, perl = TRUE)
}

#' Mensaje de error en español a partir de una condición o de un texto.
error_es <- function(e) {
  msg <- if (inherits(e, "condition")) conditionMessage(e) else paste(as.character(e), collapse = " ")
  msg <- trimws(sub("^Error( in [^:]+)?:\\s*", "", msg))
  if (!nzchar(msg)) return("Ha ocurrido un error inesperado.")
  for (pat in names(errores_conocidos)) {
    if (grepl(pat, msg, perl = TRUE)) {
      m <- regmatches(msg, regexec(pat, msg, perl = TRUE))[[1]]
      out <- errores_conocidos[[pat]]
      for (k in seq_along(m)[-1]) out <- gsub(paste0("\\", k - 1), m[k], out, fixed = TRUE)
      return(out)
    }
  }
  if (parece_ingles(msg)) return(paste0("No se ha podido completar la operación (detalle técnico: ", msg, ")."))
  msg
}
