# Estudios del CIS --------------------------------------------------------------------
# Si el archivo subido es un estudio del CIS, se ofrece armonizar sus variables
# sociodemográficas. Lo hace cisConvert(), copiada de sortCIS (R/cisConvert*.R), así
# que no hace falta tener el paquete instalado.
#
# Es un paso más del historial (op = "cis"): se puede quitar y volver a añadir, y
# aparece en el script descargable. Añade a los datos las variables armonizadas y
# las escalas y Likert convertidas; las del archivo original no se tocan.

cis_ext <- c("sav", "dta")

# ¿Es un estudio del CIS? Devuelve NULL o list(estudio, conceptos).
# Lee solo las primeras filas: bastan las etiquetas. Usa los mismos patrones con
# los que cisConvert() detecta cada concepto.
es_cis <- function(path, file_name = basename(path), min_conceptos = 3) {
  ext <- file_ext(file_name)
  if (!ext %in% cis_ext || !file.exists(path)) return(NULL)
  h <- tryCatch(suppressWarnings(
         if (ext == "dta") haven::read_dta(path, n_max = 20) else read_sav_fecha(path, n_max = 20)),
       error = function(e) NULL)
  if (is.null(h) || !ncol(h)) return(NULL)
  et <- .cis_norm(vapply(h, .cis_label, character(1)))
  hay <- vapply(.CIS_PATTERN, function(p) any(vapply(p, function(q) any(grepl(q, et)), logical(1))), logical(1))
  if (sum(hay) < min_conceptos) return(NULL)
  estudio <- .cis_get_study(h, file_name)
  if (is.na(estudio)) return(NULL)
  list(estudio = as.numeric(estudio), conceptos = names(.CIS_PATTERN)[hay])
}

# Paso del historial para un estudio detectado
cis_step <- function(cis, archivo) {
  list(op = "cis", estudio = cis$estudio, archivo = basename(archivo))
}

# Lo que devuelve cisConvert() ya leído con las reglas de CARING. Se guarda en
# memoria por archivo, porque la receta se vuelve a aplicar a menudo.
cis_cache <- new.env(parent = emptyenv())

cis_convertido <- function(archivo, estudio = NULL) {
  if (!file.exists(archivo)) stop("No encuentro el archivo del estudio del CIS: ", archivo)
  inf <- file.info(archivo)
  key <- paste(normalizePath(archivo), inf$size, as.numeric(inf$mtime), estudio %||% "")
  if (!is.null(cis_cache[[key]])) return(cis_cache[[key]])
  utils::capture.output(conv <- suppressWarnings(cisConvert(
    archivo, output = "full", study = estudio, write = FALSE, verbose = FALSE)))
  ext <- file_ext(archivo)
  orig <- suppressWarnings(if (ext == "dta") haven::read_dta(archivo, n_max = 0) else read_sav_fecha(archivo, n_max = 0))
  conv <- standardize_data(as.data.frame(conv))
  # nuevas: las armonizadas (siempre, aunque el archivo ya traiga una con ese
  # nombre) y las escalas y Likert convertidas, que van en minúsculas. Las
  # originales que cisConvert() renombra con _ORIG ya están en los datos.
  harm <- intersect(c("orden", "numentr", setdiff(.CIS_OUTPUT, c("estudio", "cues"))), names(conv))
  otras <- setdiff(names(conv), c(clean_names(names(orig)), harm))
  nuevas <- c(harm, otras[!grepl("_ORIG[0-9]*$", otras)])
  r <- list(datos = conv[nuevas], n = nrow(conv))
  if (length(ls(cis_cache)) > 20) rm(list = ls(cis_cache), envir = cis_cache)
  cis_cache[[key]] <- r
  r
}

#' Añade las variables sociodemográficas armonizadas de un estudio del CIS.
#'
#' Aplica cisConvert() (de sortCIS, incluida en caring2) al archivo original del
#' estudio y añade a `datos`, delante, las variables armonizadas (genero,
#' generacion, edad, ideologia, religion, estudios, laboral, profesional, clase),
#' las claves orden y numentr, y las escalas y Likert convertidas.
#' @param datos data.frame leído con read_any() del mismo archivo
#' @param archivo ruta del .sav o .dta original del CIS
#' @param estudio número de estudio (por defecto, el que traiga el archivo)
#' @export
cis_armonizar <- function(datos, archivo, estudio = NULL) {
  conv <- cis_convertido(archivo, estudio)
  if (nrow(datos) != conv$n) {
    stop("Las variables del CIS se calculan caso a caso sobre el archivo original: ",
         "este paso tiene que ir antes de filtrar, quitar o añadir casos.")
  }
  nuevas <- names(conv$datos)
  # Como en cisConvert(): la original que se llama igual que una nueva (sin
  # distinguir mayúsculas, porque SPSS no las distingue: ESCIDEOL y escideol) se
  # conserva con el sufijo _ORIG.
  sets <- attr(datos, "mrsets", exact = TRUE)
  for (v in names(datos)[tolower(names(datos)) %in% tolower(nuevas)]) {
    nn <- paste0(v, "_ORIG")
    while (tolower(nn) %in% tolower(c(names(datos), nuevas))) nn <- paste0(nn, "1")
    names(datos)[names(datos) == v] <- nn
  }
  out <- cbind(conv$datos, datos)
  if (!is.null(sets)) attr(out, "mrsets") <- sets
  out
}
