# Importación de datos ------------------------------------------------------------
# Convierte cualquier formato admitido en un data.frame "estándar":
#   - categóricas  -> factor (ordinal -> factor ordenado)
#   - numéricas    -> numeric
#   - texto        -> character
#   - fechas       -> Date / POSIXct
# La etiqueta de cada variable se guarda en attr(x, "label").

formatos_admitidos <- c(
  sav = "SPSS", zsav = "SPSS", por = "SPSS", dta = "Stata", sas7bdat = "SAS",
  rds = "R", rdata = "R", rda = "R", csv = "CSV", tsv = "TSV", txt = "Texto",
  xlsx = "Excel", xls = "Excel"
)

file_ext <- function(path) tolower(tools::file_ext(path))

#' Lee un archivo de datos en cualquier formato admitido.
#' @param path ruta del archivo
#' @param ext extensión (por defecto, la del archivo)
#' @export
read_any <- function(path, ext = file_ext(path)) {
  ext <- tolower(ext)
  if (!ext %in% names(formatos_admitidos)) {
    stop("Formato no admitido: .", ext, ". Formatos válidos: ",
         paste(names(formatos_admitidos), collapse = ", "))
  }
  d <- switch(ext,
    sav = , zsav = read_sav_safe(path),
    por = haven::read_por(path, user_na = FALSE),
    dta = read_dta_safe(path),
    sas7bdat = haven::read_sas(path),
    rds = readRDS(path),
    rdata = , rda = read_rdata(path),
    csv = , txt = read_text(path, sep = "auto"),
    tsv = read_text(path, sep = "\t"),
    xlsx = , xls = as.data.frame(readxl::read_excel(path, guess_max = 10000))
  )
  if (!is.data.frame(d)) stop("El archivo no contiene una tabla de datos.")
  standardize_data(d)
}

# Texto UTF-8 leído como Latin-1 ("niÃ±o" en lugar de "niño").
has_mojibake <- function(d) {
  txt <- c(names(d), unlist(lapply(d, function(x) {
    c(attr(x, "label", exact = TRUE), names(attr(x, "labels", exact = TRUE)),
      if (is.character(x)) utils::head(unique(x), 200))
  })))
  txt <- txt[!is.na(txt)]
  any(grepl("\u00C3[\u0080-\u00BF]|\u00C2[\u00A0-\u00BF]", txt, perl = TRUE))
}

read_dta_safe <- function(path) {
  d <- haven::read_dta(path)
  if (has_mojibake(d)) d <- haven::read_dta(path, encoding = "UTF-8")
  d
}

read_sav_safe <- function(path) {
  d <- haven::read_sav(path, user_na = FALSE)
  if (has_mojibake(d)) d <- haven::read_sav(path, user_na = FALSE, encoding = "UTF-8")
  d
}

# Números al estilo español en texto: "1.234,50", "980,00", "-3,5".
parse_es_number <- function(x) {
  v <- trimws(x[!is.na(x) & nzchar(trimws(x))])
  if (!length(v)) return(NULL)
  ok <- grepl("^-?(\\d{1,3}(\\.\\d{3})+|\\d+)(,\\d+)?$", v)
  if (!all(ok) || !any(grepl(",|\\.", v))) return(NULL)
  n <- suppressWarnings(as.numeric(gsub(",", ".", gsub(".", "", trimws(x), fixed = TRUE), fixed = TRUE)))
  n[!is.na(x) & !nzchar(trimws(x))] <- NA
  n
}

read_rdata <- function(path) {
  env <- new.env()
  nms <- load(path, envir = env)
  dfs <- Filter(function(n) is.data.frame(env[[n]]), nms)
  if (!length(dfs)) stop("El archivo .RData no contiene ningún data.frame.")
  env[[dfs[1]]]
}

read_text <- function(path, sep = "auto") {
  raw <- readBin(path, "raw", n = min(file.size(path), 1e6))
  enc <- if (validUTF8(rawToChar(raw[raw != as.raw(0)]))) "UTF-8" else "Latin-1"
  d <- data.table::fread(path, sep = sep, encoding = enc, data.table = FALSE,
                         na.strings = c("", "NA", "N/A"), showProgress = FALSE)
  if (enc != "UTF-8") {
    names(d) <- enc2utf8(names(d))
    for (i in seq_along(d)) if (is.character(d[[i]])) d[[i]] <- enc2utf8(d[[i]])
  }
  d
}

clean_names <- function(x) {
  x <- trimws(enc2utf8(as.character(x)))
  x[!nzchar(x)] <- "var"
  x <- gsub("\\s+", "_", x)
  x <- gsub("[\"'`:]", "", x)
  make.unique(x, sep = "_")
}

standardize_data <- function(d) {
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  names(d) <- clean_names(names(d))
  n <- nrow(d)
  for (v in names(d)) {
    x <- d[[v]]
    lab <- attr(x, "label", exact = TRUE)
    codes <- NULL
    if (inherits(x, "haven_labelled")) {
      labs <- attr(x, "labels", exact = TRUE)
      if (length(labs)) {
        # etiquetas de valor vacías o en blanco: se usa el código («1») como etiqueta
        vacia <- is.na(names(labs)) | !nzchar(trimws(names(labs)))
        if (any(vacia)) {
          names(labs)[vacia] <- as.character(unname(labs[vacia]))
          attr(x, "labels") <- labs
        }
      }
      vals <- unique(stats::na.omit(as.vector(haven::zap_labels(x))))
      if (length(labs) && length(vals) && all(vals %in% labs)) {
        x <- haven::as_factor(x, levels = "labels")
        x <- droplevels_keep(x, labs)
        codes <- stats::setNames(as.character(unname(labs)), names(labs))   # etiqueta -> código original
      } else {
        codes <- if (length(labs)) stats::setNames(as.character(unname(labs)), names(labs))
        x <- as.vector(haven::zap_labels(x))
      }
    }
    if (is.logical(x)) x <- factor(x, levels = c(FALSE, TRUE), labels = c("No", "Sí"))
    if (is.character(x)) {
      x[!is.na(x) & !nzchar(trimws(x))] <- NA
      u <- length(unique(stats::na.omit(x)))
      num <- suppressWarnings(as.numeric(x))
      es <- if (u > 0 && !all(is.na(num) == is.na(x))) parse_es_number(x)
      if (u > 0 && all(is.na(num) == is.na(x))) {
        x <- num
      } else if (!is.null(es)) {
        x <- es
      } else if (u > 0 && u <= 50 && (u < sum(!is.na(x)) || u <= 2)) {
        x <- factor(x)
      }
    }
    if (inherits(x, "POSIXlt")) x <- as.POSIXct(x)
    attributes_clean <- !(is.factor(x) || inherits(x, c("Date", "POSIXct")))
    if (attributes_clean) attributes(x) <- NULL
    if (!is.null(lab) && nzchar(lab)) attr(x, "label") <- as.character(lab)
    if (length(codes)) attr(x, "codes") <- codes
    d[[v]] <- x
  }
  rownames(d) <- NULL
  d
}

# Mantiene todas las categorías etiquetadas (aunque no aparezcan) en su orden.
droplevels_keep <- function(x, labs) {
  lv <- names(labs)
  lv <- unique(c(lv[lv %in% levels(x)], levels(x)))
  factor(as.character(x), levels = lv)
}

# Metadatos ----------------------------------------------------------------------

var_type <- function(x) {
  if (is.ordered(x)) "ordinal"
  else if (is.factor(x)) "categórica"
  else if (inherits(x, c("Date", "POSIXct"))) "fecha"
  else if (is.numeric(x)) "numérica"
  else "texto"
}

var_label <- function(d, v) {
  l <- attr(d[[v]], "label", exact = TRUE)
  if (is.null(l) || !nzchar(l)) humanize_name(v) else l
}

# Propuesta de etiqueta a partir del nombre: "confianza_ciencia" → "Confianza ciencia",
# "nivelEstudios" → "Nivel estudios". Los nombres tipo código (P12, v3_1) se dejan igual.
humanize_name <- function(v) {
  if (grepl("^[[:alpha:]]{0,3}[0-9]+([_.][0-9]+)*[[:alpha:]]?$", v)) return(v)
  x <- gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", v)
  x <- gsub("[_.]+", " ", x)
  x <- trimws(gsub("\\s+", " ", x))
  if (!nzchar(x)) return(v)
  words <- strsplit(x, " ")[[1]]
  words <- ifelse(words == toupper(words) & nchar(words) > 1, words, tolower(words))   # respeta siglas (CIS, PIB)
  x <- paste(words, collapse = " ")
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

# Variables sin etiqueta propia (para avisar y proponer una).
unlabelled_vars <- function(d) {
  names(d)[vapply(names(d), function(v) {
    l <- attr(d[[v]], "label", exact = TRUE)
    is.null(l) || !nzchar(l)
  }, logical(1))]
}

# Formato español: coma decimal y punto de miles solo a partir de 10.000 (RAE).
fmt_num <- function(x, digits = 2) {
  out <- formatC(x, format = "f", digits = digits, big.mark = ".", decimal.mark = ",")
  small <- !is.na(x) & abs(x) < 10000
  out[small] <- formatC(x[small], format = "f", digits = digits, big.mark = "", decimal.mark = ",")
  out[is.na(x)] <- "—"
  out
}

fmt_int <- function(x) fmt_num(round(as.numeric(x)), 0)

fmt_pct <- function(x, digits = 1) paste0(fmt_num(100 * x, digits), " %")

var_info <- function(d) {
  if (!ncol(d)) {
    return(data.frame(nombre = character(), etiqueta = character(), tipo = character(),
                      valores = character(), perdidos = numeric()))
  }
  do.call(rbind, lapply(names(d), function(v) {
    x <- d[[v]]
    tipo <- var_type(x)
    valores <- switch(tipo,
      "categórica" = , "ordinal" = {
        lv <- levels(x)
        if (length(lv) <= 3) paste(lv, collapse = ", ") else paste(length(lv), "categorías")
      },
      "numérica" = if (all(is.na(x))) "—" else {
        r <- range(x, na.rm = TRUE)
        dg <- if (all(x == round(x), na.rm = TRUE)) 0 else if (diff(r) >= 100) 0 else 2
        paste(fmt_num(r[1], dg), "–", fmt_num(r[2], dg))
      },
      "fecha" = if (all(is.na(x))) "—" else paste(min(x, na.rm = TRUE), "–", max(x, na.rm = TRUE)),
      paste(length(unique(stats::na.omit(x))), "valores distintos")
    )
    lab <- attr(x, "label", exact = TRUE)
    data.frame(nombre = v, etiqueta = if (is.null(lab)) "" else lab, tipo = tipo,
               valores = valores, perdidos = mean(is.na(x)), stringsAsFactors = FALSE)
  }))
}

categorical_vars <- function(d) names(d)[vapply(d, is.factor, logical(1))]
numeric_vars <- function(d) names(d)[vapply(d, is.numeric, logical(1))]
