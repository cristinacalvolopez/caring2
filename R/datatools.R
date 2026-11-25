# Herramientas de datos (fase 2) ------------------------------------------------------------
# Detección de problemas, invertir escalas, recodificar en lote, índices con alfa de
# Cronbach, unir archivos (añadir casos o variables) y libro de códigos. Las operaciones
# que cambian los datos son pasos de la receta (ver apply_step en recipe.R).

# Detectar problemas -------------------------------------------------------------------------

# Variables que parecen identificadores: un valor distinto por caso y nombre de id.
id_like_vars <- function(d) {
  if (!nrow(d)) return(character())
  names(d)[vapply(names(d), function(v) {
    x <- d[[v]]
    (is.numeric(x) || is.character(x)) && !anyNA(x) &&
      grepl("^(id|ident|identificador|registro|caso|cuestionario|n_?cuest|folio|key|clave)([_.]?[[:alnum:]]*)?$", v, ignore.case = TRUE)
  }, logical(1))]
}

missing_code_values <- c(-9, -8, -7, -1, 8, 9, 97, 98, 99, 997, 998, 999, 9997, 9998, 9999, 99999)

# Códigos de no-respuesta que se han quedado como números: valores típicos (99, 999, -9…)
# claramente separados del resto de valores.
suspicious_codes <- function(x) {
  if (!is.numeric(x)) return(numeric())
  u <- sort(unique(stats::na.omit(x)))
  cand <- u[u %in% missing_code_values]
  if (!length(cand) || length(u) < 3) return(numeric())
  resto <- setdiff(u, cand)
  if (!length(resto)) return(numeric())
  lo <- min(resto); hi <- max(resto)
  rango <- max(hi - lo, 1)
  cand[(cand > hi + 0.5 * rango & cand > 1.5 * hi) | (cand < 0 & lo >= 0)]
}

# Lista de problemas encontrados. Cada uno: list(tipo, var, titulo, detalle, accion, n).
#   accion: "dedupe" | "dedupe_id" | "drop" | "missing" | "type" | "edit"
data_problems <- function(d) {
  out <- list()
  add <- function(...) out[[length(out) + 1]] <<- list(...)
  n <- nrow(d)
  if (!n) return(out)
  ids <- id_like_vars(d)
  # Casos duplicados (todas las variables menos los identificadores)
  otras <- setdiff(names(d), ids)
  if (length(otras)) {
    dup <- sum(duplicated(as.data.frame(lapply(d[otras], as.character))))
    if (dup > 0) add(tipo = "duplicados", var = NULL, n = dup,
                     titulo = paste0(fmt_int(dup), if (dup == 1) " caso repetido" else " casos repetidos"),
                     detalle = paste0("Casos con las mismas respuestas en todas las variables",
                                      if (length(ids)) paste0(" (sin contar ", paste(ids, collapse = ", "), ")"), "."),
                     accion = "dedupe")
  }
  for (v in ids) {
    dup <- sum(duplicated(d[[v]]))
    if (dup > 0) add(tipo = "id_repetido", var = v, n = dup,
                     titulo = paste0("Identificador repetido en ", v),
                     detalle = paste0(fmt_int(dup), " casos repiten un valor de ", v, " que ya aparece antes."),
                     accion = "dedupe_id")
  }
  for (v in names(d)) {
    x <- d[[v]]
    if (is.list(x)) next
    perd <- mean(is.na(x))
    nval <- length(unique(stats::na.omit(x)))
    if (perd == 1) {
      add(tipo = "vacia", var = v, n = n, titulo = paste0(v, " está vacía"),
          detalle = "No tiene ningún valor válido.", accion = "drop")
      next
    }
    if (perd >= 0.9) add(tipo = "casi_vacia", var = v, n = sum(is.na(x)), titulo = paste0(v, " casi vacía"),
                         detalle = paste0("Le falta el ", fmt_pct(perd, 0), " de los valores."), accion = "drop")
    if (nval == 1 && !v %in% ids) add(tipo = "constante", var = v, n = n, titulo = paste0(v, " no varía"),
                                      detalle = "Todos los casos tienen el mismo valor: no sirve para comparar.", accion = "drop")
    if (is.numeric(x)) {
      cod <- suspicious_codes(x)
      if (length(cod)) add(tipo = "codigos", var = v, n = sum(x %in% cod), values = cod,
                           titulo = paste0("Posibles códigos de no-respuesta en ", v),
                           detalle = paste0("Los valores ", paste(cod, collapse = ", "), " (", fmt_int(sum(x %in% cod)),
                                            " casos) están muy separados del resto: ¿son «No sabe / No contesta»?"),
                           accion = "missing")
      if (!length(cod) && nval > 10) {
        q <- stats::quantile(x, c(0.25, 0.75), na.rm = TRUE)
        iqr <- q[2] - q[1]
        if (iqr > 0) {
          fuera <- sum(x < q[1] - 3 * iqr | x > q[2] + 3 * iqr, na.rm = TRUE)
          if (fuera > 0) add(tipo = "extremos", var = v, n = fuera,
                             titulo = paste0("Valores extremos en ", v),
                             detalle = paste0(fmt_int(fuera), if (fuera == 1) " caso tiene" else " casos tienen",
                                              " valores muy alejados del resto (más de 3 rangos intercuartílicos). Revisa si son errores."),
                             accion = "edit")
        }
      }
    }
    if (is.factor(x) && nlevels(x) > 2) {
      tb <- table(x)
      raras <- names(tb)[tb > 0 & tb < max(5, 0.01 * n)]
      if (length(raras)) add(tipo = "raras", var = v, n = length(raras),
                             titulo = paste0("Categorías con muy pocos casos en ", v),
                             detalle = paste0(paste(utils::head(raras, 4), collapse = ", "), if (length(raras) > 4) ", …",
                                              ": conviene agruparlas o tratarlas como perdidos."),
                             accion = "edit")
    }
    if (is.character(x) && nval <= 20 && n >= 30) {
      add(tipo = "texto_cat", var = v, n = nval, titulo = paste0(v, " parece categórica"),
          detalle = paste0("Es texto pero solo tiene ", nval, " valores distintos: como categórica se puede analizar."),
          accion = "type")
    }
  }
  out
}

# Quitar duplicados --------------------------------------------------------------------------

dedupe_rows <- function(d, by = NULL) {
  if (length(by)) {
    for (v in by) need_var(d, v)
    key <- d[by]
  } else {
    key <- d[setdiff(names(d), id_like_vars(d))]
  }
  dup <- duplicated(as.data.frame(lapply(key, as.character)))
  subset_rows(d, !dup)
}

# Invertir escalas ----------------------------------------------------------------------------

reverse_var <- function(x, min = NULL, max = NULL) {
  if (is.factor(x)) {
    out <- factor(as.character(x), levels = rev(levels(x)), ordered = is.ordered(x))
    codes <- attr(x, "codes", exact = TRUE)
    if (length(codes)) attr(out, "codes") <- codes
    return(out)
  }
  if (!is.numeric(x)) stop("Solo se pueden invertir variables numéricas o categóricas.")
  min <- min %||% min(x, na.rm = TRUE); max <- max %||% max(x, na.rm = TRUE)
  (min + max) - x
}

# Índices y alfa de Cronbach -------------------------------------------------------------------

# Matriz numérica de los ítems (las categóricas se puntúan por su posición: 1, 2, 3…).
items_matrix <- function(d, vars) {
  for (v in vars) need_var(d, v)
  m <- vapply(vars, function(v) {
    x <- d[[v]]
    if (is.factor(x)) as.numeric(as.integer(x))
    else if (is.numeric(x)) as.numeric(x)
    else stop("La variable ", v, " es de texto: no puede formar parte de un índice.")
  }, numeric(nrow(d)))
  matrix(m, nrow = nrow(d), dimnames = list(NULL, vars))
}

cronbach_alpha <- function(m) {
  m <- m[stats::complete.cases(m), , drop = FALSE]
  k <- ncol(m)
  if (is.null(colnames(m))) colnames(m) <- paste0("item", seq_len(k))
  if (k < 2 || nrow(m) < 3) return(list(alpha = NA_real_, n = nrow(m), items = NULL))
  a <- function(mm) {
    kk <- ncol(mm)
    vt <- stats::var(rowSums(mm))
    if (kk < 2 || !is.finite(vt) || vt == 0) return(NA_real_)
    kk / (kk - 1) * (1 - sum(apply(mm, 2, stats::var)) / vt)
  }
  total <- rowSums(m)
  items <- data.frame(
    item = colnames(m),
    r_item_total = vapply(seq_len(k), function(j) suppressWarnings(stats::cor(m[, j], total - m[, j])), numeric(1)),
    alpha_sin = vapply(seq_len(k), function(j) a(m[, -j, drop = FALSE]), numeric(1)),
    stringsAsFactors = FALSE)
  list(alpha = a(m), n = nrow(m), items = items)
}

alpha_text <- function(a) {
  if (is.na(a)) return("no se puede calcular")
  paste0(fmt_num(a, 2), " (", if (a >= 0.9) "excelente" else if (a >= 0.8) "buena" else if (a >= 0.7) "aceptable"
         else if (a >= 0.6) "dudosa" else "baja", ")")
}

build_index <- function(d, vars, method = "mean", min_valid = NULL) {
  m <- items_matrix(d, vars)
  k <- ncol(m)
  min_valid <- min_valid %||% ceiling(k / 2)
  validos <- rowSums(!is.na(m))
  media <- rowMeans(m, na.rm = TRUE)
  out <- if (method == "sum") media * k else media          # suma prorrateada si falta algún ítem
  out[validos < min_valid] <- NA
  out
}

# Unir archivos --------------------------------------------------------------------------------
# El archivo que se une se guarda en <proyecto>/anexos/ y el paso de la receta guarda su
# nombre; así la receta se puede volver a aplicar siempre sobre los datos originales.

annex_dir <- function(dir) file.path(dir, "anexos")

annex_file <- function(dir, file) {
  if (is.null(dir)) stop("Este paso necesita la carpeta del proyecto.")
  if (!grepl("^[a-f0-9]{8,40}\\.[a-z0-9]{1,6}$", file %||% "")) stop("Archivo anexo no válido.")
  f <- file.path(annex_dir(dir), file)
  if (!file.exists(f)) stop("No se encuentra el archivo que se unió (", file, ").")
  f
}

.annex_cache <- new.env(parent = emptyenv())

read_annex <- function(dir, file) {
  f <- annex_file(dir, file)
  key <- paste(normalizePath(f), file.mtime(f))
  if (!is.null(.annex_cache[[key]])) return(.annex_cache[[key]])
  d <- read_any(f)
  .annex_cache[[key]] <- d
  d
}

# Guarda un archivo subido como anexo del proyecto (comprueba tamaño y cuota).
save_annex <- function(con, user, dir, file_path, file_name) {
  ext <- file_ext(file_name)
  if (!ext %in% names(formatos_admitidos)) return(list(ok = FALSE, error = paste0("Formato no admitido (.", ext, ").")))
  max_mb <- as.numeric(get_setting(con, "max_mb"))
  if (isTRUE(file.size(file_path) > max_mb * 1024^2)) {
    return(list(ok = FALSE, error = paste0("El archivo pesa ", fmt_bytes(file.size(file_path)),
                                           " y el máximo permitido es ", fmt_num(max_mb, 0), " MB.")))
  }
  if (!is.null(user) && user_bytes(con, user$id) + file.size(file_path) > user_quota_mb(con, user) * 1024^2) {
    return(list(ok = FALSE, error = "No queda espacio en tu cuenta. Borra algún proyecto o pide más cuota."))
  }
  d <- tryCatch(read_any(file_path, ext), error = function(e) e)
  if (inherits(d, "error")) return(list(ok = FALSE, error = paste("No se ha podido leer el archivo:", error_es(d))))
  dir.create(annex_dir(dir), showWarnings = FALSE)
  name <- paste0(substr(gsub("-", "", new_id()), 1, 16), ".", ext)
  file.copy(file_path, file.path(annex_dir(dir), name))
  list(ok = TRUE, file = name, data = d)
}

# Une dos bases caso a caso (las variables se emparejan por nombre).
bind_cases <- function(a, b, origin = NULL, origin_labels = c("Original", "Añadido")) {
  vars <- union(names(a), names(b))
  out <- lapply(vars, function(v) {
    x <- a[[v]]; y <- b[[v]]
    if (is.null(x)) x <- rep(NA, nrow(a))
    if (is.null(y)) y <- rep(NA, nrow(b))
    lab <- attr(a[[v]], "label", exact = TRUE) %||% attr(b[[v]], "label", exact = TRUE)
    codes <- attr(a[[v]], "codes", exact = TRUE) %||% attr(b[[v]], "codes", exact = TRUE)
    r <- if ((is.numeric(x) || all(is.na(x))) && (is.numeric(y) || all(is.na(y))) && !is.factor(x) && !is.factor(y)) {
      c(as.numeric(x), as.numeric(y))
    } else if (inherits(x, "Date") && inherits(y, "Date")) {
      c(x, y)
    } else if (is.factor(x) || is.factor(y)) {
      lv <- unique(c(if (is.factor(x)) levels(x) else sort(unique(stats::na.omit(as.character(x)))),
                     if (is.factor(y)) levels(y) else sort(unique(stats::na.omit(as.character(y))))))
      factor(c(as.character(x), as.character(y)), levels = lv, ordered = is.ordered(x) && is.ordered(y))
    } else {
      c(as.character(x), as.character(y))
    }
    if (!is.null(lab)) attr(r, "label") <- lab
    if (length(codes) && is.factor(r)) attr(r, "codes") <- codes
    r
  })
  names(out) <- vars
  out <- as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
  if (!is.null(origin) && nzchar(origin)) {
    f <- factor(rep(origin_labels, c(nrow(a), nrow(b))), levels = origin_labels)
    attr(f, "label") <- "Archivo de origen"
    out[[origin]] <- f
  }
  out
}

# Añade a `a` variables de `b` emparejando casos por clave (se conservan todos los casos de a).
join_vars <- function(a, b, by, by_other = by, vars = NULL) {
  need_var(a, by)
  if (!by_other %in% names(b)) stop("La variable clave «", by_other, "» no está en el otro archivo.")
  kb <- as.character(b[[by_other]])
  if (anyDuplicated(kb[!is.na(kb)])) stop("La clave «", by_other, "» se repite en el otro archivo: cada caso debe aparecer una sola vez.")
  vars <- setdiff(vars %||% names(b), by_other)
  if (!length(vars)) stop("Elige al menos una variable para añadir.")
  idx <- match(as.character(a[[by]]), kb)
  if (all(is.na(idx))) stop("Ningún caso coincide: revisa que las claves sean las mismas en los dos archivos.")
  for (v in vars) {
    nv <- v
    while (nv %in% names(a)) nv <- paste0(nv, "_2")
    x <- b[[v]]
    y <- x[idx]
    attributes(y) <- attributes(x)[setdiff(names(attributes(x)), c("names", "dim", "dimnames"))]
    a[[nv]] <- y
  }
  a
}

# Libro de códigos ---------------------------------------------------------------------------

codebook_df <- function(d) {
  do.call(rbind, lapply(names(d), function(v) {
    x <- d[[v]]
    tipo <- var_type(x)
    valores <- if (is.factor(x)) {
      tb <- table(x)
      codes <- attr(x, "codes", exact = TRUE)
      paste0(vapply(names(tb), function(l) {
        cod <- if (length(codes) && l %in% names(codes)) paste0(codes[[l]], " = ") else ""
        paste0(cod, l, " (", fmt_int(tb[[l]]), ")")
      }, ""), collapse = "\n")
    } else if (is.numeric(x)) {
      if (all(is.na(x))) "—" else paste0("Mín. ", fmt_num(min(x, na.rm = TRUE)), " · Máx. ", fmt_num(max(x, na.rm = TRUE)),
                                         " · Media ", fmt_num(mean(x, na.rm = TRUE)))
    } else if (inherits(x, c("Date", "POSIXct"))) {
      if (all(is.na(x))) "—" else paste(min(x, na.rm = TRUE), "–", max(x, na.rm = TRUE))
    } else {
      paste(fmt_int(length(unique(stats::na.omit(x)))), "valores distintos")
    }
    data.frame(Variable = v, Etiqueta = attr(x, "label", exact = TRUE) %||% "", Tipo = tipo,
               Valores = valores, Válidos = fmt_int(sum(!is.na(x))), Perdidos = fmt_int(sum(is.na(x))),
               check.names = FALSE, stringsAsFactors = FALSE)
  }))
}

codebook_mr <- function(d) {
  sets <- mrsets(d)
  if (!length(sets)) return(NULL)
  do.call(rbind, lapply(sets, function(s) data.frame(
    Conjunto = s$name, Etiqueta = s$label %||% "",
    Tipo = switch(s$type, dicotomias = "Dicotomías", categorias = "Categorías", "Texto"),
    Variables = paste(s$vars, collapse = ", "), check.names = FALSE, stringsAsFactors = FALSE)))
}

write_codebook <- function(d, file, format = "docx", title = "Libro de códigos", recipe = list()) {
  cb <- codebook_df(d)
  mr <- codebook_mr(d)
  if (format == "xlsx") {
    wb <- openxlsx::createWorkbook()
    hs <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#236CDB", fontColour = "#FFFFFF")
    ws <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
    openxlsx::addWorksheet(wb, "Variables")
    openxlsx::writeData(wb, "Variables", cb, headerStyle = hs)
    openxlsx::addStyle(wb, "Variables", ws, rows = 2:(nrow(cb) + 1), cols = 1:ncol(cb), gridExpand = TRUE)
    openxlsx::setColWidths(wb, "Variables", 1:6, c(18, 40, 12, 50, 10, 10))
    openxlsx::freezePane(wb, "Variables", firstRow = TRUE)
    if (!is.null(mr)) {
      openxlsx::addWorksheet(wb, "Respuesta múltiple")
      openxlsx::writeData(wb, "Respuesta múltiple", mr, headerStyle = hs)
    }
    if (length(recipe)) {
      openxlsx::addWorksheet(wb, "Cambios")
      openxlsx::writeData(wb, "Cambios", data.frame(Paso = seq_along(recipe), Cambio = vapply(recipe, step_label, "")),
                          headerStyle = hs)
      openxlsx::setColWidths(wb, "Cambios", 2, 90)
    }
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    return(invisible(file))
  }
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, title, style = "heading 1")
  doc <- officer::body_add_par(doc, paste0(fmt_int(nrow(d)), " casos · ", ncol(d), " variables · ",
                                           format(Sys.Date(), "%d/%m/%Y")), style = "Normal")
  ft <- flextable_caring(cb)
  ft <- quiet_ft(flextable::fit_to_width(ft, 6.5))
  doc <- quiet_ft(flextable::body_add_flextable(doc, ft))
  if (!is.null(mr)) {
    doc <- officer::body_add_par(doc, "Conjuntos de respuesta múltiple", style = "heading 2")
    doc <- quiet_ft(flextable::body_add_flextable(doc, quiet_ft(flextable::fit_to_width(flextable_caring(mr), 6.5))))
  }
  if (length(recipe)) {
    doc <- officer::body_add_par(doc, "Cambios sobre los datos originales", style = "heading 2")
    for (i in seq_along(recipe)) doc <- officer::body_add_par(doc, paste0(i, ". ", step_label(recipe[[i]])), style = "Normal")
  }
  quiet_ft(print(doc, target = file))
  invisible(file)
}
