# Historial de operaciones sobre los datos ("receta") ------------------------------
# Cada operación es una lista list(op = "...", ...). Los datos originales nunca se
# modifican: los datos de trabajo = apply_recipe(original, receta). Así se puede
# deshacer, repetir sobre datos nuevos y exportar un script de R.

#' Aplica una receta (lista de pasos) a un data.frame.
#' @export
apply_recipe <- function(d, recipe, dir = NULL) {
  for (s in recipe) {
    sets <- attr(d, "mrsets", exact = TRUE)
    d <- apply_step(d, s, dir)
    # los conjuntos de respuesta múltiple viajan con los datos en todos los pasos
    if (is.null(attr(d, "mrsets", exact = TRUE)) && !is.null(sets) && !s$op %in% c("mrset", "mrset_drop", "drop")) {
      attr(d, "mrsets") <- sets
    }
  }
  d
}

need_var <- function(d, v) {
  if (is.null(v) || !v %in% names(d)) stop("La variable '", v, "' no existe.")
}

valid_new_name <- function(d, to, allow = NULL) {
  if (is.null(to) || !nzchar(to)) stop("Falta el nombre de la variable.")
  if (!grepl("^[[:alpha:]][[:alnum:]_.]*$", to)) {
    stop("El nombre '", to, "' no es válido: usa letras, números, _ o . y empieza por una letra.")
  }
  if (to %in% setdiff(names(d), allow)) stop("Ya existe una variable llamada '", to, "'.")
}

keep_label <- function(new, old, label = NULL) {
  l <- label %||% attr(old, "label", exact = TRUE)
  if (!is.null(l)) attr(new, "label") <- l
  new
}

# dir: carpeta del proyecto (solo la necesitan los pasos que unen otro archivo).
apply_step <- function(d, s, dir = NULL) {
  switch(s$op,
    dedupe = dedupe_rows(d, s$by),
    reverse = {
      for (v in s$vars) {
        need_var(d, v)
        to <- if (isTRUE(s$replace)) v else paste0(v, s$suffix %||% "_inv")
        if (!isTRUE(s$replace)) valid_new_name(d, to)
        lim <- s$limits[[v]]
        d[[to]] <- keep_label(reverse_var(d[[v]], lim$min, lim$max), d[[v]],
                              if (!isTRUE(s$replace)) paste(var_label(d, v), "(invertida)"))
      }
      d
    },
    recode_many = {
      for (v in s$vars) {
        d <- apply_step(d, list(op = "recode", var = v, to = paste0(v, s$suffix %||% "_r"), groups = s$groups,
                                others = s$others %||% "na", label = var_label(d, v)))
      }
      d
    },
    index = {
      valid_new_name(d, s$to)
      if (length(s$vars) < 2) stop("Un índice necesita al menos dos variables.")
      x <- build_index(d, s$vars, s$method %||% "mean", s$min_valid)
      attr(x, "label") <- if (nzchar(s$label %||% "")) s$label else paste("Índice:", paste(s$vars, collapse = ", "))
      d[[s$to]] <- x
      d
    },
    append = {
      b <- read_annex(dir, s$file)
      bind_cases(d, b, origin = s$origin, origin_labels = c(s$labels %||% c("Original", "Añadido")))
    },
    join = {
      b <- read_annex(dir, s$file)
      join_vars(d, b, s$by, s$by_other %||% s$by, s$vars)
    },
    rename = {
      need_var(d, s$var); valid_new_name(d, s$to, allow = s$var)
      names(d)[names(d) == s$var] <- s$to
      mr_rename_var(d, s$var, s$to)
    },
    mrset = mr_add(d, s),
    mrset_drop = {
      sets <- mrsets(d)
      if (is.null(sets[[s$name]])) stop("No existe el conjunto «", s$name, "».")
      sets[[s$name]] <- NULL
      attr(d, "mrsets") <- if (length(sets)) sets
      d
    },
    label = {
      need_var(d, s$var)
      attr(d[[s$var]], "label") <- s$label
      d
    },
    labels = {
      for (v in names(s$labels)) {
        need_var(d, v)
        attr(d[[v]], "label") <- as.character(s$labels[[v]])
      }
      d
    },
    catlabels = {
      need_var(d, s$var)
      x <- d[[s$var]]
      if (!is.factor(x)) stop("Solo se pueden cambiar las etiquetas de categorías en variables categóricas.")
      map <- unlist(s$map)
      if (!length(map) || !all(names(map) %in% levels(x))) stop("Alguna categoría no existe en ", s$var, ".")
      map <- trimws(map)
      if (any(!nzchar(map))) stop("Las etiquetas de categoría no pueden quedar vacías.")
      lab <- attr(x, "label", exact = TRUE); codes <- attr(x, "codes", exact = TRUE)
      lv <- levels(x)
      lv[match(names(map), lv)] <- map
      levels(x) <- lv                      # dos categorías con la misma etiqueta se unen
      if (length(codes)) {
        hit <- names(codes) %in% names(map)
        names(codes)[hit] <- map[names(codes)[hit]]
        attr(x, "codes") <- codes
      }
      if (!is.null(lab)) attr(x, "label") <- lab
      d[[s$var]] <- x
      d
    },
    recode = {
      need_var(d, s$var); valid_new_name(d, s$to, allow = if (isTRUE(s$replace)) s$var)
      x <- as.character(d[[s$var]])
      out <- rep(NA_character_, length(x))
      for (g in s$groups) out[x %in% g$from] <- g$name
      if (identical(s$others, "keep")) out[is.na(out) & !is.na(x)] <- x[is.na(out) & !is.na(x)]
      orig_lv <- if (is.factor(d[[s$var]])) levels(d[[s$var]]) else sort(unique(stats::na.omit(x)))
      lv <- unique(c(vapply(s$groups, `[[`, "", "name"),
                     if (identical(s$others, "keep")) setdiff(orig_lv, unlist(lapply(s$groups, `[[`, "from")))))
      new <- factor(out, levels = lv, ordered = is.ordered(d[[s$var]]))
      d[[s$to]] <- keep_label(new, d[[s$var]], s$label)
      d
    },
    cut = {
      need_var(d, s$var); valid_new_name(d, s$to)
      x <- d[[s$var]]
      if (!is.numeric(x)) stop("Solo se pueden agrupar en tramos variables numéricas.")
      br <- sort(unique(c(-Inf, as.numeric(s$breaks), Inf)))
      labs <- s$labels
      if (is.null(labs) || length(labs) != length(br) - 1) labs <- tramo_labels(br)
      new <- cut(x, breaks = br, labels = labs, right = FALSE, ordered_result = TRUE)
      d[[s$to]] <- keep_label(new, x, s$label)
      d
    },
    missing = {
      need_var(d, s$var)
      x <- d[[s$var]]
      if (is.factor(x)) {
        lab <- attr(x, "label", exact = TRUE)
        x[as.character(x) %in% s$values] <- NA
        x <- droplevels(x, exclude = s$values)
        if (!is.null(lab)) attr(x, "label") <- lab
      } else {
        x[x %in% as.numeric(s$values)] <- NA
      }
      d[[s$var]] <- x
      d
    },
    type = {
      need_var(d, s$var)
      d[[s$var]] <- keep_label(convert_type(d[[s$var]], s$type), d[[s$var]])
      d
    },
    levels = {
      need_var(d, s$var)
      x <- d[[s$var]]
      if (!is.factor(x)) stop("Solo se puede ordenar categorías en variables categóricas.")
      if (!setequal(s$levels, levels(x))) stop("La lista de categorías no coincide.")
      d[[s$var]] <- keep_label(factor(as.character(x), levels = s$levels, ordered = is.ordered(x)), x)
      d
    },
    compute = {
      valid_new_name(d, s$to)
      val <- eval_safe(s$expr, d)
      if (length(val) == 1) val <- rep(val, nrow(d))
      if (length(val) != nrow(d)) stop("La fórmula no devuelve un valor por caso.")
      if (is.logical(val)) val <- factor(val, levels = c(FALSE, TRUE), labels = c("No", "Sí"))
      if (is.character(val)) val <- factor(val)
      attr(val, "label") <- NULL          # no hereda la etiqueta de la variable de origen
      attr(val, "codes") <- NULL
      if (!is.null(s$label) && nzchar(s$label)) attr(val, "label") <- s$label
      d[[s$to]] <- val
      d
    },
    filter = {
      keep <- eval_safe(s$expr, d)
      if (!is.logical(keep) || length(keep) != nrow(d)) stop("El filtro debe ser una condición (verdadero/falso) por caso.")
      keep[is.na(keep)] <- FALSE
      subset_rows(d, keep)
    },
    drop = {
      for (v in s$vars) need_var(d, v)
      sets <- attr(d, "mrsets", exact = TRUE)
      out <- d[setdiff(names(d), s$vars)]
      attr(out, "mrsets") <- sets
      mr_drop_vars(out, s$vars)
    },
    stop("Operación desconocida: ", s$op)
  )
}

# Selecciona filas conservando las etiquetas de las variables.
subset_rows <- function(d, rows) {
  labs <- lapply(d, attr, "label", exact = TRUE)
  codes <- lapply(d, attr, "codes", exact = TRUE)
  sets <- attr(d, "mrsets", exact = TRUE)
  out <- d[rows, , drop = FALSE]
  attr(out, "mrsets") <- sets
  for (v in names(out)) {
    if (!is.null(labs[[v]])) attr(out[[v]], "label") <- labs[[v]]
    if (!is.null(codes[[v]])) attr(out[[v]], "codes") <- codes[[v]]
  }
  rownames(out) <- NULL
  out
}

tramo_labels <- function(br) {
  f <- function(x) fmt_num(x, if (all(x == round(x), na.rm = TRUE)) 0 else 2)
  vapply(seq_len(length(br) - 1), function(i) {
    a <- br[i]; b <- br[i + 1]
    if (is.infinite(a)) paste("Menos de", f(b))
    else if (is.infinite(b)) paste(f(a), "o más")
    else paste0(f(a), " a <", f(b))
  }, character(1))
}

convert_type <- function(x, type) {
  switch(type,
    "categórica" = factor(if (is.factor(x)) as.character(x) else x,
                          levels = if (is.factor(x)) levels(x) else sort(unique(stats::na.omit(x)))),
    "ordinal" = factor(if (is.factor(x)) as.character(x) else x,
                       levels = if (is.factor(x)) levels(x) else sort(unique(stats::na.omit(x))), ordered = TRUE),
    "numérica" = {
      if (is.factor(x)) {
        n <- suppressWarnings(as.numeric(as.character(x)))
        if (all(is.na(n) == is.na(x))) n else as.numeric(x)
      } else suppressWarnings(as.numeric(x))
    },
    "texto" = as.character(x),
    stop("Tipo desconocido: ", type)
  )
}

# Evaluación segura de fórmulas ----------------------------------------------------
# Solo se permiten estas funciones y los nombres de las variables.

allowed_functions <- c(
  "+", "-", "*", "/", "^", "%%", "%/%", "==", "!=", "<", ">", "<=", ">=", "&", "|", "!",
  "(", "%in%", "ifelse", "is.na", "log", "log10", "log2", "exp", "sqrt", "abs", "round",
  "floor", "ceiling", "mean", "median", "sd", "min", "max", "sum", "pmin", "pmax", "c",
  "as.numeric", "as.character", "scale", "rowMeans", "rowSums", "cbind", "nchar",
  "paste", "paste0", "toupper", "tolower", "substr", "grepl", "length"
)

check_expr <- function(e, vars) {
  if (is.call(e)) {
    fn <- e[[1]]
    if (!is.name(fn) || !as.character(fn) %in% allowed_functions) {
      stop("Función no permitida en la fórmula: ", deparse(fn)[1])
    }
    for (a in as.list(e)[-1]) if (!missing(a)) check_expr(a, vars)
  } else if (is.name(e)) {
    nm <- as.character(e)
    if (!nm %in% c(vars, "TRUE", "FALSE", "NA", "T", "F")) stop("No existe la variable '", nm, "'.")
  } else if (!is.atomic(e)) {
    stop("Expresión no permitida.")
  }
  invisible(TRUE)
}

eval_safe <- function(expr, d) {
  e <- tryCatch(str2lang(expr), error = function(err) stop("La fórmula no se entiende: revisa paréntesis y comillas."))
  check_expr(e, names(d))
  env <- list2env(lapply(d, function(x) if (is.factor(x)) as.character(x) else x),
                  parent = baseenv())
  args_named <- function(f) function(..., na.rm = TRUE) f(..., na.rm = na.rm)
  env$mean <- args_named(base::mean); env$median <- args_named(stats::median)
  env$sd <- args_named(stats::sd); env$sum <- args_named(base::sum)
  env$min <- args_named(base::min); env$max <- args_named(base::max)
  eval(e, env)
}

# Descripción y código ---------------------------------------------------------------

step_label <- function(s) {
  switch(s$op,
    rename = paste0("Renombrar ", s$var, " → ", s$to),
    label = paste0("Etiquetar ", s$var, ": «", s$label, "»"),
    dedupe = if (length(s$by)) paste0("Quitar casos con ", paste(s$by, collapse = ", "), " repetido") else "Quitar casos repetidos",
    reverse = paste0("Invertir ", paste(s$vars, collapse = ", "), if (isTRUE(s$replace)) "" else paste0(" → ", paste0(s$vars, s$suffix %||% "_inv", collapse = ", "))),
    recode_many = paste0("Recodificar ", length(s$vars), " variables (", length(s$groups), " grupos) → ", paste0(s$vars, s$suffix %||% "_r", collapse = ", ")),
    index = paste0("Crear índice ", s$to, " (", if (identical(s$method, "sum")) "suma" else "media", " de ", length(s$vars), " variables)"),
    append = paste0("Añadir casos de ", s$source %||% "otro archivo"),
    join = paste0("Añadir ", length(s$vars), if (length(s$vars) == 1) " variable de " else " variables de ",
                  s$source %||% "otro archivo", " (clave ", s$by, ")"),
    labels = paste0("Poner etiqueta a ", length(s$labels), if (length(s$labels) == 1) " variable" else " variables"),
    catlabels = paste0("Cambiar etiquetas de categorías de ", s$var, " (", length(s$map), ")"),
    recode = paste0("Recodificar ", s$var, " → ", s$to, " (", length(s$groups), " grupos)"),
    cut = paste0("Agrupar ", s$var, " en tramos → ", s$to),
    missing = paste0("Marcar como perdido en ", s$var, ": ", paste(s$values, collapse = ", ")),
    type = paste0("Cambiar ", s$var, " a ", s$type),
    levels = paste0("Reordenar categorías de ", s$var),
    compute = paste0("Crear ", s$to, " = ", s$expr),
    filter = paste0("Filtrar casos: ", s$expr),
    drop = paste0("Eliminar ", paste(s$vars, collapse = ", ")),
    mrset = paste0("Conjunto de respuesta múltiple «", s$name, "» (",
                   switch(s$type, dicotomias = "dicotomías", categorias = "categorías", "texto"), ", ",
                   length(unlist(s$vars)), if (length(unlist(s$vars)) == 1) " variable)" else " variables)"),
    mrset_drop = paste0("Quitar el conjunto «", s$name, "»"),
    s$op
  )
}

q <- function(x) paste0('"', gsub('"', '\\\\"', x), '"')
qv <- function(x) paste0("c(", paste(vapply(x, q, ""), collapse = ", "), ")")
col <- function(v) paste0("datos[[", q(v), "]]")

step_code <- function(s) {
  switch(s$op,
    rename = paste0("names(datos)[names(datos) == ", q(s$var), "] <- ", q(s$to)),
    label = paste0("attr(", col(s$var), ", \"label\") <- ", q(s$label)),
    dedupe = if (length(s$by)) paste0("datos <- datos[!duplicated(datos[", qv(s$by), "]), ]")
             else "datos <- datos[!duplicated(datos), ]",
    reverse = vapply(s$vars, function(v) {
      to <- if (isTRUE(s$replace)) v else paste0(v, s$suffix %||% "_inv")
      lim <- s$limits[[v]]
      if (!is.null(lim)) paste0(col(to), " <- (", lim$min, " + ", lim$max, ") - ", col(v))
      else paste0(col(to), " <- factor(", col(v), ", levels = rev(levels(", col(v), ")))")
    }, ""),
    recode_many = unlist(lapply(s$vars, function(v) step_code(list(op = "recode", var = v, to = paste0(v, s$suffix %||% "_r"),
                                                                    groups = s$groups, others = s$others)))),
    index = c(paste0("items <- sapply(datos[", qv(s$vars), "], as.numeric)   # categóricas: 1, 2, 3…"),
              paste0("validos <- rowSums(!is.na(items))"),
              paste0(col(s$to), " <- ifelse(validos >= ", s$min_valid %||% ceiling(length(s$vars) / 2), ", rowMeans(items, na.rm = TRUE)",
                     if (identical(s$method, "sum")) paste0(" * ", length(s$vars)) else "", ", NA)")),
    append = c(paste0("otro <- caring2::read_any(", q(s$source %||% s$file), ")"),
               "datos <- as.data.frame(data.table::rbindlist(list(datos, otro), fill = TRUE))"),
    join = c(paste0("otro <- caring2::read_any(", q(s$source %||% s$file), ")"),
             paste0("datos <- merge(datos, otro[c(", q(s$by_other %||% s$by), ", ", paste(vapply(s$vars, q, ""), collapse = ", "), ")], by.x = ",
                    q(s$by), ", by.y = ", q(s$by_other %||% s$by), ", all.x = TRUE, sort = FALSE)")),
    labels = vapply(names(s$labels), function(v) paste0("attr(", col(v), ", \"label\") <- ", q(s$labels[[v]])), ""),
    catlabels = {
      m <- unlist(s$map)
      paste0("levels(", col(s$var), ")[match(", qv(names(m)), ", levels(", col(s$var), "))] <- ", qv(unname(m)))
    },
    recode = {
      lines <- c(paste0("x <- as.character(", col(s$var), ")"),
                 "y <- rep(NA_character_, length(x))")
      for (g in s$groups) lines <- c(lines, paste0("y[x %in% ", qv(g$from), "] <- ", q(g$name)))
      if (identical(s$others, "keep")) lines <- c(lines, "y[is.na(y)] <- x[is.na(y)]")
      c(lines, paste0(col(s$to), " <- factor(y, levels = unique(c(",
                      qv(vapply(s$groups, `[[`, "", "name")), ", y[!is.na(y)])))"))
    },
    cut = paste0(col(s$to), " <- cut(", col(s$var), ", breaks = c(-Inf, ",
                 paste(s$breaks, collapse = ", "), ", Inf), right = FALSE, ordered_result = TRUE)"),
    missing = if (is.character(s$values)) {
      paste0(col(s$var), " <- droplevels(replace(", col(s$var), ", ", col(s$var), " %in% ", qv(s$values), ", NA))")
    } else {
      paste0(col(s$var), "[", col(s$var), " %in% c(", paste(s$values, collapse = ", "), ")] <- NA")
    },
    type = switch(s$type,
      "categórica" = paste0(col(s$var), " <- factor(", col(s$var), ")"),
      "ordinal" = paste0(col(s$var), " <- factor(", col(s$var), ", ordered = TRUE)"),
      "numérica" = paste0(col(s$var), " <- as.numeric(", col(s$var), ")"),
      "texto" = paste0(col(s$var), " <- as.character(", col(s$var), ")")),
    levels = paste0(col(s$var), " <- factor(", col(s$var), ", levels = ", qv(s$levels), ")"),
    compute = paste0("datos[[", q(s$to), "]] <- with(datos, ", s$expr, ")"),
    filter = paste0("datos <- subset(datos, ", s$expr, ")"),
    drop = paste0("datos <- datos[setdiff(names(datos), ", qv(s$vars), ")]"),
    mrset = {
      v <- unlist(s$vars)
      switch(s$type,
        dicotomias = c(paste0("# Cada variable es una opción; cuenta como marcada si vale ", q(s$value)),
                       paste0("conjuntos[[", q(s$name), "]] <- sapply(datos[", qv(v), "], function(x) as.integer(as.character(x) == ", q(s$value), "))")),
        categorias = c("# Cada variable es una mención; las opciones son sus categorías",
                       paste0("menciones <- lapply(datos[", qv(v), "], as.character)"),
                       "opciones <- unique(unlist(lapply(menciones, function(x) unique(stats::na.omit(x)))))",
                       paste0("conjuntos[[", q(s$name), "]] <- sapply(opciones, function(o) as.integer(Reduce(`|`, lapply(menciones, function(x) !is.na(x) & x == o))))")),
        c(paste0("# Opciones separadas por ", q(s$sep %||% ";"), " en una variable de texto"),
          paste0("partes <- strsplit(as.character(", col(v[1]), "), ", q(s$sep %||% ";"), ", fixed = TRUE)"),
          "opciones <- sort(unique(trimws(unlist(partes))))",
          paste0("conjuntos[[", q(s$name), "]] <- t(sapply(partes, function(p) as.integer(opciones %in% trimws(p))))")))
    },
    mrset_drop = paste0("conjuntos[[", q(s$name), "]] <- NULL"),
    paste("#", s$op)
  )
}

#' Genera un script de R que reproduce la receta.
#' @export
recipe_to_code <- function(recipe, archivo = "datos.sav") {
  out <- c(
    "# Script generado por CARING 2",
    paste0("# ", format(Sys.time(), "%d/%m/%Y %H:%M")),
    "# Necesita el paquete caring2 para leer los datos con las mismas reglas.",
    "",
    paste0("datos <- caring2::read_any(", q(archivo), ")"),
    if (any(vapply(recipe, function(s) s$op == "mrset", logical(1)))) "conjuntos <- list()   # conjuntos de respuesta múltiple (tablas 0/1)",
    ""
  )
  for (i in seq_along(recipe)) {
    out <- c(out, paste0("# ", i, ". ", step_label(recipe[[i]])), step_code(recipe[[i]]), "")
  }
  paste(out, collapse = "\n")
}
