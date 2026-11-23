# Paso 1 · Datos: herramientas (fase 2) ---------------------------------------------------------
# Revisar problemas, recodificar varias variables, invertir escalas, crear índices,
# unir otro archivo y libro de códigos. Se llama desde mod_datos_server().

datos_tools_menu <- function(ns) {
  div(class = "dropdown",
    tags$button(class = "btn btn-outline-primary dropdown-toggle", type = "button", `data-bs-toggle` = "dropdown",
                "Herramientas"),
    tags$ul(class = "dropdown-menu dropdown-menu-end",
      tags$li(actionLink(ns("t_recode_many"), "Recodificar varias variables", class = "dropdown-item")),
      tags$li(actionLink(ns("t_reverse"), "Invertir escalas", class = "dropdown-item")),
      tags$li(actionLink(ns("t_index"), "Crear un índice (alfa de Cronbach)", class = "dropdown-item")),
      tags$li(actionLink(ns("t_merge"), "Unir otro archivo (casos o variables)", class = "dropdown-item")),
      tags$li(tags$hr(class = "dropdown-divider")),
      tags$li(tags$h6(class = "dropdown-header", "Libro de códigos")),
      tags$li(downloadLink(ns("dl_codebook_docx"), "Word (.docx)", class = "dropdown-item")),
      tags$li(downloadLink(ns("dl_codebook_xlsx"), "Excel (.xlsx)", class = "dropdown-item"))))
}

datos_review_ui <- function(ns) {
  div(class = "c-card c-card-pad",
    div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2 mb-2",
      tags$h2("Revisar los datos"),
      span(class = "c-hint", "Posibles problemas detectados automáticamente. Ninguno se corrige sin tu permiso.")),
    uiOutput(ns("problems")))
}

datos_tools_server <- function(input, output, session, ctx, sel) {
  ns <- session$ns
  data <- ctx$data
  proj <- ctx$proj
  ro_msg <- function() showNotification("Este proyecto es de solo lectura.", type = "error")
  vars_of <- function(d, kind) {
    keep <- switch(kind,
      factor = vapply(d, is.factor, logical(1)),
      scale = vapply(d, function(x) is.factor(x) || is.numeric(x), logical(1)),
      vapply(d, function(x) TRUE, logical(1)))
    v <- names(d)[keep]
    stats::setNames(v, vapply(v, function(x) {
      l <- var_label(d, x); if (identical(l, x)) x else paste0(l, " (", x, ")")
    }, ""))
  }

  # Revisar ------------------------------------------------------------------------------------
  problems <- reactive(data_problems(data()))
  output$problems <- renderUI({
    pr <- problems()
    if (!length(pr)) return(div(class = "c-empty", style = "padding:30px 10px",
      tags$h2("Todo en orden"), p("No se han encontrado casos repetidos, variables vacías ni valores sospechosos.")))
    boton <- function(i, a) {
      lab <- switch(a, dedupe = "Quitar repetidos", dedupe_id = "Quedarse con el primero", drop = "Eliminar variable",
                    missing = "Marcar como perdidos", type = "Convertir en categórica", edit = "Revisar variable")
      if (!ctx$can_edit() && a != "edit") return(NULL)
      tags$button(class = if (a == "edit") "btn btn-light-c btn-sm" else "btn btn-outline-primary btn-sm",
                  onclick = sprintf("caringAction('%s', '%d', '%s', event)", ns("prob_action"), i, a), lab)
    }
    icono <- function(t) switch(t, duplicados = , id_repetido = "⧉", vacia = , casi_vacia = "∅", constante = "=",
                                codigos = "?", extremos = "↕", raras = "·", texto_cat = "Aa", "!")
    tagList(
      p(class = "small-2", length(pr), if (length(pr) == 1) " aviso" else " avisos"),
      div(class = "c-problems", lapply(seq_along(pr), function(i) {
        x <- pr[[i]]
        div(class = "c-problem",
          span(class = "c-problem-ico", `aria-hidden` = "true", icono(x$tipo)),
          div(class = "c-problem-txt", tags$strong(x$titulo), div(class = "small-2", x$detalle)),
          div(class = "c-problem-act", boton(i, x$accion)))
      })))
  })

  observeEvent(input$prob_action, {
    a <- input$prob_action
    pr <- problems()
    i <- suppressWarnings(as.integer(a$id))
    if (is.na(i) || i < 1 || i > length(pr)) return()
    x <- pr[[i]]
    if (a$action == "edit") {
      sel(x$var)
      updateRadioButtons(session, "view", selected = "variables")
      return()
    }
    if (!ctx$can_edit()) return(ro_msg())
    switch(a$action,
      dedupe = ctx$add_step(list(op = "dedupe")),
      dedupe_id = ctx$add_step(list(op = "dedupe", by = x$var)),
      drop = ctx$add_step(list(op = "drop", vars = x$var)),
      missing = ctx$add_step(list(op = "missing", var = x$var, values = x$values)),
      type = ctx$add_step(list(op = "type", var = x$var, type = "categórica")))
  })

  # Recodificar varias -------------------------------------------------------------------------
  observeEvent(input$t_recode_many, {
    if (!ctx$can_edit()) return(ro_msg())
    showModal(modalDialog(title = "Recodificar varias variables a la vez", size = "l", easyClose = TRUE,
      p(class = "c-hint", "Aplica la misma agrupación de categorías a varias variables (por ejemplo, una batería de preguntas ",
        "con las mismas respuestas). Se crea una variable nueva por cada una; las originales no se tocan."),
      selectizeInput(ns("rm_vars"), "Variables categóricas", vars_of(data(), "factor"), multiple = TRUE, width = "100%",
                     options = list(placeholder = "Elige dos o más variables")),
      uiOutput(ns("rm_body")),
      footer = tagList(modalButton("Cancelar"), actionButton(ns("rm_ok"), "Crear variables", class = "btn-primary"))))
  })
  output$rm_body <- renderUI({
    d <- data(); vs <- intersect(input$rm_vars, names(d))
    req(length(vs))
    lv <- unique(unlist(lapply(vs, function(v) levels(d[[v]]))))
    iguales <- all(vapply(vs, function(v) identical(levels(d[[v]]), levels(d[[vs[1]]])), logical(1)))
    k <- max(2, min(10, input$rm_ngroups %||% 2))
    tagList(
      if (!iguales) div(class = "c-msg-warn mb-2", "Las variables no tienen exactamente las mismas categorías: se muestran todas juntas."),
      numericInput(ns("rm_ngroups"), "Número de grupos", value = k, min = 2, max = 10, width = "160px"),
      div(class = "c-bucket",
        do.call(sortable::bucket_list, c(
          list(header = NULL, group_name = ns("rm_bucket"), orientation = "vertical",
               sortable::add_rank_list("Sin agrupar", labels = lv, input_id = ns("rm_grp_0"))),
          lapply(seq_len(k), function(i) sortable::add_rank_list(paste("Grupo", i), labels = NULL, input_id = ns(paste0("rm_grp_", i))))))),
      div(class = "row g-2",
        lapply(seq_len(k), function(i) div(class = "col-6 col-md-4",
          textInput(ns(paste0("rm_gname_", i)), paste("Nombre del grupo", i), value = isolate(input[[paste0("rm_gname_", i)]]) %||% paste("Grupo", i))))),
      div(class = "row g-2",
        div(class = "col-md-7", radioButtons(ns("rm_others"), "Las categorías sin agrupar…",
                                             c("pasan a ser perdidos" = "na", "se mantienen como están" = "keep"), inline = TRUE)),
        div(class = "col-md-5", textInput(ns("rm_suffix"), "Terminación del nombre nuevo", value = "_r"))))
  })
  observeEvent(input$rm_ok, {
    vs <- input$rm_vars
    if (length(vs) < 1) return(showNotification("Elige al menos una variable.", type = "error"))
    k <- max(2, min(10, input$rm_ngroups %||% 2))
    groups <- list()
    for (i in seq_len(k)) {
      items <- unlist(input[[paste0("rm_grp_", i)]])
      if (length(items)) groups <- c(groups, list(list(name = trimws(input[[paste0("rm_gname_", i)]] %||% paste("Grupo", i)), from = items)))
    }
    if (!length(groups)) return(showNotification("Arrastra al menos una categoría a un grupo.", type = "error"))
    if (anyDuplicated(vapply(groups, `[[`, "", "name"))) return(showNotification("Los grupos deben tener nombres distintos.", type = "error"))
    suf <- trimws(input$rm_suffix %||% "_r"); if (!nzchar(suf)) suf <- "_r"
    if (ctx$add_step(list(op = "recode_many", vars = vs, groups = groups, others = input$rm_others %||% "na", suffix = suf))) {
      removeModal()
      showNotification(paste("Creadas", length(vs), "variables nuevas."), type = "message")
    }
  })

  # Invertir escalas ---------------------------------------------------------------------------
  observeEvent(input$t_reverse, {
    if (!ctx$can_edit()) return(ro_msg())
    showModal(modalDialog(title = "Invertir escalas", size = "l", easyClose = TRUE,
      p(class = "c-hint", "Da la vuelta a una escala: en una de 1 a 5, el 1 pasa a ser 5 y el 5 pasa a ser 1. ",
        "Sirve para que todos los ítems de un índice vayan en el mismo sentido."),
      selectizeInput(ns("rev_vars"), "Variables (numéricas u ordinales)", vars_of(data(), "scale"), multiple = TRUE, width = "100%"),
      uiOutput(ns("rev_body")),
      checkboxInput(ns("rev_replace"), "Sustituir las variables originales (si no, se crean nuevas terminadas en _inv)", FALSE),
      footer = tagList(modalButton("Cancelar"), actionButton(ns("rev_ok"), "Invertir", class = "btn-primary"))))
  })
  output$rev_body <- renderUI({
    d <- data(); vs <- intersect(input$rev_vars, names(d))
    req(length(vs))
    num <- vs[vapply(vs, function(v) is.numeric(d[[v]]), logical(1))]
    filas <- lapply(vs, function(v) {
      x <- d[[v]]
      desc <- if (is.factor(x)) paste(levels(x)[1], "…", utils::tail(levels(x), 1), "→", utils::tail(levels(x), 1), "…", levels(x)[1])
              else paste0("valores de ", fmt_num(min(x, na.rm = TRUE), 0), " a ", fmt_num(max(x, na.rm = TRUE), 0))
      tags$li(tags$code(v), " · ", desc)
    })
    tagList(tags$ul(class = "small-2", filas),
      if (length(num)) div(class = "row g-2",
        div(class = "col-12 c-hint", "Escala teórica de las numéricas (déjalo vacío para usar el mínimo y el máximo observados de cada una):"),
        div(class = "col-6 col-md-3", numericInput(ns("rev_min"), "Mínimo", value = NA)),
        div(class = "col-6 col-md-3", numericInput(ns("rev_max"), "Máximo", value = NA))))
  })
  observeEvent(input$rev_ok, {
    d <- data(); vs <- intersect(input$rev_vars, names(d))
    if (!length(vs)) return(showNotification("Elige al menos una variable.", type = "error"))
    lim <- list()
    for (v in vs) if (is.numeric(d[[v]])) {
      mn <- if (is.na(input$rev_min %||% NA)) min(d[[v]], na.rm = TRUE) else input$rev_min
      mx <- if (is.na(input$rev_max %||% NA)) max(d[[v]], na.rm = TRUE) else input$rev_max
      lim[[v]] <- list(min = mn, max = mx)
    }
    if (ctx$add_step(list(op = "reverse", vars = vs, replace = isTRUE(input$rev_replace), suffix = "_inv", limits = lim))) removeModal()
  })

  # Índices ------------------------------------------------------------------------------------
  observeEvent(input$t_index, {
    if (!ctx$can_edit()) return(ro_msg())
    showModal(modalDialog(title = "Crear un índice", size = "l", easyClose = TRUE,
      p(class = "c-hint", "Resume varias preguntas sobre lo mismo en una sola puntuación (media o suma). ",
        "El alfa de Cronbach indica si los ítems miden de forma coherente lo mismo (a partir de 0,70 se considera aceptable)."),
      selectizeInput(ns("ix_vars"), "Ítems del índice", vars_of(data(), "scale"), multiple = TRUE, width = "100%",
                     options = list(placeholder = "Elige dos o más variables")),
      uiOutput(ns("ix_alpha")),
      div(class = "row g-2",
        div(class = "col-md-4", textInput(ns("ix_name"), "Nombre", placeholder = "indice_confianza")),
        div(class = "col-md-8", textInput(ns("ix_label"), "Etiqueta (opcional)", placeholder = "Índice de confianza institucional"))),
      div(class = "row g-2",
        div(class = "col-md-6", radioButtons(ns("ix_method"), "Cálculo", c("Media de los ítems" = "mean", "Suma de los ítems" = "sum"), inline = TRUE)),
        div(class = "col-md-6", numericInput(ns("ix_min"), "Ítems respondidos como mínimo", value = NA, min = 1))),
      footer = tagList(modalButton("Cancelar"), actionButton(ns("ix_ok"), "Crear índice", class = "btn-primary"))))
  })
  output$ix_alpha <- renderUI({
    d <- data(); vs <- intersect(input$ix_vars, names(d))
    if (length(vs) < 2) return(p(class = "c-hint", "Elige al menos dos ítems para ver el alfa de Cronbach."))
    a <- tryCatch(cronbach_alpha(items_matrix(d, vs)), error = function(e) e)
    if (inherits(a, "error")) return(div(class = "c-msg-error", error_es(a)))
    neg <- a$items$item[!is.na(a$items$r_item_total) & a$items$r_item_total < 0]
    tab <- data.frame(Ítem = vapply(a$items$item, function(v) var_label(d, v), ""),
                      `Correlación con el resto` = fmt_num(a$items$r_item_total, 2),
                      `Alfa si se quita` = fmt_num(a$items$alpha_sin, 2), check.names = FALSE)
    tagList(
      div(class = "c-stats mb-2", tags$strong("Alfa de Cronbach: ", alpha_text(a$alpha)), " · ", fmt_int(a$n), " casos con todos los ítems."),
      if (length(neg)) div(class = "c-msg-warn mb-2", paste0("Estos ítems van en sentido contrario al resto: ", paste(neg, collapse = ", "),
                           ". Inviértelos antes (Herramientas → Invertir escalas).")),
      div(style = "overflow-x:auto", html_table(tab)))
  })
  observeEvent(input$ix_ok, {
    vs <- input$ix_vars
    nm <- trimws(input$ix_name %||% "")
    mv <- input$ix_min
    step <- list(op = "index", to = nm, vars = vs, method = input$ix_method %||% "mean",
                 min_valid = if (!is.null(mv) && !is.na(mv)) as.integer(mv), label = trimws(input$ix_label %||% ""))
    if (ctx$add_step(step)) { removeModal(); sel(nm) }
  })

  # Unir otro archivo --------------------------------------------------------------------------
  merge_state <- reactiveValues(file = NULL, data = NULL, source = NULL)
  drop_pending <- function() {
    if (!is.null(merge_state$file)) {
      usado <- any(vapply(proj$recipe, function(s) identical(s$file, merge_state$file), logical(1)))
      if (!usado) unlink(file.path(annex_dir(proj$dir), merge_state$file))
    }
    merge_state$file <- NULL; merge_state$data <- NULL; merge_state$source <- NULL
  }
  observeEvent(input$t_merge, {
    if (!ctx$can_edit()) return(ro_msg())
    drop_pending()
    max_mb <- as.numeric(get_setting(ctx$con, "max_mb"))
    showModal(modalDialog(title = "Unir otro archivo", size = "l", easyClose = FALSE,
      p(class = "c-hint", "Añade a este proyecto los ", tags$strong("casos"), " de otro archivo con las mismas preguntas (por ejemplo, otra oleada), ",
        "o nuevas ", tags$strong("variables"), " de los mismos casos emparejándolos por un identificador."),
      div(`data-max-mb` = max_mb,
        fileInput(ns("mg_file"), "Archivo", accept = paste0(".", names(formatos_admitidos)), width = "100%",
                  buttonLabel = "Elegir…", placeholder = "SPSS, Stata, R, CSV, Excel…")),
      uiOutput(ns("mg_body")),
      footer = tagList(actionButton(ns("mg_cancel"), "Cancelar", class = "btn-light-c"),
                       actionButton(ns("mg_ok"), "Unir", class = "btn-primary"))))
  })
  observeEvent(input$mg_cancel, { drop_pending(); removeModal() })
  observeEvent(input$mg_file, {
    f <- input$mg_file
    req(f)
    drop_pending()
    r <- save_annex(ctx$con, ctx$state$user, proj$dir, f$datapath, f$name)
    if (!isTRUE(r$ok)) return(showNotification(r$error, type = "error", duration = 8))
    merge_state$file <- r$file; merge_state$data <- r$data; merge_state$source <- f$name
  })
  output$mg_body <- renderUI({
    b <- merge_state$data
    req(b)
    d <- data()
    comunes <- intersect(names(d), names(b))
    claves <- comunes[vapply(comunes, function(v) !anyDuplicated(stats::na.omit(as.character(b[[v]]))), logical(1))]
    tagList(
      div(class = "c-stats mb-2", tags$strong(merge_state$source), ": ", fmt_int(nrow(b)), " casos · ", ncol(b), " variables · ",
          length(comunes), " con el mismo nombre que en este proyecto."),
      radioButtons(ns("mg_mode"), "¿Qué quieres añadir?",
        c("Casos: el otro archivo tiene las mismas preguntas y otros casos" = "append",
          "Variables: el otro archivo tiene los mismos casos y otras preguntas" = "join"),
        selected = isolate(input$mg_mode) %||% (if (length(comunes) >= 0.7 * ncol(b)) "append" else "join")),
      conditionalPanel("input.mg_mode == 'append'", ns = ns,
        div(class = "c-hint mb-2",
          if (length(setdiff(names(b), names(d)))) paste0("Variables solo en el archivo nuevo (quedarán vacías en los casos actuales): ",
                                                         paste(utils::head(setdiff(names(b), names(d)), 8), collapse = ", "),
                                                         if (length(setdiff(names(b), names(d))) > 8) ", … " else ". "),
          if (length(setdiff(names(d), names(b)))) paste0("Variables que faltan en el archivo nuevo (quedarán vacías en los casos añadidos): ",
                                                         paste(utils::head(setdiff(names(d), names(b)), 8), collapse = ", "),
                                                         if (length(setdiff(names(d), names(b))) > 8) ", …" else ".")),
        textInput(ns("mg_origin"), "Variable que indica el archivo de origen (opcional)", value = "origen")),
      conditionalPanel("input.mg_mode == 'join'", ns = ns,
        div(class = "row g-2",
          div(class = "col-md-6", selectInput(ns("mg_by"), "Identificador en este proyecto", names(d),
                                              selected = intersect(c(claves, id_like_vars(d)), names(d))[1])),
          div(class = "col-md-6", selectInput(ns("mg_by_other"), "Identificador en el otro archivo", names(b),
                                              selected = intersect(c(claves, id_like_vars(b)), names(b))[1]))),
        selectizeInput(ns("mg_vars"), "Variables que se añaden", names(b), selected = setdiff(names(b), comunes),
                       multiple = TRUE, width = "100%"),
        uiOutput(ns("mg_match"))))
  })
  output$mg_match <- renderUI({
    b <- merge_state$data; d <- data()
    req(b, input$mg_by %in% names(d), input$mg_by_other %in% names(b))
    hit <- sum(as.character(d[[input$mg_by]]) %in% as.character(b[[input$mg_by_other]]))
    cls <- if (hit == 0) "c-msg-error" else if (hit < nrow(d)) "c-msg-warn" else "c-stats"
    div(class = cls, paste0(fmt_int(hit), " de ", fmt_int(nrow(d)), " casos encuentran pareja en el otro archivo",
        if (hit < nrow(d) && hit > 0) " (los demás quedarán vacíos en las variables nuevas)", "."))
  })
  observeEvent(input$mg_ok, {
    if (is.null(merge_state$file)) return(showNotification("Elige primero el archivo.", type = "error"))
    step <- if (identical(input$mg_mode, "join")) {
      list(op = "join", file = merge_state$file, source = merge_state$source, by = input$mg_by,
           by_other = input$mg_by_other, vars = input$mg_vars)
    } else {
      o <- trimws(input$mg_origin %||% "")
      list(op = "append", file = merge_state$file, source = merge_state$source,
           origin = if (nzchar(o)) o, labels = c(proj$info$archivo %||% "Original", merge_state$source))
    }
    if (!is.null(step$origin)) {
      ok <- tryCatch({ valid_new_name(data(), step$origin); TRUE }, error = function(e) { notify_error(e); FALSE })
      if (!ok) return()
    }
    if (ctx$add_step(step)) {
      merge_state$file <- NULL; merge_state$data <- NULL
      removeModal()
      showNotification("Archivo unido. El cambio aparece en el historial y se puede quitar.", type = "message")
    }
  })

  # Libro de códigos ---------------------------------------------------------------------------
  for (fmt in c("docx", "xlsx")) local({
    f <- fmt
    output[[paste0("dl_codebook_", f)]] <- downloadHandler(
      filename = function() paste0(safe_filename(proj$info$nombre), "_libro_de_codigos.", f),
      content = function(file) write_codebook(data(), file, f, title = paste("Libro de códigos ·", proj$info$nombre),
                                              recipe = proj$recipe))
    outputOptions(output, paste0("dl_codebook_", f), suspendWhenHidden = FALSE)
  })
}
