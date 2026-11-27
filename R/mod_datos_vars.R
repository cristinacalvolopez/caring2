# Datos · Variables: lista, selección de varias y organización ------------------------------
# Tabla de variables con casillas, filtros por grupo y por visibilidad, barra de acciones
# sobre la selección, tarjeta de grupos, renombrar con expresión regular y ordenar.
# Todo lo que cambia algo es un paso del historial (ver R/vargroups.R).

datos_vars_ui <- function(ns) {
  tagList(
    uiOutput(ns("vfilters")),
    uiOutput(ns("bulk")))
}

# Nombre para mostrar: «Etiqueta (nombre)»
var_display <- function(d, vars) {
  stats::setNames(vars, vapply(vars, function(v) { l <- var_label(d, v); if (identical(l, v)) v else paste0(l, " (", v, ")") }, ""))
}

js_str <- function(x) as.character(jsonlite::toJSON(as.character(x), auto_unbox = TRUE))

datos_vars_server <- function(input, output, session, ctx, sel) {
  ns <- session$ns
  data <- ctx$data
  proj <- ctx$proj
  gfilter <- reactiveVal(character())     # grupos con los que se filtra la lista
  preselect <- reactiveVal(NULL)          # variables que se marcan al volver a dibujar la tabla
  ro <- function() {
    if (ctx$can_edit()) return(FALSE)
    showNotification("Este proyecto es de solo lectura.", type = "error")
    TRUE
  }
  set_groups <- function(g) ctx$upsert_step(list(op = "vgroups",
    groups = unname(lapply(g, function(x) list(name = x$name, vars = as.character(x$vars))))))
  set_hidden <- function(h) ctx$upsert_step(list(op = "hidden", vars = as.character(intersect(names(data()), h))))

  # Lista ----------------------------------------------------------------------------------
  vinfo <- reactive({
    d <- data()
    vi <- var_info(d)
    g <- vgroups(d)
    vi$grupos <- vapply(vi$nombre, function(v) paste(names(g)[vapply(g, function(x) v %in% x$vars, logical(1))], collapse = ", "), "")
    vi$oculta <- vi$nombre %in% hidden_vars(d)
    vi
  })

  vtable <- reactive({
    vi <- vinfo()
    if (any(vi$oculta)) {
      vis <- input$vis %||% "todas"
      if (vis == "visibles") vi <- vi[!vi$oculta, , drop = FALSE]
      if (vis == "ocultas") vi <- vi[vi$oculta, , drop = FALSE]
    }
    g <- vgroups(data())
    gf <- intersect(gfilter(), names(g))
    if (length(gf)) {
      dentro <- lapply(gf, function(k) vi$nombre %in% g[[k]]$vars)
      keep <- if (identical(input$gmode, "todos")) Reduce(`&`, dentro) else Reduce(`|`, dentro)
      vi <- vi[keep, , drop = FALSE]
    }
    vi
  })

  output$vars <- reactable::renderReactable({
    vi <- vtable()
    ps <- isolate(preselect())
    if (!is.null(ps)) isolate(preselect(NULL))
    vi$perdidos_txt <- fmt_pct(vi$perdidos)
    hid <- vi$oculta
    reactable::reactable(
      vi[c("nombre", "etiqueta", "tipo", "valores", "perdidos_txt", "grupos")],
      selection = "multiple", searchable = TRUE, highlight = TRUE,
      # la casilla marca la variable; el resto de la fila la abre en el editor
      onClick = reactable::JS(sprintf(
        "function(rowInfo, column) { if (column.id === '.selection') return;
           Shiny.setInputValue('%s', rowInfo.values.nombre, {priority: 'event'}); }", ns("pick"))),
      defaultSelected = if (length(ps)) which(vi$nombre %in% ps),
      pagination = nrow(vi) > 40, defaultPageSize = 40, compact = TRUE,
      language = reactable_es(),
      theme = reactable::reactableTheme(rowSelectedStyle = list(backgroundColor = "#EAF1FB")),
      rowStyle = function(i) if (isTRUE(hid[i])) list(cursor = "pointer", color = "#6B6B6B") else list(cursor = "pointer"),
      columns = list(
        .selection = reactable::colDef(width = 38, headerStyle = list(background = "#F5F5F5")),
        nombre = reactable::colDef(name = "Nombre", minWidth = 110,
                                   style = list(fontFamily = "ui-monospace, Menlo, monospace", fontSize = "13px"),
                                   cell = function(v, i) if (isTRUE(hid[i])) tagList(v, " ", span(class = "c-chip warn", "oculta")) else v),
        etiqueta = reactable::colDef(name = "Etiqueta", minWidth = 150),
        tipo = reactable::colDef(name = "Tipo", minWidth = 95, cell = function(v) type_chip(v)),
        valores = reactable::colDef(name = "Valores", minWidth = 120),
        perdidos_txt = reactable::colDef(name = "Perdidos", minWidth = 80, align = "right",
          style = function(v, i) if (vi$perdidos[i] > 0.05) list(color = "#B3261E") else NULL),
        grupos = reactable::colDef(name = "Grupos", minWidth = 110, show = any(nzchar(vi$grupos)),
                                   style = list(fontSize = "12.5px", color = "#1F5FC4"))
      )
    )
  })

  observeEvent(input$pick, sel(input$pick))

  selected <- reactive({
    idx <- reactable::getReactableState("vars", "selected")
    vt <- vtable()
    if (!length(idx)) return(character())
    intersect(vt$nombre[idx[idx <= nrow(vt)]], names(data()))
  })
  clear_sel <- function() reactable::updateReactable("vars", selected = NA)
  select_vars <- function(vs) {
    vt <- vtable()
    if (all(vs %in% vt$nombre)) reactable::updateReactable("vars", selected = which(vt$nombre %in% vs))
    else preselect(vs)
  }

  # Filtros -------------------------------------------------------------------------------------
  output$vfilters <- renderUI({
    d <- data()
    g <- vgroups(d)
    nh <- length(hidden_vars(d))
    gf <- intersect(gfilter(), names(g))
    if (!length(g) && !nh) return(NULL)
    chip <- function(k) {
      on <- k %in% gf
      tags$button(type = "button", class = paste("c-gchip", if (on) "on"), `aria-pressed` = if (on) "true" else "false",
                  onclick = sprintf("caringAction('%s', %s, 'toggle', event)", ns("gchip"), js_str(k)),
                  k, span(class = "n", length(g[[k]]$vars)))
    }
    div(class = "c-vfilters",
      if (nh) seg_radio(ns("vis"), label = "Mostrar", selected = isolate(input$vis) %||% "todas",
                        c(stats::setNames("todas", paste0("Todas (", ncol(d), ")")),
                          stats::setNames("visibles", paste0("Visibles (", ncol(d) - nh, ")")),
                          stats::setNames("ocultas", paste0("Ocultas (", nh, ")")))),
      if (length(g)) div(class = "c-gchips", role = "group", `aria-label` = "Filtrar por grupos",
        span(class = "small-2", "Grupos:"), lapply(names(g), chip),
        if (length(gf) >= 2) seg_radio(ns("gmode"), label = "Combinar", selected = isolate(input$gmode) %||% "alguno",
                                       c("En alguno" = "alguno", "En todos" = "todos")),
        if (length(gf)) tags$a(href = "#", class = "small-2",
                               onclick = sprintf("caringAction('%s', '', 'clear', event)", ns("gchip")), "quitar el filtro"))
    )
  })

  observeEvent(input$gchip, {
    a <- input$gchip
    if (a$action == "clear") return(gfilter(character()))
    gf <- gfilter()
    gfilter(if (a$id %in% gf) setdiff(gf, a$id) else c(gf, a$id))
  })

  # Barra de acciones sobre la selección --------------------------------------------------------
  output$bulk <- renderUI({
    s <- selected()
    if (!length(s)) return(NULL)
    d <- data()
    g <- vgroups(d)
    act <- function(a, id = "") sprintf("caringAction('%s', %s, '%s', event)", ns("bulk"), js_str(id), a)
    btn <- function(label, a, cls = "", aria = NULL) tags$button(type = "button", class = paste("btn btn-sm c-bulk-btn", cls),
                                                                 onclick = act(a), `aria-label` = aria, label)
    item <- function(label, a, id = "") tags$li(tags$a(class = "dropdown-item", href = "#", onclick = act(a, id), label))
    menu <- function(label, items) div(class = "dropdown",
      tags$button(class = "btn btn-sm c-bulk-btn dropdown-toggle", type = "button", `data-bs-toggle` = "dropdown",
                  `data-bs-popper-config` = '{"strategy":"fixed"}', label),
      tags$ul(class = "dropdown-menu", items))
    en <- names(g)[vapply(g, function(x) any(s %in% x$vars), logical(1))]
    edit <- ctx$can_edit()
    div(class = "c-bulk", role = "region", `aria-label` = "Acciones con las variables seleccionadas",
      span(class = "c-bulk-n", tags$strong(length(s)), if (length(s) == 1) " seleccionada" else " seleccionadas"),
      div(class = "c-bulk-actions",
        if (edit) menu("Añadir a un grupo", tagList(
          lapply(names(g), function(k) item(k, "add", k)),
          if (length(g)) tags$li(tags$hr(class = "dropdown-divider")),
          item("Nuevo grupo con estas variables…", "newgroup"))),
        if (edit && length(en)) menu("Quitar de un grupo", lapply(en, function(k) item(k, "remove", k))),
        if (edit) btn("Ocultar", "hide"),
        if (edit && any(s %in% hidden_vars(d))) btn("Mostrar", "show"),
        if (edit) btn("Renombrar…", "rename"),
        if (edit) menu("Mover", tagList(item("Al principio", "top"), item("Al final", "bottom"))),
        if (edit) btn("Eliminar", "drop", "c-bulk-danger"),
        btn("✕", "clear", aria = "Quitar la selección")))
  })

  observeEvent(input$bulk, {
    a <- input$bulk
    if (a$action == "clear") return(clear_sel())
    if (ro()) return()
    s <- selected()
    req(length(s))
    d <- data()
    g <- vgroups(d)
    switch(a$action,
      add = {
        req(a$id %in% names(g))
        g[[a$id]]$vars <- union(g[[a$id]]$vars, s)
        if (set_groups(g)) showNotification(paste0("Añadidas al grupo «", a$id, "»."), type = "message")
      },
      remove = {
        req(a$id %in% names(g))
        g[[a$id]]$vars <- setdiff(g[[a$id]]$vars, s)
        if (set_groups(g)) showNotification(paste0("Quitadas del grupo «", a$id, "»."), type = "message")
      },
      newgroup = group_modal(vars = s),
      hide = if (set_hidden(union(hidden_vars(d), s)))
        showNotification("Ocultas: siguen en los datos, pero no se ofrecen en los análisis.", type = "message"),
      show = set_hidden(setdiff(hidden_vars(d), s)),
      rename = regex_modal("sel", s),
      top = ctx$add_step(list(op = "order", vars = s, how = "seleccionadas al principio")),
      bottom = ctx$add_step(list(op = "order", vars = c(setdiff(names(d), s), s), how = "seleccionadas al final")),
      drop = {
        session$userData$drop_many <- s
        showModal(modalDialog(title = "Eliminar variables", easyClose = TRUE,
          p(length(s), if (length(s) == 1) " variable dejará" else " variables dejarán", " de estar en los datos: ",
            tags$strong(paste(utils::head(s, 8), collapse = ", ")), if (length(s) > 8) "…", "."),
          p(class = "c-hint", "Si solo quieres que no aparezcan en las listas de los análisis, usa «Ocultar». ",
            "Puedes recuperarlas quitando este paso del historial."),
          footer = tagList(modalButton("Cancelar"), actionButton(ns("drop_many_ok"), "Eliminar", class = "btn-danger-c"))))
      })
  })

  observeEvent(input$drop_many_ok, {
    s <- session$userData$drop_many
    removeModal()
    req(length(s))
    if (ctx$add_step(list(op = "drop", vars = s)) && isTRUE(sel() %in% s)) sel(NULL)
  })

  # Grupos ---------------------------------------------------------------------------------------
  output$vgroups_ui <- renderUI({
    d <- data()
    g <- vgroups(d)
    h <- hidden_vars(d)
    gf <- gfilter()
    edit <- ctx$can_edit()
    sug <- if (edit) vgroup_suggest(d, proj$recipe, proj$dir) else list()
    link <- function(label, k, a, aria = NULL) tags$a(href = "#", `aria-label` = aria,
      onclick = sprintf("caringAction('%s', %s, '%s', event)", ns("vg_action"), js_str(k), a), label)
    tagList(
      div(class = "d-flex justify-content-between align-items-center",
        tags$h2("Grupos de variables"),
        if (edit) actionLink(ns("vg_new"), "+ Nuevo grupo")),
      if (!length(g)) p(class = "c-hint", "Reúne las variables que van juntas para filtrarlas en esta lista y elegirlas de una vez ",
                        "en Explorar, Redes y las herramientas. Una variable puede estar en varios grupos."),
      if (length(g)) tags$ul(class = "list-unstyled mb-2", lapply(names(g), function(k) {
        vs <- g[[k]]$vars
        oc <- length(vs) && all(vs %in% h)
        tags$li(class = "c-vg-item",
          div(class = "d-flex justify-content-between align-items-start gap-2",
            div(tags$strong(k), span(class = "small-2", paste0(" · ", length(vs), if (length(vs) == 1) " variable" else " variables",
                                                               if (oc) " · ocultas"))),
            div(class = "c-vg-acts small-2",
              link(if (k %in% gf) "filtrando" else "filtrar", k, "filter", paste("Filtrar la lista por", k)),
              link("seleccionar", k, "select", paste("Seleccionar las variables de", k)),
              if (edit) link(if (oc) "mostrar" else "ocultar", k, if (oc) "show" else "hide"),
              if (edit) link("editar", k, "edit", paste("Editar el grupo", k)),
              if (edit) link("quitar", k, "drop", paste("Quitar el grupo", k)))),
          div(class = "small-2 c-vg-vars", paste(utils::head(vs, 12), collapse = ", "), if (length(vs) > 12) ", …"))
      })),
      if (length(sug)) tags$details(class = "c-mr-sug mt-2",
        tags$summary(tags$strong("Grupos propuestos"), paste0(" (", length(sug), ")")),
        tags$ul(class = "list-unstyled mb-0 mt-2", lapply(seq_along(sug), function(i) {
          x <- sug[[i]]
          tags$li(class = "d-flex justify-content-between align-items-center gap-2 mt-1",
            span(tags$strong(x$name), paste0(" · ", length(x$vars), " variables")),
            tags$a(href = "#", onclick = sprintf("caringAction('%s', '%d', 'suggest', event)", ns("vg_action"), i), "crear"))
        })))
    )
  })

  observeEvent(input$vg_action, {
    a <- input$vg_action
    d <- data()
    g <- vgroups(d)
    if (a$action == "filter") { gf <- gfilter(); return(gfilter(if (a$id %in% gf) setdiff(gf, a$id) else c(gf, a$id))) }
    if (a$action == "select") {
      req(a$id %in% names(g))
      vs <- g[[a$id]]$vars
      if (!identical(gfilter(), a$id)) { preselect(vs); gfilter(a$id) } else select_vars(vs)
      return()
    }
    if (ro()) return()
    switch(a$action,
      hide = set_hidden(union(hidden_vars(d), g[[a$id]]$vars)),
      show = set_hidden(setdiff(hidden_vars(d), g[[a$id]]$vars)),
      edit = group_modal(a$id, g[[a$id]]$vars, old = a$id),
      drop = { g[[a$id]] <- NULL; if (set_groups(g)) gfilter(setdiff(gfilter(), a$id)) },
      suggest = {
        sug <- vgroup_suggest(d, proj$recipe, proj$dir)
        i <- as.integer(a$id)
        req(i >= 1, i <= length(sug))
        group_modal(sug[[i]]$name, sug[[i]]$vars)
      })
  })

  group_modal <- function(name = "", vars = character(), old = NULL) {
    d <- data()
    session$userData$vg_old <- old
    showModal(modalDialog(title = if (is.null(old)) "Nuevo grupo de variables" else "Editar el grupo", size = "l", easyClose = TRUE,
      p(class = "c-hint", "Un grupo no cambia los datos: sirve para encontrar y elegir juntas las variables que van juntas. ",
        "Una variable puede estar en varios grupos. Queda en el historial y se puede quitar."),
      textInput(ns("vg_name"), "Nombre del grupo", value = name, placeholder = "Sociodemográficas"),
      selectizeInput(ns("vg_vars"), "Variables", choices = var_display(d, names(d)), selected = vars, multiple = TRUE,
                     width = "100%", options = list(plugins = list("remove_button", "drag_drop"), placeholder = "Elige las variables…")),
      footer = tagList(modalButton("Cancelar"), actionButton(ns("vg_save"), "Guardar", class = "btn-primary"))))
  }
  observeEvent(input$vg_new, { if (!ro()) group_modal(vars = selected()) })

  observeEvent(input$vg_save, {
    nm <- trimws(input$vg_name %||% "")
    vs <- input$vg_vars
    g <- vgroups(data())
    old <- session$userData$vg_old
    if (!nzchar(nm)) return(showNotification("Ponle un nombre al grupo.", type = "error"))
    if (nm %in% setdiff(names(g), old)) return(showNotification("Ya hay un grupo con ese nombre.", type = "error"))
    if (!length(vs)) return(showNotification("Elige al menos una variable.", type = "error"))
    nuevo <- list(name = nm, vars = vs)
    if (!is.null(old) && old %in% names(g)) {
      pos <- match(old, names(g)); g[[pos]] <- nuevo; names(g)[pos] <- nm
    } else g[[nm]] <- nuevo
    if (set_groups(g)) {
      removeModal()
      if (!is.null(old) && old %in% gfilter()) gfilter(c(setdiff(gfilter(), old), nm))
    }
  })

  # Renombrar con expresión regular -------------------------------------------------------------
  rx_presets <- list(
    list(id = "prefijo", label = "Quitar un prefijo (P_)", pat = "^[[:alpha:]]+_", rep = ""),
    list(id = "sufijo", label = "Añadir un sufijo (_r)", pat = "$", rep = "_r"),
    list(id = "minus", label = "minúsculas", pat = "^(.+)$", rep = "\\L$1"),
    list(id = "mayus", label = "MAYÚSCULAS", pat = "^(.+)$", rep = "\\U$1"),
    list(id = "simbolos", label = "Cambiar símbolos por _", pat = "[^[:alnum:]_.]+", rep = "_"))

  regex_modal <- function(scope = "all", sel_vars = character()) {
    d <- data()
    g <- vgroups(d)
    session$userData$rx_sel <- sel_vars
    scopes <- c("Todas las variables" = "all")
    if (length(hidden_vars(d))) scopes <- c(scopes, "Las visibles" = "visible")
    if (length(sel_vars)) scopes <- c(scopes, stats::setNames("sel", paste0("Las seleccionadas (", length(sel_vars), ")")))
    if (length(g)) scopes <- c(scopes, "Las de un grupo" = "group")
    showModal(modalDialog(title = "Renombrar variables con una expresión regular", size = "l", easyClose = TRUE,
      p(class = "c-hint", "Escribe qué buscar en los nombres y por qué cambiarlo; antes de aplicar verás cómo quedan. ",
        "En la sustitución, ", tags$code("$1"), ", ", tags$code("$2"), "… recogen lo que va entre paréntesis; ",
        tags$code("\\U$1"), " lo pasa a mayúsculas y ", tags$code("\\L$1"), " a minúsculas."),
      div(class = "c-presets", span(class = "small-2", "Atajos:"),
        lapply(rx_presets, function(p) tags$button(type = "button", class = "btn btn-light-c btn-sm",
          onclick = sprintf("caringAction('%s', '%s', 'preset', event)", ns("rx_preset"), p$id), p$label))),
      div(class = "row",
        div(class = "col-md-6", textInput(ns("rx_pat"), "Buscar", placeholder = "^P([0-9]+)", width = "100%")),
        div(class = "col-md-6", textInput(ns("rx_rep"), "Sustituir por", placeholder = "pregunta_$1", width = "100%"))),
      div(class = "d-flex flex-wrap gap-3",
        checkboxInput(ns("rx_all"), "Todas las coincidencias de cada nombre", TRUE),
        checkboxInput(ns("rx_case"), "Sin distinguir mayúsculas y minúsculas", FALSE)),
      radioButtons(ns("rx_scope"), "Aplicar a", choices = scopes, selected = if (scope %in% scopes) scope else "all", inline = TRUE),
      if (length(g)) conditionalPanel("input.rx_scope == 'group'", ns = ns, selectInput(ns("rx_group"), "Grupo", names(g))),
      uiOutput(ns("rx_preview")),
      footer = tagList(modalButton("Cancelar"), actionButton(ns("rx_apply"), "Renombrar", class = "btn-primary"))))
  }

  observeEvent(input$rx_preset, {
    p <- Filter(function(x) x$id == input$rx_preset$id, rx_presets)[[1]]
    updateTextInput(session, "rx_pat", value = p$pat)
    updateTextInput(session, "rx_rep", value = p$rep)
  })

  rx_targets <- reactive({
    d <- data()
    switch(input$rx_scope %||% "all",
      visible = shown_vars(d),
      sel = intersect(names(d), session$userData$rx_sel %||% character()),
      group = vgroup_vars(d, input$rx_group %||% ""),
      names(d))
  })

  rx_result <- reactive({
    pat <- input$rx_pat %||% ""
    if (!nzchar(pat)) return(list(state = "empty"))
    m <- regex_rename_preview(rx_targets(), pat, input$rx_rep %||% "", isTRUE(input$rx_all), isTRUE(input$rx_case))
    if (is.null(m)) return(list(state = "bad"))
    cambia <- m[m != names(m)]
    list(state = "ok", map = cambia, problems = rename_problems(names(data()), cambia))
  })

  output$rx_preview <- renderUI({
    r <- rx_result()
    if (r$state == "empty") return(p(class = "c-hint", "Escribe una expresión para ver los cambios."))
    if (r$state == "bad") return(div(class = "c-msg-error", "La expresión no es válida."))
    if (!length(r$map)) return(p(class = "c-hint", "Ningún nombre cambia con esta expresión."))
    n <- length(r$map); mal <- sum(nzchar(r$problems))
    filas <- utils::head(seq_len(n), 200)
    tagList(
      div(class = if (mal) "c-msg-error mb-2" else "c-stats mb-2",
          n, if (n == 1) " variable cambia de nombre" else " variables cambian de nombre",
          if (mal) paste0(" · ", mal, if (mal == 1) " nombre no sirve" else " nombres no sirven", ": corrígelos antes de aplicar")),
      div(class = "c-rx-list", tags$table(class = "c-rx-table",
        tags$thead(tags$tr(tags$th("Ahora"), tags$th(""), tags$th("Quedará"))),
        tags$tbody(lapply(filas, function(i) tags$tr(class = if (nzchar(r$problems[i])) "bad",
          tags$td(tags$code(names(r$map)[i])), tags$td("→"),
          tags$td(tags$code(r$map[[i]]), if (nzchar(r$problems[i])) span(class = "c-rx-why", r$problems[i]))))))),
      if (n > 200) p(class = "c-hint", "Se muestran los 200 primeros."))
  })

  observeEvent(input$rx_apply, {
    r <- rx_result()
    if (r$state != "ok" || !length(r$map)) return(showNotification("No hay ningún nombre que cambiar.", type = "error"))
    if (any(nzchar(r$problems))) return(showNotification("Hay nombres que no sirven: corrígelos antes de aplicar.", type = "error"))
    actual <- sel()
    ok <- ctx$add_step(list(op = "rename_many", map = as.list(r$map),
                            pattern = input$rx_pat, replacement = input$rx_rep %||% ""))
    if (ok) {
      removeModal()
      if (isTRUE(actual %in% names(r$map))) sel(r$map[[actual]])
      showNotification(paste(length(r$map), if (length(r$map) == 1) "variable renombrada." else "variables renombradas."), type = "message")
    }
  })

  # Ordenar ----------------------------------------------------------------------------------------
  ord <- reactiveVal(NULL)
  ord_how <- reactiveVal("a mano")

  observeEvent(input$t_order, {
    if (ro()) return()
    d <- data()
    ord(names(d)); ord_how("a mano")
    showModal(modalDialog(title = "Ordenar las variables", size = "l", easyClose = TRUE,
      p(class = "c-hint", "Arrastra las variables a su sitio o usa un orden rápido. Es el orden de las listas de variables ",
        "y el de los datos al exportarlos."),
      div(class = "d-flex flex-wrap gap-2 mb-3",
        actionButton(ns("ord_az"), "A → Z", class = "btn-light-c btn-sm"),
        actionButton(ns("ord_za"), "Z → A", class = "btn-light-c btn-sm"),
        actionButton(ns("ord_orig"), "Orden del archivo", class = "btn-light-c btn-sm"),
        if (length(vgroups(d))) actionButton(ns("ord_groups"), "Por grupos", class = "btn-light-c btn-sm")),
      div(class = "d-flex flex-wrap gap-2 align-items-end mb-2",
        div(style = "flex:1; min-width:240px", textInput(ns("ord_rx"), "Mover las que coinciden con estas expresiones (separadas por comas, en ese orden)",
                                                          placeholder = "^P1, ^P2, _r$", width = "100%")),
        div(class = "mb-3 d-flex gap-2",
          actionButton(ns("ord_rx_top"), "Al principio", class = "btn-light-c btn-sm"),
          actionButton(ns("ord_rx_bottom"), "Al final", class = "btn-light-c btn-sm"))),
      uiOutput(ns("ord_list")),
      footer = tagList(modalButton("Cancelar"), actionButton(ns("ord_save"), "Guardar el orden", class = "btn-primary"))))
  })

  output$ord_list <- renderUI({
    o <- ord()
    req(o)
    d <- data()
    o <- intersect(o, names(d))
    div(class = "c-ord-list",
      sortable::rank_list(text = NULL, input_id = ns("ord_items"),
        labels = stats::setNames(lapply(o, function(v) {
          l <- var_label(d, v)
          tagList(tags$code(v), if (!identical(l, v)) span(class = "small-2", " ", l))
        }), o)))
  })

  ord_current <- function() { o <- input$ord_items; if (length(o)) o else ord() }
  quick_order <- function(how, label, ...) {
    r <- tryCatch(order_by(data(), how, v = ord_current(), ...), error = function(e) e)
    if (inherits(r, "error")) return(showNotification(conditionMessage(r), type = "error"))
    ord(NULL); ord(r); ord_how(label)
  }
  observeEvent(input$ord_az, quick_order("az", "A → Z"))
  observeEvent(input$ord_za, quick_order("za", "Z → A"))
  observeEvent(input$ord_orig, quick_order("original", "orden del archivo", original = names(proj$original)))
  observeEvent(input$ord_groups, quick_order("groups", "por grupos"))
  observeEvent(input$ord_rx_top, quick_order("regex", "por expresiones", patterns = input$ord_rx, top = TRUE))
  observeEvent(input$ord_rx_bottom, quick_order("regex", "por expresiones", patterns = input$ord_rx, top = FALSE))

  observeEvent(input$ord_save, {
    d <- data()
    o <- intersect(ord_current(), names(d))
    if (identical(o, names(d))) { removeModal(); return(showNotification("El orden no ha cambiado.", type = "message")) }
    how <- if (identical(o, ord())) ord_how() else "a mano"
    if (ctx$add_step(list(op = "order", vars = o, how = how))) removeModal()
  })

  observeEvent(input$t_regex, { if (!ro()) regex_modal(if (length(selected())) "sel" else "all", selected()) })

  list(selected = selected)
}
