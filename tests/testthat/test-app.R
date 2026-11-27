# Prueba de extremo a extremo en un navegador (Chrome sin ventana).
# Recorre: invitado -> proyecto de demostración -> recodificar -> explorar ->
# red -> informe -> exportar a PowerPoint. Tarda alrededor de un minuto.

test_that("recorrido completo en el navegador", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if(is.null(chromote::find_chrome()), "Sin Chrome")

  storage <- withr::local_tempdir("caring-e2e-")
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")   # en Windows, "\" rompe la orden -e
  port <- httpuv::randomPort()
  srv <- processx::process$new(
    file.path(R.home("bin"), "Rscript"),
    c("-e", sprintf("shiny::runApp('%s', port = %d, launch.browser = FALSE)", root, port)),
    env = c("current", CARING_STORAGE = storage, CARING_DB = file.path(storage, "e2e.sqlite"),
            CARING_HTML_DIR = file.path(storage, "caring")),
    stdout = "|", stderr = "2>&1")
  withr::defer(srv$kill())
  url <- sprintf("http://127.0.0.1:%d", port)
  for (i in 1:60) {
    ok <- tryCatch({ suppressWarnings(readLines(url, warn = FALSE)); TRUE }, error = function(e) FALSE)
    if (ok) break
    Sys.sleep(1)
  }
  expect_true(ok, info = "la aplicación no ha arrancado")
  app <- shinytest2::AppDriver$new(url, name = "e2e", width = 1400, height = 1000,
                                   load_timeout = 60000, timeout = 60000)
  withr::defer(app$stop(), priority = "first")
  idle <- function(ms = 1500) app$wait_for_idle(ms)
  js <- function(x) app$run_js(x)
  idle()

  # Los scripts propios se han cargado enteros (un error al cargar dejaría botones sin efecto)
  expect_equal(app$get_js("typeof window.caringAction"), "function")

  # Acceso como invitado y proyecto de demostración
  app$click("login-guest"); idle(2500)
  app$click("projects-demo"); idle(5000)
  expect_match(app$get_text("#project-name"), "demostración")

  # Recodificar educación en tres grupos
  js("document.querySelectorAll('#project-datos-vars .rt-tbody .rt-tr-group')[3].querySelector('.rt-td:not(.rt-td-select)').click()"); idle()
  js("document.querySelector('#project-datos-etab a[data-value=\"recode\"]').click()"); idle()
  js("Shiny.setInputValue('project-datos-grp_1', ['Sin estudios','Secundaria']);
      Shiny.setInputValue('project-datos-grp_2', ['Bachillerato o FP']);
      Shiny.setInputValue('project-datos-grp_3', ['Universitarios','Postgrado']);")
  app$set_inputs(`project-datos-gname_1` = "Básicos", `project-datos-gname_2` = "Medios",
                 `project-datos-gname_3` = "Superiores", wait_ = FALSE)
  app$set_inputs(`project-datos-recode_name` = "educ3", wait_ = FALSE); idle(500)
  app$click("project-datos-do_recode"); idle(2500)
  expect_match(app$get_text("#project-datos-history"), "educ3")

  # Explorar y guardar un gráfico
  js("Shiny.setInputValue('project-step', 'explorar', {priority:'event'})"); idle(2500)
  app$set_inputs(`project-explorar-mode` = "two"); idle(500)
  app$set_inputs(`project-explorar-v1` = "internet", `project-explorar-v2` = "educ3"); idle(2500)
  expect_match(app$get_text("#project-explorar-stats"), "Cramér")
  app$click("project-explorar-add_plot"); idle(3000)

  # Red de coincidencias
  js("Shiny.setInputValue('project-step', 'redes', {priority:'event'})"); idle(2500)
  app$set_inputs(`project-redes-type` = "coincidencias"); idle(1500)        # la nube de puntos es la opción por defecto
  app$set_inputs(`project-redes-vars` = c("genero", "educ3", "internet", "prensa")); idle()
  app$click("project-redes-go"); idle(5000)
  expect_match(app$get_text("#project-redes-net_info"), "nodos")
  app$click("project-redes-add"); idle(8000)

  # Informe con el gráfico y exportación
  js("Shiny.setInputValue('project-step', 'informes', {priority:'event'})"); idle(2500)
  app$click("project-informes-new_slides2"); idle(3000)
  js("document.querySelector('.ed-thumb:nth-child(2)').click()")
  js("document.querySelector('.ed-res-item button.btn-primary').click()"); idle(2500)
  n_el <- app$get_js("document.querySelectorAll('.ed-active .ed-el').length")
  expect_gt(n_el, 2)
  f <- app$get_download("project-informes-dl_pptx")
  expect_gt(file.size(f), 20000)

  logs <- as.data.frame(app$get_logs())
  errores <- logs[grepl("Error", logs$message) & !grepl("Deprecated|deprecated", logs$message), ]
  expect_equal(nrow(errores), 0, info = paste(errores$message, collapse = "\n"))
  salida <- srv$read_output()
  expect_false(grepl("Warning: Error|Execution halted", salida), info = salida)
})
