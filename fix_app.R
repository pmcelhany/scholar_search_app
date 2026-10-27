pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = FALSE)
shiny::shinyApp(ui = app_ui, server = app_server)
