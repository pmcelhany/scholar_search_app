# Launch the ShinyApp
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

# Set to FALSE for development/debugging, TRUE for production behavior
options("golem.app.prod" = TRUE)

# Register the resource path globally before the UI renders
shiny::addResourcePath("docs", normalizePath("inst/app/www", mustWork = FALSE))

# Standard golem launch (R/run_app.R returns the shiny.appobj)
#run_app()

# Alternatively, if you do not have R/run_app.R, launch directly:
shiny::shinyApp(ui = app_ui, server = app_server)