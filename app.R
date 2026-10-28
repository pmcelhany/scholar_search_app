# Launch the ShinyApp
# When deploying to a server (like Posit Connect), the entire project is usually loaded 
# as an installed package or sourced directly, rather than using pkgload.
if (requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")) {
  pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
} else {
  # Fallback for server deployments where pkgload fails
  invisible(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source))
}

# Set to FALSE for development/debugging, TRUE for production behavior
options("golem.app.prod" = TRUE)

# Register the resource path globally before the UI renders
shiny::addResourcePath("docs", normalizePath("inst/app/www", mustWork = FALSE))

# Launch directly
shiny::shinyApp(ui = app_ui, server = app_server)