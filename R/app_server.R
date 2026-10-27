app_server <- function(input, output, session) {
  # Add Resource Path using absolute path for development
  shiny::addResourcePath("docs", normalizePath("inst/app/www", mustWork = FALSE))
  
  mod_serp_search_server("serp_search_1")
}
