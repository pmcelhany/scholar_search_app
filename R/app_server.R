app_server <- function(input, output, session) {
  shiny::addResourcePath("www", system.file("app/www", package = "scholarsearchapp"))
  # Fallback for development if system.file doesn't find it (pkgload)
  if (dir.exists("inst/app/www")) {
    shiny::addResourcePath("www", "inst/app/www")
  }
  
  mod_serp_search_server("serp_search_1")
}
