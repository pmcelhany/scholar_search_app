app_ui <- function(request) {
  shiny::fluidPage(
    shiny::titlePanel("Scholar Search"),
    shiny::tabsetPanel(
      shiny::tabPanel("Literature search", mod_serp_search_ui("serp_search_1"))
    )
  )
}
