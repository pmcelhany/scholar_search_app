app_ui <- function(request) {
  shiny::fluidPage(
    shiny::titlePanel("Scholar Search"),
    shiny::tabsetPanel(
      shiny::tabPanel("Literature search", mod_serp_search_ui("serp_search_1")),
      shiny::tabPanel("User Guide", 
                      shiny::tags$iframe(
                        src = "docs/scholar_search_guide.html",
                        style = "width: 100%; height: 85vh; border: none; margin-top: 10px;"
                      )
      )
    )
  )
}
