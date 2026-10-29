library(shiny)
library(DT)
ui <- fluidPage(DTOutput("tbl"))
server <- function(input, output) {
  output$tbl <- renderDT({
    data <- data.frame(a = 1:5, b = c("A", "B", "C", "D", NA))
    datatable(data, filter="top")
  })
}
# Can't run a shiny app headless, but let's check the dt render logic.
