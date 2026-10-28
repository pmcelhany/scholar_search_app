mod_serp_search_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::singleton(shiny::tags$script(shiny::HTML(
      "Shiny.addCustomMessageHandler('serp-search-progress', function(data) {
         var element = document.getElementById(data.id);
         if (element) element.textContent = data.text;
       });"
    ))),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput(ns("csv_file"), "Search Terms CSV File",
                         accept = c(".csv", "text/csv")),
        shiny::helpText("One boolean query per row in a single column; an optional query header is supported."),
        shiny::passwordInput(ns("serpapi_key"), "SerpApi Key", value = ""),
        shiny::helpText("Leave the key blank to use SERPAPI_API_KEY on the server."),
        shiny::textInput(ns("queries_pat"),
                         "Filter Queries by Regex/Pattern (Optional)", value = ""),
        shiny::numericInput(ns("paper_limit"), "Fixed Paper Limit", value = 100, min = 1),
        shiny::h4("Search Filters"),
        shiny::checkboxInput(ns("filter_by_year"), "Filter by Year Range", value = FALSE),
        shiny::conditionalPanel(
          condition = "input.filter_by_year", ns = ns,
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput(ns("year_low"), "From Year", value = as.numeric(format(Sys.Date(), "%Y")) - 5, min = 1900, max = as.numeric(format(Sys.Date(), "%Y")) + 1)),
            shiny::column(6, shiny::numericInput(ns("year_high"), "To Year", value = as.numeric(format(Sys.Date(), "%Y")), min = 1900, max = as.numeric(format(Sys.Date(), "%Y")) + 1))
          )
        ),
        shiny::h4("Pagination Limits"),
        shiny::checkboxInput(ns("use_dynamic_papers_limit"),
                             "Enable Dynamic Paper Limit", value = FALSE),
        shiny::conditionalPanel(
          condition = "input.use_dynamic_papers_limit", ns = ns,
          shiny::numericInput(ns("dynamic_fraction"), "Dynamic Fraction",
                              value = 0.1, min = 0.01, max = 1, step = 0.05),
          shiny::numericInput(ns("dynamic_min"), "Dynamic Min Papers",
                              value = 20, min = 1),
          shiny::numericInput(ns("dynamic_max"), "Dynamic Max Papers",
                              value = 200, min = 10)
        ),
        shiny::checkboxInput(ns("use_full_abstract_lookup"),
                             "Lookup Full Abstract (OpenAlex / PubMed / CrossRef)",
                             value = TRUE),
        shiny::checkboxInput(ns("deduplicate"),
                             "Deduplicate Results",
                             value = TRUE),
        shiny::h4("Output Settings"),
        shiny::textInput(ns("file_prefix"), "Base Name for Downloaded ZIP (Optional)", 
                         value = "", placeholder = "e.g., project_alpha"),
        shiny::actionButton(ns("run_search"), "Run Search",
                            icon = shiny::icon("play"), class = "btn-primary"),
        shiny::br(),
        shiny::br(),
        shiny::uiOutput(ns("download_btn_ui"))
      ),
      shiny::mainPanel(
        shiny::uiOutput(ns("status_banner")),
        shiny::h3("Search log"),
        shiny::verbatimTextOutput(ns("log_output")),
        shiny::h3("Search summary"),
        shiny::uiOutput(ns("summary_table")),
        shiny::h3("Search results"),
        shiny::uiOutput(ns("results_table"))
      )
    )
  )
}
