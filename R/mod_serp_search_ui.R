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
        shiny::passwordInput(ns("serpapi_key"), "SerpApi Key",
                             value = Sys.getenv("SERPAPI_API_KEY", "")),
        shiny::textInput(ns("queries_pat"),
                         "Filter Queries by Regex/Pattern (queries_pat)", value = ""),
        shiny::numericInput(ns("paper_limit"), "Fixed Paper Limit", value = 100, min = 1),
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
                             "Lookup Full Abstract (Semantic Scholar / CrossRef)",
                             value = TRUE),
        shiny::textInput(ns("output_dir"), "Output Directory Path",
                         value = "output/lit_search"),
        shiny::helpText("Files are saved on the machine running this app."),
        shiny::actionButton(ns("run_search"), "Run Search",
                            icon = shiny::icon("play"), class = "btn-primary")
      ),
      shiny::mainPanel(
        shiny::uiOutput(ns("status_banner")),
        shiny::h3("Search log"),
        shiny::verbatimTextOutput(ns("log_output")),
        shiny::h3("Search summary"),
        gt::gt_output(ns("summary_table")),
        shiny::h3("Unique papers"),
        gt::gt_output(ns("results_table"))
      )
    )
  )
}
