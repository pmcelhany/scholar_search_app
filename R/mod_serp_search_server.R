mod_serp_search_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    logs <- shiny::reactiveVal(character())
    status <- shiny::reactiveVal("Upload a CSV and configure the search.")
    results <- shiny::reactiveVal(NULL)
    
    roots <- c(Home = fs::path_home(), WorkingDir = getwd(), Root = "/")
    shinyFiles::shinyDirChoose(input, "output_dir_btn", roots = roots, session = session)
    
    shiny::observe({
      if (!is.integer(input$output_dir_btn)) {
        dir_path <- shinyFiles::parseDirPath(roots, input$output_dir_btn)
        if (length(dir_path) > 0) {
          shiny::updateTextInput(session, "output_dir", value = dir_path)
        }
      }
    })

    output$status_banner <- shiny::renderUI(
      shiny::div(class = "alert alert-info", role = "status", status())
    )
    output$log_output <- shiny::renderText(paste(logs(), collapse = "\n"))
    output$summary_table <- shiny::renderUI({
      shiny::req(results())
      shiny::HTML(gt::as_raw_html(gt::gt(results()$final_search_summary)))
    })
    output$results_table <- shiny::renderUI({
      shiny::req(results())
      shiny::HTML(gt::as_raw_html(
        gt::gt(utils::head(results()$unique_papers_output, 100))
      ))
    })
    push_text <- function(id, text) {
      session$sendCustomMessage("serp-search-progress",
                                list(id = session$ns(id), text = text))
    }
    update_status <- function(text) {
      status(text)
      push_text("status_banner", text)
    }
    append_log <- function(line) {
      logs(c(logs(), line))
      push_text("log_output", paste(logs(), collapse = "\n"))
    }
    shiny::observeEvent(input$run_search, {
      valid_number <- function(value, lower, upper = Inf, integer = FALSE) {
        is.numeric(value) && length(value) == 1L && is.finite(value) &&
          value >= lower && value <= upper && (!integer || value == floor(value))
      }
      log_file <- NULL
      collected <- list()
      output_dir <- NULL
      timestamp <- NULL
      tryCatch({
        if (is.null(input$csv_file) || !file.exists(input$csv_file$datapath)) {
          stop("Please upload a search terms CSV.")
        }
        api_key <- normalize_serpapi_key(input$serpapi_key)
        if (!nzchar(api_key)) {
          api_key <- normalize_serpapi_key(Sys.getenv("SERPAPI_API_KEY", ""))
        }
        if (!nzchar(api_key)) {
          stop("Please provide a SerpApi key.")
        }
        if (!valid_number(input$paper_limit, 1, integer = TRUE)) {
          stop("Fixed Paper Limit must be a positive whole number.")
        }
        if (isTRUE(input$filter_by_year)) {
          if (!valid_number(input$year_low, 1900, integer = TRUE) ||
              !valid_number(input$year_high, 1900, integer = TRUE) ||
              input$year_low > input$year_high) {
            stop("Please provide a valid year range (From Year must be less than or equal to To Year).")
          }
        }
        
        if (isTRUE(input$filter_by_year)) {
          if (!valid_number(input$year_low, 1900, integer = TRUE) ||
              !valid_number(input$year_high, 1900, integer = TRUE) ||
              input$year_low > input$year_high) {
            stop("Please provide a valid year range (From Year must be less than or equal to To Year).")
          }
        }
        
        if (isTRUE(input$use_dynamic_papers_limit) &&
            (!valid_number(input$dynamic_fraction, 0.01, 1) ||
             !valid_number(input$dynamic_min, 1, integer = TRUE) ||
             !valid_number(input$dynamic_max, 10, integer = TRUE) ||
             input$dynamic_min > input$dynamic_max)) {
          stop("Check the dynamic fraction and paper limits; minimum must not exceed maximum.")
        }
        if (is.null(input$output_dir) || !nzchar(trimws(input$output_dir))) {
          stop("Please provide an output directory.")
        }
        queries <- parse_query_csv(input$csv_file$datapath, input$queries_pat)
        output_dir <- path.expand(trimws(input$output_dir))
        if (!dir.exists(output_dir) &&
            !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)) {
          stop("Cannot create the output directory.")
        }
        if (file.access(output_dir, 2) != 0) stop("The output directory is not writable.")
        timestamp <- gsub("\\.", "_", format(Sys.time(), "%Y%m%d_%H%M%OS6", tz = "UTC"))
        log_file <- file.path(output_dir, paste0("search_log_", timestamp, ".txt"))
        logs(character())
        results(NULL)
        update_status(sprintf("Searching %d queries...", length(queries)))
        serp_log(paste("Starting search session:", timestamp), log_file, append_log)
        shiny::withProgress(message = "Searching literature", value = 0, {
          for (index in seq_along(queries)) {
            update_status(sprintf("Searching query %d of %d.", index, length(queries)))
            collected[[index]] <- process_single_search_term(
              query = queries[index], api_key = api_key,
              paper_limit = input$paper_limit,
              use_dynamic_papers_limit = isTRUE(input$use_dynamic_papers_limit),
              dynamic_fraction = input$dynamic_fraction,
              dynamic_min = input$dynamic_min, dynamic_max = input$dynamic_max,
              use_full_abstract_lookup = isTRUE(input$use_full_abstract_lookup),
              s2_api_key = Sys.getenv("SEMANTIC_SCHOLAR_API_KEY", ""),
              log_file = log_file, log_callback = append_log,
              year_low = if (isTRUE(input$filter_by_year)) input$year_low else NULL,
              year_high = if (isTRUE(input$filter_by_year)) input$year_high else NULL
            )
            # Persist completed queries so later failures do not discard results.
            results(save_search_outputs(collected, output_dir, timestamp, 
                                        do_deduplicate = isTRUE(input$deduplicate),
                                        file_prefix = trimws(input$file_prefix)))
            shiny::incProgress(1 / length(queries),
                                detail = sprintf("Query %d of %d", index, length(queries)))
          }
        })
        failed <- sum(purrr::map_lgl(collected, "failed"))
        message <- sprintf("Finished: %d papers; %d failed or partial queries. Saved to %s",
                           nrow(results()$unique_papers_output), failed, output_dir)
        serp_log(message, log_file, append_log)
        update_status(message)
        if (failed > 0) shiny::showNotification(
          "Some queries failed or returned partial results. See the log.", type = "warning"
        )
      }, error = function(e) {
        # Only validation messages are exposed; unexpected errors may include API URLs.
        message <- if (is.null(log_file)) conditionMessage(e) else
          "Search interrupted. Completed queries remain saved; check the output directory and API connectivity."
        update_status(message)
        if (!is.null(log_file)) {
          try(serp_log(message, log_file, append_log), silent = TRUE)
        }
        shiny::showNotification(message, type = "error", duration = NULL)
      })
    })
    invisible(results)
  })
}
