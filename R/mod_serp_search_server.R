mod_serp_search_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    logs <- shiny::reactiveVal(character())
    status <- shiny::reactiveVal("Upload a CSV and configure the search.")
    results <- shiny::reactiveVal(NULL)
    current_output_dir <- shiny::reactiveVal(NULL)

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
        
        timestamp <- gsub("\\.", "_", format(Sys.time(), "%Y%m%d_%H%M%OS6", tz = "UTC"))
        output_dir <- file.path(tempdir(), paste0("scholar_search_", timestamp))
        current_output_dir(output_dir)
        
        queries <- parse_query_csv(input$csv_file$datapath, input$queries_pat)
        
        if (!dir.exists(output_dir) &&
            !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)) {
          stop("Cannot create the temporary output directory.")
        }
        
        log_file <- file.path(output_dir, paste0("search_log_", timestamp, ".txt"))
        logs(character())
        results(NULL)
        update_status(sprintf("Searching %d queries...", length(queries)))
        serp_log(paste("Starting search session:", timestamp), log_file, append_log)
        shiny::withProgress(message = "Searching literature", value = 0, {
          for (index in seq_along(queries)) {
            update_status(sprintf("Searching query %d of %d.", index, length(queries)))
            
            # Wrapper function to allow automated retry on rate limit failures
            execute_query_with_retry <- function(query, is_retry = FALSE) {
              if (is_retry) {
                # If we are retrying, wait 60 seconds to let the rate limit reset
                shiny::showNotification(
                  "Rate limit encountered. Pausing for 60 seconds before retrying the failed search.",
                  type = "warning", duration = 60, id = "rate_limit_warning"
                )
                serp_log("Rate limit hit. Pausing 60 seconds before retry...", log_file, append_log)
                Sys.sleep(60)
                serp_log("Resuming search...", log_file, append_log)
              }
              
              process_single_search_term(
                query = query, api_key = api_key,
                paper_limit = input$paper_limit,
                use_dynamic_papers_limit = isTRUE(input$use_dynamic_papers_limit),
                dynamic_fraction = input$dynamic_fraction,
                dynamic_min = input$dynamic_min, dynamic_max = input$dynamic_max,
                use_full_abstract_lookup = isTRUE(input$use_full_abstract_lookup),
                log_file = log_file, log_callback = append_log,
                year_low = if (isTRUE(input$filter_by_year)) input$year_low else NULL,
                year_high = if (isTRUE(input$filter_by_year)) input$year_high else NULL,
                progress_callback = function(detail_msg) {
                  shiny::incProgress(0, detail = sprintf("Query %d of %d: %s", index, length(queries), detail_msg))
                }
              )
            }

            # First attempt
            query_result <- execute_query_with_retry(queries[index])
            
            # If it failed but it wasn't an out_of_credits error, try exactly ONE long-pause retry
            if (isTRUE(query_result$failed) && !isTRUE(query_result$out_of_credits)) {
               query_result <- execute_query_with_retry(queries[index], is_retry = TRUE)
            }
            
            collected[[index]] <- query_result
            
            if (isTRUE(collected[[index]]$out_of_credits)) {
              shiny::showModal(shiny::modalDialog(
                title = "SerpApi Credits Depleted",
                "Your SerpApi account has run out of searches. The search has been aborted to prevent further errors. Please visit serpapi.com to upgrade your plan or purchase more credits.",
                easyClose = TRUE,
                footer = shiny::modalButton("Dismiss")
              ))
              # Remove the empty failed placeholder so it doesn't crash the save function
              collected[[index]] <- NULL
              break
            }
            
            # Persist completed queries so later failures do not discard results.
            results(save_search_outputs(collected, output_dir, timestamp, 
                                        do_deduplicate = isTRUE(input$deduplicate),
                                        file_prefix = trimws(input$file_prefix)))
            shiny::incProgress(1 / length(queries),
                                detail = sprintf("Query %d of %d", index, length(queries)))
          }
        })
        failed <- sum(purrr::map_lgl(collected, "failed"))
        message <- sprintf("Finished: %d papers; %d failed or partial queries. Ready for download.",
                           nrow(results()$unique_papers_output), failed)
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
    
    output$download_btn_ui <- shiny::renderUI({
      shiny::req(results())
      shiny::downloadButton(session$ns("download_results"), "Download Results (ZIP)", class = "btn-success")
    })
    
    output$download_results <- shiny::downloadHandler(
      filename = function() {
        base_name <- trimws(input$file_prefix)
        if (!nzchar(base_name)) base_name <- "scholar_search"
        paste0(base_name, "_", format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC"), ".zip")
      },
      content = function(file) {
        shiny::req(current_output_dir())
        zip_dir <- current_output_dir()
        
        owd <- setwd(zip_dir)
        on.exit(setwd(owd))
        
        # Zip all files in the current temporary output directory
        zip::zip(file, files = list.files(zip_dir, all.files = TRUE, no.. = TRUE))
      }
    )

    invisible(results)
  })
}
