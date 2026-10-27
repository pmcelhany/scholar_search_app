serp_log <- function(message, log_file = NULL, log_callback = NULL) {
  line <- sprintf("[%s UTC] %s",
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"), message)
  if (!is.null(log_file)) cat(line, "\n", file = log_file, append = TRUE)
  if (!is.null(log_callback)) log_callback(line)
  invisible(line)
}

parse_query_csv <- function(file_path, pattern = NULL) {
  rows <- utils::read.csv(
    file_path, header = FALSE, colClasses = "character",
    stringsAsFactors = FALSE, check.names = FALSE, fill = FALSE,
    fileEncoding = "UTF-8-BOM", na.strings = character()
  )
  if (ncol(rows) != 1L) {
    stop("The CSV must contain exactly one column of boolean search queries.")
  }
  queries <- trimws(rows[[1L]])
  if (length(queries) && tolower(queries[1L]) %in%
      c("query", "queries", "search_query", "search_queries", "search_term",
        "search_terms", "boolean_search_term", "boolean_search_terms")) {
    queries <- queries[-1L]
  }
  queries <- queries[!is.na(queries) & nzchar(queries)]
  if (!is.null(pattern) && nzchar(pattern)) {
    keep <- tryCatch(
      grepl(pattern, queries, perl = TRUE),
      error = function(e) stop("Invalid query filter pattern.", call. = FALSE)
    )
    queries <- queries[keep]
  }
  if (!length(queries)) stop("No search queries remain after reading and filtering.")
  unique(queries)
}

robust_serp_search <- function(query, api_key, start_idx = 0, num = 20,
                               log_file = NULL, log_callback = NULL,
                               max_retries = 3) {
  api_key <- trimws(api_key)
  if (!nzchar(api_key)) stop("A SerpApi key is required.")
  for (attempt in seq.int(0L, max_retries)) {
    response <- tryCatch(
      httr::GET(
        "https://serpapi.com/search",
        query = list(engine = "google_scholar", q = query, api_key = api_key,
                     start = start_idx, num = num, hl = "en"),
        httr::timeout(30)
      ),
      error = function(e) NULL
    )
    # Do not log request errors or response bodies: they can contain credentials.
    if (is.null(response)) {
      serp_log("SerpApi connection failed.", log_file, log_callback)
      return(NULL)
    }
    status <- httr::status_code(response)
    if (status == 200L) {
      result <- tryCatch(
        jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"),
                           simplifyVector = FALSE),
        error = function(e) NULL
      )
      if (!is.list(result) || !is.null(result$error)) {
        serp_log("SerpApi returned an invalid response or API error.",
                 log_file, log_callback)
        return(NULL)
      }
      return(result)
    }
    if (status == 401L) {
      stop(errorCondition(
        paste("SerpApi authentication failed (HTTP 401).",
              "Verify the selected key in your SerpApi dashboard.",
              "To use SERPAPI_API_KEY, select the server option;",
              "restart the app after changing the environment variable."),
        class = "serp_auth_error"
      ))
    }
    if (status != 429L) {
      serp_log(sprintf("SerpApi request failed with HTTP %d.", status),
               log_file, log_callback)
      return(NULL)
    }
    if (attempt < max_retries) {
      delay <- 2^attempt + stats::runif(1, 2, 5)
      serp_log(sprintf("HTTP 429: retrying in %.2f seconds.", delay),
               log_file, log_callback)
      Sys.sleep(delay)
    }
  }
  serp_log("Max retries exceeded for SerpApi.", log_file, log_callback)
  NULL
}

serp_text <- function(value, default = NA_character_) {
  if (is.null(value) || !length(value) || is.na(value[[1L]])) return(default)
  as.character(value[[1L]])
}

lookup_s2_metadata <- function(title, api_key = "", log_file = NULL,
                               log_callback = NULL) {
  if (is.na(serp_text(title)) || !nzchar(title)) return(NULL)
  clean_title <- stringr::str_trim(stringr::str_remove_all(title, "\\[.*?\\]"))
  normalize <- function(x) {
    stringr::str_to_lower(stringr::str_replace_all(x, "[^[:alnum:]]", ""))
  }
  norm_target <- normalize(clean_title)
  if (!nzchar(norm_target)) return(NULL)
  headers <- if (nzchar(api_key)) c(`x-api-key` = api_key) else character()
  execute_query <- function(text, quoted = FALSE) {
    for (attempt in 0:2) {
      Sys.sleep(1 + stats::runif(1, 0, 1))
      response <- tryCatch(
        httr::GET(
          "https://api.semanticscholar.org/graph/v1/paper/search",
          query = list(query = text, limit = if (quoted) 1 else 3,
                       fields = "abstract,title,journal,venue,externalIds"),
          httr::add_headers(.headers = headers), httr::timeout(10)
        ),
        error = function(e) NULL
      )
      if (is.null(response)) return(NULL)
      if (httr::status_code(response) == 200L) {
        body <- tryCatch(
          jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"),
                             simplifyVector = FALSE),
          error = function(e) NULL
        )
        for (candidate in body$data) {
          norm_candidate <- normalize(serp_text(candidate$title, ""))
          if (nzchar(norm_candidate) &&
              (norm_target == norm_candidate ||
               grepl(norm_target, norm_candidate, fixed = TRUE) ||
               grepl(norm_candidate, norm_target, fixed = TRUE))) {
            journal <- if (is.list(candidate$journal)) {
              serp_text(candidate$journal$name, serp_text(candidate$venue))
            } else {
              serp_text(candidate$venue)
            }
            return(list(abstract = serp_text(candidate$abstract),
                        journal = journal,
                        doi = serp_text(candidate$externalIds$DOI)))
          }
        }
        return(NULL)
      }
      if (httr::status_code(response) != 429L) return(NULL)
      if (attempt < 2L) Sys.sleep(5 * (attempt + 1L))
    }
    NULL
  }
  serp_log(sprintf("  Attempting exact metadata match for: %s", clean_title),
           log_file, log_callback)
  result <- execute_query(paste0('"', clean_title, '"'), TRUE)
  if (is.list(result)) return(result)
  serp_log("  Exact match failed; attempting validated unquoted match.",
           log_file, log_callback)
  execute_query(clean_title)
}

lookup_crossref_journal <- function(doi, log_file = NULL) {
  if (is.na(serp_text(doi)) || !nzchar(doi)) return(NULL)
  response <- tryCatch(
    httr::GET(paste0("https://api.crossref.org/works/",
                     utils::URLencode(doi, reserved = TRUE)), httr::timeout(10)),
    error = function(e) NULL
  )
  if (is.null(response) || httr::status_code(response) != 200L) return(NULL)
  body <- tryCatch(
    jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"),
                       simplifyVector = FALSE),
    error = function(e) NULL
  )
  journal <- serp_text(body$message$`container-title`)
  if (is.na(journal) || !nzchar(journal)) NULL else journal
}

serp_empty_papers <- function() {
  dplyr::tibble(
    title = character(), authors = list(), year = integer(),
    publicationTypes = list(), journal = list(), externalIds = list(),
    abstract = character(), venue = character(), search_query = character(),
    search_rank = integer()
  )
}

serp_display_papers <- function(papers) {
  dplyr::tibble(
    search_query = papers$search_query,
    first_author_last_name = purrr::map_chr(papers$authors, function(authors) {
      if (!length(authors)) return(NA_character_)
      trimws(strsplit(authors[1L], ",", fixed = TRUE)[[1L]][1L])
    }),
    publication_year = papers$year, title = papers$title,
    journal_name = purrr::map_chr(papers$journal, function(j) serp_text(j$name)),
    authors_full = purrr::map_chr(papers$authors, paste, collapse = ", "),
    publication_type = purrr::map_chr(papers$publicationTypes, paste, collapse = ", "),
    search_rank = papers$search_rank, abstract = papers$abstract,
    doi = purrr::map_chr(papers$externalIds, function(ids) serp_text(ids$DOI))
  )
}

process_single_search_term <- function(
    query, api_key, paper_limit = 100, use_dynamic_papers_limit = FALSE,
    dynamic_fraction = 0.1, dynamic_min = 20, dynamic_max = 200,
    use_full_abstract_lookup = TRUE, s2_api_key = "",
    log_file = NULL, log_callback = NULL) {
  serp_log(paste("Searching for:", query), log_file, log_callback)
  limit <- paper_limit
  hits <- list()
  total_matches <- NA_real_
  start_idx <- 0L
  failed <- FALSE
  while (length(hits) < limit) {
    result <- robust_serp_search(
      query, api_key, start_idx = start_idx, num = 20,
      log_file = log_file, log_callback = log_callback
    )
    if (is.null(result)) {
      failed <- TRUE
      break
    }
    if (start_idx == 0L) {
      total_matches <- suppressWarnings(as.numeric(
        serp_text(result$search_information$total_results)
      ))
      if (!is.finite(total_matches) || total_matches < 0) total_matches <- NA_real_
      serp_log(sprintf("  Total matches: %s", total_matches), log_file, log_callback)
      if (use_dynamic_papers_limit && !is.na(total_matches)) {
        limit <- min(dynamic_max, max(dynamic_min,
                                      ceiling(total_matches * dynamic_fraction)))
        limit <- min(limit, total_matches)
        serp_log(sprintf("  Dynamic paper limit: %d", limit), log_file, log_callback)
      }
    }
    page <- result$organic_results
    if (!is.list(page) || !length(page) || limit == 0) break
    hits <- utils::head(c(hits, page), limit)
    serp_log(sprintf("  Retrieved %d papers.", length(hits)), log_file, log_callback)
    if (length(hits) >= limit || is.null(result$serpapi_pagination$`next`)) break
    start_idx <- start_idx + length(page)
    Sys.sleep(stats::runif(1, 1, 2))
  }
  reformat_author <- function(name) {
    name <- trimws(stringr::str_remove_all(name, "\\.\\.\\.|…"))
    if (!nzchar(name)) return(NA_character_)
    if (grepl(",", name, fixed = TRUE)) return(name)
    parts <- strsplit(name, "\\s+")[[1L]]
    if (length(parts) == 1L) return(name)
    paste0(utils::tail(parts, 1), ", ", paste(utils::head(parts, -1), collapse = " "))
  }
  parsed <- purrr::imap(hits, function(hit, index) {
    if (!is.list(hit)) return(NULL)
    title <- serp_text(hit$title)
    summary <- serp_text(hit$publication_info$summary)
    parts <- if (!is.na(summary)) strsplit(summary, "\\s+-\\s+")[[1L]] else character()
    names <- purrr::map_chr(hit$publication_info$authors, function(author) {
      if (is.list(author)) serp_text(author$name, "") else serp_text(author, "")
    })
    if (length(parts)) names <- c(names, strsplit(parts[1L], ",", fixed = TRUE)[[1L]])
    authors <- unique(purrr::map_chr(names[!is.na(names)], reformat_author))
    authors <- authors[!is.na(authors) & nzchar(authors)]
    journal <- if (length(parts) >= 2L) {
      trimws(stringr::str_remove(parts[2L], ",?\\s*\\b\\d{4}\\b"))
    } else NA_character_
    if (!is.na(journal) && !nzchar(journal)) journal <- NA_character_
    metadata <- if (use_full_abstract_lookup) {
      lookup_s2_metadata(title, api_key = s2_api_key,
                         log_file = log_file, log_callback = log_callback)
    } else NULL
    doi <- serp_text(metadata$doi)
    if (is.na(journal) || stringr::str_detect(journal, "\\.\\.\\.|…")) {
      crossref <- if (!is.na(doi)) lookup_crossref_journal(doi, log_file) else NULL
      journal <- serp_text(crossref, serp_text(metadata$journal, journal))
    }
    journal_name <- stringr::str_trim(stringr::str_remove_all(journal, "\\.\\.\\.|…"))
    dplyr::tibble(
      title = title, authors = list(authors),
      year = as.integer(stringr::str_extract(summary, "\\b(19|20)\\d{2}\\b")),
      publicationTypes = list("Journal Article"),
      journal = list(list(name = journal_name)), externalIds = list(list(DOI = doi)),
      abstract = serp_text(metadata$abstract, serp_text(hit$snippet)), venue = journal_name,
      search_query = query, search_rank = as.integer(index)
    )
  })
  full_data <- dplyr::bind_rows(c(list(serp_empty_papers()), parsed))
  list(
    full_data = full_data, display_data = serp_display_papers(full_data),
    summary = dplyr::tibble(search_query = query, total_matches = total_matches,
                           retrieved = nrow(full_data), failed = failed),
    failed = failed
  )
}

create_ris_record <- function(paper) {
  lines <- "TY  - JOUR"
  add <- function(tag, value) {
    value <- serp_text(value)
    if (!is.na(value) && nzchar(value)) {
      lines <<- c(lines, paste0(tag, "  - ",
                               stringr::str_replace_all(value, "\\s+", " ")))
    }
  }
  add("TI", paper$title)
  for (author in unlist(paper$authors)) add("AU", author)
  add("PY", paper$year)
  add("JO", if (is.list(paper$journal)) paper$journal$name else paper$journal)
  add("DO", paper$externalIds$DOI)
  add("AB", paper$abstract)
  rank <- if (!is.null(paper$highest_search_rank)) {
    paper$highest_search_rank
  } else paper$search_rank
  if (!is.na(serp_text(rank))) add("M2", sprintf("search_rank: %04d", as.integer(rank)))
  paste(c(lines, "ER  - "), collapse = "\n")
}

save_search_outputs <- function(results_list, output_dir, session_timestamp) {
  if (!grepl("^[A-Za-z0-9_-]+$", session_timestamp)) stop("Invalid session timestamp.")
  if (!dir.exists(output_dir) &&
      !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("Cannot create the output directory.")
  }
  full_data <- dplyr::bind_rows(c(
    list(serp_empty_papers()), purrr::map(results_list, "full_data")
  ))
  display_data <- serp_display_papers(full_data)
  summaries <- dplyr::bind_rows(c(
    list(dplyr::tibble(search_query = character(), total_matches = double(),
                       retrieved = integer(), failed = logical())),
    purrr::map(results_list, "summary")
  ))
  deduplicate <- function(data, year_column) {
    data <- data[!is.na(data$title) & nzchar(trimws(data$title)), , drop = FALSE]
    if (!nrow(data)) {
      data$associated_queries <- character()
      data$highest_search_rank <- integer()
      return(data)
    }
    grouped <- dplyr::group_by_at(data, c("title", year_column))
    grouped <- dplyr::mutate(
      grouped, associated_queries = paste(unique(search_query), collapse = "; "),
      highest_search_rank = min(search_rank)
    )
    dplyr::ungroup(dplyr::slice_min(grouped, order_by = search_rank,
                                   n = 1, with_ties = FALSE))
  }
  unique_full <- deduplicate(full_data, "year")
  unique_papers <- deduplicate(display_data, "publication_year")
  paths <- c(
    log = file.path(output_dir, paste0("search_log_", session_timestamp, ".txt")),
    ris = file.path(output_dir, paste0("unique_results_", session_timestamp, ".ris")),
    summary = file.path(output_dir, paste0("search_summary_", session_timestamp, ".csv")),
    papers = file.path(output_dir, paste0("unique_papers_", session_timestamp, ".csv")),
    checkpoint = file.path(output_dir, paste0("search_state_", session_timestamp, ".rds"))
  )
  records <- purrr::map_chr(purrr::transpose(unique_full), create_ris_record)
  writeLines(paste(records, collapse = "\n\n"), paths[["ris"]], useBytes = TRUE)
  csv_safe <- function(data) {
    # CSV quoting alone does not prevent spreadsheet formulas in API metadata.
    for (column in names(data)) {
      if (!is.character(data[[column]])) next
      formula <- !is.na(data[[column]]) &
        grepl("^[[:space:]]*[=+@-]", data[[column]])
      data[[column]][formula] <- paste0("'", data[[column]][formula])
    }
    data
  }
  utils::write.csv(csv_safe(summaries), paths[["summary"]], row.names = FALSE, na = "",
                   fileEncoding = "UTF-8")
  utils::write.csv(csv_safe(unique_papers), paths[["papers"]], row.names = FALSE, na = "",
                   fileEncoding = "UTF-8")
  saveRDS(results_list, paths[["checkpoint"]])
  serp_log(sprintf("Saved %d unique papers and %d query summaries.",
                   nrow(unique_papers), nrow(summaries)), paths[["log"]])
  list(unique_full_data_output = unique_full, unique_papers_output = unique_papers,
       final_search_summary = summaries, combined_full_data = full_data,
       combined_search_results = display_data, paths = paths)
}
