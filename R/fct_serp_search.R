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

normalize_serpapi_key <- function(api_key) {
  if (is.null(api_key)) return("")
  api_key <- trimws(api_key, whitespace = "[\\h\\v]")
  api_key <- sub("^SERPAPI_API_KEY\\s*=\\s*", "", api_key, perl = TRUE)
  api_key <- sub("^(['\"])(.*)\\1$", "\\2", api_key, perl = TRUE)
  trimws(api_key, whitespace = "[\\h\\v]")
}

robust_serp_search <- function(query, api_key, start_idx = 0, num = 20,
                               log_file = NULL, log_callback = NULL,
                               max_retries = 3, year_low = NULL, year_high = NULL) {
  api_key <- normalize_serpapi_key(api_key)
  if (!nzchar(api_key)) stop("A SerpApi key is required.")
  
  query_params <- list(engine = "google_scholar", q = query, api_key = api_key,
                       start = start_idx, num = num, hl = "en")
  
  if (!is.null(year_low) && !is.null(year_high)) {
    query_params$as_ylo <- year_low
    query_params$as_yhi <- year_high
  }

  for (attempt in seq.int(0L, max_retries)) {
    response <- tryCatch(
      httr::GET(
        "https://serpapi.com/search",
        query = query_params,
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
    if (status != 429L) {
      serp_log(sprintf("SerpApi request failed with HTTP %d.", status),
               log_file, log_callback)
      return(NULL)
    }
    
    # Check if the 429 is due to empty credits rather than throughput limits
    error_body <- tryCatch(
      jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.list(error_body) && !is.null(error_body$error) && 
        grepl("run out of searches", error_body$error, ignore.case = TRUE)) {
      serp_log("CRITICAL: SerpApi account has run out of searches.", log_file, log_callback)
      # Return a specific string to signal hard failure to the caller
      return("OUT_OF_CREDITS")
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

reconstruct_openalex_abstract <- function(inverted_index) {
  if (is.null(inverted_index) || length(inverted_index) == 0) return(NA_character_)
  words <- names(inverted_index)
  max_pos <- max(unlist(inverted_index))
  word_vector <- character(max_pos + 1)
  for (w in words) {
    pos <- unlist(inverted_index[[w]])
    word_vector[pos + 1] <- w
  }
  paste(word_vector, collapse = " ")
}

lookup_openalex_metadata <- function(title, doi = NA_character_, link = NA_character_, log_file = NULL, log_callback = NULL) {
  # 1. Attempt exact lookup by DOI first
  extracted_doi <- doi
  if (is.na(extracted_doi) && !is.na(link) && nzchar(link)) {
    m <- regexpr("10\\.\\d{4,9}/[-._;()/:A-Z0-9]+", link, ignore.case = TRUE)
    if (m > 0) extracted_doi <- regmatches(link, m)
  }
  
  if (!is.na(extracted_doi) && nzchar(extracted_doi)) {
    # OpenAlex expects doi: prefix in the path, but requires standardizing
    clean_doi <- stringr::str_remove(extracted_doi, "^https?://(dx\\.)?doi\\.org/")
    
    response <- tryCatch(
      httr::GET(
        paste0("https://api.openalex.org/works/doi:", clean_doi),
        httr::add_headers(`User-Agent` = "mailto:scholarsearchapp@example.com"),
        httr::timeout(10)
      ),
      error = function(e) NULL
    )
    
    if (!is.null(response) && httr::status_code(response) == 200L) {
      body <- tryCatch(jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE), error = function(e) NULL)
      if (is.list(body)) {
        journal <- if (!is.null(body$primary_location) && !is.null(body$primary_location$source)) {
          body$primary_location$source$display_name
        } else NA_character_
        return(list(
          abstract = reconstruct_openalex_abstract(body$abstract_inverted_index),
          journal = serp_text(journal),
          doi = extracted_doi
        ))
      }
    }
  }
  
  # 2. Fall back to title search
  if (is.na(serp_text(title)) || !nzchar(title)) return(NULL)
  clean_title <- stringr::str_trim(stringr::str_remove_all(title, "\\[.*?\\]"))
  if (!nzchar(clean_title)) return(NULL)
  
  response <- tryCatch(
    httr::GET(
      "https://api.openalex.org/works",
      query = list(
        `filter` = paste0("title.search:", clean_title),
        select = "abstract_inverted_index,title,doi,primary_location",
        per_page = 1
      ),
      httr::add_headers(`User-Agent` = "mailto:scholarsearchapp@example.com"),
      httr::timeout(10)
    ),
    error = function(e) NULL
  )
  
  if (!is.null(response) && httr::status_code(response) == 200L) {
    body <- tryCatch(jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE), error = function(e) NULL)
    if (is.list(body) && length(body$results) > 0) {
      hit <- body$results[[1]]
      journal <- if (!is.null(hit$primary_location) && !is.null(hit$primary_location$source)) {
        hit$primary_location$source$display_name
      } else NA_character_
      found_doi <- if (!is.null(hit$doi)) stringr::str_remove(hit$doi, "^https?://doi\\.org/") else NA_character_
      return(list(
        abstract = reconstruct_openalex_abstract(hit$abstract_inverted_index),
        journal = serp_text(journal),
        doi = found_doi
      ))
    }
  }
  
  NULL
}

lookup_crossref_metadata <- function(doi, log_file = NULL) {
  if (is.na(serp_text(doi)) || !nzchar(doi)) return(NULL)
  response <- tryCatch(
    httr::GET(
      paste0("https://api.crossref.org/works/", utils::URLencode(doi, reserved = TRUE)),
      httr::add_headers(`User-Agent` = "ScholarSearchApp/1.0 (mailto:admin@example.com)"),
      httr::timeout(10)
    ),
    error = function(e) NULL
  )
  if (is.null(response) || httr::status_code(response) != 200L) return(NULL)
  body <- tryCatch(
    jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"),
                       simplifyVector = FALSE),
    error = function(e) NULL
  )
  
  journal <- serp_text(body$message$`container-title`)
  if (!is.na(journal) && !nzchar(journal)) journal <- NA_character_
  
  abstract <- serp_text(body$message$abstract)
  if (!is.na(abstract)) {
    abstract <- stringr::str_remove_all(abstract, "<jats:title>.*?</jats:title>|<title>.*?</title>")
    abstract <- stringr::str_remove_all(abstract, "<[^>]+>")
    abstract <- stringr::str_trim(stringr::str_replace_all(abstract, "\\s+", " "))
  }
  
  list(journal = journal, abstract = abstract)
}

serp_empty_papers <- function() {
  dplyr::tibble(
    title = character(), authors = list(), year = integer(),
    publicationTypes = list(), journal = list(), externalIds = list(),
    abstract = character(), full_abstract_retrieved = logical(), venue = character(), search_query = character(),
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
    use_full_abstract_lookup = TRUE,
    log_file = NULL, log_callback = NULL, year_low = NULL, year_high = NULL, progress_callback = NULL) {
  serp_log(paste("Searching for:", query), log_file, log_callback)
  limit <- paper_limit
  hits <- list()
  total_matches <- NA_real_
  start_idx <- 0L
  failed <- FALSE
  while (length(hits) < limit) {
    if (!is.null(progress_callback)) {
      progress_callback(sprintf("Paginating SerpApi: fetched %d / %d requested papers...", length(hits), limit))
    }
    result <- robust_serp_search(
      query, api_key, start_idx = start_idx, num = 20,
      log_file = log_file, log_callback = log_callback,
      year_low = year_low, year_high = year_high
    )
    
    if (identical(result, "OUT_OF_CREDITS")) {
      return(list(failed = TRUE, out_of_credits = TRUE))
    }
    
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
  
  if (!is.null(progress_callback)) {
    progress_callback(sprintf("Found %d papers. Looking up abstracts and DOIs...", length(hits)))
  }
  
  parsed <- purrr::imap(hits, function(hit, index) {
    if (!is.null(progress_callback)) {
      progress_callback(sprintf("Enriching metadata for paper %d of %d...", index, length(hits)))
    }
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
    link <- serp_text(hit$link)
    
    metadata <- if (use_full_abstract_lookup) {
      lookup_openalex_metadata(title = title, link = link, 
                               log_file = log_file, log_callback = log_callback)
    } else NULL
    doi <- serp_text(metadata$doi)
    if (is.na(doi) && !is.na(link) && nzchar(link)) {
      m <- regexpr("10\\.\\d{4,9}/[-._;()/:A-Z0-9]+", link, ignore.case = TRUE)
      if (m > 0) doi <- regmatches(link, m)
    }
    
    crossref <- if (!is.na(doi) && use_full_abstract_lookup) lookup_crossref_metadata(doi, log_file) else NULL
    
    if (is.na(journal) || stringr::str_detect(journal, "\\.\\.\\.|…")) {
      journal <- serp_text(crossref$journal, serp_text(metadata$journal, journal))
    }
    
    final_abstract <- serp_text(metadata$abstract)
    if (is.na(final_abstract) || !nzchar(final_abstract)) {
      final_abstract <- serp_text(crossref$abstract, serp_text(hit$snippet))
    }
    
    journal_name <- stringr::str_trim(stringr::str_remove_all(journal, "\\.\\.\\.|…"))
    dplyr::tibble(
      title = title, authors = list(authors),
      year = as.integer(stringr::str_extract(summary, "\\b(19|20)\\d{2}\\b")),
      publicationTypes = list("Journal Article"),
      journal = list(list(name = journal_name)), externalIds = list(list(DOI = doi)),
      abstract = final_abstract, 
      full_abstract_retrieved = (!is.na(serp_text(metadata$abstract)) && nzchar(serp_text(metadata$abstract))) || 
                                (!is.na(serp_text(crossref$abstract)) && nzchar(serp_text(crossref$abstract))),
      venue = journal_name,
      search_query = query, search_rank = as.integer(index)
    )
  })
  full_data <- dplyr::bind_rows(c(list(serp_empty_papers()), parsed))
  
  # Calculate the percentage of retrieved papers that have a full abstract (not just a snippet)
  abstract_count <- sum(isTRUE(full_data$full_abstract_retrieved) | full_data$full_abstract_retrieved == TRUE, na.rm = TRUE)
  percent_with_abstract <- if (nrow(full_data) > 0) (abstract_count / nrow(full_data)) * 100 else 0
  
  list(
    full_data = full_data, display_data = serp_display_papers(full_data),
    summary = dplyr::tibble(search_query = query, total_matches = total_matches,
                           retrieved = nrow(full_data), 
                           percent_with_abstract = paste0(round(percent_with_abstract, 1), "%"),
                           search_status = if (failed) "failed (see log)" else "successful"),
    failed = failed,
    out_of_credits = FALSE
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

save_search_outputs <- function(results_list, output_dir, session_timestamp, 
                                do_deduplicate = TRUE, file_prefix = "") {
  if (!grepl("^[A-Za-z0-9_-]+$", session_timestamp)) stop("Invalid session timestamp.")
  if (!dir.exists(output_dir) &&
      !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("Cannot create the output directory.")
  }
  full_data <- dplyr::bind_rows(c(
    list(serp_empty_papers()), 
    purrr::map(purrr::keep(results_list, ~!isTRUE(.x$out_of_credits)), "full_data")
  ))
  display_data <- serp_display_papers(full_data)
  summaries <- dplyr::bind_rows(c(
    list(dplyr::tibble(search_query = character(), total_matches = double(),
                       retrieved = integer(), percent_with_abstract = character(), 
                       search_status = character())),
    purrr::map(purrr::keep(results_list, ~!isTRUE(.x$out_of_credits)), "summary")
  ))
  deduplicate <- function(data, year_column) {
    data <- data[!is.na(data$title) & nzchar(trimws(data$title)), , drop = FALSE]
    if (!nrow(data)) {
      data$associated_queries <- character()
      data$highest_search_rank <- integer()
      return(data)
    }
    if (do_deduplicate) {
      grouped <- dplyr::group_by_at(data, c("title", year_column))
      grouped <- dplyr::mutate(
        grouped, associated_queries = paste(unique(search_query), collapse = "; "),
        highest_search_rank = min(search_rank)
      )
      dplyr::ungroup(dplyr::slice_min(grouped, order_by = search_rank,
                                     n = 1, with_ties = FALSE))
    } else {
      data$associated_queries <- data$search_query
      data$highest_search_rank <- data$search_rank
      data
    }
  }
  unique_full <- deduplicate(full_data, "year")
  unique_papers <- deduplicate(display_data, "publication_year")
  dedup_prefix <- if (do_deduplicate) "unique_" else "all_"
  base_prefix <- if (nzchar(file_prefix)) paste0(file_prefix, "_") else ""
  
  paths <- c(
    log = file.path(output_dir, paste0(base_prefix, "search_log_", session_timestamp, ".txt")),
    ris = file.path(output_dir, paste0(base_prefix, dedup_prefix, "results_", session_timestamp, ".ris")),
    summary = file.path(output_dir, paste0(base_prefix, "search_summary_", session_timestamp, ".csv")),
    papers = file.path(output_dir, paste0(base_prefix, dedup_prefix, "papers_", session_timestamp, ".csv")),
    checkpoint = file.path(output_dir, paste0(base_prefix, "search_state_", session_timestamp, ".rds"))
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
  serp_log(sprintf("Saved %d papers and %d query summaries.",
                   nrow(unique_papers), nrow(summaries)), paths[["log"]])
  list(unique_full_data_output = unique_full, unique_papers_output = unique_papers,
       final_search_summary = summaries, combined_full_data = full_data,
       combined_search_results = display_data, paths = paths)
}
