# Issue: Implement SerpApi Literature Search Module (`lit-search`)

## Context & Objectives
We need to migrate the literature search pipeline currently implemented in the `lit_search_serp.qmd` script into a fully functioning, high-quality Golem Shiny module. 

The module must allow users to upload a CSV containing boolean search terms (one per row), configure specific search and dynamic limit parameters, specify a local output directory, execute the multi-query search with progress tracking, and save all artifacts (RIS, CSV, RDS, Log) directly to the chosen directory.

**Reference Documents:** 
- `copilot-instructions.md` (Strict architectural and token constraints)
- `lit_search_serp.qmd` (Source of truth for business logic and API integrations)

---

## Strict Token, Scope & Architecture Constraints

- **Scope Boundary:** Modify ONLY the files explicitly listed in the **Target Files** section. Do NOT refactor untouched files or inspect modules outside this scope (`dev/`, `diagram/`, `report/`).
- **Golem & Reactivity Standards:**
  - Wrap all UI IDs in `ns()`. Inside the server module, reference them as `input$<id>` (no `ns()`).
  - Isolate search execution side effects inside `observeEvent(input$run_search, { ... })`. Never place long-running API searches in bare `reactive()` or `render*()` blocks.
  - R Execution: Do NOT add `source()` or `library()` calls in `R/`. Use strict `pkg::function()` syntax. Golem auto-loads the `R/` directory.
  - Declare all required R package dependencies in `DESCRIPTION` (Imports/Suggests).

---

## Target Files

| Action | File Path | Purpose |
| :--- | :--- | :--- |
| **Create** | `R/fct_serp_search.R` | Core search, Semantic Scholar lookup, CrossRef lookup, RIS formatting, and file export logic extracted from `lit_search_serp.qmd`. |
| **Create** | `R/mod_serp_search_ui.R` | UI module function (`mod_serp_search_ui`). |
| **Create** | `R/mod_serp_search_server.R` | Server module function (`mod_serp_search_server`). |
| **Update** | `R/app_ui.R` | Add the new search module tab/UI to the main application layout. |
| **Update** | `R/app_server.R` | Invoke the server module: `mod_serp_search_server("serp_search_1")`. |
| **Update** | `DESCRIPTION` | Ensure required dependencies (e.g., `httr`, `jsonlite`, `stringr`, `dplyr`, `purrr`) are in `Imports`. |

---

## Functional Requirements & UI Specification

### 1. Module UI (`R/mod_serp_search_ui.R`)
Inside `mod_serp_search_ui <- function(id)`, organize inputs into a clean layout with the following controls:

- **Input File Chooser:** 
  - `shiny::fileInput(ns("csv_file"), "Search Terms CSV File", accept = c(".csv", "text/csv"))` (Must contain one boolean search term per row).
- **API Credentials:**
  - `shiny::passwordInput(ns("serpapi_key"), "SerpApi Key", value = Sys.getenv("SERPAPI_API_KEY", ""))`
- **Search Parameters:**
  - `shiny::textInput(ns("queries_pat"), "Filter Queries by Regex/Pattern (queries_pat)", value = "")`
  - `shiny::numericInput(ns("paper_limit"), "Fixed Paper Limit", value = 100, min = 1)`
- **Dynamic Limit Controls:**
  - `shiny::checkboxInput(ns("use_dynamic_papers_limit"), "Enable Dynamic Paper Limit", value = FALSE)`
  - `shiny::conditionalPanel` (evaluating the dynamic flag):
    - `shiny::numericInput(ns("dynamic_fraction"), "Dynamic Fraction", value = 0.1, min = 0.01, max = 1.0, step = 0.05)`
    - `shiny::numericInput(ns("dynamic_min"), "Dynamic Min Papers", value = 20, min = 1)`
    - `shiny::numericInput(ns("dynamic_max"), "Dynamic Max Papers", value = 200, min = 10)`
- **Metadata Enrichment:**
  - `shiny::checkboxInput(ns("use_full_abstract_lookup"), "Lookup Full Abstract (Semantic Scholar / CrossRef)", value = TRUE)`
- **Output Directory Chooser:**
  - `shiny::textInput(ns("output_dir"), "Output Directory Path", value = "output/lit_search")`
- **Action Trigger:**
  - `shiny::actionButton(ns("run_search"), "Run Search", icon = shiny::icon("play"), class = "btn-primary")`

### 2. Main Panel Display
- Summary status / real-time execution progress banner (`shiny::uiOutput(ns("status_banner"))`).
- Log console output to show search progress (`shiny::verbatimTextOutput(ns("log_output"))`).
- Search summary table (`gt::gt_output(ns("summary_table"))`).
- Unique deduplicated papers preview table (`gt::gt_output(ns("results_table"))`).

---

## Backend Business Logic (`R/fct_serp_search.R`)

Extract and adapt functions from `lit_search_serp.qmd`. Pass credentials and parameters explicitly.

1. **`parse_query_csv(file_path, pattern = NULL)`**: Read the CSV, filter by `pattern` (if provided), and return a vector of boolean search queries.
2. **`robust_serp_search(...)`**: Call SerpApi (`https://serpapi.com/search`) via `httr::GET()`. Implement exponential backoff on HTTP 429.
3. **`lookup_s2_metadata(...)` & `lookup_crossref_journal(...)`**: Port the Semantic Scholar and CrossRef API logic verbatim for metadata enrichment.
4. **`process_single_search_term(...)`**: 
   - Apply dynamic paper limit math: `limit = min(dynamic_max, max(dynamic_min, ceiling(total_matches * dynamic_fraction)))`
   - Extract authors, journal, DOI, year, and abstract.
5. **`save_search_outputs(results_list, output_dir, session_timestamp)`**: 
   - Ensure `output_dir` exists. 
   - Aggregate, deduplicate, and write the 5 artifacts: `.txt` log, `.ris` citations, `search_summary.csv`, `unique_papers.csv`, and `.rds` checkpoint.

---

## Server Reactivity (`R/mod_serp_search_server.R`)

- Inside `observeEvent(input$run_search, { ... })`, validate all inputs (e.g., CSV uploaded, API key provided, output directory valid). Use `shiny::showNotification()` for errors.
- Wrap the main execution loop in `shiny::withProgress()` and increment progress (`shiny::incProgress()`) for each search query processed.
- Capture the search logs and dynamically append them to a reactive value so they can be rendered in `ns("log_output")`.
- Upon completion, render the deduplicated summary and results tables.

---

## Automated Verification Protocol

Before finalizing the pull request, execute the following commands in the R console to confirm code integrity:

```R
# 1. Check syntax and package loadability
Rscript -e "devtools::load_all()"

# 2. Verify dependencies and namespace match DESCRIPTION
Rscript -e "devtools::document()"

# 3. Parse ALL R files in the R/ directory for syntax errors
Rscript -e "invisible(lapply(list.files('R', pattern = '\\.R$', full.names = TRUE), parse))"
```