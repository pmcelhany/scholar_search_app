

# Repository Guidelines: R Shiny Golem App

## Token & Scope Constraints (Strict)

- **Scoped Edits:** Modify ONLY files explicitly referenced in the task
  issue. Do NOT refactor untouched files.
- **Context Economy:** Do NOT inspect files outside the target module
  unless required to trace shared variables/inputs.
- **No Unsolicited Dependencies:** Do NOT add new R package dependencies
  without explicit approval.

## Architecture & File Mapping

- **Framework:** Golem Modular Shiny App. Heavy processing delegates to
  R Quarto (`.qmd`).
- **Core Modules:**
  - `lit-tag-builder` (`R/builder_ui.R`, `R/builder_server.R`): CSV/XLSX
    ingest, lit-tag DB creation/editing.
  - `lit-tag-viewer` (`R/viewer_ui.R`, `R/viewer_server.R`): Search,
    ggplot dynamic viz, Quarto report generation
    (`report/lit_tag_report_template.qmd`).
- **Ignored Directories (DO NOT READ OR EDIT):**
  - `dev/` (Golem scratch scripts)
  - `diagram/` (Graphviz DOT files)
  - `report/*.html`, `report/*.pdf`, `report/*.docx` (Generated report
    outputs)

## Coding Standards & Anti-Patterns

### Reactivity & Golem Rules

- **Namespacing:** Always wrap UI element IDs in `ns()` inside UI
  modules. Inside server functions, access via `input$id` without
  `ns()`.
- **Reactivity:** Use `eventReactive()` and `observeEvent()` to isolate
  side effects. Never run heavy computations directly inside bare
  `reactive()` or `render*` functions without `bindEvent()`.
- **Package Management:**
  - All R packages must be declared in `DESCRIPTION` (Imports/Suggests).
  - Use `pkg::function()` syntax. Never call `library()` inside `R/`
    scripts.
- **R Execution:** Do NOT add `source()` calls in `R/`; Golem auto-loads
  all files in `R/`.

## Automated Verification Protocol

Before finalizing any pull request, execute the following commands to
confirm code integrity:

### 1. Check syntax and package loadability

Rscript -e “devtools::load_all()”

### 2. Verify dependencies and namespace match DESCRIPTION

Rscript -e “devtools::document()”

### 3. Parse ALL R files in the R/ directory for syntax errors

Rscript -e “invisible(lapply(list.files(‘R’, pattern = ‘\\R\$’,
full.names = TRUE), parse))”
