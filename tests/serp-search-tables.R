mod_serp_search_ui <- getFromNamespace("mod_serp_search_ui", "scholarsearchapp")
mod_serp_search_server <- getFromNamespace("mod_serp_search_server", "scholarsearchapp")

ui <- as.character(mod_serp_search_ui("serp_search_1"))
for (id in c("summary_table", "results_table")) {
  stopifnot(grepl(paste0('id="serp_search_1-', id, '"'), ui, fixed = TRUE))
}
stopifnot(!grepl("gt_shiny", ui, fixed = TRUE))

shiny::testServer(mod_serp_search_server, {
  results(list(
    final_search_summary = data.frame(query = "summary-query"),
    unique_papers_output = data.frame(title = paste0("paper-marker-", seq_len(101), "-end"))
  ))
  session$flushReact()

  summary_html <- output$summary_table$html
  papers_html <- output$results_table$html
  stopifnot(
    grepl("<table", summary_html, fixed = TRUE),
    grepl("summary-query", summary_html, fixed = TRUE),
    grepl("<table", papers_html, fixed = TRUE),
    grepl("paper-marker-100-end", papers_html, fixed = TRUE),
    !grepl("paper-marker-101-end", papers_html, fixed = TRUE),
    !grepl("gtShiny", summary_html, fixed = TRUE),
    !grepl("gtShiny", papers_html, fixed = TRUE)
  )

  results(list(
    final_search_summary = data.frame(query = "updated-query"),
    unique_papers_output = data.frame(title = "updated-paper")
  ))
  session$flushReact()
  stopifnot(
    grepl("updated-query", output$summary_table$html, fixed = TRUE),
    grepl("updated-paper", output$results_table$html, fixed = TRUE),
    !grepl("paper-marker-", output$results_table$html, fixed = TRUE)
  )
})
