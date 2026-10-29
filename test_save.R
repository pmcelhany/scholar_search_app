pkgload::load_all()
results_list <- list(
  list(
    full_data = serp_empty_papers(),
    display_data = serp_display_papers(serp_empty_papers()),
    summary = dplyr::tibble(search_query="test", total_matches=1, retrieved=0, percent_with_abstract="0%", search_status="successful"),
    failed = FALSE, out_of_credits = FALSE
  )
)
tmp <- tempdir()
out <- save_search_outputs(results_list, tmp, "20261028_test")
print("Success!")
