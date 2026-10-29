pkgload::load_all()
# Create a dummy paper
paper <- dplyr::tibble(
  title = "A test paper", authors = list(c("Smith", "Jones")), year = 2026,
  publicationTypes = list("Journal Article"), journal = list(list(name="Test Journal")), 
  externalIds = list(list(DOI="10.123/456")), abstract = "Abstract text", 
  full_abstract_retrieved = TRUE, venue = "Test Journal", search_query = "test",
  search_rank = 1L
)

results_list <- list(
  list(
    full_data = paper,
    display_data = serp_display_papers(paper),
    summary = dplyr::tibble(search_query="test", total_matches=1, retrieved=1, percent_with_abstract="100%", search_status="successful"),
    failed = FALSE, out_of_credits = FALSE
  )
)
tmp <- tempdir()
out <- save_search_outputs(results_list, tmp, "20261028_test2")
print("Success!")
