library(DT)
pkgload::load_all()
paper <- dplyr::tibble(
  title = "A test paper", authors = list(c("Smith", "Jones")), year = 2026,
  publicationTypes = list("Journal Article"), journal = list(list(name="Test Journal")), 
  externalIds = list(list(DOI="10.123/456")), abstract = "Abstract text", 
  full_abstract_retrieved = TRUE, venue = "Test Journal", search_query = "test",
  search_rank = 1L
)
df <- serp_display_papers(paper)
print("Creating DT")
tryCatch({
  d <- datatable(df, filter='top')
  print("DT created")
}, error = function(e) print(e))
