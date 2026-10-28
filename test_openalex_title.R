# Test a title search in OpenAlex
title <- "Field notes on public communication and engagement for the LOC-NESS Ocean Alkalinity Enhancement field trial"
response <- httr::GET(
  "https://api.openalex.org/works",
  query = list(
    `filter` = paste0("title.search:", title),
    select = "abstract_inverted_index,title,doi,primary_location"
  ),
  httr::add_headers(`User-Agent` = "mailto:test@example.com")
)
body <- jsonlite::fromJSON(httr::content(response, as = "text"), simplifyVector = FALSE)
print(length(body$results))
if (length(body$results) > 0) print(body$results[[1]]$title)
