response <- httr::GET(
  "https://api.openalex.org/works/doi:10.1007/s00521-024-09893-7",
  httr::add_headers(`User-Agent` = "mailto:test@example.com")
)
body <- jsonlite::fromJSON(httr::content(response, as = "text"), simplifyVector = FALSE)
idx <- body$abstract_inverted_index
# Correctly parse the inverted index list
words <- names(idx)
max_pos <- max(unlist(idx))
word_vector <- character(max_pos + 1)
for (w in words) {
  pos <- unlist(idx[[w]])
  word_vector[pos + 1] <- w
}
print(substring(paste(word_vector, collapse = " "), 1, 100))
