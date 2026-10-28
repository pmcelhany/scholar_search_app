response <- httr::GET("https://api.crossref.org/works/10.1007/s00521-024-09893-7")
body <- jsonlite::fromJSON(httr::content(response, as = "text"), simplifyVector = FALSE)
print(names(body$message))
if (!is.null(body$message$abstract)) {
  print(substring(body$message$abstract, 1, 100))
} else {
  print("No abstract in CrossRef")
}
