response <- httr::GET("https://api.semanticscholar.org/graph/v1/paper/DOI:10.1007/s00521-024-09893-7",
          query = list(fields = "abstract,title,journal,venue,externalIds"))
cat(httr::status_code(response), "\n")
print(str(jsonlite::fromJSON(httr::content(response, as="text"))))
