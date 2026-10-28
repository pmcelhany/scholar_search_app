extract_doi <- function(url) {
  if (is.null(url) || is.na(url)) return(NA_character_)
  # Common DOI regex
  m <- regexpr("10\\.\\d{4,9}/[-._;()/:A-Z0-9]+", url, ignore.case = TRUE)
  if (m > 0) return(regmatches(url, m))
  NA_character_
}
print(extract_doi("https://link.springer.com/article/10.1007/s00521-024-09893-7"))
print(extract_doi("https://doi.org/10.1016/j.envres.2023.116664"))
print(extract_doi("https://somethingelse.com/paper"))
