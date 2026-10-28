clean_abstract <- function(text) {
  if (is.null(text) || is.na(text)) return(NA_character_)
  # Remove JATS tags like <jats:title>Abstract</jats:title> or <jats:p>
  text <- stringr::str_remove_all(text, "<jats:title>.*?</jats:title>")
  text <- stringr::str_remove_all(text, "<[^>]+>")
  text <- stringr::str_trim(text)
  text <- stringr::str_replace_all(text, "\\s+", " ")
  text
}
abstract <- "<jats:title>Abstract</jats:title>\n                  <jats:p>Artificial neural networks (ANN), machine learning</jats:p>"
print(clean_abstract(abstract))
