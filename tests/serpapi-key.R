normalize_serpapi_key <- getFromNamespace("normalize_serpapi_key", "scholarsearchapp")

for (value in c(
  "user-key", " \tuser-key\r\n", '"user-key"', "'user-key'",
  'SERPAPI_API_KEY="user-key"', "SERPAPI_API_KEY = 'user-key'",
  "\u00a0user-key\u00a0", '" user-key "'
)) {
  stopifnot(identical(normalize_serpapi_key(value), "user-key"))
}

for (value in list(NULL, "", " \t\r\n", '""', "''", 'SERPAPI_API_KEY=""')) {
  stopifnot(identical(normalize_serpapi_key(value), ""))
}

stopifnot(identical(normalize_serpapi_key("invalid-key"), "invalid-key"))
