#create a queries test file

library(tidyverse)
library(here)


d_queries <- data.frame(q = c("jello AND cat",
"lobster AND mcdr"))

write_csv(
  d_queries, 
  file = here("data/test_queries.csv"), 
  col_names = FALSE
)
