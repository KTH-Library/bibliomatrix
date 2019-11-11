library(readr)

indicators <- read_csv("data-raw/bibmon_terms.csv")

usethis::use_data(indicators, overwrite = TRUE, internal = TRUE)
