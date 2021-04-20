library(tidyverse)
library(data.table)

lu_files <- list.files(here::here("data-raw"), pattern = "landuse.csv", full.names = TRUE)
lu_munis <- list.files(here::here("data-raw"), pattern = "landuse.csv", full.names = FALSE) %>%
  str_sub(1, 3)

walk2(lu_files, lu_munis, function(x, y) {
  z <- fread(x)
  z[, city := y]
  
  x <- str_replace(x, ".csv", ".rds")
  write_rds(z, file=x, compress = "gz")
})


access_files <- list.files(here::here("data/access"), pattern = ".csv", full.names = TRUE)

walk(access_files, function(x) {
  data <- fread(x)
  x <- str_replace(x, ".csv", ".rds")
  
  write_rds(data, file=x, compress = "gz")
}) 

access_df <- map(access_files, fread) %>% rbindlist()