library(tidyverse)
library(data.table)
library(sf)
library(h3jsr)

library(mapview)
library(viridis)
library(scales)
library(patchwork)

source("./R/fun/fca.R")

# ttm <- load_tt_matrix(muni = "poa")
# grid <- load_hex_grid(muni = "poa")

purrr::walk(munis, function(x) {
  access <- rbind(
    calculate_access(muni = x, mode = "walk", jobs = "abs"),
    calculate_access(muni = x, mode = "walk", jobs = "rel")
  )
    

  write_csv(access, here::here("data/access", sprintf("%s_walk_access.csv", x)))
})





