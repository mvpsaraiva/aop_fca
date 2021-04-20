library(tidyverse)
library(data.table)
library(sf)
library(h3jsr)

library(mapview)
library(viridis)
library(scales)
library(patchwork)

source("./R/fun/fca.R")

# 2SFCA function ----------------------------------------------------------
ttm <- load_tt_matrix(muni = "poa", transport_mode = "walk", opportunity = "abs")


twostep_fca_accessibility <- function(tt_matrix, fun = "binary", time_threshold = 5, max_threshold = 60) {
  
  time_step <- 1
  ## make a copy of the travel time matrix to preserve original data
  tt_matrix <- copy(tt_matrix) %>% setDT()
  # tt_matrix <- copy(ttm) %>% setDT()
  
  total_population <- tt_matrix %>% select(origin, population) %>%
    distinct() %>%
    summarise(population = sum(population)) %>%
    .$population 
  
  total_jobs <- tt_matrix %>% select(destination, opportunities) %>%
    distinct() %>%
    summarise(opportunities = sum(opportunities)) %>%
    .$opportunities 
  
  city_ppr <- total_jobs / total_population
  city_bp <- total_population / total_jobs
  
  ## calculate catchment areas
  tt_matrix[, orig_catchment := time_threshold]
  tt_matrix[, dest_catchment := time_threshold]
  
  ## do destination
  done <- FALSE
  while (!done) {
    dest_ppr <- tt_matrix[tt_median <= dest_catchment, .(opportunities = first(opportunities), 
                                                         pop_served = sum(population)), by = destination]
    dest_ppr[, bp := opportunities * city_bp]
    dest_ppr <- dest_ppr[pop_served < bp]
    tt_matrix[destination %in% dest_ppr$destination, dest_catchment := dest_catchment + time_step]
    
    done <- nrow(tt_matrix[destination %in% dest_ppr$destination & dest_catchment < max_threshold]) == 0
  }
  
  
  local_ppr <- tt_matrix[tt_median <= dest_catchment, .(ppr = first(opportunities) / sum(population)), by = destination ]
  tt_matrix[local_ppr, on = "destination", ppr := i.ppr]
  
  ## do origin
  done <- FALSE
  while (!done) {
    origin_bp <- tt_matrix[tt_median <= orig_catchment, .(population = first(population), 
                                                          ppr_accessed = sum(ppr)), by = origin]
    origin_bp <- origin_bp[ppr_accessed < city_ppr]
    tt_matrix[origin %in% origin_bp$origin, orig_catchment := orig_catchment + time_step]
    
    done <- nrow(tt_matrix[origin %in% origin_bp$origin & orig_catchment < max_threshold]) == 0
  }
  
  ## tt_matrix as output by load_tt_matrix() function
  ## fun can be "binary" or "gaussian"
  ## time_threshold in minutes
  
  # impedance (binary or gaussian function) ----
  if (fun == "binary") {
    ## impedances for origin and destination
    tt_matrix[, impedance := fifelse(tt_median <= time_threshold, 1, 0)]
  } else {
    ## impedances for origin and destination
    tt_matrix[, impedance := gaussian_weight(tt_median, b = time_threshold)]
  }
  
  ## Step 1 - reaportion the demand to jobs proportionally to impedance ----
  tt_matrix <- tt_matrix[impedance > 0]
  tt_matrix[, pop_served := population * impedance]
  tt_matrix[, ppr := opportunities / pop_served]
  
  fca <- tt_matrix[, .(metric = "2sfca",
                       fun = fun,
                       threshold = time_threshold,
                       accessibility = sum(ppr, na.rm=T)), by=origin]
}