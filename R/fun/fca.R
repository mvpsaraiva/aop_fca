munis <-
  c("for",
    "spo",
    "rio",
    "cur",
    "poa",
    "bho",
    "bsb",
    "sal",
    "man",
    "rec",
    "goi",
    "bel",
    "gua",
    "cam",
    "slz",
    "sgo",
    "mac",
    "duq",
    "cgr",
    "nat"
  )


# load data function----------------------------------------------------------
load_tt_matrix <- function(muni, transport_mode = "walk", opportunity = "abs", 
                           filtered = TRUE) {
  ## land use and population data
  land_use <- fread(here::here("data-raw", sprintf("%s_landuse.csv", muni)))

  ## travel time matrix
  tt_matrix <- fread(here::here("data-raw", sprintf("%s_%s_ttm.csv", muni, transport_mode)))


  tt_matrix[land_use, on = c("origin" = "id_hex"), population := i.pop_total]
  
  if (opportunity == "abs") 
  { tt_matrix[land_use, on = c("destination" = "id_hex"), opportunities := empregos_total] }
  if (opportunity == "rel") 
  { tt_matrix[land_use, on = c("destination" = "id_hex"), opportunities := empregos_percent] }

  if (filtered) {
    tt_matrix <- subset(tt_matrix, population > 0 & opportunities > 0)
  }
  
  return(tt_matrix)
}

load_hex_grid <- function(muni) {
  ## land use and population data
  grid <- fread(here::here("data-raw", sprintf("%s_landuse.csv", muni)))
  grid$geometry <- h3jsr::h3_to_polygon(grid$id_hex)

  grid <- sf::st_as_sf(grid)
  
  return(grid)
}


# Gaussian weight function ------------------------------------------------

## gaussian weight function
## tt is the travel time
## b is the "bandwidth", defining the function's rate of decay
gaussian_weight <- function(tt, b) {exp((-0.5) * (tt / b) * (tt / b))}


# 2SFCA function ----------------------------------------------------------

twostep_fca_accessibility <- function(tt_matrix, fun = "binary", time_threshold = 30) {
  
  ## make a copy of the travel time matrix to preserve original data
  tt_matrix <- copy(tt_matrix) %>% setDT()
  
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

# BFCA function---------------------------------------------------------------

bfca_accessibility <- function(tt_matrix, fun = "binary", time_threshold = 30) {
  
  ## make a copy of the travel time matrix to preserve original data
  tt_matrix <- copy(tt_matrix) %>% setDT()
  
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
  
  # calculate weights i (normalized impedance by origin id) ----
  # tt_matrix[, wi := (impedance*opportunities)/sum(impedance*opportunities), by=origin]
  tt_matrix[, wi := impedance/sum(impedance), by=origin]
  
  # calculate weights j (normalized impedance by destination) ----
  # tt_matrix[, wj := (impedance*population)/sum(impedance*population), by=destination]
  tt_matrix[, wj := impedance/sum(impedance), by=destination]
  
  ## Step 1 - reaportion the demand to jobs proportionally to weight i ----
  tt_matrix[, pop_served := population * wi]
  tt_matrix <- tt_matrix[pop_served > 0]
  
  ## Step 2 - calculate provider-to-population ration (ppr) at each destination ----
  tt_matrix[, ppr := opportunities / pop_served]
  tt_matrix[, ppr_w := ppr * wj]
  
  bfca <- tt_matrix[, .(metric = "bfca",
                        fun = fun,
                        threshold = time_threshold,
                        accessibility = sum(ppr, na.rm=T)), by=origin]
  
  return(bfca)
  
}


# PFCA function---------------------------------------------------------------

pfca_accessibility <- function(tt_matrix, fun = "binary", time_threshold = 30) {
  
  ## make a copy of the travel time matrix to preserve original data
  tt_matrix <- copy(tt_matrix)
  
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
  
  # calculate weights i (normalized impedance by origin id) ----
  tt_matrix[, wi := (impedance*opportunities)/sum(impedance*opportunities), by=origin]
  # tt_matrix[, wi := impedance/sum(impedance), by=origin]
  
  # calculate weights j (normalized impedance by destination) ----
  tt_matrix[, wj := (impedance*population)/sum(impedance*population), by=destination]
  # tt_matrix[, wj := impedance/sum(impedance), by=destination]
  
  ## Step 1 - reaportion the demand to jobs proportionally to weight i ----
  tt_matrix[, population_served := population * wi]
  # tt_matrix <- tt_matrix[pop_served > 0]
  
  ## Step 2 - calculate provider-to-population ration (ppr) at each destination ----
  tt_matrix[, opportunities_provided := opportunities * wj]
  tt_matrix[, ppr := opportunities_provided / population_served]

  pfca <- tt_matrix[, .(metric = "pfca",
                        fun = fun,
                        threshold = time_threshold,
                        accessibility = sum(ppr, na.rm=T)), by=origin]
  
  return(pfca)
  
}




# Calculate Accessibilities -----------------------------------------------


calculate_access <- function(muni, mode, thresholds = c(15, 30, 45, 60), 
                             jobs = "abs") {
  ttm <- load_tt_matrix(muni = muni, transport_mode = mode, opportunity = jobs, filtered = FALSE)
  
  calculate_distance_access <- function(ttm,
                                        decay_function = "binary", 
                                        threshold = 15,
                                        normalised = FALSE) {
    
    if (decay_function == "binary") {
      if (normalised) {
        res <- ttm[tt_median <= threshold, 
                   .(accessibility = sum(opportunities) / .N), 
                   by = .(origin)]
        res[, metric := "norm cumulative"] 
      } else {
        res <- ttm[tt_median <= threshold, .(accessibility = sum(opportunities)), by = .(origin)]
        res[, metric := "abs cumulative"] 
      }
    } 
    
    if (decay_function == "gaussian") {
      if (normalised) {
        res <- ttm[, .(accessibility = 
                         sum(opportunities * gaussian_weight(tt_median, b = threshold)) / 
                         sum(gaussian_weight(tt_median, b = threshold))), 
                   by = .(origin)]
        res[, metric := "norm gravity"] 
      } else {
        res <- ttm[, .(accessibility = sum(opportunities * gaussian_weight(tt_median, b = threshold))), 
                   by = .(origin)]
        res[, metric := "abs gravity"] 
      }
    } 
    
    res[, fun := decay_function]
    res[, threshold := threshold]

    return(res)
  }
  
  calculate_fca <- function(ttm, 
                            metric = "bfca", 
                            decay_function = "binary", 
                            threshold = 15) {
    
    if (metric == "2sfca") {
      fun <- twostep_fca_accessibility
    }
    if (metric == "bfca") {
      fun <- bfca_accessibility
    }
    if (metric == "pfca") {
      fun <- pfca_accessibility
    }
    
    res <- fun(ttm, fun = decay_function, time_threshold = threshold)
    return(res)
  }
  
  fca <- c(
    map(thresholds, calculate_fca, ttm = ttm, metric = "2sfca", decay_function = "binary"),
    map(thresholds, calculate_fca, ttm = ttm, metric = "2sfca", decay_function = "gaussian"),
    map(thresholds, calculate_fca, ttm = ttm, metric = "bfca", decay_function = "binary"),
    map(thresholds, calculate_fca, ttm = ttm, metric = "bfca", decay_function = "gaussian"),
    map(thresholds, calculate_fca, ttm = ttm, metric = "pfca", decay_function = "binary"),
    map(thresholds, calculate_fca, ttm = ttm, metric = "pfca", decay_function = "gaussian")
  ) %>%
    rbindlist()
  
  fca[, city := muni]
  fca[, mode := mode]
  fca[, jobs := jobs]
  fca <- fca[, .(city, mode, metric, jobs, fun, threshold, origin, accessibility)]
  
  dba <- c(
    map(thresholds, calculate_distance_access, ttm = ttm, decay_function = "binary", normalised = FALSE),
    map(thresholds, calculate_distance_access, ttm = ttm, decay_function = "binary", normalised = TRUE),
    map(thresholds, calculate_distance_access, ttm = ttm, decay_function = "gaussian", normalised = FALSE),
    map(thresholds, calculate_distance_access, ttm = ttm, decay_function = "gaussian", normalised = TRUE)
  ) %>% 
    rbindlist()
  
  dba[, city := muni]
  dba[, mode := mode]
  dba[, jobs := jobs]
  dba <- dba[, .(city, mode, metric, jobs, fun, threshold, origin, accessibility)]
  
  access <- rbind(dba, fca)
  
  return(access)
}



