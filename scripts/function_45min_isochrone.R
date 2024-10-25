#! Setting up workspace
## Modifying options
options(scipen = 999)
options(timeout = 999)
## Installing and loading required packages
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("dplyr")
usePackage("purrr")
usePackage("sf")
usePackage("gtfsrouter")
usePackage("tidytransit")
usePackage("tigris")
usePackage("osrm")
usePackage("nngeo")
usePackage("lubridate")
usePackage("ggplot2")

## Function to compute 45min isochrone given blockgroupid and lubridated start_time
compute.45min.isochrone <- function(blockgroupid, start_time) {
  
  ## Find 45-min walk isochrone for blockgroup
  walk_iso <- blkgrp.iso.cumulative[[blockgroupid]]
  
  ## Compute travel time to all stations
  travel.to.stations <-
    tibble(tt = ttmatrix$durations[blockgroupid,],
           stop_id = chicago.gtfs$stops$stop_id) %>% 
    filter(tt <= 45) %>%
    mutate(min_start_time = start_time + tt * 60,
           max_start_time = start_time + 45 * 60,
           allowed_timerange = max_start_time - min_start_time) %>%
    filter(allowed_timerange > 0)
  
  ## Use RAPTOR to figure out time to each station
  transit.stop.times <- filter_stop_times(chicago.gtfs, 
                                          "2024-10-22",
                                          min_departure_time = travel.to.stations[1,] %>%
                                            pull("min_start_time"))
  raptor.df <- raptor(
    transit.stop.times,
    chicago.gtfs$transfers,
    stop_ids = travel.to.stations[1,] %>% pull('stop_id'),
    time_range = travel.to.stations[1,] %>% pull('allowed_timerange'),
    max_transfers = 3,
    keep = 'shortest'
  )
  for (i in 2:nrow(travel.to.stations)) {
    transit.stop.times <- filter_stop_times(chicago.gtfs, 
                                            "2024-10-22",
                                            min_departure_time = travel.to.stations[i,] %>%
                                              pull("min_start_time"))
    raptor.df <- rbind(raptor.df,
                       raptor(
                         transit.stop.times,
                         chicago.gtfs$transfers,
                         stop_ids = travel.to.stations[1,] %>% pull('stop_id'),
                         time_range = travel.to.stations[1,] %>% pull('allowed_timerange'),
                         max_transfers = 3,
                         keep = 'shortest'
                       ))
  }
  
  ## Calculate columns in raptor.df
  raptor.df <- raptor.df %>%
    mutate(journey_arrival_time = as.numeric(journey_arrival_time)) %>%
    mutate(remain_time = (start_time + 45 * 60) - journey_arrival_time) %>%
    filter(remain_time >0) %>%
    select(to_stop_id, remain_time) %>%
    bind_cols(tibble("GEOID" = blockgroupid, "start" = start_time))
  
  ## Calculate cumulative isochrone based on raptor.df
  transit_iso <- map_dfr(1:nrow(temp2), function(idx) {
    stop.iso.cumulative[[raptor.df[idx,]$to_stop_id]] %>%
      filter(isomax <= raptor.df[idx,]$remain_time / 60) %>%
      slice_max(isomax, n = 1)
  }) %>%
    st_union() %>%
    st_make_valid()
  
  ## Combine the walk and transit isochrones
  combined_iso <- st_union(walk_iso, transit_iso)  %>%
    st_sf(GEOID = blockgroupid, start = start_time) %>%
    st_make_valid()
  
  return(combined_iso)
}