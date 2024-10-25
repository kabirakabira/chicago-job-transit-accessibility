## Before running this script:
# 1. Download the .Rdata file and place it in "/dependencies/Chicago_OSRM.rdata".
#    Link: 
# 2. If you want to perform the OSRM calculations yourself, ignore the above but make sure
#    you follow the steps in "/dependencies/osrm-backend_walkthrough.txt".
# 3. 

## Steps in this script:
# 1. Download/read in GTFS data for Chicago.
# 2. Download Cook County blockgroups and compute centroids.
# 3. Set up osrm-backend locally using Docker.
# 4. Connect to the locally hosted server.
# 5. Calculate OSRM-based isochrones.
# 6. Calculate RAPTOR isochrones for a 45-min travel budget.

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
## Read in function to make 45-min isochrone
source("scripts/function_45min_isochrone.R")

#! Step 1: Read in Chicago GTFS from Mobility Database, convert to SF
## Download GTFS feed if not already downloaded
if (!file.exists("data/raw/chicago_gtfs_10_16_24.zip")) {
  download.file("https://files.mobilitydatabase.org/mdb-389/mdb-389-202410170023/mdb-389-202410170023.zip",
                "data/raw/chicago_gtfs_10_16_24.zip",
                quiet = TRUE)
}
## Read in and convert to SF
chicago.gtfs <- read_gtfs("data/raw/chicago_gtfs_10_16_24.zip")
chicago.gtfs <- gtfs_as_sf(chicago.gtfs, skip_shapes = FALSE, crs = 4326, quiet = TRUE)

## Extract the transit stops
chicago.transitstops <- chicago.gtfs$stops

#! Step 2: Download Cook County block groups and calculate centroids.
cook.blockgroups <- tigris::block_groups(
  state = "IL",
  county = "Cook County",
  year = 2023
) %>%
  st_centroid() %>%
  st_transform(st_crs(4326)) %>%
  select(GEOID)

chicago.boundary <- tigris::places(state = "IL") %>%
  filter(NAME == "Chicago") %>%
  select(NAME, geometry) %>%
  st_transform(st_crs(4326))

chicago.blockgroups <- cook.blockgroups %>%
  st_intersection(chicago.boundary) %>%
  select(GEOID, geometry)

#! Step 3: Set up osrm-backend by following the readme at "dependencies/osrm-backend_docker_walkthrough.txt"

#! Step 4: Connect to the locally hosted OSRM through R.
options(osrm.server = "localhost:5000/")
options(osrm.profile = "foot")

#! Step 5: Calculate the following using the OSRM server.
## 1. "ttmatrix" - list; "duration" = travel time matrix from block groups to transit stops
## 2. "stop_iso" - list; isochrones for each stop
## 3. "blkgrp_iso" - list; isochrones for each block group
## Note: These calculations take a while. If you have done them previously,
## you can simply read them in from a saved Rdata file. See below.

if (file.exists("dependencies/Chicago_OSRM.rdata")) {
  ## If the rdata file already exists, simply load it in
  load("dependencies/Chicago_OSRM.rdata")
} else {
  ## calculate ttmatrix ~ 2 mins
  ttmatrix <- osrmTable(src = chicago.blockgroups,
                        dst = chicago.transitstops,
                        measure = c('duration'))
  ## round the ttmatrix durations to 0 decimal points
  ttmatrix$durations <- round(ttmatrix$durations)
  ## add identifier to ttmatrix
  row.names(ttmatrix$durations) <-  chicago.blockgroups$GEOID
  
  ## calculate stop_iso ~ 2 hrs
  stop.iso <- list()
  stop.iso <- map(1:nrow(chicago.transitstops),
                  possibly(function(x){osrmIsochrone(loc=chicago.transitstops[x,],
                                                     breaks = seq(0,45,1))},
                           otherwise = NULL))
  ## add identifiers to stop_iso
  names(stop.iso) <- chicago.gtfs$stops$stop_id
  
  ## calculate blkgrp_iso ~ 
  blkgrp.iso <- list()
  blkgrp.iso <- map(1:nrow(chicago.blockgroups),
                    possibly(function(x){osrmIsochrone(loc=chicago.blockgroups[x,],
                                                       breaks = seq(0,45,1))},
                             otherwise = NULL))
  ## add identifiers to blkgrp_iso
  names(blkgrp.iso) <- chicago.blockgroups$GEOID
  
  ## Save these into an RData file for later use
  save(ttmatrix, stop.iso, blkgrp.iso,
       file = "dependencies/Chicago_OSRM.rdata")
}

#! Step 6: Calculate cumulative isochrones from these features
## Turn off spherical geometry
sf_use_s2(FALSE)

## Cumulative isochrones for transit stops for each minute
stop.iso.cumulative <- map(stop.iso, ~nngeo::st_remove_holes(.))

## Cumulative isochrones for block groups - just need the 45 min isochrone
blkgrp.iso.cumulative <- map(blkgrp.iso,
                             ~ slice_max(., isomax, n = 1) %>% 
                               nngeo::st_remove_holes() %>% 
                               st_geometry() %>% 
                               st_make_valid()
)

#! Step 7: Infer a transfer table between stops
## Recover xy coords for transit stops
chicago.gtfs$stops <- chicago.gtfs$stops %>%
  bind_cols(st_coordinates(chicago.gtfs$stops)) %>%
  rename(stop_lon = X, stop_lat = Y)

#! Pass a blockgroup id and start time to the 45-min isochrone function
blockgroupid <- "170313302002"
start_time <- "9:00" %>% hm %>% period_to_seconds
isochrone_45min <- compute.45min.isochrone(blockgroupid, start_time)

