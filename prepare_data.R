
# Script for preparing data to the application

rm(list=ls())

library(NVEDATA)
library(raster)
library(rgdal)
library(rgeos)

source("data_handling.R")

# Function for computing polygons surrouding watersheds

wsh_polygon <- function(data) {
  
  print(data$regine_main)
  
  # Read watershed indices
  
  wsh_index <- data$wsh_index
  
  wsh_index <- as.numeric(wsh_index) + 1
  
  # Create raster
  
  all_index <- seq(1, 1550 * 1195)
  
  all_index[all_index %in% wsh_index == FALSE] = NA
  all_index[all_index %in% wsh_index == TRUE] = 1
  
  tmp_raster <- raster(matrix(all_index, nrow = 1550, ncol = 1195, byrow = TRUE), xmn = -75000, xmx = 1120000, ymn = 6450000, ymx = 8000000)
  
  crs(tmp_raster) <- sp::CRS("+proj=utm +zone=33 +datum=WGS84")
  
  # Raster to polygon
  
  wsh_poly <- rasterToPolygons(tmp_raster, dissolve = TRUE)
  
  wsh_poly <- spTransform(wsh_poly, CRS("+init=epsg:4326"))
  
  # Augment to data
  
  data$wsh_poly <- wsh_poly
  
  return(data)
  
}

# Function for rounding and reducing the data

reduce_data <- function(data_list) {
  
  data_list$Prec <- round(data_list$Prec,1)
  data_list$Runoff <- round(data_list$Runoff,1)
  data_list$wsh_index <- NULL
  
  return(data_list)
  
}

# Load data

load("//hdata/fou/Avrenningskart/Data/data_r_files/senorge_daily_v20.RData")

# Rename data

data_main <- data_daily

# Compute statistics

data_main <- lapply(data_main, comp_stats)

# Polygons for watersheds

data_main <- lapply(data_main, wsh_polygon)

# Reduce data size by removing some entries and rounding data

data_main <- lapply(data_main, reduce_data)

# Save to files

save(data_main, file = "data/senorge_main.RData")

# Prepare precipitation map

library(lubridate)
library(raster)

stime <- head(data_daily[[1]]$time_vec, 1)
etime <- tail(data_daily[[1]]$time_vec, 1)

time_vec <- seq(stime, etime, by = "days")

prec_acc <- 0

for (iday in seq_along(time_vec)) {
  
  print(paste("Processing date:", time_vec[iday]))
  
  syear <- substr(time_vec[iday], 1, 4)
  smonth <- substr(time_vec[iday], 6, 7)
  sday <- substr(time_vec[iday], 9, 10)
  
  sdate <- paste(syear, smonth, sday, sep = "_")
  
  filename <- paste("//hdata/grid/metdata/met_obs_v2.1/rr/", syear, "/rr_", sdate, ".bil", sep = "")
  
  fid <- file(filename,"rb")
  prec_vec <- readBin(fid, integer(), n=1195*1550, size=2)
  close(fid)
  
  prec_vec[prec_vec == -1] <- NA
  
  prec_acc <- prec_acc + prec_vec/10
  
}

prec_acc <- 365 * prec_acc / length(time_vec)

prec <- matrix(prec_acc, nrow = 1550, ncol = 1195, byrow = TRUE)

prec_raster <- raster(prec, xmn = -75000, xmx = 1120000, ymn = 6450000, ymx = 8000000)

crs(prec_raster) <- sp::CRS("+proj=utm +zone=33 +datum=WGS84")

prec_raster <- projectRasterForLeaflet(prec_raster)

save(prec_raster, file = "data/prec_raster.RData")


# # Empty raster
# 
# empty_raster <- raster(matrix(NA, nrow = 1550, ncol = 1195), xmn = -75000, xmx = 1120000, ymn = 6450000, ymx = 8000000)
# 
# crs(empty_raster) <- sp::CRS("+proj=utm +zone=33 +datum=WGS84")
# 
# empty_raster <- projectRasterForLeaflet(empty_raster)
# 
# save(empty_raster, file = "data/empty_raster.RData")




