# Created: 30/09/2024
# Last modified: 29/08/2025
# Author: Seb Dunnett sebdunnett@pm.me

#########
# SETUP #
#########

# Install package manager pacman if not installed
# Load packages
if (!require("pacman")) install.packages("pacman")
p_load(terra,tidyverse,sf,giscoR)

# Working directory <- CHANGE THIS
setwd("D:/aoi")

# Template WGS raster
r = rast(res=1/120)

# Helper function to transform rasters to fuzzy logistic distributions
fuzzy_log <- function(x,midpoint,steepness) {
  result <- rep(NA, length(x))
  ayra <- !is.na(x)
  starr <- x[ayra]
  result[ayra] <- 1 / (1 + exp(steepness * (starr - midpoint)))
  return(result)
}

# Helper function to standardise rasters 0 to 1
standardise01 <- function(r){
  rmin = global(r,"min",na.rm=T)[[1]]
  rmax = global(r,"max",na.rm=T)[[1]]
  ruger = app(r, function(x) (x - rmin) / (rmax - rmin))
  return(ruger)
}

check_dist <- function(r,r_trans,midpoint,sampleSize){
  omah = spatSample(c(r,r_trans),size=sampleSize,as.raster=FALSE,na.rm=TRUE)
  names(omah) <- c("x","y")
  lay = ggplot(omah,aes(x=x,y=y)) + geom_line() + geom_vline(xintercept = midpoint, col="darkred")
  print(lay)
}

# Land mask (this analysis is terrestrial only)
land.mask = rasterize(gisco_get_coastallines(resolution="03"),r)

# Water bodies mask
lakes = st_read("data/GLWD_level1") |>
  rasterize(r)

# Template rasters in Equal Earth projection for saving
eq1km = rast(res=1000,crs=crs("EPSG:8857"),xmin=-17243959.06,ymin=-8392927.6,xmax=17243959.06,ymax=8392927.6)
eq10km = rast(res=10000,crs=crs("EPSG:8857"),xmin=-17243959.06,ymin=-8392927.6,xmax=17243959.06,ymax=8392927.6)
eq50km = rast(res=50000,crs=crs("EPSG:8857"),xmin=-17243959.06,ymin=-8392927.6,xmax=17243959.06,ymax=8392927.6)

############
# ROADLESS #
############

# Read in roadless shapefile
roadless = st_read("data/Global_roadlessareas/Global_roadlessareas.shp")

# Rasterise to WGS template and transform skewed data
# Areas without shapefile coverage have roads so background 0
# Except Greenland and Antarctica, flagged by authors as no data - mask those out
roadless.r = rasterize(roadless,r,field="Area_km.",background=0) |>
  mask(gisco_get_countries() |> filter(!ISO3_CODE %in% c("GRL","ATA")))

# Simple square root transformation to flatten distribution
roadless.fuzzy = standardise01(roadless.r^0.5)

# Standardise 0 to 1
roadless.std = standardise01(roadless.r)

# Save
writeRaster(roadless.std,"scratch/roadless_std.tif",overwrite=TRUE)
writeRaster(roadless.fuzzy,"scratch/roadless_fuzzy.tif",overwrite=TRUE)

########
# GHSL #
########

# GHSL CATEGORIES
# 30: URBAN CENTRE GRID CELL
# 23: DENSE URBAN CLUSTER GRID CELL
# 22: SEMI-DENSE URBAN CLUSTER GRID CELL
# 21: SUBURBAN OR PERI-URBAN GRID CELL
# 13: RURAL CLUSTER GRID CELL
# 12: LOW DENSITY RURAL GRID CELL
# 11: VERY LOW DENSITY RURAL GRID CELL
# 10: WATER GRID CELL
# NoData [-200]

# Read in GHSL and reproject to template raster
ghsl = rast("data/GHS_SMOD_E2020_GLOBE_R2023A_54009_1000_V2_0/GHS_SMOD_E2020_GLOBE_R2023A_54009_1000_V2_0.tif") |>
  project(r,method="near")

# Reclass to fuzzy membership categories
ghsl.fuzzy = classify(ghsl,cbind(c(10,11,12,13,21,22,23,30,-200),
           c(0,1,0.9,0.6,0.2,0.1,0,0,NA)))

# Save
writeRaster(ghsl.fuzzy,"scratch/ghsl_fuzzy.tif",overwrite=TRUE)

#######
# GDP #
#######

# Read in GDP raster, resample to template and log(x+1) transform heavily skewed data
gdp = rast("data/rast_gdpTot_1990_2020_30arcsec.tif")$gdp_2020 |>
  resample(r,method="bilinear") |>
  log1p()

# Transform data to fuzzy logistic distribution
# Midpoint determined to be USD 2,000,000 and log1p(2000000) = 14.5
# Transition set to occur over 5 units of log1p(gdp)
gdp.fuzzy <- app(gdp, fuzzy_log, 
                 midpoint = 14.5, steepness = (log(19)/5))

# Standardise 0 to 1
gdp.std = 1-standardise01(gdp)

# Save
writeRaster(gdp.std,"scratch/gdp_std.tif",overwrite=TRUE)
writeRaster(gdp.fuzzy,"scratch/gdp_fuzzy.tif",overwrite=TRUE)

###############
# TRAVEL TIME #
###############

# Read in travel time (in minutes) to nearest settlement of >50,000 people and resample to template
# Classify missing data
ttime = rast("data/travel_time_to_cities_9.tif") |>
  classify(cbind(65535,NA)) |>
  resample(r,method="bilinear")

# Transform data to fuzzy logistic distribution
# Midpoint determined to be 24hrs (1440 minutes)
# Steepness of 0.0012 gives an appropriate transition
ttime.fuzzy <- 1 - app(ttime, fuzzy_log, 
                 midpoint = 1440, steepness = 0.0012)

# Standardise 0 to 1
ttime.std = standardise01(ttime)

# Save
writeRaster(ttime.std,"scratch/ttime_std.tif",overwrite=TRUE)
writeRaster(ttime.fuzzy,"scratch/ttime_fuzzy.tif",overwrite=TRUE)

##################
# MODIFIED LANDS #
##################

# Read in human modified layer 300m resolution
# Aggregate x3 and resample to template
gHM = rast("data/HMv20240801_2022s_AA_300.tif") |>
  aggregate(fact=3,fun="mean",na.rm=TRUE) |>
  resample(r,method="bilinear")

# Transform data to fuzzy logistic distribution
# Midpoint determined to be 0.1
# Steepness of 25 gives an appropriate transition
gHM.fuzzy <- app(gHM, fuzzy_log, 
               midpoint = 0.1, steepness = 25)

# Standardise 0 to 1
gHM.std = 1-gHM 

# Save
writeRaster(gHM.std,"scratch/gHM_std.tif",overwrite=TRUE)
writeRaster(gHM.fuzzy,"scratch/gHM_fuzzy.tif",overwrite=TRUE)

####################
# CRITICAL HABITAT #
####################

# Read in count of likely CH features per cell
# Missing data are 0
# Standardise 0 to 1
ch.nlikely = rast("data/likely_nfeatures.tif") |>
  classify(cbind(NA,0)) |>
  standardise01()

# Read in count of potential CH features per cell
# Missing data are 0
# Standardise 0 to 1
ch.npotential = rast("data/potential_nfeatures.tif") |>
  classify(cbind(NA,0)) |>
  standardise01()

# Weighted average of Likely and Potential CH
ch.wtd = 0.2*ch.npotential + 0.8*ch.nlikely

# Standardise by reclassification
ch.std = rast("data/Basic_Critical_Habitat_Raster_WGS.tif") |>
  classify(cbind(c(0,1,10),c(0,0.5,1)))

# Save
writeRaster(ch.std,"scratch/ch_std.tif",overwrite=TRUE)
writeRaster(ch.wtd,"scratch/ch_wtd.tif",overwrite=TRUE)

####################
# COMBINING LAYERS #
####################

# Combine input data with equal weighting
# Mask out Antarctica as only two datasets have data there
# Mask out freshwater bodies
fz.screening.layer = app(c(roadless.fuzzy,ghsl.fuzzy,gdp.fuzzy,ttime.fuzzy,gHM.fuzzy,ch.wtd), fun = mean, na.rm=TRUE) |>
  mask(gisco_get_countries() |> filter(ISO3_CODE != "ATA")) |>
  mask(lakes,inverse=TRUE)

std.screening.layer = app(c(roadless.std,ghsl.fuzzy,gdp.std,ttime.std,gHM.std,ch.std), fun = mean, na.rm=TRUE) |>
  mask(gisco_get_countries() |> filter(ISO3_CODE != "ATA")) |>
  mask(lakes,inverse=TRUE)

# Create outputs at 1km, 10km and 50km resolution in Equal Earth projection
fz.screening.layer.1km = project(fz.screening.layer,eq1km,method="bilinear",mask=TRUE)
std.screening.layer.1km = project(std.screening.layer,eq1km,method="bilinear",mask=TRUE)

fz.screening.layer.10km = project(fz.screening.layer,eq10km,method="bilinear",mask=TRUE)
std.screening.layer.10km = project(std.screening.layer,eq10km,method="bilinear",mask=TRUE)

fz.screening.layer.50km = project(fz.screening.layer,eq50km,method="bilinear",mask=TRUE)
std.screening.layer.50km = project(std.screening.layer,eq50km,method="bilinear",mask=TRUE)

# Save final outputs
writeRaster(fz.screening.layer.1km,"outputs/wtd_screening_layer.tif",overwrite=TRUE)
writeRaster(fz.screening.layer.10km,"outputs/wtd_screening_layer_10km.tif",overwrite=TRUE)
writeRaster(fz.screening.layer.50km,"outputs/wtd_screening_layer_50km.tif",overwrite=TRUE)

writeRaster(std.screening.layer.1km,"outputs/std_screening_layer.tif",overwrite=TRUE)
writeRaster(std.screening.layer.10km,"outputs/std_screening_layer_10km.tif",overwrite=TRUE)
writeRaster(std.screening.layer.50km,"outputs/std_screening_layer_50km.tif",overwrite=TRUE)

################
# BINARY LAYER #
################

# Convert inputs to binary yes/no using set thresholds
roadless.binary = roadless.r>0
ghsl.binary = classify(ghsl,cbind(c(10,11,12,13,21,22,23,30,-200),
                                 c(FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,NA)))
gdp.binary = gdp<log1p(2000000)
ttime.binary = ttime>1440
gHM.binary = gHM<0.1
ch.binary = rast("data/Basic_Critical_Habitat_Raster_WGS.tif")>0

# Combine where a cell requires a precautionary buffer is required if any input is TRUE
# Mask out Antarctica as only two datasets have data there
# Mask out freshwater bodies
binary.screening.layer = app(c(roadless.binary,ttime.binary,gHM.binary,ch.binary,gHM.binary,gdp.binary),any,na.rm=TRUE) |>
  mask(gisco_get_countries() |> filter(ISO3_CODE != "ATA")) |>
  mask(lakes,inverse=TRUE)

# Create outputs at 1km, 10km and 50km resolution in Equal Earth projection
binary.screening.layer.1km = project(binary.screening.layer,eq1km,method="near",mask=TRUE)
binary.screening.layer.10km = project(binary.screening.layer,eq10km,method="near",mask=TRUE)
binary.screening.layer.50km = project(binary.screening.layer,eq50km,method="near",mask=TRUE)

# Save final outputs
writeRaster(binary.screening.layer.1km,"outputs/binary_screening_layer.tif",overwrite=TRUE)
writeRaster(binary.screening.layer.10km,"outputs/binary_screening_layer_10km.tif",overwrite=TRUE)
writeRaster(binary.screening.layer.50km,"outputs/binary_screening_layer_50km.tif",overwrite=TRUE)
  