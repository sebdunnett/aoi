# Created: 30/09/2024
# Last modified: 19/08/2025
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

# Land mask (this analysis is terrestrial only)
land.mask = rasterize(gisco_get_coastallines(resolution="03"),r)

# Template rasters in Equal Earth projection for saving
eq1km = rast(res=1000,crs=crs("EPSG:8857"),xmin=-17243959.06,ymin=-8392927.6,xmax=17243959.06,ymax=8392927.6)
eq10km = rast(res=10000,crs=crs("EPSG:8857"),xmin=-17243959.06,ymin=-8392927.6,xmax=17243959.06,ymax=8392927.6)
eq50km = rast(res=50000,crs=crs("EPSG:8857"),xmin=-17243959.06,ymin=-8392927.6,xmax=17243959.06,ymax=8392927.6)

############
# ROADLESS #
############

# Read in roadless shapefile
roadless = st_read("Global_roadlessareas/Global_roadlessareas.shp")

# Rasterise to WGS template and transform skewed data
roadless.r = rasterize(roadless,r,field="Area_km.",background=NA)
roadless.r.trans = roadless.r^0.5

# Standardise 0 to 1
roadless.std = standardise01(roadless.r.trans)

# Areas without shapefile coverage have roads
# Reclassify missing data to 0
roadless.fuzzy = classify(roadless.std,cbind(NA,0))

# Save
writeRaster(roadless.std,"roadless_std.tif",overwrite=TRUE)
writeRaster(roadless.fuzzy,"roadless_fuzzy.tif",overwrite=TRUE)

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
ghsl = rast("GHS_SMOD_E2020_GLOBE_R2023A_54009_1000_V2_0/GHS_SMOD_E2020_GLOBE_R2023A_54009_1000_V2_0.tif") |>
  project(r,method="near")

# Reclass to fuzzy membership categories
ghsl.fuzzy = classify(ghsl,cbind(c(10,11,12,13,21,22,23,30,-200),
           c(0,1,0.9,0.6,0.2,0.1,0,0,NA)))

# Save
writeRaster(ghsl.fuzzy,"ghsl_fuzzy.tif",overwrite=TRUE)

#######
# GDP #
#######

# Read in GDP raster, resample to template and log(x+1) transform heavily skewed data
gdp = rast("rast_gdpTot_1990_2022_30arcmin.tif")$gdp_tot_2022 |>
  resample(r,method="bilinear") |>
  log1p()

# Transform data to fuzzy logistic distribution
# Midpoint determined to be USD 2,000,000 and log1p(2000000) = 14.5
# Transition set to occur over 5 units of log1p(gdp)
gdp.fuzzy <- app(gdp_log, fuzzy_log, 
                 midpoint = 14.5, steepness = (log(19)/2.5))

# Standardise 0 to 1
gdp.std = 1-standardise01(gdp)

# Save
writeRaster(gdp.std,"gdp_std.tif",overwrite=TRUE)
writeRaster(gdp.fuzzy,"gdp_fuzzy.tif",overwrite=TRUE)

###############
# TRAVEL TIME #
###############

# Read in travel time (in minutes) to nearest settlement of >50,000 people and resample to template
ttime = rast("cit_017_accessibility_to_cities/accessibility_to_cities_2015_v1.0.tif") |>
  resample(r,method="bilinear")

# Classify missing data
rcl <- matrix(c(-10000, 0, NA), ncol = 3, byrow = TRUE)
ttime <- classify(ttime, rcl, right=FALSE)

# Transform data to fuzzy logistic distribution
# Midpoint determined to be 24hrs (1440 minutes)
# Steepness of 0.004 gives an appropriate transition
ttime.fuzzy <- 1 - app(ttime, fuzzy_log, 
                 midpoint = 1440, steepness = 0.004)

# Standardise 0 to 1
ttime.std = standardise01(ttime)

# Save
writeRaster(ttime.std,"ttime_std.tif",overwrite=TRUE)
writeRaster(ttime.fuzzy,"ttime_fuzzy.tif",overwrite=TRUE)

##################
# MODIFIED LANDS #
##################

# Read in human modified layer 300m resolution
# Aggregate x3 and resample to template
gHM = rast("HMv20240801_2022s_AA_300.tif") |>
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
writeRaster(gHM.std,"gHM_std.tif",overwrite=TRUE)
writeRaster(gHM.fuzzy,"gHM_fuzzy.tif",overwrite=TRUE)

####################
# CRITICAL HABITAT #
####################

# Read in count of likely CH features per cell
# Missing data are 0
# Standardise 0 to 1
ch_nlikely = rast("likely_nfeatures.tif") |>
  classify(cbind(NA,0)) |>
  standardise01()

# Read in count of potential CH features per cell
# Missing data are 0
# Standardise 0 to 1
ch_npotential = rast("potential_nfeatures.tif") |>
  classify(cbind(NA,0)) |>
  standardise01()

# Weighted average of Likely and Potential CH
ch.wtd = 0.2*ch_npotential + 0.8*ch_nlikely

# Standardise by reclassification
ch.std = rast("Basic_Critical_Habitat_Raster_WGS.tif") |>
  classify(cbind(c(0,1,10),c(0,0.5,1)))

# Save
writeRaster(ch.std,"ch_std.tif",overwrite=TRUE)
writeRaster(ch.wtd,"ch.wtd.tif",overwrite=TRUE)

####################
# COMBINING LAYERS #
####################

# Water bodies mask
lakes = st_read("D:/aoi/GLWD_level1") |>
  rasterize(r)

# Combine input data with equal weighting
# Mask with land and water bodies masks
wtd_screening_layer = app(c(roadless.fuzzy,ghsl.fuzzy,gdp.fuzzy,ttime.fuzzy,gHM.fuzzy,ch.wtd), fun = mean, na.rm=TRUE) |>
  mask(land.mask) |>
  mask(lakes,inverse=TRUE)

std_screening_layer = app(c(roadless.std,ghsl.fuzzy,gdp.std,ttime.std,gHM.std,ch.std), fun = mean, na.rm=TRUE) |>
  mask(land.mask) |>
  mask(lakes,inverse=TRUE)

# Create outputs at 10 and 50km resolution
wtd.screening.layer.10km = project(wtd_screening_layer,eq10km,method="bilinear",mask=TRUE)
std.screening.layer.10km = project(std_screening_layer,eq10km,method="bilinear",mask=TRUE)

wtd.screening.layer.50km = project(wtd_screening_layer,eq50km,method="bilinear",mask=TRUE)
std.screening.layer.50km = project(std_screening_layer,eq50km,method="bilinear",mask=TRUE)

# Save final outputs
writeRaster(wtd_screening_layer,"wtd_screening_layer.tif",overwrite=TRUE)
writeRaster(wtd.screening.layer.10km,"wtd_screening_layer_10km.tif",overwrite=TRUE)
writeRaster(wtd.screening.layer.50km,"wtd_screening_layer_50km.tif",overwrite=TRUE)

writeRaster(std_screening_layer,"std_screening_layer.tif",overwrite=TRUE)
writeRaster(std.screening.layer.10km,"std_screening_layer_10km.tif",overwrite=TRUE)
writeRaster(std.screening.layer.50km,"std_screening_layer_50km.tif",overwrite=TRUE)
