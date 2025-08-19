# Created: 15/08/2025
# Last modified: 19/08/2025
# Author: Seb Dunnett sebdunnett@pm.me

#########
# SETUP #
#########

# Install package manager pacman if not installed
# Load packages
if (!require("pacman")) install.packages("pacman")
p_load(sf,terra,tidyverse,cowplot,scico,giscoR)

# Working directory <- CHANGE THIS
setwd("D:/aoi")

# Template rasters in Equal Earth projection
eq1km = rast(res=1000,crs="EPSG:8857",xmin=-17243959.06,ymin=-8392927.6,xmax=17243959.06,ymax=8392927.6)
eq10km = rast(res=10000,crs="EPSG:8857",xmin=-17243959.06,ymin=-8392927.6,xmax=17243959.06,ymax=8392927.6)
eq50km = rast(res=50000,crs="EPSG:8857",xmin=-17243959.06,ymin=-8392927.6,xmax=17243959.06,ymax=8392927.6)

#Load fuzzy membership function raster data
fuzzy10 = rast("wtd_screening_layer_10km.tif")
fuzzy50 = rast("wtd_screening_layer_50km.tif")

# Load standardised raster data
std10 = rast("std_screening_layer_10km.tif")
std50 = rast("std_screening_layer_50km.tif")

#######################
# BACKGROUND & INSETS #
#######################

# Create background for Equal Earth plots
sphere <- st_graticule(ndiscr = 10000, margin = 10e-6) %>%
  st_transform(crs = st_crs("ESRI:54009")) %>%
  st_convex_hull() %>%
  summarise(geometry = st_union(geometry)) |>
  st_buffer(1000) |> st_union() |> st_buffer(-1000) |>
  st_transform("EPSG:8857")

# Western Euruope
westeur = giscoR::gisco_get_countries() |>
  left_join(gisco_countrycode) |>
  filter(un.regionsub.name=="Western Europe") |> st_crop(st_bbox(c(xmin=-20,xmax=20,ymax=90,ymin=40),crs=4326)) |>
  st_transform(8857) |>
  st_bbox() |>
  st_as_sfc()

# South America
amazon = st_bbox(c(xmin = -62.5, ymin = -20, xmax = -45, ymax = -7.5), crs = st_crs(4326)) |>
  st_transform(8857) |>
  st_bbox() |>
  st_as_sfc()

# India
india = st_bbox(c(xmin = 67.5, ymin = 20, xmax = 83, ymax = 33), crs = st_crs(4326)) |>
  st_transform(8857) |>
  st_bbox() |>
  st_as_sfc()

##############
# FUZZY PLOT #
##############

# Set range for all plots
fuzz_range <- c(0,1)

# Western Europe inset
westeur_fuzz = crop(fuzzy10,westeur) |>
  as.data.frame(xy=TRUE)
westeur_fuzz_inset = ggplot() +
    geom_sf(data=westeur,col=NA,fill="lightblue") +
    geom_raster(data=westeur_fuzz,aes(x=x,y=y,fill=mean)) +
    cowplot::theme_map() +
    scico::scale_fill_scico(palette="lajolla",limits=fuzz_range) +
    theme(legend.position="none")

# South American inset
amazon_fuzz = crop(fuzzy10,amazon) |>
  as.data.frame(xy=TRUE)
amazon_fuzz_inset = ggplot() +
  geom_sf(data=amazon,col=NA,fill="lightblue") +
  geom_raster(data=amazon_fuzz,aes(x=x,y=y,fill=mean)) +
  cowplot::theme_map() +
  scico::scale_fill_scico(palette="lajolla",limits=fuzz_range) +
  theme(legend.position="none")

# Indian inset
india_fuzz = crop(fuzzy10,india) |>
  as.data.frame(xy=TRUE)
india_fuzz_inset = ggplot() +
  geom_sf(data=india,col=NA,fill="lightblue") +
  geom_raster(data=india_fuzz,aes(x=x,y=y,fill=mean)) +
  cowplot::theme_map() +
  scico::scale_fill_scico(palette="lajolla",limits=fuzz_range) +
  theme(legend.position="none")

# Global plot
global_fuzz = ggplot() +
  geom_sf(data=sphere,col=NA,fill="lightblue") +
  geom_raster(data=as.data.frame(fuzzy50,xy=TRUE),aes(x=x,y=y,fill=mean)) +
  geom_sf(data=westeur,fill=NA,col="white") +
  geom_sf_text(data=westeur,label="B",col="white") +
  geom_sf(data=india,fill=NA,col="white") +
  geom_sf_text(data=india,label="C",col="white") +
  geom_sf(data=amazon,fill=NA,col="white") +
  geom_sf_text(data=amazon,label="A",col="white") +
  cowplot::theme_map() +
  scico::scale_fill_scico(name=NULL,palette="lajolla",limits=fuzz_range) +
  guides(fill = guide_colorbar(
    barwidth = 0.5,
    barheight = 15
  )) +
  theme(
    legend.text = element_text(size=8)
  )

# Combine into one plot
top_row = cowplot::plot_grid(amazon_fuzz_inset,westeur_fuzz_inset,india_fuzz_inset,nrow=1,labels=LETTERS[1:3])
global_fuzz_plot = plot_grid(top_row,global_fuzz,ncol=1,rel_heights = c(0.3,0.7))

#####################
# STANDARDISED PLOT #
#####################

# Set range for all plots
std_range <- c(0,1)

# Western Europe inset
westeur_std = crop(std10,westeur) |>
  as.data.frame(xy=TRUE)
westeur_std_inset = ggplot() +
  geom_sf(data=westeur,col=NA,fill="lightblue") +
  geom_raster(data=westeur_std,aes(x=x,y=y,fill=mean)) +
  cowplot::theme_map() +
  scico::scale_fill_scico(palette="lajolla", limits=std_range) +
  theme(legend.position="none")

# South American inset
amazon_std = crop(std10,amazon) |>
  as.data.frame(xy=TRUE)
amazon_std_inset = ggplot() +
  geom_sf(data=amazon,col=NA,fill="lightblue") +
  geom_raster(data=amazon_std,aes(x=x,y=y,fill=mean)) +
  cowplot::theme_map() +
  scico::scale_fill_scico(palette="lajolla", limits=std_range) +
  theme(legend.position="none")

# Indian inset
india_std = crop(std10,india) |>
  as.data.frame(xy=TRUE)
india_std_inset = ggplot() +
  geom_sf(data=india,col=NA,fill="lightblue") +
  geom_raster(data=india_std,aes(x=x,y=y,fill=mean)) +
  cowplot::theme_map() +
  scico::scale_fill_scico(palette="lajolla", limits=std_range) +
  theme(legend.position="none")

# Global plot
global_std = ggplot() +
  geom_sf(data=sphere,col=NA,fill="lightblue") +
  geom_raster(data=as.data.frame(std50,xy=TRUE),aes(x=x,y=y,fill=mean)) +
  geom_sf(data=westeur,fill=NA,col="black") +
  geom_sf_text(data=westeur,label="B",col="black") +
  geom_sf(data=india,fill=NA,col="black") +
  geom_sf_text(data=india,label="C",col="black") +
  geom_sf(data=amazon,fill=NA,col="black") +
  geom_sf_text(data=amazon,label="A",col="black") +
  cowplot::theme_map() +
  scico::scale_fill_scico(name=NULL,palette="lajolla") +
  guides(fill = guide_colorbar(
    barwidth = 0.5,
    barheight = 15
    )) +
  theme(
    legend.text = element_text(size=8)
  )

# Combine
top_row = cowplot::plot_grid(amazon_std_inset,westeur_std_inset,india_std_inset,nrow=1,labels=LETTERS[1:3])
global_std_plot = plot_grid(top_row,global_std,ncol=1,rel_heights = c(0.3,0.7))

########
# PLOT #
########

global_fuzz_plot

global_std_plot

########
# SAVE #
########

ggsave("main_plot.pdf",global_fuzz_plot,width=8,height=5,units="in")
ggsave("main_plot_alt.pdf",global_std_plot,width=8,height=5,units="in")

ggsave("main_plot.png",global_fuzz_plot,width=8,height=5,units="in")
ggsave("main_plot_alt.png",global_std_plot,width=8,height=5,units="in")
