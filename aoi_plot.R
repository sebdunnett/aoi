# Created: 15/08/2025
# Last modified: 29/08/2025
# Author: Seb Dunnett sebdunnett@pm.me

#########
# SETUP #
#########

# Install package manager pacman if not installed
# Load packages
if (!require("pacman")) install.packages("pacman")
p_load(sf,terra,tidyverse,cowplot,scico,giscoR,MCMCpack,rasterVis)

# Working directory <- CHANGE THIS
setwd("D:/aoi")

# Template rasters in Equal Earth projection
eq1km = rast(res=1000,crs="EPSG:8857",xmin=-17243959.06,ymin=-8392927.6,xmax=17243959.06,ymax=8392927.6)
eq10km = rast(res=10000,crs="EPSG:8857",xmin=-17243959.06,ymin=-8392927.6,xmax=17243959.06,ymax=8392927.6)
eq50km = rast(res=50000,crs="EPSG:8857",xmin=-17243959.06,ymin=-8392927.6,xmax=17243959.06,ymax=8392927.6)

#Load fuzzy membership function raster data
fuzzy10 = rast("outputs/fz_screening_layer_10km.tif")
fuzzy50 = rast("outputs/fz_screening_layer_50km.tif")

# Load standardised raster data
std10 = rast("outputs/std_screening_layer_10km.tif")
std50 = rast("outputs/std_screening_layer_50km.tif")

# Load binary raster data
bin10 = rast("outputs/binary_screening_layer_10km.tif")
bin50 = rast("outputs/binary_screening_layer_50km.tif")

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
    scico::scale_fill_scico(palette="lajolla",limits=fuzz_range,direction=-1) +
    theme(legend.position="none")

# South American inset
amazon_fuzz = crop(fuzzy10,amazon) |>
  as.data.frame(xy=TRUE)
amazon_fuzz_inset = ggplot() +
  geom_sf(data=amazon,col=NA,fill="lightblue") +
  geom_raster(data=amazon_fuzz,aes(x=x,y=y,fill=mean)) +
  cowplot::theme_map() +
  scico::scale_fill_scico(palette="lajolla",limits=fuzz_range,direction=-1) +
  theme(legend.position="none")

# Indian inset
india_fuzz = crop(fuzzy10,india) |>
  as.data.frame(xy=TRUE)
india_fuzz_inset = ggplot() +
  geom_sf(data=india,col=NA,fill="lightblue") +
  geom_raster(data=india_fuzz,aes(x=x,y=y,fill=mean)) +
  cowplot::theme_map() +
  scico::scale_fill_scico(palette="lajolla",limits=fuzz_range,direction=-1) +
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
  geom_sf(data=gisco_get_countries(country="ATA") |> st_transform(8857),col=NA,fill="gray") +
  cowplot::theme_map() +
  scico::scale_fill_scico(name=NULL,palette="lajolla",limits=fuzz_range,direction=-1) +
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
  scico::scale_fill_scico(palette="lajolla", limits=std_range,direction=-1) +
  theme(legend.position="none")

# South American inset
amazon_std = crop(std10,amazon) |>
  as.data.frame(xy=TRUE)
amazon_std_inset = ggplot() +
  geom_sf(data=amazon,col=NA,fill="lightblue") +
  geom_raster(data=amazon_std,aes(x=x,y=y,fill=mean)) +
  cowplot::theme_map() +
  scico::scale_fill_scico(palette="lajolla", limits=std_range,direction=-1) +
  theme(legend.position="none")

# Indian inset
india_std = crop(std10,india) |>
  as.data.frame(xy=TRUE)
india_std_inset = ggplot() +
  geom_sf(data=india,col=NA,fill="lightblue") +
  geom_raster(data=india_std,aes(x=x,y=y,fill=mean)) +
  cowplot::theme_map() +
  scico::scale_fill_scico(palette="lajolla", limits=std_range,direction=-1) +
  theme(legend.position="none")

# Global plot
global_std = ggplot() +
  geom_sf(data=sphere,col=NA,fill="lightblue") +
  geom_raster(data=as.data.frame(std50,xy=TRUE),aes(x=x,y=y,fill=mean)) +
  geom_sf(data=westeur,fill=NA,col="white") +
  geom_sf_text(data=westeur,label="B",col="white") +
  geom_sf(data=india,fill=NA,col="white") +
  geom_sf_text(data=india,label="C",col="white") +
  geom_sf(data=amazon,fill=NA,col="white") +
  geom_sf_text(data=amazon,label="A",col="white") +
  geom_sf(data=gisco_get_countries(country="ATA") |> st_transform(8857),col=NA,fill="gray") +
  cowplot::theme_map() +
  scico::scale_fill_scico(name=NULL,palette="lajolla",direction=-1) +
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

###############
# BINARY PLOT #
###############

# Western Europe inset
westeur_bin = crop(bin10,westeur) |>
  as.data.frame(xy=TRUE)
westeur_bin_inset = ggplot() +
  geom_sf(data=westeur,col=NA,fill="lightblue") +
  geom_raster(data=westeur_bin,aes(x=x,y=y,fill=factor(any,labels=c("No","Yes")))) +
  cowplot::theme_map() +
  scale_fill_manual(values = scico(n=3,palette="lajolla")[2:3]) +
  theme(legend.position="none")

# South American inset
amazon_bin = crop(bin10,amazon) |>
  as.data.frame(xy=TRUE)
amazon_bin_inset = ggplot() +
  geom_sf(data=amazon,col=NA,fill="lightblue") +
  geom_raster(data=amazon_bin,aes(x=x,y=y,fill=factor(any,labels=c("No","Yes")))) +
  cowplot::theme_map() +
  scale_fill_manual(values = scico(n=3,palette="lajolla")[2:3]) +
  theme(legend.position="none")

# Indian inset
india_bin = crop(bin10,india) |>
  as.data.frame(xy=TRUE)
india_bin_inset = ggplot() +
  geom_sf(data=india,col=NA,fill="lightblue") +
  geom_raster(data=india_bin,aes(x=x,y=y,fill=factor(any,labels=c("No","Yes")))) +
  cowplot::theme_map() +
  scale_fill_manual(values = scico(n=3,palette="lajolla")[2:3]) +
  theme(legend.position="none")

# Global plot
global_bin = ggplot() +
  geom_sf(data=sphere,col=NA,fill="lightblue") +
  geom_raster(data=as.data.frame(bin50,xy=TRUE),aes(x=x,y=y,fill=factor(any,labels=c("No","Yes")))) +
  geom_sf(data=westeur,fill=NA,col="black") +
  geom_sf_text(data=westeur,label="B",col="black") +
  geom_sf(data=india,fill=NA,col="black") +
  geom_sf_text(data=india,label="C",col="black") +
  geom_sf(data=amazon,fill=NA,col="black") +
  geom_sf_text(data=amazon,label="A",col="black") +
  geom_sf(data=gisco_get_countries(country="ATA") |> st_transform(8857),col=NA,fill="gray") +
  cowplot::theme_map() +
  scale_fill_manual(name="Precautionary\napproach?", values=scico(n=3,palette="lajolla")[2:3]) +
  theme(
    legend.text = element_text(size=8)
  )

# Combine
top_row = cowplot::plot_grid(amazon_bin_inset,westeur_bin_inset,india_bin_inset,nrow=1,labels=LETTERS[1:3])
global_bin_plot = plot_grid(top_row,global_bin,ncol=1,rel_heights = c(0.3,0.7))

#######################
# SUPP WEIGHTING PLOT #
#######################

# Import individual datasets, project to ~10km Equal Earth for speed
gdp.10.eq = rast("scratch/gdp_std.tif") |>
  project(eq10km,method="bilinear")
ttime.10.eq = rast("scratch/ttime_std.tif") |>
  project(eq10km,method="bilinear")
gHM.10.eq = rast("scratch/gHM_std.tif") |>
  project(eq10km,method="bilinear")
ghsl.10.eq = rast("scratch/ghsl_fuzzy.tif") |>
  project(eq10km,method="near")
roadless.10.eq = rast("scratch/roadless_std.tif") |>
  project(eq10km,method="bilinear")
ch.10.eq = rast("scratch/ch_std.tif") |>
  project(eq10km,method="near")

# Stack
# Mask land (excl. Antarctica)
stack = c(gdp.10.eq,ttime.10.eq,gHM.10.eq,ghsl.10.eq,roadless.10.eq,ch.10.eq) |>
  mask(gisco_get_countries() |> filter(ISO3_CODE != "ATA") |> st_transform(8857))

# Apply random weightings nine times
rlist = replicate(9, {
  wts = as.numeric(rdirichlet(1, rep(1, 6)))
  out.rast = app(stack*wts,"sum",na.rm=TRUE)
  return(out.rast)
}, simplify = FALSE)

# Prepare output rasters to plot
rplot = rast(rlist)
names(rplot) <- paste0("Run ",1:9)
plot.df = as.data.frame(rplot,xy=TRUE) |>
  pivot_longer(3:11,names_to = "facet", values_to = "value")

# Plot
wts_plots = ggplot() +
  geom_sf(data=sphere,col=NA,fill="lightblue") +
  geom_raster(data=plot.df, aes(x=x,y=y,fill=value)) +
  geom_sf(data=gisco_get_countries(country="ATA") |> st_transform(8857),col=NA,fill="gray") +
  facet_wrap(~facet) +
  theme_map() +
  scico::scale_fill_scico(name=NULL,palette="lajolla",direction=-1,limits=c(0,1)) +
  guides(fill = guide_colorbar(
    barwidth = 0.5,
    barheight = 15
  )) +
  theme(
    legend.text = element_text(size=8)
  )

###################
# AOI BUFFER PLOT #
###################

# Use some administrative region spatial data from Germany as dummy data
# Our "site footprint" 
site.footprint =  gisco_get_nuts(country="DEU",nuts_level=2) |>
  st_transform(8857) |>
  filter(NAME_LATN=="Unterfranken") |>
  mutate(label="Site footprint",
         bf=NA)

# Our "area of influence"
rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
g = gisco_get_countries(country="DEU")
gg = st_geometry(g)
gr = (gg - st_centroid(gg)) * rot(pi/2) * .75 + st_centroid(gg)
gr = st_as_sf(gr) |>
  st_set_crs(4326)
gr = st_set_geometry(gr,"geometry")

aoi =  gr |>
  st_transform(8857) |>
  st_difference(site.footprint) |>
  mutate(label="Actual area of influence for indirect impacts",
         bf=NA)

# Set demonstrative buffers
buffers = rbind(st_buffer(site.footprint,50000),
                st_buffer(site.footprint,100000),
                st_buffer(site.footprint,250000)) |>
  mutate(bf = c("Buffer 1","Buffer 2","Buffer 3"),
         label=NA)

# Plot
aoi_plot = ggplot(bind_rows(site.footprint,aoi,buffers)) +
  geom_sf(aes(fill=label,col=bf)) +
  theme_void() +
  scale_fill_manual(values=scico(palette="acton",n=5)[4:5],name=NULL,na.value = NA, na.translate = FALSE) +
  scale_colour_manual(values=scico(palette="bamako",n=4,direction=-1)[2:4],name=NULL,na.value = NA, na.translate = FALSE) +
  guides(colour = guide_legend(override.aes = list(fill = NA)),
         fill = guide_legend(override.aes = list(col = NA))) +
  theme(legend.box.background = element_rect(fill = "grey85", colour = NA),
        legend.margin = margin(6, 6, 6, 6))

########
# PLOT #
########

global_fuzz_plot

global_std_plot

global_bin_plot

wts_plots

aoi_plot

########
# SAVE #
########

ggsave("outputs/main_plot.pdf",global_fuzz_plot,width=8,height=5,units="in")
ggsave("outputs/main_plot_alt.pdf",global_std_plot,width=8,height=5,units="in")
ggsave("outputs/supp_plot_bin.pdf",global_bin_plot,width=8,height=5,units="in")
ggsave("outputs/supp_plot_wts.pdf",wts_plots,width=8,height=5,units="in")
ggsave("outputs/aoi_buffer_plot.pdf",aoi_plot,width=8,height=5,units="in")

ggsave("outputs/main_plot.png",global_fuzz_plot,width=8,height=5,units="in")
ggsave("outputs/main_plot_alt.png",global_std_plot,width=8,height=5,units="in")
ggsave("outputs/supp_plot_bin.png",global_bin_plot,width=8,height=5,units="in")
ggsave("outputs/supp_plot_wts.png",wts_plots,width=8,height=5,units="in")
ggsave("outputs/aoi_buffer_plot.png",aoi_plot,width=8,height=5,units="in")

