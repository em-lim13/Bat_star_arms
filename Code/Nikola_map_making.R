# This is the code that makes the .Rdata file from Nikola!
# He wrote this to download the OSM land polygon, repair the coastline, and then cut just the BC chunk so I can work with it on my old computer!
# Oct 20, 2023
# Nikola Radoicic

# Define functions -----
library(sf)
library(ggplot2)

get_world_map <- function(land_path){
  land <- st_read(land_path)
  chk <- st_is_valid(land)
  # from https://tinyurl.com/2rncztte
  if(!all(chk)){
    message("Repairing invalid polygons ...")
    ind <- which(!chk)
    land[ind,] <- st_make_valid(land[ind,])
  }
  land
}

clip_map<- function(map, xmin, ymin, xmax, ymax){
  # from https://tinyurl.com/ysx6hymb
  bbox <- c(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax)
  crs = 4326
  sf_use_s2(FALSE)
  bbox_sf <- bbox %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_as_sf(crs = crs)
  cropped <- st_crop(st_transform(map, crs = crs), bbox_sf)
  cropped
}


plot_map <- function(data, title, filename){
  pale_blue <- paste("#b9d1df", sep="")
  ggplot(data = data) +
    geom_sf(fill = pale_blue, colour = "white") +
    labs(title = title) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "white"),
          axis.text = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 13, colour = "black"),
          legend.title = element_text(size = 12, colour = "black"),
          legend.text = element_text(size = 11, colour = "black"),
          panel.grid.major = element_line(color = "white")) +
    xlab("Longitude") + ylab("Latitude")
  ggsave(filename)
}


# This is the code to load the map, cut it, and save it ------
xmin <- -152.095
xmax <- -101.030
ymin <- 42.812
ymax <- 63.666

# Downloaded from:
# https://osmdata.openstreetmap.de/data/land-polygons.html

# or run this
# mkdir -p shapes
# curl -k -o shapes/land-polygons-complete-4326.zip https://osmdata.openstreetmap.de# /download/land-polygons-complete-4326.zip
# cd shapes
# unzip land-polygons-complete-4326.zip

# load shapefile
shapefile_path <- "./shapes/land-polygons-complete-4326/land_polygons.shp"

world <- get_world_map(shapefile_path)
slice <- clip_map(world, xmin, ymin, xmax, ymax)
save(slice, file="bc_map.Rdata")
plot_map(slice, "British Columbia", "bc.png")