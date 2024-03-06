# Script to create plot themes

# Load packages
library(ggplot2)
library(tidyverse)
library(ggspatial)

# Make a basic publication theme
theme_pub <- function() {
  
  theme_bw() +
    theme(axis.text = element_text(colour = "black", size = 10),
          axis.title = element_text(colour = "black", size = 11),
          legend.text = element_text(colour = "black", size = 10),
          legend.title = element_text(colour = "black", size = 11),
          # Panel things
          panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          # Legend
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"),
          # Facet
          strip.background = element_rect(fill = "grey80", color = "grey30"),  
          strip.text.x = element_text(size = 10, color = "black"),  
          strip.text.y = element_text(size = 10, color = "black",angle = -90))
}

# Scatter plot theme ----
scatter_theme <- function(data, x, y, arm){
  ggplot(data, aes(x = {{x}}, y = {{y}}, colour = {{arm}}, pch = {{arm}}, fill = {{arm}}, lty = {{arm}})) +
    geom_point() +
    geom_smooth(method = "lm") +
    scale_colour_manual(values = c("#DF5E29", "#28477D"),
                        name = "Arm number",
                        labels = c("Typical", "Atypical"),
                        guide = guide_legend(override.aes = 
                                 list(size = 3))) +
    scale_shape_discrete(name = "Arm number",
                         labels = c("Typical", "Atypical")) +
    scale_fill_manual(values = c("#DF5E29", "#28477D"),
                      name = "Arm number",
                      labels = c("Typical", "Atypical")) +
    scale_linetype_discrete(name = "Arm number",
                            labels = c("Typical", "Atypical")) +
    theme_pub()
}

# Annotation -----
place_label <- function(label, size = 10, ...) {
  annotate("text", label = label, x = -Inf, y = Inf, 
           vjust = 1.4, hjust = -0.15, size = size, ...)
}


# Mapping -----
# create a palette for the map
pal <- c(
  "dodgerblue2",        # 1
  "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "steelblue4",         # 5
  "#FF7F00", # orange
  "darkturquoise",
  "black", 
  "blue1",
  "skyblue2",          # 10
  "darkorange4", 
  "maroon", 
  "chartreuse3", 
  "#FB9A99", # lt pink   
  "darkmagenta",         # 15
  "brown"
) # Pal from https://stackoverflow.com/questions/9563711/r-color-palettes-for-many-data-classes
# pie(rep(1, 16), col = pal)

# map function for site map (the old map)
site_map <- function(coord_data, map_data){
  ggplot() +
    geom_sf(data = {{map_data}}, fill = "grey", colour = "white") +
    # add points
    geom_sf(data = {{coord_data}}, 
            alpha = 0.6,
            colour = "black",
            pch = 21,
            aes(size = percent,
                fill = site_num)) +
    # trim map
    coord_sf(xlim = c(-125.26, -125.09), ylim = c(48.81, 48.91), expand = FALSE) +
    # add text
    geom_text(data = {{coord_data}},
              aes(x = Longitude, y = Latitude, 
                  label = site_num,
                  colour = site_num),
              fontface = "bold",
              nudge_x = {{coord_data}}$xjit) +
    # scale colour, size, and pch
    scale_size_continuous(name = "Atypical stars (%)",
                          range = c(0.1, 6),
                          limits = c(0, 15)) + 
    scale_fill_manual(name = "Site", 
                      values = pal,
                      labels = site_names) +
    scale_colour_manual(values = pal, guide = "none") +
    # add aesthetic elements
    theme_bw() +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "white"),
          axis.text = element_text(size = 12, colour = "black"),
          axis.title = element_text(size = 13, colour = "black"),
          legend.title = element_text(size = 12, colour = "black"),
          legend.text = element_text(size = 11, colour = "black"),
          legend.margin = margin(0,0,0,0, unit="cm")
    ) +
    xlab("Longitude") + ylab("Latitude") +
    annotation_scale(location = "br", width_hint = 0.4) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    guides(size = guide_legend(order = 1),
           fill = guide_legend(order = 2),
           label = guide_legend(order = 2))
}

# big inset map
inset_map <- function(map_data){
  ggplot() +
    geom_sf(data = {{map_data}}, fill = "grey", colour = "white") +
    coord_sf(xlim = c(-128.5, -123), ylim = c(48.25, 51), expand = FALSE) +
    # add red rectangle for the site map region
    geom_rect(aes(xmin = -125.26, xmax = -125.09, ymin = 48.81, ymax = 48.91), color = "red", fill = NA, inherit.aes = FALSE) +
    # add aesthetic elements
    theme_bw() +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "white"),
          panel.border = element_rect(fill = NA, colour = "black"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.ticks.length = unit(0, "pt"),
          plot.title = NULL,
          plot.margin=grid::unit(c(0,0,0,0), "mm"))
}
