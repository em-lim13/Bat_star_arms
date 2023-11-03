# Analysis for bat star project
# Initially compiled by Andrew Bickell, updated and edited by Em Lim and Isabelle Côté
# Written 2022-09-26, updated by Em on June 15, 2023 and beyond


### Loading in required packages ----
library(tidyverse)
library(ggplot2)
library(lme4)
library(nlme) # lme()
library(DHARMa)
library(kableExtra)
library(ggeffects)

# Packages for map
library(sf)
library(patchwork) # for inset map
library(ggspatial)

#For multiple comparisons
#emmeans better when model has interactions (marginal means)
library(emmeans)

#multcomp fine when model doesn't have interactions (main effects only)
library(multcomp)

# Source plot themes
source("Code/Plot_themes.R")

### Loading in required data -----
community_data <- read_csv("Data/community_survey_data.csv")
righting_time_data1 <- read_csv("Data/righting_time_data.csv")
morphology_data1 <- read_csv("Data/morphology_data.csv")
table_data <- read_csv("Data/table_data.csv")

# Data manipulations 
# Data for population surveys
species_arm_data <- community_data %>% 
  mutate(arm_number = as.factor(arm_number)) %>% 
  mutate(arm_bin = as.factor(ifelse(arm_number =="5", 0, 1 )), 
         species = factor(species, levels = c("bat", "velcro", "leather", "ochre",  "painted", "blood", "giant_pink",  "mottled",  "vermillion")),
         site = factor(site, levels = c("Aguilar", "BMSC_Dock", "Cia", "Dixon_Inside", "Dodger_Channel", "Ellis_Island", "Goby_Town", "Ohiat", "Ross_Main", "Ross_North", "Ross_Slug", "Ross_South", "Scott's_Outside", "Scotts_Inside","Wizard_North","Wizard_South")))


### Bat star only data for pop surveys 
bat_arm_data <- species_arm_data  %>% 
  filter(species == "bat") 

### Morphology data 
morphology_data <- morphology_data1 %>% 
  as.data.frame() %>%
  rename(site = Location, 
         species = Species, 
         area = Area_cm2, 
         arm_number = Number.Arms, 
         longest_arm = Longest.Arm, 
         average_length = Average.Length, 
         symmetry_cv = Symmetry.CV, 
         circle = Circle, 
         circle_ratio = Circle.Ratio)  %>% 
  mutate(arm_bin = as.factor(ifelse(arm_number == "5", "0", "1")),
         arm_number = as.factor(arm_number),
         arm_type = ifelse(arm_number =="5", "normal", "abnormal"),
         site = as.factor(site),
         longest_arm_centre = scale(longest_arm, scale = FALSE)) %>% 
  filter(arm_number != "4")

### Colour data 
colour_data <- bat_arm_data %>% 
  as.data.frame() %>%
  filter(colour != "Blue_light", colour != "blue_light") 

### Feeding data 
feeding_data <- bat_arm_data %>% 
  mutate(feed_bin = ifelse(feeding =="F", 1, 0 ),
         substrate = factor(substrate, levels = c("S", "C", "R")),
         depth_centre = scale(depth_m, scale = FALSE)) %>% 
  filter(arm_number != "4") %>% 
  drop_na(feeding) 

### Righting time data 
righting_time_data <- righting_time_data1 %>% 
  as.data.frame() %>%
  mutate(arm_type = ifelse(arms == "5", "Normal", "Abnormal"),
         arm_bin = as.factor(ifelse(arms =="5", 0, 1)),
         size_centre = scale(size_cm, scale = FALSE)) %>%
  filter(arms != "4") %>%
  drop_na(time_total_s) # drop the star that didn't flip


### Making a table ------
table_data %>% 
  kbl() %>%
  kable_classic(full_width = T, html_font = "Cambria")


### Sea star community surveys ------

# Which species have the most atypical arms?

### Creating a GLM to look at atypical arm number frequency across nine sea star species 
# We actually only want to look at the species that had atypical arms (5 species)
atypical_species <- species_arm_data %>%
  filter(species != "blood") %>%
  filter(species != "giant_pink") %>%
  filter(species != "mottled") %>%
  filter(species != "vermillion") 
  
community_glm <- glm(arm_bin ~ species, family = binomial(link = "logit"), data = atypical_species)

# Check residuals
# On the left panel, we see a qqplot of the standardized residuals - these should fall roughly along the straight line in a model that fits reasonably well. 
# On the right panel, we see a plot of our standardized residuals against the standardized model predictions - this helps us see if there is any weird patterning in our residuals and to look for issues like heteroskedasticity (i.e., if the amount of spread of our data around the modelled line increases with certain values of our predictor).
plot(DHARMa::simulateResiduals(community_glm))

summary(community_glm)

# pairwise comparisons
em <- emmeans(community_glm, "species")
contrast(em, "pairwise", adjust = "Tukey")

summary(glht(community_glm, mcp(species="Tukey")))


# Which sites have the most stars with atypical arms?

### Creating a GLM to look at atypical arm number frequency across sixteen sites 
all_stars_arm_site_glm <- glm(arm_bin ~ site, family = binomial(link = "logit"), data = species_arm_data)

# check residuals
plot(simulateResiduals(all_stars_arm_site_glm))

summary(all_stars_arm_site_glm)

em <- emmeans(all_stars_arm_site_glm, "site")
contrast(em, "pairwise", adjust = "Tukey")

summary(glht(all_stars_arm_site_glm, mcp(site="Tukey")))

# Where are bat stars showing the highest # atypical arms?

### Creating a GLM to look at atypical arm number frequency for bat stars across sixteen sites 

bat_arm_site_glm <- glm(arm_bin ~ site, family = binomial(link = "logit"), data = bat_arm_data)
summary(bat_arm_site_glm)

# check residuals
plot(simulateResiduals(bat_arm_site_glm))

em <- emmeans(bat_arm_site_glm, "site")
contrast(em, "pairwise", adjust = "Tukey")

summary(glht(bat_arm_site_glm, mcp(site="Tukey")))

  
### Morphology comparisons -----

### Colour analysis 
#Chi square test for colour 
colour_data %>% 
  dplyr::select(colour, arm_type) %>% 
  table() %>% 
  chisq.test()

### Generating a linear model to determine if oral surface area is affected by the presence of supernumerary arms in bat stars 
# Center arm length so the comparisons are made at the average arm length, not arm length = 0
oral_sa_lm <- lme(area ~ arm_bin*longest_arm_centre, random = ~ 1|site, data = morphology_data)
summary(oral_sa_lm)

# Check residuals
plot(resid(oral_sa_lm))
plot(oral_sa_lm)


# Comparison of length of longest arm for typical and atypical bat stars
length_t_test <- t.test(longest_arm ~ arm_bin, data = morphology_data)
length_t_test
  


# Feeding and substrate surveys  ------
feeding_data_summary <- feeding_data %>% count(arm_number)

### Creating a GLM for feeding data 
# center depth
feeding_glm <- glmer(feed_bin ~ arm_bin*substrate + depth_centre + (1|site) , family = binomial(link = "logit"), data = feeding_data)

summary(feeding_glm)

# check residuals
plot(simulateResiduals(feeding_glm))


### Righting time trials ----
righting_time_summary1 <- righting_time_data %>% count(arms) # add the 1 7 arm star that didn't flip to this

### Generating a linear model to determine if righting time is being affected by the presence of supernumerary arms in bat stars 
# Scale size
righting_time_lm <- lme(time_total_s ~ arm_bin*size_centre, random = ~ 1|Site, data = righting_time_data)

summary(righting_time_lm)

# Check residuals
plot(resid(righting_time_lm))
plot(righting_time_lm)



# Figures -----

# Figure 1 -----
# Load community data and figure out % abnormal for each site
norm <- community_data %>%
  count(site, arm_type) %>%
  filter(arm_type == "normal") %>%
  transmute(site = site,
            normal = n)

pie <- community_data %>%
  count(site, arm_type) %>%
  filter(arm_type == "abnormal") %>%
  transmute(site = site,
            abnormal = n) %>%
  left_join(norm, by = "site") %>%
  mutate(total = normal + abnormal,
         percent = (abnormal/total)*100,
         site_num = c(16, 14, 12, 9, 11, 13, 10, 15, 1, 3, 2, 4, 8, 7, 6, 5))

# Load coordinates
coords <- table_data %>%
  as.data.frame() %>%
  mutate(lat = Latitude,
         long = Longitude,
         site_num = `Site number`) %>%
  left_join(pie, by = "site_num") %>%
  st_as_sf(coords = c("long", "lat")) %>%
  st_set_crs(4326) %>%
  # dodge certain points
  mutate(xjit = ifelse(site_num == 3 | 
                         site_num == 4 |
                         site_num == 6 |
                         site_num == 8, -0.006, 0.006),
         site_num = as.factor(site_num))


# Load GREAT map from Nikola!
# the documentation for the creation of this in Nikola_map_making.R
load("~/Documents/PhD/Collaborations/Andrew Bickell/Bat_Star_Project/Data/bc_map.Rdata") 
bc_map <- slice # rename

# Load the lower quality map so I can play with plots quickly
potato_map <- sf::st_read("Data/Decent_shapefiles/eez.shp") %>%
  st_sf() %>%
  st_set_crs(4326)


# Define colours
blue <- paste("#b9d1df", sep="")

sf_use_s2(FALSE)


# Create legend
site_names <- as.list(paste(coords$site_num, coords$`Site name`, sep = ": "))
 
# Make maps!
main_map <- site_map(coord_data = coords, map_data = bc_map)

smaller_map <- inset_map(map_data = bc_map)


# Put site map and inset map together with patchwork
main_map + 
  inset_element(
    smaller_map, 
    left = 0, 
    bottom = 0.5, 
    right = 0.5, 
    top = 1.067,
    align_to = 'panel'
  )

ggsave("Pub_figs/Fig.1_inset.png", device = "png", height = 150, width = 250, units = c("mm"), dpi = 600)

# Make sure to cite OSM data, "Map data © OpenStreetMap contributors" and link https://www.openstreetmap.org/copyright
# might have to change to height = 150, width = 173 to meet journal style guide


# Figure 2: oral SA ----
# plot
scatter_theme(data = morphology_data, 
              x = longest_arm, 
              y = area, 
              arm = arm_bin) +
  xlab("Length of longest arm (mm)") +
  ylab(expression(paste("Oral surface area (mm"^"2",")"))) +
  xlim(3.9, 12) +
  ylim(0, 200) +
  theme(legend.position=c(0.19, 0.85))


ggsave("Pub_figs/Fig.2.png", device = "png", height = 130, width = 173, units = c("mm"), dpi = 600)


# Figure 3: feeding ----
### Creating figure for feeding frequency of bat stars with supernumerary arms on each substrate type

# Plot the actual model output
# build model with un-centred depth
feeding_glm_plot <- glmer(feed_bin ~ arm_bin*substrate + depth_m + (1|site) , family = binomial(link = "logit"), data = feeding_data)

v <- seq(from = 1.9, to = 11.3,  length.out = 50)


# use model to predict proportion of stars feeding on each substrate type
sum_stats_feed <- ggpredict(feeding_glm_plot, terms = c("depth_m [v]", "arm_bin", "substrate")) %>% 
  dplyr::rename(depth_m = x,
                arm_bin = group,
                substrate = facet,
                feeding_percent = predicted) %>% 
  as.data.frame()

# build the plot
ggplot() +
  geom_jitter(data = feeding_data, 
              aes(x = depth_m, y = feed_bin, colour = arm_bin, pch = arm_bin),
              alpha = 0.8, size = 1.5, position = position_jitter(height = .04)) +
  geom_line(data = sum_stats_feed,
     aes(x = depth_m, y = feeding_percent, colour = arm_bin, lty = arm_bin), 
     linewidth = 1) +
  geom_ribbon(data = sum_stats_feed,
              aes(x = depth_m, y = feeding_percent, fill = arm_bin, ymin = conf.low, ymax = conf.high), 
              alpha = 0.3) +
  facet_wrap(~substrate, 
             strip.position = "top", 
             labeller = as_labeller(c(S = "Sand", 
                                      C = "Cobble", 
                                      R = "Rock"))) +
  theme_pub()  +
  labs(y = "Probability of bat star feeding", x = "Depth (m)", colour = "Arms") +
  scale_colour_manual(values = c("#DF5E29", "#28477D"),
                      name = "Arm number",
                      labels = c("Typical", "Atypical")) +
  scale_shape_discrete(name = "Arm number",
                       labels = c("Typical", "Atypical")) +
  scale_fill_manual(values = c("#DF5E29", "#28477D"),
                    name = "Arm number",
                    labels = c("Typical", "Atypical")) +
  scale_linetype_discrete(name = "Arm number",
                          labels = c("Typical", "Atypical")) +
  guides(pch = guide_legend(override.aes =
                              list(size = 3)))

ggsave("Pub_figs/Fig.3.png", device = "png", height = 130, width = 173, units = c("mm"), dpi = 600)


  
 # Simple geom_smooth plot
 # use scatter_theme function from Plot_themes.R
 scatter_theme(data = feeding_data, x = depth_m, y = feed_bin, arm = arm_bin) +
   facet_wrap(~substrate,
              labeller = as_labeller(c(S = "Sand", 
                                       C = "Cobble", 
                                       R = "Rock"))) +
   labs(x = "Depth (m)", y = "Probability of bat star feeding")


# Figure 4: righting time -----
scatter_theme(data = righting_time_data, 
              x = size_cm, 
              y = time_total_s, 
              arm = arm_bin) +
  xlab("Length of longest arm (mm)") +
  ylab("Righting time (s)") +
  theme(legend.position = c(0.1, 0.89))


ggsave("Pub_figs/Fig.4.png", device = "png", height = 130, width = 173, units = c("mm"), dpi = 600)


# Extra -----

#Correlation between distance from Ross North and % atypical bat stars

distances <- read.csv("sites.csv")
str(distances)

dist_lm <- lm(percent_atyp~distance, data = distances)

summary(dist_lm)

distance_plot <- 
  ggplot(distances, aes(distance, percent_atyp)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Distance from Ross North") +
  ylab("% Atypical bat stars") +
  theme_classic() 
distance_plot

#Correlation between SW to NE rank (temp gradient) and % atypical bat stars

rank_lm <- lm(percent_atyp~geo_rank, data = distances)

summary(rank_lm)

rank_plot <- 
  ggplot(distances, aes(geo_rank, percent_atyp)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("SW to NE rank") +
  ylab("% Atypical bat stars") +
  theme_classic() 
rank_plot
