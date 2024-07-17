rm(list=ls())

# This script plays around with EJ data fr ch5 in TfC assessment

library(tidyr)
library(readr)
library(dplyr)

library(sf)
library(terra)
library(raster)
# library(geodata) #gadm() and world()
# library(MazamaSpatialUtils) #convert iso2 into iso3
library(tidyterra)
library(ggplot2)
library(ggthemes)
library(scales)

library(SpatialKDE)

library(rnaturalearth)
library(rnaturalearthdata)

setwd('C:/Users/yanis/Documents/scripts/IPBES-Data/IPBES_TCA_ch5_ejatlas/')

# explore input data----

ej_data1 = read_csv('data/Cluster_analysis_data_TSU.csv')
# updated sectors
ej_data2 = read_csv('data/Cluster_analysis_data_TSU_V2.csv') %>% 
  dplyr::select("id", "cluster","category_recoded")
ej_data2 %>%  distinct(category_recoded)

ej_data = ej_data1 %>% 
  left_join(ej_data2, by = c('id','cluster'))
names(ej_data)

ej_data %>%  group_by(cluster) %>% distinct(id) %>% count()
# cluster  n
# 1        755 --> resist
# 2        1507 --> reform
# 3        540 --> transform

ej_data %>% filter(is.na(cluster)) #no NAs
ej_data %>% filter(is.na(id))
ej_data %>% filter(is.na(Lat)) # 4 locations are incomplete
ej_data %>% filter(is.na(Lon)) # 3 locations are incomplete
ej_data %>% distinct(id) %>%  count() #no duplicates 
ej_data %>% distinct(category.clean)
ej_data %>% distinct(category_recoded)

# prep data 
ej_data_clean = ej_data %>% 
  dplyr::select("id","cluster","category.clean", "category_recoded",
                "start.year.coded",
                "project.status.simplified", "project.status.clean",
                "Country","Lat","Lon",
                "population.type.clean") %>% 
  # clean sectors updated
  dplyr::mutate(sector = as.factor(gsub('Climate policies[/]impacts and all others','Climate',category.clean))) %>% 
  dplyr::mutate(sector_grouped = gsub('^ii$','Industries, other infrastructure',category_recoded)) %>% 
  dplyr::mutate(sector_grouped = gsub('^ff$','Fossil fuels',sector_grouped)) %>%
  dplyr::mutate(sector_grouped = gsub('^affl$','Agriculture, Forestry, Fisheries and Livestock',sector_grouped)) %>%
  dplyr::mutate(sector_grouped = gsub('mining$','Mining',sector_grouped)) %>%
  dplyr::mutate(sector_grouped = gsub('dams$','Dams',sector_grouped)) %>%
  dplyr::mutate(sector_grouped = gsub('^other$','Others',sector_grouped)) %>%
  # clean year
  dplyr::mutate(start.year.clean = as.integer(gsub('POST','',start.year.coded))) %>% 
  dplyr::select(-start.year.coded) %>% 
  # make clusters and sectors as factors
  dplyr::mutate(cluster = as.factor(cluster)) %>% 
  dplyr::mutate(category.clean = as.factor(category.clean)) %>% 
  dplyr::mutate(sector_grouped = as.factor(sector_grouped)) 
   
ej_data_clean %>% distinct(sector_grouped)        

# get spatial object
ej_data_sp = ej_data_clean %>% 
  # remove NAs or odd values in lat/long
  filter(!is.na(Lat)) %>% 
  filter(!is.na(Lon)) %>% 
  filter(Lat != -20339962) %>% 
  # convert to sf object
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326)

# checks
ej_data_sp %>% filter(is.na(geometry))

# save
#write_sf(ej_data_sp, 'data/ej_data.gpkg')
#ej_data_sp = read_sf('data/ej_data.gpkg')

#transform to robinson
robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
ej_data_sp_robin <- sf::st_transform(ej_data_sp, crs = robin) # changes the projection

#checks
ej_data_sp_robin %>% filter(is.na(category.clean))
ej_data_sp_robin %>% filter(is.na(category_recoded))

# save
#write_sf(ej_data_sp_robin, 'data/ej_data_robin.gpkg')
#ej_data_sp_robin = read_sf('data/ej_data_robin.gpkg')

# Plot ej locations----

# get global data
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# color blind friendly theme for discrete variables (for continuous use viridis)
#scale_colour_discrete <- scale_colour_colorblind

ggplot() +
  geom_sf(data = ej_data_sp, aes(color = cluster)) +
  theme_minimal()

ggplot() +
  geom_sf(data = ej_data_sp_robin, aes(color = cluster)) + 
  theme(
  panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), # sets latitude and longitude lines 
  panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
  #panel.border = element_rect(colour = "black", fill=NA, size=0.5) # sets panel border
)  


# Overlay with biodiversity conservation hotspots----

### Priority maps
indic_path = 'C:/Users/yanis/Documents/IPBES/biodiversity_indic/important_biodiversity_areas/BiodiversityOnly/'

# ranked (1-100, 1 is the most important areas)
biodiv = rast(paste0(indic_path, 'BiodiversityOnly/10km/minshort_speciestargetswithPA_esh10km_repruns10_ranked.tif'))
plot(biodiv) 

# Project to Robinson
robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
biodiv_robin <- project(biodiv, robin)
#writeRaster(biodiv_robin,'C:/Users/yanis/Documents/scripts/IPBES-Data/IPBES_TCA_ch5_ejatlas/outputs/priority_areas_biodiversitywithPA_10km.tiff')

ej_data_sp_robin = ej_data_sp_robin %>% 
  mutate(cluster = gsub('1','Resists cases',cluster)) %>% 
  mutate(cluster = gsub('2','Reform cases',cluster)) %>% 
  mutate(cluster = gsub('3','Transform cases',cluster))
  

# PLot 

factor_robin <- biodiv_robin %>% mutate(cats = cut(minshort_speciestargetswithPA_esh10km_repruns10_ranked,
                                              breaks = c(0, 10, 40, 60, 90,100),
                                              labels = c("Very High", "High", "Average", "Low","Very Low")
))

# all clusters
plot = ggplot() +
  geom_spatraster(data = factor_robin, aes(fill = cats)) +
  geom_sf(data = ej_data_sp_robin, aes(shape = cluster, color = cluster), size = 1.5) +
  
  # scale for points
  scale_shape_manual("Cluster",values = c(15, 16, 17)) +
  scale_color_manual("Cluster",values = c("blue", "pink", "#000000")) +
  
  #scale for raster
  scale_fill_whitebox_d(
    "Priority",
    palette = "viridi",
    direction = 1,
    na.value = "transparent"
    ) +
  # legends
  # labs(
  #   title = "Areas of global significance for biodiversity conservation with EJ cases",
  #   fill = "Priority",
  #   colour = "Cluster",
  #   ) +

  theme(
    #panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5), # sets latitude and longitude lines 
    panel.background = element_rect(fill = "#FFFFFF")# sets background panel color 
    #legend.position = "bottom"
    )  +
  coord_sf(crs = robin)

plot
#ggsave(file="outputs/background_map_green.pdf", plot=plot, width=10, height=8, dpi = 500)
#ggsave(file="outputs/background_map_green_w?clusters.pdf", plot=plot, width=10, height=8, dpi = 500)

