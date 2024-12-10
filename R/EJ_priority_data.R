rm(list=ls())

# This script plays around with EJ data fr ch5 in TfC assessment

library(tidyr)
library(readr)
library(dplyr)

library(sf)
library(terra)
library(raster)

library(tidyterra)
library(ggplot2)
library(ggthemes)
library(scales)
library(svglite)

library(rnaturalearth)
library(rnaturalearthdata)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#setwd('C:/Users/JLU-SU/Documents/GitHub/IPBES-Data/IPBES_TCA_ch5_ejatlas/')
getwd()

# explore input data----

# ej_data1 = read_csv('data/Cluster_analysis_data_TSU.csv')
# # updated sectors
# ej_data2 = read_csv('data/Cluster_analysis_data_TSU_V2.csv') %>% 
#   dplyr::select("id", "cluster","category_recoded")
# ej_data2 %>%  distinct(category_recoded)
# 
# ej_data = ej_data1 %>% 
#   left_join(ej_data2, by = c('id','cluster'))
# names(ej_data)
# 
# ej_data %>%  group_by(cluster) %>% distinct(id) %>% count()
# # cluster  n
# # 1        755 --> resist
# # 2        1507 --> reform
# # 3        540 --> transform
# 
# ej_data %>% filter(is.na(cluster)) #no NAs
# ej_data %>% filter(is.na(id))
# ej_data %>% filter(is.na(Lat)) # 4 locations are incomplete
# ej_data %>% filter(is.na(Lon)) # 3 locations are incomplete
# ej_data %>% distinct(id) %>%  count() #no duplicates 
# ej_data %>% distinct(category.clean)
# ej_data %>% distinct(category_recoded)
# 
# # prep data 
# ej_data_clean = ej_data %>% 
#   dplyr::select("id","cluster","category.clean", "category_recoded",
#                 "start.year.coded",
#                 "project.status.simplified", "project.status.clean",
#                 "Country","Lat","Lon",
#                 "population.type.clean") %>% 
#   # clean sectors updated
#   dplyr::mutate(sector = as.factor(gsub('Climate policies[/]impacts and all others','Climate',category.clean))) %>% 
#   dplyr::mutate(sector_grouped = gsub('^ii$','Industries, other infrastructure',category_recoded)) %>% 
#   dplyr::mutate(sector_grouped = gsub('^ff$','Fossil fuels',sector_grouped)) %>%
#   dplyr::mutate(sector_grouped = gsub('^affl$','Agriculture, Forestry, Fisheries and Livestock',sector_grouped)) %>%
#   dplyr::mutate(sector_grouped = gsub('mining$','Mining',sector_grouped)) %>%
#   dplyr::mutate(sector_grouped = gsub('dams$','Dams',sector_grouped)) %>%
#   dplyr::mutate(sector_grouped = gsub('^other$','Others',sector_grouped)) %>%
#   # clean year
#   dplyr::mutate(start.year.clean = as.integer(gsub('POST','',start.year.coded))) %>% 
#   dplyr::select(-start.year.coded) %>% 
#   # make clusters and sectors as factors
#   dplyr::mutate(cluster = as.factor(cluster)) %>% 
#   dplyr::mutate(category.clean = as.factor(category.clean)) %>% 
#   dplyr::mutate(sector_grouped = as.factor(sector_grouped)) 
#    
# ej_data_clean %>% distinct(sector_grouped)        
# 
# # get spatial object
# ej_data_sp = ej_data_clean %>% 
#   # remove NAs or odd values in lat/long
#   filter(!is.na(Lat)) %>% 
#   filter(!is.na(Lon)) %>% 
#   filter(Lat != -20339962) %>% 
#   # convert to sf object
#   st_as_sf(coords = c("Lon", "Lat"), crs = 4326)
# 
# # checks
# ej_data_sp %>% filter(is.na(geometry))

# Load spatial EJ data----

ej_data_sp = read_sf('../data/ej_cases/ej_data.gpkg')

#transform to robinson
robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
ej_data_sp_robin <- sf::st_transform(ej_data_sp, crs = robin) # changes the projection

#checks
ej_data_sp_robin %>% filter(is.na(category.clean))
ej_data_sp_robin %>% filter(is.na(category_recoded))

# properly name clusters
ej_data_sp_robin = ej_data_sp_robin %>% 
  mutate(cluster = gsub('1','Resists cases',cluster)) %>% 
  mutate(cluster = gsub('2','Reform cases',cluster)) %>% 
  mutate(cluster = gsub('3','Transform cases',cluster))

# save
#write_sf(ej_data_sp_robin, 'data/ej_data_robin.gpkg')
#ej_data_sp_robin = read_sf('data/ej_data_robin.gpkg')

# Plot ej locations----

# get global data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Plot location of clusters
ggplot() +
  geom_sf(data = ej_data_sp_robin, aes(color = cluster)) + 
  theme(
  panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), # sets latitude and longitude lines 
  panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
  #panel.border = element_rect(colour = "black", fill=NA, size=0.5) # sets panel border
)  


# Overlay with biodiversity conservation hotspots----

### Priority maps
#indic_path = 'G:/My Drive/global_indic/biodiversity_indic/important_biodiversity_areas/BiodiversityOnly/BiodiversityOnly'
# Downloaded from https://doi.org/10.5281/zenodo.5006332

# ranked (1-100, 1 is the most important areas)
biodiv = rast('../data/important_biodiversity_areas/minshort_speciestargetswithPA_esh10km_repruns10_ranked.tif')
plot(biodiv) 

# Project to Robinson
robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
biodiv_robin <- project(biodiv, robin)

# Bin ranked priority areas (1-100) into 6 classes
factor_robin <- biodiv_robin %>% tidyterra::mutate(cats = cut(minshort_speciestargetswithPA_esh10km_repruns10_ranked,
                                              breaks = c(0, 10, 40, 60, 90,100),
                                              labels = c("Very High", "High", "Average", "Low","Very Low")
))

# plot all clusters
plot = ggplot() +
  # world
  geom_sf(data = world, color = "gray60", fill = "gray60") +
  # priority map
  geom_spatraster(data = factor_robin, aes(fill = cats)) +
  # clusters
  geom_sf(data = ej_data_sp_robin, aes(shape = cluster, color = cluster), alpha= 0.65, size = 1.5) +
  
  # scale for points
  scale_shape_manual("Cluster",values = c(16, 15, 17)) +
  scale_color_manual("Cluster",values = c("#000000", "pink", "blue")) +
  
  #scale for raster
  scale_fill_whitebox_d(
    "Priority",
    palette = "gn_yl",
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
ggsave(file="../outputs/priority_map_w_clusters_10k.svg", plot=plot, width=10, height=8, dpi = 300)

#Calculate number of cases falling within the top 30% of land area for biodiversity
high_biodiv_robin = biodiv_robin %>% 
  tidyterra::filter(minshort_speciestargetswithPA_esh10km_repruns10_ranked <= 30) %>% 
  mutate(high_priority = if_else(minshort_speciestargetswithPA_esh10km_repruns10_ranked <= 30,
                                 true = 1,
                                 false = 0))
plot(high_biodiv_robin$high_priority) 
plot(high_biodiv_robin$minshort_speciestargetswithPA_esh10km_repruns10_ranked) 

# Convert the sf object to a SpatVector for compatibility with terra
ej_data_sp_robin_terra <- vect(ej_data_sp_robin)

# Extract the raster values at the point locations
extracted_values <- extract(biodiv_robin, ej_data_sp_robin_terra)

# Combine the extracted values with the original data
ej_data_combined <- ej_data_sp_robin %>% 
  cbind(extracted_values) %>% 
  rename(priority_rank = minshort_speciestargetswithPA_esh10km_repruns10_ranked) %>% 
  dplyr::select(-geom, -ID) %>% 
  st_drop_geometry(NULL) %>% 
  write_csv("../outputs/cases_priority.csv")

# Filter for high priority areas (where raster value equals 1)
high_priority_points <- ej_data_combined %>% 
  filter(priority_rank <= 30) %>% 
  filter(!is.na(priority_rank))
  
# Count the number of points in each cluster
cluster_counts <- table(high_priority_points$cluster)  # Replace 'cluster' with the actual column name

# Print the results
print(cluster_counts)
