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

#library(rnaturalearth)
#library(rnaturalearthdata)

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
write_sf(ej_data_sp, 'data/ej_data.gpkg')
ej_data_sp = read_sf('data/ej_data.gpkg')

#transform to robinson
robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
ej_data_sp_robin <- sf::st_transform(ej_data_sp, crs = robin) # changes the projection

#checks
ej_data_sp_robin %>% filter(is.na(category.clean))
ej_data_sp_robin %>% filter(is.na(category_recoded))

# save
#write_sf(ej_data_sp_robin, 'data/ej_data_robin.gpkg')
ej_data_sp_robin = read_sf('data/ej_data_robin.gpkg')

# Plot ej locations----

# get global data
# gadm = read_sf("C:/Users/yanis/Documents/regions/gadm36_mol_20200805/gadm36_mol_simp2.shp") %>% 
#   dplyr::filter(is.na(PARENT_ID)) %>% 
#   dplyr::select(ISO3, NAME)
# transform to robinson
# gadm_robin <- sf::st_transform(gadm, crs = robin) # changes the projection
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


# Kernel density estimation for each cluster----

# split by cluster
points_cl1 <- filter(ej_data_sp_robin, cluster == 1) 
plot(points_cl1$geometry)
points_cl2 <- filter(ej_data_sp_robin, cluster == 2) 
plot(points_cl2$geometry, add=TRUE)
points_cl3 <- filter(ej_data_sp_robin, cluster == 3) 
plot(points_cl3$geometry, add=TRUE)

# create grid 
grid_robin <- create_grid_hexagonal(ej_data_sp_robin, cell_size = 10000) #10km
#plot(grid_robin$geometry)

# create raster
raster_robin <- create_raster(ej_data_sp_robin, cell_size = 100000) #100km

# calculate KDE over grid
kde_cl1_grid <- kde(points_cl1, band_width = 150000, grid = grid_robin) 
plot(kde_cl1_grid)

kde_cl2_grid <- kde(points_2, band_width = 150000, grid = grid_robin) 
plot(kde_cl2_grid)

kde_cl3_grid <- kde(points_3, band_width = 150000, grid = grid_robin) 
plot(kde_cl3_grid)

# calculate KDE over raster
kde_cl1_raster <- kde(points_cl1, band_width = 150000, grid = raster_robin)
plot(kde_cl1_raster)

kde_cl2_raster <- kde(points_cl2, band_width = 150000, grid = raster_robin)
plot(kde_cl2_raster)

kde_cl3_raster <- kde(points_cl3, band_width = 150000, grid = raster_robin)
plot(kde_cl3_raster)

# lots on 0 --> convert to NA
m <- c(0, NA)
rclmat <- matrix(m, ncol=2, byrow=TRUE)

kde_cl1_raster_noNA <- reclassify(kde_cl1_raster, rclmat)
kde_cl2_raster_noNA <- reclassify(kde_cl2_raster, rclmat)
kde_cl3_raster_noNA <- reclassify(kde_cl3_raster, rclmat)

# Plot using ggplot

kde_cl1_raster_df <- as.data.frame(kde_cl1_raster_noNA, xy = TRUE)
kde_cl1_raster_df <- kde_cl1_raster_df %>%
  mutate(layer_cl = cut(layer, breaks = 6))

kde_cl1_raster_plot =
  ggplot() +
  geom_sf(data = world) + 
  geom_raster(data = kde_cl1_raster_df , aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c(option = "turbo", na.value = "transparent", name = 'KDE cluster 1') +
  #geom_sf(data = ej_data_sp_robin, aes(color = cluster)) + 
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), # sets latitude and longitude lines 
    panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
    #panel.border = element_rect(colour = "black", fill=NA, size=0.5) # sets panel border
  )  +
  coord_sf(crs = robin)

kde_cl1_raster_plot

kde_cl2_raster_df <- as.data.frame(kde_cl2_raster_noNA, xy = TRUE)
kde_cl2_raster_df <- kde_cl2_raster_df %>%
  mutate(layer_cl = cut(layer, breaks = 6))

kde_cl2_raster_plot =
  ggplot() +
  geom_sf(data = world) + 
  geom_raster(data = kde_cl2_raster_df , aes(x = x, y = y, fill = layer)) +
  #scale_fill_viridis_c(na.value = "transparent", name = 'KDE cluster 2') +
  scale_fill_viridis_c(option = "turbo", na.value = "transparent", name = 'KDE cluster 2') +
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), # sets latitude and longitude lines 
    panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
    #panel.border = element_rect(colour = "black", fill=NA, size=0.5) # sets panel border
  )  +
  coord_sf(crs = robin)

kde_cl2_raster_plot

kde_cl3_raster_df <- as.data.frame(kde_cl3_raster_noNA, xy = TRUE)
kde_cl3_raster_df <- kde_cl3_raster_df %>%
  mutate(layer_cl = cut(layer, breaks = 6))

kde_cl3_raster_plot =
  ggplot() +
  geom_sf(data = world) + 
  geom_raster(data = kde_cl3_raster_df , aes(x = x, y = y, fill = layer)) +
  #scale_fill_viridis_c(na.value = "transparent",name = 'KDE cluster 3') +
  scale_fill_viridis_c(option = "turbo", na.value = "transparent", name = 'KDE cluster 3') +
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), # sets latitude and longitude lines 
    panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
    #panel.border = element_rect(colour = "black", fill=NA, size=0.5) # sets panel border
  )  +
  coord_sf(crs = robin)

kde_cl3_raster_plot



# Plot by IPBES region-----
ipbes_regions_simp <- read_sf("C:/Users/yanis/Documents/regions/IPBES_Regions_Subregions/IPBES_Regions_Subregions2OK_simp.gpkg") 

#transform to robinson
robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
regions_robin <- sf::st_transform(ipbes_regions_simp, crs = robin) # changes the projection
data = filter(regions_robin, is.na(parent_id))

# all clusters
plot = ggplot() +
  geom_sf(data = data) +
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
ggsave(file="outputs/clusters_ipbes_regions.svg", plot=plot, width=10, height=8, dpi = 500)

# intersect points and regions
ej_data_sp_robin_ipbes = ej_data_sp_robin %>% 
  # join with IPBES regions
  st_join(regions_robin) 

# Aggregate by region
ej_data_region = ej_data_sp_robin_ipbes %>% 
  # remove points that did not overlap with any region (218)
  filter(!is.na(name)) %>% 
  # count number of conflicts  by region and cluster
  group_by(cluster,name) %>% 
  distinct(id.x, .keep_all = TRUE) %>% 
  count() %>% 
  # calculate proportion
  mutate(prop_ej = n/2802) %>% 
  # remove point geometry
  st_drop_geometry()

# Regions
ipbes_regions_simp_robin_ej_data = regions_robin %>% 
  left_join(ej_data_region, by = 'name') %>% 
  rename(geometry = geom)

# Subregion
ipbes_SUBregions_simp_robin_ej_data = filter(ipbes_regions_simp_robin_ej_data, !is.na(parent_id))

# Plots
ipbes_SUBregions_simp_robin_ej_data_cl1 = 
  ggplot() + 
  geom_sf(data = filter(ipbes_regions_simp_robin_ej_data, cluster == 1), mapping = aes(fill = prop_ej)) + 
  scale_fill_viridis_c(, name = 'Proportion of cluster 1') +
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), # sets latitude and longitude lines 
    panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
  )  

ipbes_SUBregions_simp_robin_ej_data_cl1

ipbes_SUBregions_simp_robin_ej_data_cl2 = 
  ggplot() + 
  geom_sf(data = filter(ipbes_regions_simp_robin_ej_data, cluster == 2), mapping = aes(fill = prop_ej)) + 
  scale_fill_viridis_c(, name = 'Proportion of cluster 2') +
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), # sets latitude and longitude lines 
    panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
  )  

ipbes_SUBregions_simp_robin_ej_data_cl2

ipbes_SUBregions_simp_robin_ej_data_cl3 = 
  ggplot() + 
  geom_sf(data = filter(ipbes_regions_simp_robin_ej_data, cluster == 3), mapping = aes(fill = prop_ej)) + 
  scale_fill_viridis_c(, name = 'Proportion of cluster 3') +
    theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), # sets latitude and longitude lines 
    panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
    #panel.border = element_rect(colour = "black", fill=NA, size=0.5) # sets panel border
  )  

ipbes_SUBregions_simp_robin_ej_data_cl3

# Plot sectors as categories------
ej_data_sp_robin %>% distinct(category_recoded)
ej_data_sp_robin %>% filter(is.na(category_recoded))

# by cluster
ggplot(data = ej_data_clean, aes(x = cluster, fill = sector)) +
  geom_bar(position = "fill") + ylab("proportion") +
  stat_count(geom = "text", 
             aes(label = stat(count)),
             position=position_fill(vjust=0.5), colour="white")

# by sector
show_col(colorblind_pal()(8))
my.x.labels <- c("Agriculture, Forestry,\nFisheries and Livestock","Dams", "Fossil fuels", "Industries,\nother infrastructure", "Mining", "Others") # first create labels, add \n where appropriate.

ggplot(data = ej_data_clean, aes(x = sector_grouped, fill = cluster)) +
  geom_bar(position = "fill") + #this calculates proportions instead of total nubers
  labs(y = "Proportion of cases", x = '') +
  scale_colour_colorblind(, name = 'Cluster type') +
  scale_x_discrete(labels= my.x.labels) +
  stat_count(geom = "text", 
             aes(label = stat(count)),
             position=position_fill(vjust=0.5), colour="white")

# Plot ej cases by sector
#okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E499", "#0072B2", "#D55E00", "#CC79A7","#FFFFFF", "#000000", "#20FF40", "#DD00A7")
#okabe6 <- c("#E69F00", "#56B4E9", "#009E73", "#F0E499", "#0072B2", "#D55E00")

ggplot() +
  geom_sf(data = world) + 
  geom_sf(data = filter(ej_data_sp_robin, cluster == 1), aes(color = sector_grouped)) + 
  #scale_color_manual(values = okabe6, name = 'Sectors in cluster 3') +
  scale_colour_colorblind(, name = 'Sectors in cluster 1') +
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), # sets latitude and longitude lines 
    panel.background = element_rect(fill = "#FFFFFF") # sets background panel color 
    #panel.border = element_rect(colour = "black", fill=NA, size=0.5) # sets panel border
  )  +
  coord_sf(crs = robin)

# Temporal analysis----

temp_ej_data_clean = ej_data_clean %>% 
  #group_by(cluster, start.year.clean) %>% 
  #count() %>% 
  # count cases in clusters
  group_by(cluster) %>% 
  add_count() %>% 
  ungroup() %>% 
  rename("n_by_cluster" = n) %>% 
  # count cases in each year
  group_by(start.year.clean) %>% 
  add_count() %>% 
  ungroup() %>% 
  rename("n_by_year" = n) %>% 
  # count cases in year/cluster combination
  group_by(cluster, start.year.clean) %>% 
  add_count() %>% 
  ungroup() %>% 
  rename("n_by_cluster_year" = n) %>% 
  # calculate percentages of total cases per year
  mutate(perc_cases_per_year = n_by_cluster_year/n_by_year) #%>% 
  #distinct(cluster, start.year.clean, n_by_cluster,n_by_year,n_by_cluster_year,perc_cases_per_year)
 
 # lines: total numbers
ggplot(data = temp_ej_data_clean, aes(x = start.year.clean, y = n_by_cluster_year, group = cluster, color = cluster)) +
  geom_line() +
  geom_point() + 
  labs(y="Number of EJ cases", x = "Start year")

# lines : proportion
ggplot(data = temp_ej_data_clean, aes(x = start.year.clean, y = perc_cases_per_year, group = cluster, color = cluster)) +
  geom_line() +
  geom_point() + 
  labs(y="Proportion of EJ cases", x = "Start year")


# bars; proportion (when I create the proportion)
ggplot(data = temp_ej_data_clean, aes(x = start.year.clean, y = perc_cases_per_year, fill = cluster)) +
  geom_bar(stat="identity") +
  labs(y = "Proportion of cases", x = '')

# bars: proportion (when ggplot creates it automatically)
ggplot(data = temp_ej_data_clean, aes(x = start.year.clean, fill = cluster)) +
  geom_bar(position = "fill") + #
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percentage of cases", x = '') +
    stat_count(geom = "text", 
             aes(label = stat(count)),
             position=position_fill(vjust=0.5), colour="white")


# Overlay with human modification/biodiversity hotspots----

### richness maps-----
indic_path = 'C:/Users/yanis/Documents/IPBES/nexus_indicators/all_harmonized/'

vert_sp_rich = rast(paste0(indic_path, 'B_038_Species_Richness_4taxa_harm.tif'))
vert_sp_rich = rast(paste0(indic_path, 'B_038_Threatened_Species_Richness_4taxa_harm.tif'))

vert_sp_rich = vert_sp_rich %>% rename('Richness' = 'Amphibians_THR_SR_2022')

# reclassify to remove all Os
m <- c(0, NA)
rclmat <- matrix(m, ncol=2, byrow=TRUE)

vert_sp_rich_noNA <- classify(vert_sp_rich, rclmat)

# Project to Robinson
vert_sp_rich_noNA <- project(vert_sp_rich_noNA, robin)

# PLot 
ggplot() +
  geom_spatraster(data = vert_sp_rich_noNA, show.legend = FALSE) +
  geom_sf(data = filter(ej_data_sp_robin, cluster == 1)) + 
  scale_fill_whitebox_c(
    palette = "muted",
    n.breaks = 12,
  ) +
  labs(
    title = "Threatened Vertebrate species richness (cluster 1)",
  ) +
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), # sets latitude and longitude lines 
    panel.background = element_rect(fill = "#FFFFFF")# sets background panel color 
  )  +
  coord_sf(crs = robin)

### Priority maps-------
indic_path = 'C:/Users/yanis/Documents/IPBES/biodiversity_indic/important_biodiversity_areas/BiodiversityOnly/'

# ranked (1-100, 1 is the most important areas)
biodiv = rast(paste0(indic_path, 'BiodiversityOnly/10km/minshort_speciestargetswithPA_esh10km_repruns10_ranked.tif'))
plot(biodiv) 

# Project to Robinson
robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
biodiv_robin <- project(biodiv, robin)
writeRaster(biodiv_robin,'C:/Users/yanis/Documents/scripts/IPBES-Data/IPBES_TCA_ch5_ejatlas/outputs/priority_areas_biodiversitywithPA_10km.tiff')

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
  #geom_sf(data = ej_data_sp_robin, aes(shape = cluster, color = cluster), size = 1.5) +
  
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
ggsave(file="outputs/backgroup_map_viridis.pdf", plot=plot, width=10, height=8, dpi = 500)

#all cluster (raster as df)

factor_robin_df <- as.data.frame(factor_robin, xy = TRUE)


# all clusters
plot = ggplot() +
  geom_tile(data = factor_robin_df, aes(x = x, y = y,fill = cats)) +
  #geom_sf(data = ej_data_sp_robin, aes(shape = cluster, color = cluster), size = 1.5) +
  
  # scale for points
  scale_shape_manual("Cluster",values = c(15, 16, 17)) +
  scale_color_manual("Cluster",values = c("blue", "pink", "#000000")) +
  
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
    panel.background = element_rect(fill = "#FFFFFF"),# sets background panel color 
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  )  +
  coord_sf(crs = robin)

plot
ggsave(file="outputs/backgroup_map_df_green.svg", plot=plot, width=10, height=8, dpi = 500)


# by cluster
plot_cluster = ggplot() +
  geom_spatraster(data = factor_robin, aes(fill = cats)) +
  # EJ cases
  #geom_sf(data = filter(ej_data_sp_robin, cluster==1), size = 1.5, shape = 15, col = "lightgray") +
  #geom_sf(data = filter(ej_data_sp_robin, cluster==2), size = 1.5, shape = 16, col = "#FF10F0") +
  geom_sf(data = filter(ej_data_sp_robin, cluster==3), size = 1.5, shape = 17, col = "#000000") +

  
  #scale for raster
  scale_fill_whitebox_d(
    "Priority",
    palette = "viridi",
    direction = -1,
    na.value = "transparent"
  ) +
  # legends
  labs(
    title = "Areas of global significance for biodiversity conservation with EJ cases (cluster 3)",
    ) +
  
  theme(
    #panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5), # sets latitude and longitude lines 
    panel.background = element_rect(fill = "#FFFFFF")# sets background panel color 
  )  +
  coord_sf(crs = robin)

plot_cluster
ggsave(file="outputs/backgroup_map_cluster3.svg", plot=plot_cluster, width=10, height=8, dpi = 300)
