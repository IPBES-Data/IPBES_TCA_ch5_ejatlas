# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
Global priority maps of areas with high conservation importance

# Downloaded from https://doi.org/10.5281/zenodo.5006332

Content:
This data repository contains the results of the NatureMap ( naturemap.earth/) conservation prioritization effort. The maps were created by jointly optimizing biodiversity and NCPs such as carbon and/or water. 

Usage notes:
Maps are supplied at both 10km and 50km resolution unless specified differently in the manuscript.
All maps that aim to find priority areas for all species considered in the analysis, utilize a series of representative sets. 
The ranks for each layer are area-specific and can be used to extract summary statistics by simple subsetting. 
For example:
To obtain the top 30% of land area for biodiversity and carbon, one needs to create a mask of all areas lower than a value of 30 from the respective ranked layers.

For convenience two files are supplied that contain the fraction of land area per grid cell times 1000. Multiplying those with the cell area (100km2, respectively 2500km2) gives the exact amount of land area in a given grid cell.
These are labelled " globalgrid_mollweide_**km.tif " can be used to create masks for the priority maps.

Spatial resolution: 
10 and 50 km

Geographic projection: 
World Mollweide Equal Area projection
PROJ4 ( +proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs )

Filename suffix description:

'minshort_speciestargets'
=- Problem formulation where targets were achieved by minimzing a shortfall

'repruns10'
=- The number of representative that were used to create the ranked layer

'biome.id'
=- Species distribution were split by biome, thus creating separate targets for subpopulation

'withPA'
=- Fractions of current protected areas (Date: WDPA 2019) were locked in as baseline and starting budget. Approximately 15% of the globe. Note that not entire grid cells, but fractions were locked in and build opon!

'carbon'
=- Carbon was included in the prioritization and jointly optimized together with the other assets by giving it equal weighting (see manuscript)

'water'
=- Water was included in the prioritization and jointly optimized together with the other assets by giving it equal weighting (see manuscript)

License:
CC-BY-SA 4.0

Citation:
Jung, Martin, Andy Arnell, Xavier De Lamo, Shaenandhoa Garcia-Rangel, Matthew Lewis, Jennifer Mark, Cory Merow et al. (2021) "Areas of global importance for terrestrial biodiversity, carbon, and water." Nature Ecology & Evolution

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
