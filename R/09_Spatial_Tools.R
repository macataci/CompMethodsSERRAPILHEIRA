#Classic packages R for spatial:
#stars, terra, leaflet, mapview
#raster, sf, tmap
#vector data in shapefile format .shp, .shx, .dbf and projection .prj


#https://rspatialdata.github.io/

#install.packages("sf")
#install.packages("tmap")
#install.packages("raster")

#install.packages("rnaturalearth")
#install.packages("remotes")
#remotes::install_github("ropensci/rnaturalearthhires")

library(rnaturalearth)
library(rnaturalearthhires)
library(sf)
library(tmap)
library(raster)
library(dplyr)

data(World)
#geometry es lista de multipolygon
tm_shape(World) +  tm_borders()

#head(World)
names(World)
class(World)
dplyr::glimpse(World)

plot(World[1])
plot(World[,1])
plot(World[1,])
plot(World["pop_est"])
head(World[, 1:4])
class(World)
class(World$geometry)

head(sf::st_coordinates(World))
no_geom <- sf::st_drop_geometry(World)
class(no_geom)
st_bbox(World)

names(World)
unique(World$continent)

World %>%
  filter(continent == "South America") %>%
  tm_shape() +
  tm_borders()

World %>%
  mutate(our_countries = if_else(iso_a3 %in% c("COL","BRA", "MEX"), "red", "grey")) %>%
  tm_shape() +
  tm_borders() +
  tm_fill(col = "our_countries") +
  tm_add_legend("fill",
                "Countries",
                col = "red")


bra <- ne_states(country = "brazil", returnclass = "sf")
plot(bra)

dir.create("data/shapefiles", recursive = TRUE)
st_write(obj = bra, dsn = "data/shapefiles/bra.shp", delete_layer = TRUE)

bra2 <- read_sf("data/shapefiles/bra.shp")
class(bra)
class(bra2)
plot(bra)
plot(bra2)

dir.create(path = "data/raster/", recursive = TRUE)
tmax_data <- getData(name = "worldclim", var = "tmax", res = 10, path = "data/raster/")
plot(tmax_data)
is(tmax_data) #the data are a raster stack, several rasters piled
dim(tmax_data)
extent(tmax_data)
res(tmax_data)

#---------Choose your own adventure:

#Check the vignettes in sf
#Check the tutorials in rspatialdata
#Check the multiple CRAN TaskViews regarding spatial data
