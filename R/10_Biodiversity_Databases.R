#install.packages("rgbif")
#install.packages("Taxonstand")
#install.packages("CoordinateCleaner")
#install.packages("maps")

library(rgbif)
library(Taxonstand)
library(CoordinateCleaner)
library(maps)
library(dplyr)
library(tmap)
library(sf)

#palettes

library(RColorBrewer)
display.brewer.all(type = "seq")
display.brewer.all(type = "div")


#
species <- "Myrsine coriacea"
occs <- occ_search(scientificName = species, limit = 100000)
names(occs)

myrsine.data <- occs$data
colnames(myrsine.data)
dir.create("data/raw/", recursive = TRUE)
write.csv(myrsine.data,
          "data/raw/myrsine_data.csv",
          row.names = FALSE)
sort(unique(myrsine.data$scientificName))
table(myrsine.data$taxonomicStatus)
table(myrsine.data$scientificName, myrsine.data$taxonomicStatus)
species.names <- unique(myrsine.data$scientificName)
dim(species.names)
tax.check <- TPL(species.names)
tax.check

# creating new object w/ original and new names after TPL
new.tax <- data.frame(scientificName = species.names,
                      genus.new.TPL = tax.check$New.Genus,
                      species.new.TPL = tax.check$New.Species,
                      status.TPL = tax.check$Taxonomic.status,
                      scientificName.new.TPL = paste(tax.check$New.Genus,
                                                     tax.check$New.Species))
# now we are merging raw data and checked data
myrsine.new.tax <- merge(myrsine.data, new.tax, by = "scientificName")

dir.create("data/processed/", recursive = TRUE)
write.csv(myrsine.new.tax,
          "data/processed/data_taxonomy_check.csv",
          row.names = FALSE)
plot(decimalLatitude ~ decimalLongitude, data = myrsine.data, asp = 1)
map(, , , add = TRUE)
myrsine.coord <- myrsine.data[!is.na(myrsine.data$decimalLatitude)
                              & !is.na(myrsine.data$decimalLongitude),]

# output w/ only potential correct coordinates
geo.clean <- clean_coordinates(x = myrsine.coord,
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               species = "species",
                               value = "clean")
table(myrsine.coord$country)
table(geo.clean$country)
par(mfrow = c(1, 2))
plot(decimalLatitude ~ decimalLongitude, data = myrsine.data, asp = 1)
map(, , , add = TRUE)
plot(decimalLatitude ~ decimalLongitude, data = geo.clean, asp = 1)
map(, , , add = TRUE)
par(mfrow = c(1, 1))
myrsine.new.geo <- clean_coordinates(x = myrsine.coord,
                                     lon = "decimalLongitude",
                                     lat = "decimalLatitude",
                                     species = "species",
                                     value = "spatialvalid")

table(myrsine.new.geo$.summary)
tail(names(myrsine.new.geo))
# merging w/ original data
dim(myrsine.data)
dim(myrsine.new.geo)


# merging w/ original data
myrsine.new.geo2 <- merge(myrsine.data, myrsine.new.geo,
                          all.x = TRUE,
                          by = "key")
dim(myrsine.new.geo2)
full_join(myrsine.data, myrsine.new.geo)
plot(decimalLatitude.x ~ decimalLongitude.x, data = myrsine.new.geo2, asp = 1,
     col = if_else(myrsine.new.geo2$.summary, "green", "red"))
map(, , , add = TRUE)
write.csv(myrsine.new.geo2,
          "data/processed/myrsine_coordinate_check.csv",
          row.names = FALSE)

##tmap

myrsine.final <- left_join(myrsine.coord, myrsine.new.geo2)
nrow(myrsine.final)
myrsine_sf <- st_as_sf(myrsine.final, coords = c("decimalLongitude", "decimalLatitude"))
st_crs(myrsine_sf)
myrsine_sf <- st_set_crs(myrsine_sf, 4326)
st_crs(myrsine_sf)
#dir.create("data/shapefiles", recursive = T)
#st_write(myrsine_sf, dsn = "data/shapefiles/myrsine.shp")

data(World)

SAm_map <- World %>%
  filter(continent %in% c("South America", "North America")) %>%
  tm_shape() +
  tm_borders()


SAm_map + tm_shape(myrsine_sf) + tm_bubbles(size = 0.2, col = ".summary")

tmap_mode("view")
World %>%
  filter(continent %in% c("South America", "North America")) %>%
  tm_shape() +
  tm_borders() +
  tm_shape(myrsine_sf) +
  tm_bubbles(size = 0.2)
