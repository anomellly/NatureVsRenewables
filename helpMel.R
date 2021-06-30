library(rgeos);library(raster);library(sp);library(rgdal);library(sf)

# Set wd
setwd("C:/Users/mel/Desktop/Bioecon/NatureVsRenewables/help")

# Read in Indo raster map
indoRaster = raster("indoMollweideProj.tif")
plot(indoRaster)

# Read in Indo Shp map 
indoProj = readOGR(dsn=getwd(), layer = "indonesiaMollweideShp")

# Read in Wind DPI (Development Potential Indices) maps 
#wind = raster("Wind_DPI.tif")

# Clip wind DPI map to indonesia only
#windIndo = mask(wind,indoProj)

# Save Wind maps in Indo
#writeRaster(windIndo, "wind_DPI_Indo.tif")

# Read in Wind DPI maps clipped to Indonesia 
windIndo = raster("wind_DPI_Indo.tif")

############
## Mammals## 

# Read mammals shapefiles 
mammalShp = readOGR(dsn=getwd(), layer = "mammalIndo")

# Project to Mollweide projection
mammalsProj = spTransform(mammalShp, crs(windIndo))

# Combine rows with same species together (because each there are duplicate rows for the same species on different islands)
aggregateMammals = aggregate(mammalsProj, c("binomial", "category")) 
# Plot to check
plot(aggregateMammals)
#this plot shows us two additional non-indonesian islands on the left, so we need to subset to just Indonesia

# Subset to just Indonesian islands 
library(rgeos)
#  Attempt 1: use gIntersection
newMammalIndo2 = gIntersection(aggregateMammals, indoProj) 
# gIntersection gives me a SpatialPolygon, but i need a SPDF..
# Attempt 2: use st_intersection
#newMammalIndo1 = st_intersection(aggregateMammals, indoProj)
# doesn't work on SPDF..
#coerce into SPDF so that we can writeOGR
#newMammalIndo1 = as(newMammalIndo, "SpatialPolygonsDataFrame")
#plot(newMammalIndo1)

# Attempt 3: use raster::intersect to subset 
newMammalIndo3 = raster::intersect(aggregateMammals, indoProj)
plot(indoProj)
plot(newMammalIndo3[newMammalIndo3$binomial=="Acerodon celebensis",], add=T, col="green") 
#woo it overlaps nicely!! 

# I dont think you need to run the next 6 lines, unless you are interested in spending >30 minutes waiting to extract shapefiles of each species
# Extracting each mammal species in mammalShp
species = paste0("~/mammals_", i)
for (i in seq_len(nrow(newMammalIndo3))){
  species = paste0("~/mammals_", i)
  writeOGR(newMammalIndo3[i,], layer = species, driver = "ESRI Shapefile", dsn = getwd())
}

# Reading in each shp layer to check.. though i'm not sure if this works..
mammalNames = dir(getwd(), "*.shp")
for (mammalNames in mammalNames) {
  assign(mammalNames, readOGR(mammalNames))
}

# Read in 3 shp files instead of all 270 to check 

#setwd to folder containing the 3 mammal shp files 
setwd("C:/Users/mel/Desktop/Bioecon/NatureVsRenewables/help/mammals")

# to check if raster maps are the same as shapefile maps
mammal1 = readOGR(dsn= getwd(), layer = "~_mammals_1")
plot(indoRaster, main="mammal 1-2 shapefiles")
plot(mammal1, add=T, col = "red")
  mammal2 = readOGR(dsn = getwd(), layer = "~_mammals_2")
plot(mammal2, add=T, col = "blue")
mammal3 = readOGR(dsn = getwd(), layer = "~_mammals_3")
plot(mammal3, add=T, col = "purple")

# Herein lies our problem... the mammal plots dont overlap with the indonesia map nicely
# which suggests that the extents/projection of the maps dont match up  
########################################################################

###################################################################
# This is where i attempt to convert all shapefiles into rasters: #
setwd("C:/Users/mel/NatureVsRenewables/data/mammals")
mammalList = list.files(pattern= '*.shp', full.names = FALSE)
#result = data.frame(names = mammalNames, rows = c(NA))

install.packages("stringr")
library(stringr)

# Convert each layer to a raster....? 
setwd("C:/Users/mel/NatureVsRenewables/data/mammals2") #test folder with 2 maps only
for (i in seq_along(mammalList)) {
mammal = readOGR(dsn = ".", layer = tools::file_path_sans_ext(mammalList[i]))  

r2 = raster(indoRaster1) # i think there might be a problem here
#r2.1 = raster(as(mammal, "Spatial"), ncol=36000,nrow=15000)
#r2 = raster(as(mammal, "Spatial"), res = res(indoRaster1), extent = extent(indoRaster1), crs = crs(indoRaster1))
#extent(r2) = extent(mammal)
mammal$val1 = rep(1, nrow(mammal))
mammalRaster = rasterize(mammal, r2, field = 'val1', background=0)
#extent(mammalRaster) = extent(indoRaster1)
writeRaster(mammalRaster,filename = paste0("/Users/mel/NatureVsRenewables/data/", "mammal_", 
                                           stringr::str_pad(string = i, width =4, side = "left", pad = 0 )),
                                           format = "GTiff", overwrite = T)
}

plot(r2)
# test read in files
setwd("C:/Users/mel/NatureVsRenewables/data/mammals")

mammal1 = readOGR(dsn = getwd(), layer = "~_mammals_1")
indonesia = readOGR(dsn=getwd(), layer ="indonesia" )

#######
# Plot
setwd("C:/Users/mel/NatureVsRenewables/data/")
mammal1Raster = raster("mammal_0001.tif")
mammal2Raster = raster("mammal_0002.tif")
plot(indoRaster1)
plot(mammal1Raster, add=T)
plot(mammal2Raster, add=T, col="blue")

dev.off()
plot(indoRaster1)
plot(mammal, add=T, col="red")
plot(mammalRaster, add=T, col="blue")
