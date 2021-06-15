install.packages("rgeos")
library(rgeos);library(raster);library(sp);library(rgdal);library(sf)
setwd("C:/Users/conse/Desktop/Desktop/mel/Bioecon")
setwd("C:/Users/mel/Desktop/Bioecon/NatureVsRenewables/data")


#Read in indonesia maps 
indonesia = readOGR(dsn=getwd(), layer ="indonesia" )
crs(indonesia)

# Read in Wind DPI maps
#wind = raster("Wind_DPI.tif")
crs(wind)
plot(wind)

# Project indonesia map onto Mollweide Projection
indoProj = spTransform(indonesia, crs(windIndo))
plot(indoProj)

# Clip wind DPI map to indonesia only
#windIndo = mask(wind,indoProj)
plot(windIndo,add=T)
plot(indoProj)

# Save Wind maps in Indo
#writeRaster(windIndo, "wind_DPI_Indo.tif")
windIndo = raster("wind_DPI_Indo.tif")

# Read CSP 
#csp = raster("CSP_DPI.tif")
#cspIndo = mask(csp, indoProj)
#writeRaster(cspIndo, "csp_DPI_Indo.tif")
cspIndo = raster("csp_DPI_Indo.tif")

# Read PV 
#pv = raster("PV_DPI.tif")
#pvIndo = mask(pv, indoProj)
#writeRaster(pvIndo, "pv_DPI_Indo.tif")
pvIndo = raster("pv_DPI_Indo.tif")

# Read KBA, mammals 
#kba = readOGR(dsn=getwd(), layer = "KBAsIndonesia")
mammals = readOGR(dsn=getwd(), layer = "mammalIndo")

crs(kba)
crs(mammals)

# Project to Mollweide projection
kbaProj = spTransform(kba, crs(windIndo))
mammalsProj = spTransform(mammals, crs(windIndo))

## KBA ##
# KBA - Convert polygons to rasters
# set resolution similar to DPI map resolutions
#r = raster(as(kbaProj, "Spatial"),ncol=36000,nrow=15000)
#extent(r) = extent(kbaProj)
#kbaProj$val = rep(1, nrow(kbaProj))
#kbaRaster = rasterize(kbaProj, r, field ="val", background=0)
#extent(kbaRaster) = extent(indoProj)
#writeRaster(kbaRaster, "kbaMollweirdProj.tif")

kbaRaster = raster("kbaMollweirdProj.tif")

plot(indoProj)
plot(kbaProj, add=T, col="red")
plot(kbaProj, main = "kbaProj")
plot(kbaRaster1, main="kbaRaster", axes=F, bty="n")

## Mammals## 
# Mammals - Convert to raster 
r1 = raster(as(mammalsProj, "Spatial"), ncol=36000,nrow=15000)
extent(r1) = extent(mammalsProj)
mammalsProj$val = rep(1, nrow(mammalsProj))
mammalsRaster = rasterize(mammalsProj, r1, field = "val", background = 0)
extent(mammalsRaster) = extent(indoProj)
writeRaster(mammalsRaster,"mammalsMollweideProj.tif")

plot(indoProj)
plot(mammalsRaster)
plot(mammalsProj, add=T, col="red")


# combine rows with same species together 
aggregateMammals = aggregate(mammalsProj, "binomial")
plot(indoProj, main="aggregated mammals")
plot(aggregateMammals, add=T,col="green")


# Extracting each mammal species in mammal 
for (i in seq_len(nrow(aggregateMammals))){
  species = paste0("~/mammals_", i)
  writeOGR(aggregateMammals[i,], layer = species, driver = "ESRI Shapefile", dsn = getwd())
}

# Read in each layer.. 
mammalNames = dir(getwd(), "*.shp")
for (mammalNames in mammalNames) {
  assign(mammalNames, read(mammalNames))
}

# Convert each layer to a raster....? 
for (i in 283) {
  
  
}

#testing change
