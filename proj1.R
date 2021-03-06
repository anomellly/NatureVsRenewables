install.packages("rgeos")
library(rgeos);library(raster);library(sp);library(rgdal);library(sf)
setwd("C:/Users/conse/Desktop/Desktop/mel/Bioecon")
setwd("C:/Users/mel/NatureVsRenewables/data")


#Read in indonesia maps (shapefile)
indonesia = readOGR(dsn=getwd(), layer ="indonesia" )
crs(indonesia)
extent(indonesia)

# Project indonesia map onto Mollweide Projection
indoProj = spTransform(indonesia, crs(windIndo))
plot(indoProj)
extent(indoProj)

# Clip wind DPI map to indonesia only
#windIndo = mask(wind,indoProj)
plot(windIndo,add=T)
plot(indoProj)

# Convert indonesia map to raster 
r0 = raster(as(indoProj, "Spatial"), ncol=36000,nrow=15000)
extent(r0) = extent(indoProj)
indoProj$val = rep(1, nrow(indoProj))
indoRaster = rasterize(indoProj, r0, field = "val", background = 0)
extent(indoRaster) = extent(windIndo)
writeRaster(indoRaster,"indoMollweideProj.tif")

# Read in Indo map
indoRaster1 = raster("indoMollweideProj.tif")
plot(indoRaster1)

# Read in Wind DPI maps
#wind = raster("Wind_DPI.tif")
crs(wind)
plot(wind)


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

#to check if the raster maps are accurate 
plot(indoProj)
acerodon = mammalsProj[mammalsProj$binomial == "Acerodon celebensis",]
humilis = mammalsProj[mammalsProj$binomial == "Acerodon humilis",]
plot(mammalsProj[mammalsProj$binomial == "Acerodon celebensis"], add=T, col="red")
plot(acerodon, add=T, col="red") 
plot(humilis, add=T, col="green")

#####################
######## KBA ########
# KBA - Convert polygons to rasters
# set resolution similar to DPI map resolutions
#r = raster(as(kbaProj, "Spatial"),ncol=36000,nrow=15000)
#extent(r) = extent(kbaProj)
#kbaProj$val = rep(1, nrow(kbaProj))
#kbaRaster = rasterize(kbaProj, r, field ="val", background=0)
#extent(kbaRaster) = extent(indoProj)
#writeRaster(kbaRaster, "kbaMollweirdProj.tif")

kbaRaster = raster("kbaMollweirdProj.tif")
extent(kbaRaster) = extent(windIndo)

plot(indoProj)
plot(kbaProj, add=T, col="red")
plot(kbaProj, main = "kbaProj")
plot(kbaRaster, main="kbaRaster", axes=F, bty="n")

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


# Combine rows with same species together (because each there are duplicate rows for the same species on different islands)
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
  assign(mammalNames, readOGR(mammalNames))
}

# to check if raster maps are the same as shapefile maps
mammal1 = readOGR(dsn= getwd(), layer = "~_mammals_1")
plot(indoProj, main="mammal 1-2 shapefiles")
plot(mammal1, add=T, col = "red")

mammal2 = readOGR(dsn = getwd(), layer = "~_mammals_2")
plot(mammal2, add=T, col = "green")

# Method 2: 
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
