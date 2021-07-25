library(rgeos);library(raster);library(sp);library(rgdal);library(sf)
library(velox)

# Set wd
#setwd("C:/Users/mel/Desktop/Bioecon/NatureVsRenewables/help")
setwd("C:/Users/mel/NatureVsRenewables/data")

#############
# Indonesia #

# Read in Indo Shp map 
indo = readOGR(dsn=getwd(), layer = "indonesia")

# Convert to raster
r <- raster(as(indo, "Spatial"),ncols = 400, nrows = 400)
indo$val1 = rep(1, nrow(indo))
#set background as NA to remove cells in the ocean (saves time)
indoRaster <- rasterize(indo, r, field = "val1", background = NA)
plot(indoRaster)

# Converting raster to polygons 
# Placeholer for all species information to be stored
indoGrid = rasterToPolygons(x=indoRaster, fun=NULL, n=4, na.rm=T, digits = 12, dissolve = F)

plot(indoGrid)
head(indoGrid)

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

#install.packages("devtools")
library(devtools)
#install_github("hunzikp/velox")
library(velox)
#cant install velox as it only supports R < ver 4 

# Loop through all species and puts them into a list list_sps
list_sps = list()

for (i in 1:nrow(aggregateMammals)){
  sps_i = aggregateMammals[i,]
  sps_i = spTransform(sps_i, crs(indoGrid))
  r0 <- raster(as(indoGrid, "Spatial"), ncols = 400, nrows = 400)
  sps_i$val1 = rep(1,nrow(sps_i))
  sps_i_ras <- rasterize(sps_i, r, field = "val1", background = NA)
  
  #rasters converted to velox raster objects 
  list_sps[[i]] = velox(sps_i_ras)
  print(i)
  
}

# Convert list_sps into velox object, for extraction to work 
list_sps2 = velox(list_sps)

start_time = Sys.time() #to calculate the time 
output1 = list_sps2$extract(indoGrid, fun=mean) 
#why is output1 a matrix 
end_time = Sys.time()
end_time-start_time
#took 1.5 minutes to do 283 maps

# Name the columns 
colnames(output1) = aggregateMammals$binomial
#output1 isnt a matrix anymore...? 
head(output1, 150)


############
# Wind DPI #

wind = raster("C:\\Users\\mel\\Desktop\\Bioecon\\DPI\\Wind_DPI.tif")
# Check if projections are the same
crs(wind) 
crs(indoProj)

# Clip wind map with velox 
wind2 = velox(wind) #convert to velox object first 
wind2$crop(extent(indoProj))

rm(wind)

# Extract wind DPI in Indonesia only
windExtr <- wind2$extract(indoGrid, fun=mean)

# supposedly adding windExtr as a new column 
output2 = data.frame(output1)
output2$windDPI = windExtr


# Check if extraction has worked correctly 
#idg wats goin on
indoGrid1 = indoGrid
indoGrid1$Tupaia.ferruginea = output2$Tupaia.ferruginea #assign mammal A as a layer in indoGrid

indoGridTest = indoGrid1[!is.na(indoGrid$Tupaia.ferruginea),]

indoGrid1$Nycticebus.javanicus = output2$Nycticebus.javanicus

indoGridTest = indoGrid1[!is.na(indoGrid$Nycticebus.javanicus),]

par(mfrow=c(1,2))
plot(indoGridTest, lwd=0.01, col="blue")
plot(indoGrid1, lwd = 0.001, col = indoGrid1$Tupaia.ferruginea) #colour by factor
plot(indoGrid1, lwd = 0.001, col = c("blue")[indoGrid1$Tupaia.ferruginea], add=T)
#y is nothing coming up.... 

#my attempt idk sia
dev.off()

par(mfrow=c(1,2))
plot(indoGrid, lwd=0.01)
plot(output2$Tupaia.ferruginea, col="blue")


##########
# CSP DPI#
csp = raster("C:\\Users\\mel\\Desktop\\Bioecon\\DPI\\CSP_DPI.tif")

# Project to mollewoide 
indoProj = spTransform(indo, crs(csp))
crs(csp)
crs(indoProj)

csp2 = velox(csp)
csp2$crop(extent(indoProj))
rm(csp)

cspExtr <- csp2$extract(indoGrid, fun=mean)
indoGrid1$cspDPI <- cspExtr
head(output2,150)


r = raster(as(indoGrid, "Spatial"), ncols=400, nrows=400)
indoGrid1$cspDPI = output2$cspDPI
cspRaster = rasterize(indoGrid1, r, field="cspDPI", background=NA)

plot(indoRaster)
plot(cspRaster, add=T, col="blue")

plot(cspRaster)

