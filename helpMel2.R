install.packages("rgeos")
install.packages("rgdal")
install.packages("sf")

library(rgeos);
library(raster);
library(sp);
library(rgdal);
library(sf)

# Set wd
setwd("D:\\Dropbox\\honors projects\\2021-2\\MelanieWind")
setwd("C:/Users/mel/Desktop/Bioecon/NatureVsRenewables/help")

# Read in Indo raster map. RC: 500 million cells is too much, should be less than 500k.
#indoRaster = raster("indoMollweideProj.tif")
#plot(indoRaster)



# Read in Indo Shp map 
indoProj = readOGR(dsn=getwd(), layer = "indonesiaMollweideShp")
plot(indoProj)

extent(indoProj)

# Convert to raster 
r <- raster(as(indoProj, "Spatial"), ncols = 400, nrows = 400)
indoProj$val1 = rep(1,nrow(indoProj))
#background NA is better than 0 as we get rid of the cells for the ocean, table is much smaller
indoRaster <- rasterize(indoProj, r, field="val1", background = NA)
plot(indoRaster)

#I convert the raster into polygons
#This is the placeholder for all the species information to be stored
indoGrid = rasterToPolygons(x=indoRaster, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)

plot(indoGrid)
head(indoGrid)


############
## Mammals## 

# Read mammals shapefiles 
mammalShp = readOGR(dsn=getwd(), layer = "mammalIndo")

View(mammalShp[mammalShp$binomial == "Zaglossus attenboroughi",])

# Project to Mollweide projection
mammalsProj = spTransform(mammalShp, crs(indoRaster))

# Combine rows with same species together (because each there are duplicate rows for the same species on different islands)
aggregateMammals = aggregate(mammalsProj, c("binomial","category"))
# Plot to check. RC: nice but we lose the other columns, like threat status
plot(aggregateMammals)
#this plot shows us two additional non-indonesian islands on the left, so we need to subset to just Indonesia


###################################################################
# This is where i attempt to convert all shapefiles into rasters: #
#I use the package velox for extract that is much faster, need to install from github

install.packages("devtools")

library(devtools)
install_github("hunzikp/velox")
library(velox)


list_sps = list()
#This loops through all the species, makes them a raster and puts them in a list
#Putting all the species rasters in a list makes extract work even faster doing all at once
for(i in 1:nrow(aggregateMammals)){

sps_i = aggregateMammals[i,]

sps_i = spTransform(sps_i, crs(indoGrid))

r <- raster(as(indoGrid, "Spatial"), ncols = 400, nrows = 400)
sps_i$val1 = rep(1,nrow(sps_i))
sps_i_ras <- rasterize(sps_i, r, field="val1", background = NA)

#rasters are converted to velox raster objects
list_sps[[i]] = velox(sps_i_ras)

print(i)#to see how many species have been done

}#closes species loop

#The list needs to be converted again in velox object, otherwise extract does not work
list_sps2 = velox(list_sps)


start_time <- Sys.time()

output1 <- list_sps2$extract(indoGrid, fun=mean)

end_time <- Sys.time()
end_time-start_time #takes one minute to do 283 maps for mammals


head(output1,150) 

#Each column is one mammal I put the names
colnames(output1) = aggregateMammals$binomial

#Now extracting separately for variables that are on their own like DPI wind, KBAs, PAs etc
#For instance with DPI

# Read in Wind DPI (Development Potential Indices) maps 
wind = raster("Wind_DPI\\Wind_DPI.tif")

plot(wind)

#Takes ages, trying velox
# Clip wind DPI map to indonesia only
#windIndo = mask(wind,indoGrid)

#windProj <- projectRaster(windIndo, crs=crs(indoGrid))


# Save Wind maps in Indo. RC: I think writing out and in may be creating problems.
#Let's try to use directly here the masked object.
#writeRaster(windIndo, "wind_DPI_Indo.tif")

# Read in Wind DPI maps clipped to Indonesia.
#RC: Wind DPI seems to be just ones, maybe it is a duplicate of IndoRaster
#windIndo = raster("wind_DPI_Indo.tif")
#Project is Mollweide but just in case I make sure the Indonesia map is the same projection
indoProj2 = spTransform(indoProj, crs(windIndo))

#I try to crop with velox first, quite fast
wind2 = velox(wind)

#I crop wind DPI to Indonesia using velox
wind2$crop(extent(indoProj2))


rm(wind)

#Extract and add as new variable to the output

windExtr <- wind2$extract(indoGrid, fun=mean)

#Add it as a new column
#Indonesia has very little wind farm potential, mostly NAs. Let's try with solar as well

output1$windDPI <- windExtr

output2 = data.frame(output1)

output2$windDPI <- windExtr

head(output2,150)


#Doing some checks with some species to see the extraction worked correctly

#bring the values back to indoGrid for plotting
indoGrid$Tupaia.ferruginea = output2$Tupaia.ferruginea

indoGridTest = indoGrid[!is.na(indoGrid$Tupaia.ferruginea),]

indoGrid$Nycticebus.javanicus = output2$Nycticebus.javanicus

indoGridTest = indoGrid[!is.na(indoGrid$Nycticebus.javanicus),]




par(mfrow=c(1,2))

plot(indoGridTest, lwd = 0.01, col = "blue")

plot(indoGrid, lwd = 0.001, col = indoGrid$Tupaia.ferruginea)
plot(indoGrid, lwd = 0.001, col = c("blue")[indoGrid$Tupaia.ferruginea])


#Names of columns has introduced a dot instead of space
spsTest = aggregateMammals[aggregateMammals$binomial=="Tupaia ferruginea",]
spsTest = aggregateMammals[aggregateMammals$binomial=="Nycticebus javanicus",]
spsTest = aggregateMammals[aggregateMammals$binomial=="Zaglossus attenboroughi",]


plot(indoGrid, lwd = 0.001)
plot(spsTest, add=T, col = "red")
plot(spsTest)

extent(spsTest)[1]

sc = 1
plot(indoProj, axes = FALSE, xlim = c(extent(spsTest)[1]-100000*sc, extent(spsTest)[2]+100000*sc), 
ylim = c(extent(spsTest)[3]+100000*sc, extent(spsTest)[4]-100000*sc))
plot(spsTest, add=T, col = "red")

sc = 10
plot(indoProj, axes = FALSE, xlim = c(10502804-100000*sc, 10504659+100000*sc), ylim = c(-833592.8+100000*sc, -830731.8-100000*sc))
plot(spsTest, add=T, col = "red")

10502804- 10504659

indoGrid$windDPI = output2$windDPI

plot(indoGrid, lwd = 0.001, col = indoGrid$windDPI)

#Adding solar DPI now##############

# Read in concentrated solar DPI (Development Potential Indices) maps 
solar = raster("CSP_DPI\\CSP_DPI.tif")

indoProj2 = spTransform(indoProj, crs(solar))

#I try to crop with velox first, quite fast
solar2 = velox(solar)

#I crop solar DPI to Indonesia using velox
solar2$crop(extent(indoProj2))


rm(solar)

#Extract and add as new variable to the output

solarExtr <- solar2$extract(indoGrid, fun=mean)

#Add it as a new column
#Indonesia has very little wind farm potential, mostly NAs. Let's try with solar as well

output2$solarDPI <- solarExtr

head(output2,150)



#To see how solar DPI looks we rasterize only that field (solarDPI) and plot
r <- raster(as(indoGrid, "Spatial"), ncols = 400, nrows = 400)
indoGrid$solarDPI = output2$solarDPI
solarRaster <- rasterize(indoGrid, r, field="solarDPI", background = NA)

plot(indoProj)

plot(solarRaster,add=T)



#We have two useful objects now, indoGrid that will allow us to put any variable back into a map and
#output2 which is the main data we will use in Gurobi
#I save them in the computer

write.csv(output2, "output2.csv", row.names = F)
writeOGR(indoGrid, ".", "indoGrid", driver="ESRI Shapefile")



#################################################################################################################
#GUROBI##########################################################################################################
#################################################################################################################

#Let's try to answer the question of maximizing solar power generation while not allowing any endemic mammals species to lose
#more than x % of their range

setwd("D:\\Dropbox\\honors projects\\2021-2\\MelanieWind")

data1 = read.csv("output2.csv", header = T)
head(data1)

#I replace NA with zero
data1[is.na(data1)]=0

#The data are planning units in rows and species, wind and solar DPI in columns
#We are going to use sps as constraints and solar DPI as objective function, I will split them, exclude last two columns

dataSps = data1[,1:(ncol(data1)-2)]
head(dataSps)

#Gurobi expects sps by rows and planning units by columns, I transpose dataSps
dataSps2 = t(dataSps)

# load the gurobi R package (requires that both Gurobi and the
# Gurobi R package have both been installed):

#install.packages("C:\\gurobi912\\win64\\R\\gurobi_9.1-2.zip", repos=NULL)
#install.packages("slam")

#library(slam)
#library(gurobi)
require(gurobi)

# specify the number of species:
ns <- nrow(dataSps2) # species
# number of planning units
N <- ncol(dataSps2)
# create a vector of type of constraint. In this case, choosing a planning unit means they will put solar panels
#and that species with a range there will lose some of their range so the constraint is that we don't want that to be greater
#than a threshold of how much we are willing to lose
sense <- rep("<=", ns)
# set the targets (the right hand side of the constraint equations);
# here, we assume that we don't want any of the species to lose more than 0.2% of their range
targetlevel <- 0.002
rhs <- rep(0, ns)#just creating a vector first
#RC: this works out how much is 0.2% of the current range for a given sps is
rhs <- targetlevel * apply(dataSps2,1,sum)

# set up Gurobi model object in R
model <- list()

# set this as a maximization problem:
model$modelsense <- "max"
# set all decision variables as binary:
model$vtype <- "B"
# vector of state values that are being maximized (solar DPI):
#This translates the matrix into a vector similar to each of the rows for species
model$obj <- as.vector(t(data1$solarDPI))
# assign the constraints matrix object, and the right hand side
model$A <- dataSps2
model$rhs <- rhs
model$sense <- sense
# set the parameters that control the algorithm (the algorithm stops when
# a gap of 0.5% is achieved in this example):
params <- list(Presolve=2,MIPGap=0.005)
# solve the problem
result <- gurobi(model,params)
# the result object contains several data objects including the objective
# value achieved (result$objval) and the vector of decision variable
result$objval
# values (0 or 1 in our example because the variables are binary).

result$x


#Let's see which areas would be converted into solar power

indoGrid2 = readOGR(dsn=getwd(), layer = "indoGrid")

#We bring the solution from results$x, add it to indoGrid as a new column and plot the raster that results from it
r <- raster(as(indoGrid, "Spatial"), ncols = 400, nrows = 400)
indoGrid$solarSol = result$x
solarSolRaster <- rasterize(indoGrid, r, field="solarSol", background = NA)

plot(indoProj)

plot(solarSolRaster,add=T)

#Compare with all the areas where it could have gone

par(mfrow=c(2,1))
plot(indoProj, main = "potential DPI", mar = rep(0, 4))
plot(solarRaster,add=T)

plot(indoProj, main = "optimal solution", mar = rep(0, 4))
plot(solarSolRaster,add=T)

#Which species are most affected by this solution
#I subset the species for the columns (planning units) chosen
#I get the number of cells of range they lost
apply(dataSps2[,result$x==1],1,sum)
#As a percentage of their total range, then sorted
sort(apply(dataSps2[,result$x==1],1,sum)/apply(dataSps2,1,sum), decreasing = T)


#trying to analyze sps solar power trade-offs

#I put in a function to be faster

optimSolar = function(targetlevel){
rhs <- targetlevel * apply(dataSps2,1,sum)
model <- list()
model$modelsense <- "max"
model$vtype <- "B"
model$obj <- as.vector(t(data1$solarDPI))
model$A <- dataSps2
model$rhs <- rhs
model$sense <- sense
params <- list(Presolve=2,MIPGap=0.005)
result <- gurobi(model,params)
result$objval
}

targetlevelSeq = seq(0,1,0.01)

objVec = sapply(targetlevelSeq,optimSolar)

plot(targetlevelSeq,objVec, type = "l") 



        