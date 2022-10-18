install.packages(c("sp","rgdal","raster","rgeos","geosphere","dismo"))

library(sp) # classes for vector data (polygons, points, lines)
library(rgdal) # basic operations for spatial data
library(raster) # handles rasters
library(rgeos) # methods for vector files
library(geosphere) # more methods for vector files
library(dismo) # species distribution modeling tools

bio1<- raster("WORLDCLIM_Rasters/wc2.1_10m_bio_1.tif")
plot(bio1)

#variables here: https://www.worldclim.org/data/bioclim.html

bio1

round(res(bio1)[1], 2)
round(res(bio1)[1] * 100, 0) 

# you can do math on a raster just like any variable in R. Let's convert to F

bio1_f <- bio1 * (9/5)+32
plot(bio1_f)

# If our rasters have the same extent and resolution we can stack them. 

clim_stack <- stack(list.files("WORLDCLIM_Rasters", full.names = TRUE, pattern = ".tif"))

#tell students how list.files is useful
ll<-list.files("WORLDCLIM_Rasters", full.names = TRUE, pattern = ".tif")

plot(clim_stack, nc = 5) # nc plots five columns of the 19 rasters

clim_stack

#pick a subset of 3-5 variables. 

my_clim_stack <- subset(clim_stack, c(9, 12, 14))


my_clim_stack <- stack(
  raster('WORLDCLIM_Rasters/wc2.1_10m_bio_2.tif'),
  raster('WORLDCLIM_Rasters/wc2.1_10m_bio_4.tif'),
  raster('WORLDCLIM_Rasters/wc2.1_10m_bio_17.tif')
)

plot(my_clim_stack)

pairs(my_clim_stack) #pairs is a base R function that plots univariate distribution and bivariate relationships

# lets load the shapefile

countries <- shapefile("Country_Shapefiles/ne_10m_admin_0_countries.shp")
countries
head(countries)

plot(countries, col='goldenrod', border='darkblue')  

# now plot the raster and the shapefile :
#for accurate mapping of the clicks you will need a new divice
dev.new()
plot(my_clim_stack[[2]]) # plot mean annual temperature
plot(countries, add=TRUE) # add countries shapefile
mySites <- as.data.frame(click(n=10))

names(mySites) <- c('longitude', 'latitude')
mySites

write.csv(mySites, "my_sites.csv")

env <- as.data.frame(extract(my_clim_stack, mySites))
env

#you can zoom a region by defining extent and replotting
zm <- drawExtent()
plot(my_clim_stack[[1]], ext=zm)
plot(countries, add=TRUE)

mySites <- cbind(mySites, env)
mySites


myCrs <- projection(my_clim_stack) # get projection info

# make into points file
mySitesShape <- SpatialPointsDataFrame(coords=mySites, data=mySites, proj4string=CRS(myCrs))
mySitesShape

plot(my_clim_stack[[2]])
plot(countries, add=TRUE)
points(mySitesShape, pch=16) # show sites on the map

writeOGR(mySitesShape, 'My_locations', 'mySitesShape', driver='ESRI Shapefile')

projection(mySitesShape)
projection(countries)

countries <- spTransform(countries, CRS(projection(mySitesShape)))

countries$id <- 1:nrow(countries)

myCountries <- over(mySitesShape, countries)
as.data.frame(myCountries) # just look at data frame portion

myCountries <- countries[countries$id %in% myCountries$id, ]
plot(myCountries)
points(mySitesShape, col='red', pch=16)

# converts shapefile to raster... takes a *long* time for lareg rasters
myCountriesMask <- rasterize(myCountries, my_clim_stack[[2]])
myCountriesMask <- myCountriesMask * 0 + 1 # make all values 1

my_clim_sites <- my_clim_stack[[2]] * myCountriesMask
plot(my_clim_sites)


# pick random points to model against

bg <- as.data.frame(randomPoints(my_clim_stack, n=10000)) # 10,000 random sites
names(bg) <- c('longitude', 'latitude')
head(bg)

#show a plot of the random points
plot(my_clim_stack[[2]])
points(bg, pch='.') # plot on map

# extract enviro variables for the random points
bgEnv <- as.data.frame(extract(my_clim_stack, bg))
head(bgEnv)

# join with the lat and long
bg <- cbind(bg, bgEnv)

# combine background and my sites 

presBg <- c(rep(1, nrow(mySites)), rep(0, nrow(bg)))

trainData <- data.frame(
  presBg,
  rbind(mySites, bg))

head(trainData) # first 6 rows
tail(trainData)

names(my_clim_stack)

# where does this model come from: 

myModel <- glm(
  presBg ~ wc2.1_10m_bio_2*wc2.1_10m_bio_4*wc2.1_10m_bio_17 + I(wc2.1_10m_bio_2^2) + I(wc2.1_10m_bio_4^2) + I(wc2.1_10m_bio_17^2),
  data=trainData,
  family='binomial',
  weights=c(rep(1, nrow(mySites)), rep(nrow(mySites) / nrow(bg), nrow(bg)))
)

summary(myModel)

myWorld <- predict(
  my_clim_stack,
  myModel,
  type='response'
)

myWorld

plot(myWorld)
plot(countries, add=TRUE)
points(mySitesShape, col='red', pch=16)

###

writeRaster(myWorld, 'My_Climate_Niche/myWorld', format='GTiff', overwrite=TRUE, progress='text')

myWorldThresh <- myWorld >= quantile(myWorld, 0.75)
plot(myWorldThresh)

# convert all values not equal to 1 to NA...
# using "calc" function to implement a custom function
myWorldThresh <- calc(myWorldThresh, fun=function(x) ifelse(x==0 | is.na(x), NA, 1))

# get random sites
myBestSites <- randomPoints(myWorldThresh, 10000)
myBestEnv <- as.data.frame(extract(my_clim_stack, myBestSites))

# plot world's climate
smoothScatter(x=bgEnv$wc2.1_10m_bio_2, y=bgEnv$wc2.1_10m_bio_17, col='lightblue', xlab='Mean Diurnal Range', ylab='Precipitation of Driest Quarter')
points(myBestEnv$wc2.1_10m_bio_2, myBestEnv$wc2.1_10m_bio_17, col='red', pch=16, cex=0.2)
points(mySites$wc2.1_10m_bio_2, mySites$wc2.1_10m_bio_17, pch=16)
legend(
  'bottomright',
  inset=0.01,
  legend=c('world', 'my niche', 'my locations'),
  pch=16,
  col=c('lightblue', 'red', 'black'),
  pt.cex=c(1, 0.4, 1)
  
)
