Demo R maps
========================================================
Code for the workshop 'Maps with R' for the R Users Meeting Madrid.

```{r options, include=FALSE}
opts_chunk$set(warning = FALSE, error=TRUE, message=TRUE, tidy=TRUE, fig.width=7, fig.height=7)
```

```{r setwd}
setwd("~/Dropbox/Maps_with_R/")
```

1. Read shapefiles into R and plot some meaningful data

> Load a Shapefile into R  (downladed from [madrid.org](http://www.madrid.org/nomecalles/DescargaBDTCorte.icm))         

##### THE PARKS
```{r load_parks}
library(maptools)
library(sp)

getinfo.shape("~/Dropbox/Maps_with_R/Shapefiles/MAD_CM_parques_jardines/parques.shp")

ED50<-CRS(paste("+proj=utm +zone=30 +ellps=intl +units=m +no_defs"))
# http://cicero.azavea.com/docs/epsg_codes.html equal to 'ED50(ED77) / UTM zone 38N' but Marid is in the UTM zone 30

parquesMadrid <- readShapePoints("~/Dropbox/Maps_with_R/Shapefiles/MAD_CM_parques_jardines/parques.shp", proj4string = ED50)
str(parquesMadrid, max.level = 2)
head(parquesMadrid@data)
parquesMadrid@bbox

parquesMadrid@data <- as.data.frame(apply(parquesMadrid@data, 2, function(x) iconv(x, "latin1", "UTF-8")))
plot(parquesMadrid, col = parquesMadrid$MUNICIPIO)

parquesMadrid <- parquesMadrid[parquesMadrid$MUNICIPIO == "Madrid", ]
plot(parquesMadrid, col = parquesMadrid$MUNICIPIO)

plot(parquesMadrid, pch = 20, cex = 2, col = "springgreen3")
```

#### THE NEIGHBORHOODS

```{r load_barrios}
getinfo.shape("~/Dropbox/Maps_with_R/Shapefiles/MAD_barrios_madrid/barrios_madrid.shp") # It is a SpatialPolygonsDataFrame
barriosMadrid <- readShapePoly("~/Dropbox/Maps_with_R/Shapefiles/MAD_barrios_madrid/barrios_madrid.shp", proj4string = ED50)
str(barriosMadrid, max.level = 2)

head(coordinates(barriosMadrid))
barriosMadrid@bbox
head(barriosMadrid@data)

plot(barriosMadrid)
plot(parquesMadrid, pch = 20, cex = 2, col = "springgreen3", add = TRUE)

```

> Plot some meaningful data   

#### POPULATION DENSITY   
Read the csv file with the Madrid density http://www-2.munimadrid.es/CSE6/control/seleccionDatos?numSerie=14010100010
```{r pop_den}
denMad <- read.csv("Data/denBarriosMad.csv", sep = ";", stringsAsFactors = FALSE)
colnames(denMad) <- c("Barrio", "Densidad")
head(denMad)
# Los barrios tienen una estructura ##.# Barrio (el resto distritos)

denMad <- denMad[which(grepl("[0-9]{2}\\.[0-9]", denMad$Barrio) == TRUE), ]
head(denMad)

length(denMad$Barrio)
length(barriosMadrid$DESBDT)
```

Merge the data with the SpatialPolygonDataFrame
```{r merge_data}
head(barriosMadrid@data)
head(denMad$Barrio)

# Get ready the Spatial object data to merge
barriosMadrid$DESBDT <- iconv(barriosMadrid$DESBDT, "latin1", "UTF-8")

barriosNamesCodes <- strsplit(sub(" ", "\\.", barriosMadrid$DESBDT), "\\.")
head(barriosNamesCodes)

barriosNamesCodes <- do.call(rbind, barriosNamesCodes)
head(barriosNamesCodes)
barriosMadrid$BarrioCod <- barriosNamesCodes[ ,1]
barriosMadrid$Barrio <- barriosNamesCodes[ ,2]
head(barriosMadrid@data)


# Get ready the density data.frame to merge
head(denMad)
denMad$Barrio <- gsub(".", "", denMad$Barrio, fixed = TRUE)
head(denMad)

barriosNamesCodes <- do.call(rbind, strsplit(sub(" ", "\\.", denMad$Barrio), "\\."))
head(barriosNamesCodes)

denMad$Barrio <- barriosNamesCodes[ ,2]
denMad$BarrioCod <- barriosNamesCodes[ ,1]
head(denMad)
head(barriosMadrid@data)

# Merge the Spatial object data.frame with the density data.frame

identical(sort(barriosMadrid$BarrioCod), sort(denMad$BarrioCod))


barriosMadrid@data <- merge(barriosMadrid@data, denMad[ ,c(2,3)])
head(barriosMadrid@data)
rm(barriosNamesCodes)
```

#### PLOT THE DATA
```{r plot_data fig.width=8, fig.height=7}
plot(barriosMadrid, col = barriosMadrid$Densidad)


# SP library
library(RColorBrewer)
colors <- colorRampPalette(brewer.pal(9, "YlGnBu"))(16)
spplot(barriosMadrid, "Densidad", col.regions = colors)


# CLASS INT library
if (!"classInt" %in% installed.packages()) install.packages("classInt")

library(classInt)

int <- classIntervals(barriosMadrid$Densidad, 10, style = "pretty")
int
colors <- colorRampPalette(brewer.pal(9, "YlGnBu"))(10)
barriosMadrid$colors <- findColours(int, colors)
head(barriosMadrid@data)

plot(barriosMadrid, col = barriosMadrid$colors)
legend("bottomleft", cex = .7, legend = leglabs(int$brks), fill = colors, title = "Population density Madrid (2013)")
```

#### PLOT THE NEIGHBORHOOD LABELS
```{r labels}
head(coordinates(barriosMadrid))
length(coordinates(barriosMadrid))

cents <- coordinates(barriosMadrid)

pointLabel(cents[ ,1], cents[ ,2], labels=barriosMadrid$Barrio)

png("labels.png", width = 2500, height = 2100)
plot(barriosMadrid, col = barriosMadrid$colors)
legend("bottomleft", cex = .9, legend = leglabs(int$brks), fill = colors, title = "Population density Madrid (2013)")
plot(parquesMadrid, pch = 20, cex = 4, col = "springgreen3", add = TRUE)
pointLabel(cents[ ,1], cents[ ,2], labels=barriosMadrid$Barrio)
dev.off()
```

#### PLOT DENSITY AS POINTS
```{r den_as_points}
dots <- dotsInPolys(barriosMadrid, barriosMadrid$Densidad)  # Plot as many dots as people living in every polygon
plot(dots, pch = 16, cex = .1, col = "violetred3")


plot(barriosMadrid, col = "grey90")
plot(dots, pch = 16, cex = .1, col = "violetred3", add = TRUE)
plot(parquesMadrid, pch = 20, cex = 1, col = "springgreen3", add = TRUE)
```

#### PLOT THE METRO LINES
```{r metro}
metroLines <- readShapeLines("~/Dropbox/Maps_with_R/Shapefiles/MAD_lineas_metro/lineas.shp", proj4string = ED50)


plot(barriosMadrid, col = "grey90")
plot(dots, pch = 16, cex = .1, col = "violetred3", add = TRUE)
plot(parquesMadrid, pch = 20, cex = 1, col = "springgreen3", add = TRUE)
plot(metroLines, col = "turquoise3", lwd = 2, add = TRUE)
```


2. Plot a map on a street map. 

### GGMAP AND GGPLOT LIBRARIES (also rgdal and rgeos)

#### 1.CHANGE THE COORDINATE SYSTEM, as we have UTM in our objects and ggplot works on **long, lat** coordinates.      
> For this purpose, the **rgdal** library is needed.   

```{r rgdal}
library(rgdal)

str(barriosMadrid, max.level = 2)
barriosMadrid@proj4string


barriosLongLat <- spTransform(barriosMadrid, CRS("+proj=longlat"))  # Transform the coordinates to lon lat
parquesLongLat <- spTransform(parquesMadrid, CRS("+proj=longlat"))
metroLongLat <- spTransform(metroLines, CRS("+proj=longlat"))  # we need to have told to R already qich were the original projection
```

#### 2.Create a data.frame from the Spatial**DataFrame data slot, containing the long and lat variables, as ggplot plots data.frames and not Spatial objects

```{r ggplot}
library(ggplot2)

# Points DataFrame to ggplot
head(coordinates(parquesLongLat))

long <- coordinates(parquesLongLat)[, 1]  
lat <- coordinates(parquesLongLat)[, 2]
dataParques <- cbind(parquesLongLat@data, long, lat)
head(dataParques)

ggplot(dataParques, aes(long, lat)) +
geom_point(colour = "springgreen3", size = 4)


# Polygons DataFrame to ggplot
head(coordinates(barriosLongLat))
barriosLongLat@data$id <- rownames(barriosLongLat@data)
dataBarrios <- fortify(barriosLongLat, region="id")  # requires rgeos package. explicitly identifies attribute rows by the .dbf offset.
head(dataBarrios)
class(dataBarrios)
dataBarrios <- merge(dataBarrios, barriosLongLat@data)
head(dataBarrios)


# Lines DataFrame to ggplot
str(coordinates(metroLongLat@lines[[1]]))
length(metroLongLat@lines)
head(coordinates(metroLongLat@lines[[1]]))


metroLongLat@data$id <- rownames(metroLongLat@data)
dataMetro <- fortify(metroLongLat, region="id")  # requires rgeos package
head(dataMetro)
class(dataMetro)
dataMetro <- merge(dataMetro, metroLongLat@data)
head(dataMetro)
```

#### 3.Once we have data.frames ready plot them on a streetmap
```{r}
library(ggmap)
MadridMap <- qmap(location = "Museo del Prado", zoom = 15, source = "google")
MadridMap
MadridMap <- qmap(location = barriosLongLat@bbox, zoom = 11, maptype = "terrain")  # Get the map from Google maps
MadridMap


MadridMap + 
	geom_polygon(aes( x = long, y = lat, group = group, fill = Densidad), data = dataBarrios, alpha = 0.5, colour = "white") +
	scale_fill_gradient(low="grey90", high="black", limits=c(0,500)) +
	geom_path(aes(x = long, y = lat, group = group), data = dataMetro, colour = "turquoise3") +
	geom_point(aes(x = long, y = lat), data = dataParques,  , size = 5, alpha = 0.8, colour = "springgreen3")+
	theme(legend.position = "none")
```


3. Plot a map with ggplot without a background street map. 
```{r}
ggplot() + geom_polygon(aes(x=long, y=lat, group = group, fill = Densidad), data = dataBarrios) + 
	scale_fill_gradient(low="grey90", high="black", limits=c(0,500)) +
	geom_point(aes(x=long, y=lat, size = 5), data = dataParques, colour = "springgreen3") +
	geom_path(aes(x=long, y = lat, group = group), data = dataMetro, colour = "turquoise3") +
	theme(legend.position = "none", 
  	  panel.background = element_blank(),
  	  panel.grid = element_blank(),
  	  axis.ticks = element_blank(),
  	  text = element_blank())
```

4. googleVis
```{r}
library(googleVis)
emigrantes <- read.csv("Data/emigrantes.csv", sep = ";")
head(emigrantes)

## me quedo las que no empiezan por dos mayúsculas
emigrantes <- emigrantes[which(grepl("^[A-Z]{2}", emigrantes$pais) == FALSE), ]
head(emigrantes)

emigrantesMap <- gvisGeoChart(emigrantes, locationvar = "pais", colorvar = "X2012", options = list(width = 800, height = 500))

plot(emigrantesMap)

cat(emigrantesMap$html$chart, file="emigrantes2012.html")
```