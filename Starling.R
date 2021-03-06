# Starlings - Hjortkaer
# Script to handle and analyze starling gps data
# Date: Oct 2016
# Author: Lars Dalby

library(RColorBrewer)
library(raster)
library(loggr)
library(sp)
library(data.table)
library(maptools)
library(rgeos)
library(rasterVis)
library(dichromat)
library(readxl)
library(dplyr)
library(lubridate)

setwd('c:/Users/lada/Dropbox/Data filer R/Starling/')
# setwd('/Users/Lars/Dropbox/Data filer R/Starling/')
# Both years - uncleaned data. Cleaning done in loop further down.
loggerpth = 'c:/Users/lada/Dropbox/StarlingGPS/Logger/Loggers15-16/'
loggers = dir(loggerpth)
# Define local variables:
AvailGridDist = 50  # The gridsize for the availability points
utm32 = CRS('+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
longlat = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
ringingsite = SpatialPoints(cbind(482806.60016627, 6154932.799999), proj4string = utm32)
# Transform to shiny projection:
# ringingsite = spTransform(ringingsite, CRS("+init=epsg:4326"))

# Read in the base map:
fields = readShapePoly('o:/ST_Lada/Projekter/Starling/BaseMapHjortkaer.shp')
fields = fields[fields$FEAT_TYPE != "Lake",]  # Essentially non-habitat for starlings, so no need to map it.
fields = fields[!fields$FID %in% c(110,117),]  # Remove two field polygons which overlap with forest polygons.
proj4string(fields) = utm32
# and Henning's field recordings:
crops = as.data.table(read_excel('C:/Users/lada/Dropbox/StarlingGPS/Hjortkaer/GIS_Crop_Hjortkaer_LD.xls'))
crops[FID == 202, Crop2015:='Fold']  # Fix inconsistent recording of Fold/Grass
crops[, c("Note2015", "Note2016"):=NULL]
crops[Crop2015 == 'MINUS', Crop2015:=NA]  # These fields were not recorded in 2015, so we just NA them.
# Join them onto the basemap using the FID column:
fdata = as.data.table(left_join(fields@data, crops, by='FID'))
fdata[, cat:=NULL]
# Ensure name match between base map and field recordings
fdata[FEAT_TYPE == 'P3', FEAT_TYPE:='Meadow']
# Overwrite all the rows that didn't have field recording, with the basemap type:
fdata[FID == 0, Crop2016Early:=FEAT_TYPE]
fdata[FID == 0, Crop2016Late:=FEAT_TYPE]
fdata[FID == 0, Crop2015:=FEAT_TYPE]
# Ensure name match between base map and field recordings
fdata[Crop2015 == 'Trees', Crop2015:= 'Forest']
fdata[Crop2016Early == 'Trees', Crop2016Early:= 'Forest']
fdata[Crop2016Late == 'Trees', Crop2016Late:= 'Forest']
# Some of the spring sown crops are bare ground this time of the year
# so these are simply combined into BareGround:
fdata[Crop2016Early == 'Maize', Crop2016Early:= 'BareGround']
fdata[Crop2016Late == 'Maize', Crop2016Late:= 'BareGround']
fdata[Crop2016Early == 'Fold', Crop2016Early:= 'Grazing']
fdata[Crop2016Late == 'Fold', Crop2016Late:= 'Grazing']
fdata[Crop2015 == 'Fold', Crop2015:= 'Grazing']
fdata[grep('S_', Crop2016Early), Crop2016Early:= 'BareGround']
fdata[grep('S_', Crop2016Late), Crop2016Late:= 'BareGround']
fdata[grep('S_', Crop2015), Crop2015:= 'BareGround']
fdata[grep('W_', Crop2016Early), Crop2016Early:= 'WinterCrop']
fdata[grep('W_', Crop2016Late), Crop2016Late:= 'WinterCrop']
fdata[grep('W_', Crop2015), Crop2015:= 'WinterCrop']
# Rape is a rare crop and functionally similar to the other winter crops at this time:
fdata[Crop2016Early == 'Rape', Crop2016Early:= 'WinterCrop']
fdata[Crop2016Late == 'Rape', Crop2016Late:= 'WinterCrop']
fdata[Crop2015 == 'Rape', Crop2015:= 'WinterCrop']
# Beet is a rare crop and is functionally similar to the other spring crops, hence bare ground:
fdata[Crop2016Early == 'Beet', Crop2016Early:= 'BareGround']
fdata[Crop2016Late == 'Beet', Crop2016Late:= 'BareGround']
fdata[Crop2015 == 'Beet', Crop2015:= 'BareGround']
# HIGH grass and Grass was not consistently recorded, so combine into Grass
fdata[Crop2015 == 'HIGH grass', Crop2015:= 'Grass']
fdata[, c('FEAT_TYPE', 'FID'):=NULL]
fdata[, PolyID:=1:nrow(fdata)]
fields@data = fdata
# Here we remove the building and garden where the birds where ringed:
pfields = fields  #save an object for plotting the map for the paper
fields = fields[!fields$PolyID %in% c(638, 287, 636, 257),]  # Careful here - hardcoded polyID's!
# fields = spTransform(fields, CRS("+init=epsg:4326"))
# bb = bbox(fields)
# Just checking:
# plot(fields)
# invisible(text(coordinates(fields), labels=as.character(fields$Crop2016Early), cex=0.7, pos = 1))
# invisible(text(coordinates(fields), labels=as.character(fields$FID), cex=0.7, pos = 1))
# Make availability grid:
newavll = ExpandAvailGrid(fields, AvailGridDist, utm = TRUE)
# Transform to shiny coords:
# newavll = spTransform(newavll, CRS("+init=epsg:4326"))
# Save the objects needed for vis in shiny app:
# save(list = c('bb', 'fields', 'ringingsite', 'newavll'), file = 'C:/Users/lada/Git/shiny/Starlings/Data/fields.RData')

# Availability handled outside the loop as they are the same for all 
availtype = over(newavll, fields)
availtype$Dist = as.vector(gDistance(ringingsite, newavll, byid = TRUE))  
availtype = availtype[!is.na(Crop2016Early) | !is.na(Crop2016Late) | !is.na(Crop2015),]
availtype[, Response:=0]

# Read in the logger metadata:
meta = as.data.table(read_excel('c:/Users/lada/Dropbox/StarlingGPS/MetadataHjortkaer15-16.xlsx'))
meta[,LoggerID:=paste(Logger, Year, sep = '-')]

TheList = vector('list', length = length(loggers))
ThePlotList = TheList
for (i in seq_along(loggers)) {
	temp = CleanRawFile(file.path(loggerpth, loggers[i]), HDOPmax = 2.5, type = 'gipsy-5')
	season = stringr::str_sub(loggers[i], start = -8, end = -5)
	temp = temp[year(Date) == as.numeric(season),]  # Make sure we only get one year of data in case of reuse of logger.
	temp = temp[Speed == 0,]  # Only use observations where the bird didn't move
	temp = temp[as.ITime(Date) %between% c(3.5*3600, 18*3600), ]  # Only use day time observations
	if(loggers[i] == 'S1_8E03627_12052016.txt') {
		temp = temp[Date >= ymd('2016-05-07'),]
	}
# Make spatial object:
	coordinates(temp) = ~Longitude+Latitude
	proj4string(temp) = longlat
	sputm = spTransform(temp, utm32)
	spdists = as.vector(gDistance(ringingsite, sputm, byid = TRUE))
	sputm$Dist = spdists  # Add to visualization object, so we can display in popup info in the app
	loggerno = stringr::str_split(loggers[i], '_')[[1]][1]  # Get the ID of the logger
	loggerno = paste(loggerno, season, sep = '-')
	sputm$Sex = meta[LoggerID == loggerno, Sex]
# Use
	# usetype = rbindlist(over(sputm, fields, returnList = TRUE))
	usetype = over(sputm, fields)
	usetype$Dist = spdists
	usetype = usetype[!is.na(Crop2016Early) | !is.na(Crop2016Late) | !is.na(Crop2015),]
	usetype[, Response:=1]
# Combine use and availability    
	temp = rbind(availtype, usetype)
	temp[, Sex:=meta[LoggerID == loggerno, Sex]]
	temp[, LoggerID:=loggerno]
	temp[, Year:=season]
	TheList[[i]] = temp
	sputm$LoggerID = loggerno
	ThePlotList[[i]] = sputm
}
# Combine the items in TheList to a data.table:
starlings = rbindlist(TheList)
early = c('S15', 'S14', 'S13', 'S17', 'S4a', 'S11', 'S9a', 'S12', 'S1')
early = paste(early, '2016', sep = '-')
late = c('S4b', 'S9b')
late = paste(late, '2016', sep = '-')
starlings[LoggerID %in% early, Cover:=Crop2016Early]
starlings[LoggerID %in% late, Cover:=Crop2016Late]
starlings[grep('2015', LoggerID), Cover:=Crop2015]
starlings[, c('Crop2016Early', 'Crop2016Late', 'Crop2015'):=NULL]
setcolorder(starlings, c('LoggerID', 'Sex', 'Year', 'Cover', 'Dist', 'PolyID', 'Response'))
starlings[, Dist:=round(Dist)]
starlings = starlings[!is.na(Cover),]
# save(starlings, file = 'C:/Users/lada/Git/shiny/Starlings/Data/data.RData')
spstarlings = do.call('rbind', ThePlotList)
spstarlings = spTransform(spstarlings, CRS("+init=epsg:4326"))
# save(spstarlings, file = 'C:/Users/lada/Git/shiny/Starlings/Data/starlings.RData')









# pdf(file = 'C:/Users/lada/Dropbox/StarlingGPS/IndividualUse.pdf')
# pdf(file = '/Users/Lars/Dropbox/StarlingGPS/IndividualUse.pdf')
for (i in seq_along(loggers)) {
	 print(lp + layer(sp.points(ThePlotList[[i]], col = rainbow(7)[i], cex = .5)) +
	layer(panel.text(481700, 6156000, stringr::str_split(loggers[i], '_')[[1]][1]))
	)
}
# dev.off()





buffer = 1200
xlim = c(482806.60016627 - buffer, 482806.60016627 +buffer)
ylim = c(6154932.799999 - buffer, 6154932.799999 + buffer)

# Fig 1
# S9a-2016 Early period
col = colorschemes$Categorical.12[1:8]  # Important to get the correct number of levels!
col[2] = 'black'
col[3] = 'forestgreen'
col[4] = 'grey'
col[8] = 'khaki4'

pfields@data$Crop2015 = as.factor(pfields@data$Crop2015)
pfields@data$Crop2016Early = as.factor(pfields@data$Crop2016Early)
pfields@data$Crop2016Late = as.factor(pfields@data$Crop2016Late)


# Figure 1 (logger S9a):
inch = 2.54
height = 11/inch
width = 16/inch
par(mar = (c(1, 1, 1, 2) + 0.1)/2, oma = rep(0.1, 4))
# pdf(file = 'C:/Users/lada/Dropbox/StarlingGPS/Fig1a.pdf', height = 8/inch, width = 10/inch)
postscript(file = 'C:/Users/lada/Dropbox/StarlingGPS/Fig1a2017.eps', height = height, width = width)
# pdf(file = 'C:/Users/lada/Dropbox/StarlingGPS/Fig1a2017.pdf', height = 8/inch, width = 10/inch)
set_Polypath(FALSE)  # Appears to be needed to make lwd work...
logger = unique(ThePlotList[[16]]$LoggerID)
spplot(pfields, 'Crop2016Early', col.regions = col, scales = list(draw = FALSE), col = 'lightgrey', lwd = 0.2,
        main = logger, xlim = xlim, ylim = ylim) +
  latticeExtra::layer(sp.points(ThePlotList[[16]], col = 'black', pch = 1, cex = 0.5, lwd = 0.5))  + 
  latticeExtra::layer(sp.points(ringingsite, pch = 23, col = 'black', fill = 'dodgerblue2', cex = 1)) +
  latticeExtra::layer(SpatialPolygonsRescale(layout.scale.bar(height = 0.1), 
                                             offset = c(483300,6153800), scale = 500, fill=c("white","black"))) +
  latticeExtra::layer(sp.text(c(483800,6153950), txt = "500 m", cex = 0.5)) + 
  latticeExtra::layer(sp.text(c(483300,6153950), txt = "0 m", cex = 0.5)) + 
  latticeExtra::layer(sp.polygons(gBuffer(ringingsite, width = 1000, quadsegs = 20), lwd = 0.3))
dev.off()

postscript(file = 'C:/Users/lada/Dropbox/StarlingGPS/Fig1b2017.eps', height = height, width = width)
# pdf(file = 'C:/Users/lada/Dropbox/StarlingGPS/Fig1b2017.pdf', height = 8/inch, width = 10/inch)
set_Polypath(FALSE)  # Appears to be needed to make lwd work...
logger = unique(ThePlotList[[13]]$LoggerID)
spplot(pfields, 'Crop2015', col.regions = col, scales = list(draw = FALSE), col = 'lightgrey', lwd = 0.4,
       main = logger, xlim = xlim, ylim = ylim) +
  latticeExtra::layer(sp.points(ThePlotList[[13]], col = 'black', pch = 1, cex = 0.5, lwd = 0.5))  + 
  latticeExtra::layer(sp.points(ringingsite, pch = 23, col = 'black', fill = 'dodgerblue2', cex = 1)) +
  latticeExtra::layer(SpatialPolygonsRescale(layout.scale.bar(height = 0.1), 
                                             offset = c(483300,6153800), scale = 500, fill=c("white","black"))) +
  latticeExtra::layer(sp.text(c(483800,6153950), txt = "500 m", cex = 0.5)) + 
  latticeExtra::layer(sp.text(c(483300,6153950), txt = "0 m", cex = 0.5)) + 
  latticeExtra::layer(sp.polygons(gBuffer(ringingsite, width = 1000, quadsegs = 20), lwd = 0.3))
dev.off()

#---- The supplement figures:
early = c('S15', 'S14', 'S13', 'S17', 'S4a', 'S11', 'S9a', 'S12', 'S1')
early = paste(early, '2016', sep = '-')
late = c('S4b', 'S9b')
late = paste(late, '2016', sep = '-')
pdf(file = 'C:/Users/lada/Dropbox/StarlingGPS/SupplementFigures2017.pdf', paper = 'a4')
set_Polypath(FALSE)
for(i in seq_along(ThePlotList)){
  logger = unique(ThePlotList[[i]]$LoggerID)
  if(length(grep(pattern = "2015", logger)) > 0)
  {print(spplot(pfields, 'Crop2015', col.regions = col, scales = list(draw = FALSE), col = 'lightgrey', lwd = 0.4,
                main = logger, xlim = xlim, ylim = ylim) +
           latticeExtra::layer(sp.points(ThePlotList[[i]], col = 'black', pch = 1, cex = 0.5, lwd = 0.7))  + 
           latticeExtra::layer(sp.points(ringingsite, pch = 23, col = 'black', fill = 'dodgerblue2', cex = 1)) +
           latticeExtra::layer(SpatialPolygonsRescale(layout.scale.bar(height = 0.1), 
                                                      offset = c(483300,6153800), scale = 500, fill=c("white","black"))) +
           latticeExtra::layer(sp.text(c(483800,6153950), txt = "500 m", cex = 0.7)) + 
           latticeExtra::layer(sp.text(c(483300,6153950), txt = "0 m", cex = 0.7)) + 
           latticeExtra::layer(sp.polygons(gBuffer(ringingsite, width = 1000, quadsegs = 20), lwd = 0.3)))}
  if(logger %in% early)
  {print(spplot(pfields, 'Crop2016Early', col.regions = col, scales = list(draw = FALSE), col = 'lightgrey', lwd = 0.4,
                main = logger, xlim = xlim, ylim = ylim) +
           latticeExtra::layer(sp.points(ThePlotList[[i]], col = 'black', pch = 1, cex = 0.5, lwd = 0.7))  + 
           latticeExtra::layer(sp.points(ringingsite, pch = 23, col = 'black', fill = 'dodgerblue2', cex = 1)) +
           latticeExtra::layer(SpatialPolygonsRescale(layout.scale.bar(height = 0.1), 
                                                      offset = c(483300,6153800), scale = 500, fill=c("white","black"))) +
           latticeExtra::layer(sp.text(c(483800,6153950), txt = "500 m", cex = 0.7)) + 
           latticeExtra::layer(sp.text(c(483300,6153950), txt = "0 m", cex = 0.7)) + 
           latticeExtra::layer(sp.polygons(gBuffer(ringingsite, width = 1000, quadsegs = 20), lwd = 0.3)))
  }
  if(logger %in% late) {
    print(spplot(pfields, 'Crop2016Late', col.regions = col, scales = list(draw = FALSE), col = 'lightgrey', lwd = 0.4,
                 main = logger, xlim = xlim, ylim = ylim) +
            latticeExtra::layer(sp.points(ThePlotList[[i]], col = 'black', pch = 1, cex = 0.5, lwd = 0.7))  + 
            latticeExtra::layer(sp.points(ringingsite, pch = 23, col = 'black', fill = 'dodgerblue2', cex = 1)) +
            latticeExtra::layer(SpatialPolygonsRescale(layout.scale.bar(height = 0.1), 
                                                       offset = c(483300,6153800), scale = 500, fill=c("white","black"))) +
            latticeExtra::layer(sp.text(c(483800,6153950), txt = "500 m", cex = 0.7)) + 
            latticeExtra::layer(sp.text(c(483300,6153950), txt = "0 m", cex = 0.7)) + 
            latticeExtra::layer(sp.polygons(gBuffer(ringingsite, width = 1000, quadsegs = 20), lwd = 0.3)))
  }
}
dev.off()


# April 24 2017 - calculate areas in response to reviewers:
landcover = st_read('o:/ST_Lada/Projekter/Starling/BaseMapHjortkaer.shp')
circ = st_as_sf(gBuffer(ringingsite, width = 1000, quadsegs = 20))
st_crs(landcover)
st_crs(circ)
int = st_intersection(landcover, circ)
int$Area = st_area(int$geoms)

plot(landcover$geometry, col = 'green')  
plot(circ$geometry, add = TRUE)
plot(int$geoms, col = 'red', add = TRUE)

st_area(circ$geometry)  # Get the area of the circle
intdt = as.data.table(int)
intdt[, .(AreaByType = sum(Area)), by = FEAT_TYPE]  # Get the area by type
intdt[, .(AreaByType = sum(Area)), by = FEAT_TYPE][, sum(AreaByType)]  # Check that its sums to <= area of circle

# FEAT_TYPE      AreaByType
# 1:        P3  261133.083 m^2
# 2:  Building   20190.885 m^2
# 3:         0 2367057.910 m^2
# 4:    Garden   97786.246 m^2
# 5:     Trees   37560.432 m^2
# 6:      Fold   11219.288 m^2
# 7:    Meadow    5159.562 m^2
# 8:    Forest  156542.493 m^2
# 9:      Lake   14343.542 m^2

(20190.885 + 97786.246 + 37560.432 + 156542.493 + 14343.542)/st_area(circ$geometry)















