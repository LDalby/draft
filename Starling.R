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
# library(lme4)
library(rasterVis)
library(dichromat)
library(readxl)
library(dplyr)

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
proj4string(fields) = utm32
# and Henning's field recordings:
crops = as.data.table(read_excel('C:/Users/lada/Dropbox/StarlingGPS/Hjortkaer/GIS_Crop_Hjortkaer_LD.xls'))
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
fdata[grep('S_', Crop2016Early), Crop2016Early:= 'BareGround']
fdata[grep('S_', Crop2016Late), Crop2016Late:= 'BareGround']
# HIGH grass and Grass was not consistently recorded, so combine into Grass
fdata[Crop2015 == 'HIGH Grass', Crop2016:= 'Grass']
fdata[, c('FEAT_TYPE', 'FID'):=NULL]
fdata[, PolyID:=1:nrow(fdata)]
fields@data = fdata
# Transform to shiny coords:
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

TheList = vector('list', length = length(loggers))
ThePlotList = TheList
for (i in seq_along(loggers)) {
	temp = CleanRawFile(file.path(loggerpth, loggers[i]), HDOPmax = 2.5, type = 'gipsy-5')
	season = stringr::str_sub(loggers[i], start = -8, end = -5)
	temp = temp[year(Date) == as.numeric(season),]  # Make sure we only get one year of data in case of reuse of logger.
	temp = temp[Speed == 0,]  # Only use observations where the bird didn't move
	temp = temp[as.ITime(Date) %between% c(3.5*3600, 18*3600), ]  # Only use day time observations
# Make spatial object:
	coordinates(temp) = ~Longitude+Latitude
	proj4string(temp) = longlat
	sputm = spTransform(temp, utm32)
	spdists = as.vector(gDistance(ringingsite, sputm, byid = TRUE))
	sputm$Dist = spdists  # Add to visualization object, so we can display in popup info in the app
# Use
	# usetype = rbindlist(over(sputm, fields, returnList = TRUE))
	usetype = over(sputm, fields)
	usetype$Dist = spdists
	usetype = usetype[!is.na(Crop2016Early) | !is.na(Crop2016Late),]
	usetype[, Response:=1]
# Combine use and availability    
	temp = rbind(availtype, usetype)
	loggerno = stringr::str_split(loggers[i], '_')[[1]][1]  # Get the ID of the logger
	loggerno = paste(loggerno, season, sep = '-')
	temp[, LoggerID:=loggerno]
	TheList[[i]] = temp
	sputm$LoggerID = loggerno
	ThePlotList[[i]] = sputm
}
# Combine the items in TheList to a data.table:
starlings = rbindlist(TheList)
early = c('S15', 'S14', 'S13', 'S17', 'S4a', 'S11', 'S9a', 'S12', 'S1')
early = paste(early, '2016', sep = '-')
late = c('S21', 'S4b', 'S9b')
late = paste(late, '2016', sep = '-')
starlings[LoggerID %in% early, Cover:=Crop2016Early]
starlings[LoggerID %in% late, Cover:=Crop2016Late]
starlings[grep('2015', LoggerID), Cover:=Crop2015]
starlings[, c('Crop2016Early', 'Crop2016Late', 'Crop2015'):=NULL]
setcolorder(starlings, c('LoggerID', 'Cover', 'Dist', 'PolyID', 'Response'))
starlings[, Dist:=round(Dist)]
save(starlings, file = 'C:/Users/lada/Git/shiny/Starlings/Data/data.RData')
spstarlings = do.call('rbind', ThePlotList)
spstarlings = spTransform(spstarlings, CRS("+init=epsg:4326"))
save(spstarlings, file = 'C:/Users/lada/Git/shiny/Starlings/Data/starlings.RData')
















# pdf(file = 'C:/Users/lada/Dropbox/StarlingGPS/IndividualUse.pdf')
# pdf(file = '/Users/Lars/Dropbox/StarlingGPS/IndividualUse.pdf')
for (i in seq_along(loggers)) {
	 print(lp + layer(sp.points(ThePlotList[[i]], col = rainbow(7)[i], cex = .5)) +
	layer(panel.text(481700, 6156000, stringr::str_split(loggers[i], '_')[[1]][1]))
	)
}
# dev.off()







# Attempt at Fig1
levels(fieldsutmpoly@data$field_type) = sapply(levels(fieldsutmpoly@data$field_type), FUN = ReclassifyHabitat)
col = colorschemes$Categorical.12[1:8]
pdf(file = 'C:/Users/lada/Dropbox/StarlingGPS/Fig1.pdf')
spplot(fieldsutmpoly, 'field_type', col.regions = col, scales = list(draw = TRUE)) +
 layer(sp.points(ThePlotList[[1]], col = 'black'))+
 layer(sp.points(ThePlotList[[2]], col = 'black'))+
 layer(sp.points(ThePlotList[[3]], col = 'black'))+
 layer(sp.points(ThePlotList[[4]], col = 'black'))+
 layer(sp.points(ThePlotList[[5]], col = 'black'))+
 layer(sp.points(ThePlotList[[6]], col = 'black'))+
 layer(sp.points(ThePlotList[[7]], col = 'black'))
spplot(fieldsutmpoly, 'field_type', col.regions = col, scales = list(draw = TRUE),
 xlim = c(481750, 483500), ylim = c(6154000,6155500)) +
 layer(sp.points(ThePlotList[[1]], col = 'black'))+
 layer(sp.points(ThePlotList[[2]], col = 'black'))+
 layer(sp.points(ThePlotList[[3]], col = 'black'))+
 layer(sp.points(ThePlotList[[4]], col = 'black'))+
 layer(sp.points(ThePlotList[[5]], col = 'black'))+
 layer(sp.points(ThePlotList[[6]], col = 'black'))+
 layer(sp.points(ThePlotList[[7]], col = 'black'))
dev.off()

# Figure 1 (logger S9):
inch = 2.54
par(mar = (c(1, 1, 1, 2) + 0.1)/2, oma = rep(0.1, 4))
# pdf(file = 'C:/Users/lada/Dropbox/StarlingGPS/Fig1a.pdf', height = 8/inch, width = 10/inch)
postscript(file = 'C:/Users/lada/Dropbox/StarlingGPS/Fig1a.eps', height = 8/inch, width = 10/inch)
spplot(fieldsutmpoly, 'field_type', col.regions = col, scales = list(draw = FALSE)) +
  latticeExtra::layer(sp.points(ThePlotList[[6]], col = 'black', pch = 1, cex = 0.5))  + 
  latticeExtra::layer(sp.points(farmcenter, pch = 23, col = 'black', fill = 'lightgrey', cex = 1))
dev.off()
# Scale bar needs to be added by hand.

# Figure S1:  WIP...
# library(ggplot2)
# library(ggmap)
# map = fortify(fieldsutmpoly)
# fieldsutmpoly@data$ID = 0:(nrow(fieldsutmpoly@data)-1)
# plotdata = merge(map, fieldsutmpoly@data, by.x = 'id', by.y = 'ID', all.x = TRUE)

# pdf(file = 'C:/Users/lada/Dropbox/StarlingGPS/FigS1.pdf', height = 10, width = 5*5)
# ggplot() + geom_polygon(data=plotdata, aes(x=long, y=lat, group=group, fill = field_type)) + 
#  scale_fill_brewer(palette = "Set2") + coord_fixed() + coord_equal() + 
# geom_point(data = starlings[LoggerID != 'S9' & Response == 1,], aes(x = Longitude, y = Latitude)) + 
# theme_bw() + facet_wrap(~LoggerID, ncol = 3) + coord_cartesian(xlim = c(481750, 483500), ylim = c(6154000,6154000+1750))
# dev.off()

# pdf(file = 'C:/Users/lada/Dropbox/StarlingGPS/Fig1b.pdf', height = 8/inch, width = 10/inch)
# ggplot() + geom_polygon(data=plotdata, aes(x=long, y=lat, group=group, fill = field_type)) + 
#  scale_fill_brewer(palette = "Set2") + coord_fixed() + coord_equal() + 
# geom_point(data = starlings[LoggerID == 'S9' & Response == 1,], aes(x = Longitude, y = Latitude)) + 
# theme_bw()
# dev.off()
# coord_cartesian(xlim = c(481750, 483500), ylim = c(6154000,6154000+1750))
# Analyse
# We need to figure out how exactly we want to implement the models:
# The model below are not converging at the moment, but that might be
# due to the wrong model specification.
# Set the types:
# starlings[, LoggerID:=as.factor(LoggerID)]
# M1 = glmer(Response ~ FieldType*log10(Dist) + (1 | LoggerID), control=glmerControl(optimizer="bobyqa"), data = starlings, nAGQ = 10, family = binomial(link = logit))

