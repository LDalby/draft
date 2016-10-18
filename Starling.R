# Starlings
# Script to handle and analyze starling gps data
# Date: Oct 2016
# Author: Lars Dalby

library(RColorBrewer)
library(raster)
library(loggr)
library(sp)
library(magrittr)
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
# 'S1_8A81455_12052015.txt'  Too few data points
# 2015:
loggers = c('S2_8E03440_12052015.txt', 'S3_8A42334_11052015.txt', 'S5_8E03442_16052015.txt', 
	'S7_8E03443_18052015.txt',	'S8_8E03444_14052015.txt', 'S9_8A43447_18052015.txt', 
	'S10_8E03446_18052015.txt')
# 2016: 
# The 2016 loggers were cleaned one by one as there were a couple of things that 
# needed sorting out. 
loggerpth = 'c:/Users/lada/Dropbox/StarlingGPS/Logger/Logger2016/'
files = dir(loggerpth)
loggers = files[-grep('clean', files)]
# Define local variables:
AvailGridDist = 50  # The gridsize for the availability points
utm32 = CRS('+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
longlat = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# longlat = CRS("+proj=longlat +datum=WGS84 +no_defs")
ringingsite = SpatialPoints(cbind(482806.60016627, 6154932.799999), proj4string = utm32)

# Read in the base map:
fields = readShapePoly('o:/ST_Lada/Projekter/Starling/BaseMapHjortkaer.shp')
proj4string(fields) = utm32
# and Henning's field recordings:
# crops = as.data.table(read_excel('c:/Users/lada/Dropbox/Hjortkaer/GIS_Crop_Hjortkaer.xls'))
crops = as.data.table(read_excel('C:/Users/lada/Dropbox/StarlingGPS/Hjortkaer/GIS_Crop_Hjortkaer_LD.xls'))
crops[, c("Note2015", "Note2016"):=NULL]
# Join them onto the basemap using the FID column:
fdata = as.data.table(left_join(fields@data, crops, by='FID'))
fdata[, cat:=NULL]
# Overwrite all the rows that didn't have field recording, with the basemap type:
fdata[FID == 0, Crop2016Early:=FEAT_TYPE]
fdata[FID == 0, Crop2016Late:=FEAT_TYPE]
fdata[FID == 0, Crop2015:=FEAT_TYPE]
fields@data = fdata
# Just checking:
plot(fields)
invisible(text(coordinates(fields), labels=as.character(fields$Crop2016Early), cex=0.7, pos = 1))
invisible(text(coordinates(fields), labels=as.character(fields$FID), cex=0.7, pos = 1))
# Make availability grid:
newavll = ExpandAvailGrid(fields, AvailGridDist, utm = TRUE)
availdists = as.vector(gDistance(ringingsite, newavll, byid = TRUE))
#spplot(newavll, zcol = 'Dist')  # Just chekcing - looks okay
# col = brewer.pal(11, 'Set3')
col = colorschemes$Categorical.12[1:8]
lp = levelplot(rfields, att = 'Crop2016Early', col.regions = col)
lp = levelplot(rfields, att = 'Crop2016Early')
lp = lp + layer(sp.polygons(fields))
lp + layer(sp.points(newavll, pch = 19, col = 'red', cex = .5))
lp + layer(sp.points(ringingsite, pch = 23, col = 'black', fill = 'white', cex = 2))

TheList = vector('list', length = length(loggers))
ThePlotList = TheList
for (i in seq_along(loggers)) {
	temp = CleanRawFile(file.path(loggerpth, loggers[i]), HDOPmax = 2.5, type = 'gipsy-5')
	temp = temp[year(Date) == 2016,]  # Only use observations where the bird didn't move
	temp = temp[Speed == 0,]  # Only use observations where the bird didn't move
	temp = temp[hour(Date) < 18,]  # Only use day time observations
# Make spatial object:
	coordinates(temp) = ~Longitude+Latitude
	proj4string(temp) = longlat
	sputm = spTransform(temp, utm32)
	spdists = as.vector(gDistance(ringingsite, sputm, byid = TRUE))
# Availability
	availtype = over(newavll, fields)
	availtype$Dist = availdists
	availtype = availtype[!is.na(Crop2016Early) | !is.na(Crop2016Late),]
	availtype[, Response:=0]
# Use
	usetype = over(sputm, fields)
	usetype$Dist = spdists
	usetype = usetype[!is.na(Crop2016Early) | !is.na(Crop2016Late),]
	usetype[, Response:=1]
# Combine use and availability    
	temp = rbind(availtype, usetype)
	loggerno = stringr::str_split(loggers[i], '_')[[1]][1]  # Get the ID of the logger
	temp[, LoggerID:=loggerno]
	TheList[[i]] = temp
	sputm$LoggerID = loggerno
	ThePlotList[[i]] = sputm
}
# pdf(file = 'C:/Users/lada/Dropbox/StarlingGPS/IndividualUse.pdf')
# pdf(file = '/Users/Lars/Dropbox/StarlingGPS/IndividualUse.pdf')
for (i in seq_along(loggers)) {
	 print(lp + layer(sp.points(ThePlotList[[i]], col = rainbow(7)[i], cex = .5)) +
	layer(panel.text(481700, 6156000, stringr::str_split(loggers[i], '_')[[1]][1]))
	)
}
# dev.off()
# Combine the items in TheList to a data.table:
starlings = rbindlist(TheList)
setnames(starlings, old = c('ID', 'field_type'), new = c('PolyID', 'FieldType'))
write.table(starlings, file = paste0('Starlings', Sys.Date(), '.txt'), quote = FALSE, row.names = FALSE)
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
