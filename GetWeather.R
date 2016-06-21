# Generate weather files for ALMaSS landscapes
# Author: Lars Dalby
# Date: 16/6/2016
# The python call is modified from this gis SE answer:
# http://gis.stackexchange.com/questions/49814/reading-file-geodatabase-using-r

# Currently written as two loops - could be done in one, but to keep the python
# process somewhat isolated, I've split it up.

library(raster)
library(data.table)
library(rgdal)
# Get the names of all the landscapes:
landscapes = dir('o:/ST_LandskabsGenerering/harelav/HareValidation/')
script = "c:/Users/lada/Git/draft/ExportFeatureClassGDB.py"
pathtogdb = 'O:/ST_LandskabsGenerering/harelav/HareValidation'
inFeatures = "polymask"  # Name of the shapefile in the gdb
for (i in seq_along(landscapes)) {
	landscape = landscapes[i]
	featureClass = file.path(pathtogdb, landscape, 'project.gdb')
	outLocation = file.path(pathtogdb, landscape)
	outFeatureClass = paste0(landscape,".shp")
	system2('python', args = c(shQuote(script),shQuote(featureClass),shQuote(inFeatures),shQuote(outLocation),shQuote(outFeatureClass))) 
}
# With the shapefiles in place we can extract from the E-OBS data using the 
# landscape polygons.
eobs = c('o:/ST_Lada/Data/E-OBS/tg_0.25deg_reg_v12.0.nc', 'o:/ST_Lada/Data/E-OBS/rr_0.25deg_reg_v12.0.nc')
for (i in seq_along(landscapes)) {
	landscape = landscapes[i]
	outLocation = file.path(pathtogdb, landscape)
	outFeatureClass = paste0(landscape,".shp")
	shp = file.path(outLocation, outFeatureClass)
	spobject = readOGR(shp, landscape)
	spobject = spTransform(spobject, CRS('+proj=longlat +datum=WGS84'))
	spobject = spPolygons(spobject, crs = projection(spobject))
	weather = ExtractEOBS(eobs, spobject, metric = 'mean')
	setnames(weather, old = 'MeanTemperature', new = 'Temperature')
	setcolorder(weather, c('Year', 'Month', 'Day', 'Temperature', 'Wind', 'Precipitation'))
	outfile = file.path(pathtogdb, landscape, paste0(landscape, '.pre'))
	WritePolyref(Table = weather, PathToFile = outfile, Headers = FALSE, Type = 'Farm') 
}

# The Vejlerne landscape:
script = "c:/Users/lada/Git/draft/ExportFeatureClassGDB.py"
inFeatures = "vejlerne"  # Name of the shapefile in the gdb
featureClass = 'O:/ST_Lada/Div/FraC/Landskabsgenerering/landskaber/Vejlerne/project.gdb'
outLocation = 'O:/ST_Lada/Div/FraC/Landskabsgenerering/landskaber/Vejlerne'
outFeatureClass = "vejlerne.shp"
system2('python', args = c(shQuote(script),shQuote(featureClass),shQuote(inFeatures),shQuote(outLocation),shQuote(outFeatureClass))) 

spobject = file.path(outLocation, outFeatureClass)
spobject = readOGR(spobject, 'vejlerne')
spobject = spTransform(spobject, CRS('+proj=longlat +datum=WGS84'))
spobject = spPolygons(spobject, crs = projection(spobject))
weather = ExtractEOBS(eobs, spobject, metric = 'mean')
setnames(weather, old = 'MeanTemperature', new = 'Temperature')
setcolorder(weather, c('Year', 'Month', 'Day', 'Temperature', 'Wind', 'Precipitation'))
outfile = 'C:/MSV/ALMaSS_inputs/Weather/Vejlerne.pre'
WritePolyref(Table = weather, PathToFile = outfile, Headers = FALSE, Type = 'Farm') 