# Read ascii of ALMaSS landscape 
# Remap values to goose numbers from sim
print('foo')
# Import system modules
from arcpy import env
import arcpy, traceback, sys, time, gc, os
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")
arcpy.env.parallelProcessingFactor = "75%"
# Set local variables
inFields = "O:/ST_LandskabsGenerering/gis/dkgis.gdb/MarkerDK2013"
inRingingSites = "O:/ST_Starlings/GIS/RingingSites.txt"
# Set spatial reference:
prj = arcpy.SpatialReference("WGS 1984 UTM Zone 32N")

# Convert fields to layer file
arcpy.MakeFeatureLayer_management(inFields, "inFields_lyr")

locations = ['KostraedeBanker', 'RavnstrupSoe', 'TingvadBorum', 'AuKaloe', 'HjortkaerEndrup', 'HHGlumsoe']
pathtofields = "O:/ST_Starlings/GIS/Fields/"


try:
	# Import the ringing sites
	# Set the local variables
	x_coords = "X"
	y_coords = "Y"
	out_Layer = "ringingsites"
	# saved_Layer = "C:/Users/lada/Desktop/ringingsites.lyr"
	saved_shp = "C:/Users/lada/Desktop/ringingsites.shp"
	selected_ringsite = "C:/Users/lada/Desktop/kaloe.shp"
	# Make the XY event layer...
	arcpy.MakeXYEventLayer_management(inRingingSites, x_coords, y_coords, out_Layer, prj)
	# Copy layer to generate OIDs:
	arcpy.CopyFeatures_management(out_Layer, saved_shp)
	# Convert shape to feature:
	arcpy.MakeFeatureLayer_management(saved_shp, "RingSite")
	# Select one ringing site:
	arcpy.SelectLayerByAttribute_management("RingSite", "NEW_SELECTION", "\"Location\" = 'AuKaloe'")
	# # Select by distance
	arcpy.SelectLayerByLocation_management("inFields_lyr",'WITHIN_A_DISTANCE', "RingSite", "2000 Meters", 'NEW_SELECTION', "NOT_INVERT")
	# arcpy.DefineProjection_management(outRaster, prj)
	arcpy.CopyFeatures_management('inFields_lyr', 'C:/Users/lada/Desktop/AuKaloeFields.shp')


except:
	tb = sys.exc_info()[2]
	tbinfo = traceback.format_tb(tb)[0]
	pymsg = "PYTHON ERRORS:\nTraceback Info:\n" + tbinfo + "\nError Info:\n     " +        str(sys.exc_type) + ": " + str(sys.exc_value) + "\n"
	msgs = "ARCPY ERRORS:\n" + arcpy.GetMessages(2) + "\n"

	arcpy.AddError(msgs)
	arcpy.AddError(pymsg)

	print msgs
	print pymsg

	arcpy.AddMessage(arcpy.GetMessages(1))
	print arcpy.GetMessages(1)



# Tjek det shapefile.strip trick ud:
# Name: CopyFeatures_Example2.py
# Description: Convert all shapefiles in a folder to geodatabase feature classes
# Requirements: os module
 
# Import system modules
import arcpy
from arcpy import env
import os
 
# Set environment settings
env.workspace = "C:/data"
 
# Set local variables
outWorkspace = "c:/output/output.gdb"
 
# Use ListFeatureClasses to generate a list of shapefiles in the
#  workspace shown above.
fcList = arcpy.ListFeatureClasses()
 
# Execute CopyFeatures for each input shapefile
for shapefile in fcList:
    # Determine the new output feature class path and name
    outFeatureClass = os.path.join(outWorkspace, shapefile.strip(".shp"))
    arcpy.CopyFeatures_management(shapefile, outFeatureClass)