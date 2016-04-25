# Read ascii of ALMaSS landscape 
# Remap values to goose numbers from sim

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

try:
	# Import the ringing sites
	# Set the local variables
	x_coords = "X"
	y_coords = "Y"
	out_Layer = "ringingsites"
	saved_Layer = "C:/Users/lada/Desktop/ringingsites.lyr"
	# Make the XY event layer...
	arcpy.MakeXYEventLayer_management(inRingingSites, x_coords, y_coords, out_Layer, prj)
	# Save to a layer file (may not be needed any longer...)
	arcpy.SaveToLayerFile_management(out_Layer, saved_Layer)
	# Copy layer to generate OIDs:
	arcpy.CopyFeatures_management(IN, OUT)
	# Select one ringing site:
	arcpy.SelectLayerByAttribute_management(out_Layer, "NEW_SELECTION",' "Location" = "AuKaloe" ')
	# Write the selected features to a new featureclass
	# arcpy.CopyFeatures_management("lyr", "chihuahua_10000plus")
	# Select by distance
	arcpy.SelectLayerByLocation_management(in_layer = 'inFields', overlap_type = 'WITHIN_A_DISTANCE', search_distance = 2000, select_features = out_Layer, selection_type = 'NEW_SELECTION')
	# arcpy.DefineProjection_management(outRaster, prj)
	arcpy.CopyFeatures_management('inFields', 'AuKaloeFields')


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
