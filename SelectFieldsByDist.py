# Subset polygon layer with x,y from txt file
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
# The list of location names
locations = ['KostraedeBanker', 'RavnstrupSoe', 'TingvadBorum', 'AuKaloe', 'HjortkaerEndrup', 'HHGlumsoe']
pathtofields = "O:/ST_Starlings/GIS/Fields/"
# Import the ringing sites
x_coords = "X"
y_coords = "Y"
out_Layer = "ringingsites"
saved_shp = "O:/ST_Starlings/GIS/ringingsites.shp"
if arcpy.Exists(saved_shp):
        arcpy.Delete_management(saved_shp)

# Convert fields to layer file
arcpy.MakeFeatureLayer_management(inFields, "inFields_lyr")
# Make the XY event layer...
arcpy.MakeXYEventLayer_management(inRingingSites, x_coords, y_coords, out_Layer, prj)
# Copy layer to generate OIDs:
arcpy.CopyFeatures_management(out_Layer, saved_shp)
# Convert shape to feature:
arcpy.MakeFeatureLayer_management(saved_shp, "RingSite")
# Define function to construct WHERE clause:
def buildWhereClause(table, field, value):
  """Constructs a SQL WHERE clause to select rows having the specified value
  within a given field and table."""
  # Add DBMS-specific field delimiters
  fieldDelimited = arcpy.AddFieldDelimiters(table, field)
  # Determine field type
  fieldType = arcpy.ListFields(table, field)[0].type
  # Add single-quotes for string field values
  if str(fieldType) == 'String':
      value = "'%s'" % value
  # Format WHERE clause
  whereClause = "%s = %s" % (fieldDelimited, value)
  return whereClause

for index in range(len(locations)):
	fieldname = "Location"
	fieldvalue = locations[index]
	where_clause = buildWhereClause("RingSite", fieldname, fieldvalue)
	# Select one ringing site:
	arcpy.SelectLayerByAttribute_management("RingSite", "NEW_SELECTION", where_clause)
	# Select by distance
	arcpy.SelectLayerByLocation_management("inFields_lyr",'WITHIN_A_DISTANCE', "RingSite", "2000 Meters", 'NEW_SELECTION', "NOT_INVERT")
	# Make the file name
	fieldlocfile = locations[index] + ".shp"
	fieldloc = os.path.join(pathtofields, fieldlocfile)
	arcpy.CopyFeatures_management('inFields_lyr', fieldloc)

