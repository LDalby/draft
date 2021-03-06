# Subset polygon layer with x,y from txt file
# Import system modules
from arcpy import env
import arcpy, traceback, sys, time, gc, os
from arcpy.sa import *
arcpy.CheckOutExtension("Spatial")
arcpy.env.parallelProcessingFactor = "75%"
# Set local variables
inGDB = "O:/ST_LandskabsGenerering/gis/dkgis.gdb"
inLayers = ["MarkerDK2013", "soer", "bygning", "lavbebyg", "skov", "paragraf3"]
inRingingSites = "O:/ST_Starlings/GIS/RingingSites.txt"
# Set spatial reference:
prj = arcpy.SpatialReference("WGS 1984 UTM Zone 32N")
# The list of location names
locations = ['KostraedeBanker', 'RavnstrupSoe', 'TingvadBorum', 'AuKaloe', 'HjortkaerEndrup', 'HHGlumsoe']
pathtofields = "O:/ST_Starlings/GIS/Fields/"  # output fields
# Import the ringing sites
x_coords = "X"
y_coords = "Y"
out_Layer = "ringingsites"
saved_shp = "O:/ST_Starlings/GIS/ringingsites.shp"
if arcpy.Exists(saved_shp):
  arcpy.Delete_management(saved_shp)
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

# Make the XY event layer...
arcpy.MakeXYEventLayer_management(inRingingSites, x_coords, y_coords, out_Layer, prj)
# Copy layer to generate OIDs:
arcpy.CopyFeatures_management(out_Layer, saved_shp)
# Convert shape to feature:
arcpy.MakeFeatureLayer_management(saved_shp, "RingSite")

for jndex in range(len(inLayers)):
  # Convert fields to layer file
  targetLayer = os.path.join(inGDB, inLayers[jndex])
  arcpy.MakeFeatureLayer_management(targetLayer, inLayers[jndex] + "_lyr")
  print targetLayer
  for index in range(len(locations)):
    fieldname = "Location"
    fieldvalue = locations[index]
    print fieldvalue
    where_clause = buildWhereClause("RingSite", fieldname, fieldvalue)
    # Select one ringing site:
    arcpy.SelectLayerByAttribute_management("RingSite", "NEW_SELECTION", where_clause)
    # Select by distance
    arcpy.SelectLayerByLocation_management(inLayers[jndex] + "_lyr",'WITHIN_A_DISTANCE', "RingSite", "1500 Meters", 'NEW_SELECTION', "NOT_INVERT")
    # Make the file name
    layerlocfile = locations[index] + inLayers[jndex] + ".shp"
    fieldloc = os.path.join(pathtofields, layerlocfile)
    if arcpy.Exists(fieldloc):
      arcpy.Delete_management(fieldloc)
    print fieldloc
    arcpy.CopyFeatures_management(inLayers[jndex] + "_lyr", fieldloc)
# Remove anything from the p-3 layer that is covered in any of the other layers
xyTol = "1 Meters"
# Merge the shapefiles into one, but leave the paragraf3 layer:
for index in range(len(locations)):
  finallocfile = locations[index] + "Final.shp"
  finalloc = os.path.join(pathtofields, finallocfile)
  if arcpy.Exists(finalloc):
      arcpy.Delete_management(finalloc)
  outpath = os.path.join(pathtofields, locations[index])
  arcpy.Merge_management([outpath+inLayers[0]+".shp",outpath+inLayers[1]+".shp",
  outpath+inLayers[2]+".shp",outpath+inLayers[3]+".shp",outpath+inLayers[4]+".shp"], finalloc)
  # Setup paths
  difflocfile = locations[index] + "P3diff.shp"
  diffloc = os.path.join(pathtofields, difflocfile)
  if arcpy.Exists(diffloc):
      arcpy.Delete_management(diffloc)
  # The p-3 layer:
  p3layer = outpath+inLayers[5]+".shp"
  # Erase
  arcpy.Erase_analysis(p3layer, finalloc, diffloc, xyTol)
