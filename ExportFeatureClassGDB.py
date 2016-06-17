# Snippet from gis.stackexchange answer:
# http://gis.stackexchange.com/questions/49814/reading-file-geodatabase-using-r
# Import shapefile stored in file geodatabase
import os,sys
import arcpy
from arcpy import env
from sys import argv ### This is needed to import variables

script, featureClass, inFeatures, outLocation, outFeatureClass = argv
env.workspace = featureClass ### set working directory
arcpy.FeatureClassToFeatureClass_conversion(inFeatures, outLocation,outFeatureClass)