# Author: IMZILEN TAHA
# Contact: taha.imzilen@ird.fr
# Institut: IRD
# Responsable: BARDE JULIEN
# 

#this is a test 
# Versions : v0=only point geometry # 
# created:05-05-2015
# Versions : v1= point line and polygon geometry # 
# created:14-04-2017
#current version: v1
#last update:07-08-2018

### . DOCUMENTATION / PARAMETERS DESCRIPTION

############
# id = Environmental_Data_Enrichment
 title = Environmental_Data_Enrichment
 abstract = This process take in parameter a spatio-temporal dataset and an OPeNDAP dataset URL and  enrich the input dataset with OPeNDAP data. Input dataset must be an SHP file of point line or polygon geometry

#########

#shp: the spatial (environmental) data should be an shapefile ( points, lines , polygons)

#nameTime: column name of date in the attributs table of the shapefile / if doesn't exist, we let it empty and we define the parameter InsertTime

#InsertTime: to add a date of observation "%d-%m-%Y". Only if the parameter nameTime doesn't exist

#opendapurl: to add an opendap url for the environmental data

#ParName: to add the name of the variable in the netcdf which interests us.

#longitudeName: the name of the dimension of longitude in the netcdf file: usually the value  is 'longitude'

#latitudeName: the name of the dimension of latitude in the netcdf file: usually the value  is 'latitude'

#depth: to define the value of depth (m) : by default, the value is -1 for the surface environmental values

#SpatialMethod: spatial method to look for value.there is too methods (default & spatialBuffer). the default value is 'default' to look for
#the closest value. and SpatialBuffer to look for each buffer the corresponding values

#valSBuffer: only if SpatialMethod== SpatialBuffer. this parameter allow to add a value (KM) to creat the buffer

#timeMethod: time method to look for value. there is too methods (ClosestTime & TemporalBuffer). the default value is 'ClosestTime' to 
#look for the closest time value. and TemporalBuffer to look for each temporal buffer the corresponding values

#valTBuffer: only if valTBuffer== TemporalBuffer.this parameter allow to add a value (days) to creat the temporal buffer.

#TemporalOperator: only if valTBuffer== TemporalBuffer.this parameter allow to define if the buffer temporal should be around, before or after the time of observations.


