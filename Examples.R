# Author: IMZILEN TAHA
# Contact: taha.imzilen@ird.fr
# Institut: IRD
# Responsable: BARDE JULIEN
# 
# 
# Versions : v0=only point geometry # 
# created:05-05-2015
# Versions : v1= point line and polygon geometry # 
# created:14-04-2017
#current version: v1
#last update:07-08-2018

#############################
###### 1. USE EXAMPLE 

# EXP1: sp Polygone (Polygon data used is available in the repository)
data_Enriched <- Environmental_Data_Enrichment(shp = 'Longhurst_world_v4_2010.shp',
InsertTime="16-05-2010",
opendapurl='http://mdst-macroes.ird.fr:8080/thredds/dodsC/macroes/world/sst/modis/5d/1m/w5d_modis_sst2_4km_months_20100101_20101231.r2010.0.qual0.extrac.avg.float.nc',
ParName='sst2',
longitudeName='longitude',
latitudeName='latitude')
spplot(data_Enriched,"sea_surface_temperature",main=paste("sea_surface_temperature - :",InsertTime),sub="Example")


##EXP2:sp Line (Line data used isn't available: confidential)

data_Enriched <- Environmental_Data_Enrichment(
  shp= 'FADs2009.shp',
  InsertTime="16-05-2010",
  opendapurl='http://thredds.d4science.org/thredds/dodsC/public/netcdf/Oscar/oscar_vel_1992-2017_180.nc',
  ParName='u',
  longitudeName='longitude',
  latitudeName='latitude')
spplot(data_Enriched,"Ocean_Surface_Zonal_Currents",main=paste("Ocean_Surface_Zonal_Currents - :",InsertTime),sub="Example")


##EXP3:sp point (points data used isn't available: confidential)

data_Enriched <- Environmental_Data_Enrichment(
  shp= 'fads_clean.positions_clean_2009.shp',
  nameTime="pt_date",
  opendapurl='http://thredds.d4science.org/thredds/dodsC/public/netcdf/Oscar/oscar_vel_1992-2017_180.nc',
  ParName='u',
  longitudeName='longitude',
  latitudeName='latitude')
spplot(data_Enriched,"Ocean_Surface_Zonal_Currents",main=paste("Ocean_Surface_Zonal_Currents - :"),sub="Example")


