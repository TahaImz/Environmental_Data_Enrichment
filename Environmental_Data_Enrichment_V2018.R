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



###### F. Function 

Environmental_Data_Enrichment <- function(shp, nameTime="", InsertTime="",
                                                opendapurl, ParName,
                                                longitudeName= "longitude",
                                                latitudeName="latitude", 
                                                depth = -1,
                                                SpatialMethod ="default", 
                                                # SpatialMethod = "SpatialBuffer", 
                                                valSBuffer = 3,
                                                timeMethod="ClosestTime" ,
                                                # timeMethod="TemporalBuffer",
                                                TemporalOperator = "around" ,  ## "before","after", 
                                                valTBuffer = 60 ,
                                                verbose=TRUE )  {
  
  ##----------------------------Packages --------------------------------##
  
  ##----------------------------Packages --------------------------------##
  library(ncdf4)
  library(rgdal)
  library(raster)
  library(chron)
  library(rgeos)
  # How to install IRDTunaAtlas in R : https://github.com/jsubei/IRDTunaAtlas/wiki
  
  # require(IRDTunaAtlas)
  # library(FigisGeoUtils.R) to use "Utils.R"
  # source("Utils.R")
  
  ##-------------------------- check Input data ---------------------##
  
#   if (nchar(WFSurl)==0) {
#     stop("no WFSurl parameter",call.=F)
#   }
  
  if (nchar(opendapurl)==0) {
    stop("no opendapUrl parameter",call.=F)
  }
  
  if (nchar(ParName)==0) {
    stop("no varName parameter",call.=F)
  }
  
  if (nchar(nameTime)==0 && nchar(InsertTime)==0) {
    stop("no Time parameter",call.=F)
  }
  
  if (nchar(nameTime)>0 && nchar(InsertTime)>0) {
    stop("error Time parameter (insert time or name column time:choose one of the two parameter!)",call.=F)
  }
  
  ##-----  reading data
  
  if (verbose) {
    cat("Opening the netCDF\n\n")
  } 
  nc <- nc_open(opendapurl)
  
  if (verbose) {
    cat("Opening shapefile Observations Data\n")
  } 
  
  # dataObservation <- readData(dataType="WFS",url=WFSurl,)
  # dataObservation <- readOGR(dsn=paste(dirRData,shp,'.shp',sep=''),layer=shp,verbose=TRUE)
  dataObservation <- readOGR(dsn=shp,layer=unlist(strsplit(unlist(strsplit(shp,'/'))[length(unlist(strsplit(shp,'/')))],'.shp'))[1],verbose=TRUE)
  
  
  if (nchar(nameTime)>0){
    if (! nameTime %in% names(dataObservation)) {
      stop(paste("Cannot find name time :",nameTime),call.=F)
    }
    
    
    if (verbose) {
      cat("\nCheck variables and dimensions\n\n")
    }
    ## --------------------------time from Obs Data Columns------------------##
    
    dataObservation[[nameTime]] <- gsub("/","-",dataObservation[[nameTime]])
    dataObservation[[nameTime]] <- as.POSIXct(strptime(dataObservation[[nameTime]], "%Y-%m-%d"), tz="GMT")
  }
  
  if (nchar(InsertTime)>0){
    if (verbose) {
      cat("\nCheck variables and dimensions\n\n")
    }
    InsertTime <- gsub("/","-",InsertTime)
    InsertTime <- as.POSIXct(strptime(InsertTime, "%d-%m-%Y"), tz="GMT")
  }
  ## comments:  column of time data must be in the form year-month-day or year/month/day .
  
  
  
  ## ----------------read vals times from the netCDF ------------------##
  
  parName <- ParName
  ipar <- which(as.numeric(names(nc$var)==parName)==1)
  
  if (length(ipar) == 0) {
    stop(paste("Cannot find Parameter name:", parName),call.=F)
  }
  
  ### read time origin ------->
  
  origin <- strptime(unlist(strsplit(nc$var[[ipar]]$dim[[nc$var[[ipar]]$ndims]]$units, " since "))[2], format="%Y-%m-%d %H:%M:%S")  
  
  if (is.na(origin)) {
    origin <- strptime(unlist(strsplit(nc$var[[ipar]]$dim[[nc$var[[ipar]]$ndims]]$units, " since "))[2], format="%Y-%m-%dT%H:%M:%S")
  }
  if (is.na(origin)) {
    origin <- strptime(unlist(strsplit(nc$var[[ipar]]$dim[[nc$var[[ipar]]$ndims]]$units, " since "))[2], format="%Y-%m-%dT%H:%M:%SZ")
  }
  if (is.na(origin)) {
    origin <- strptime(unlist(strsplit(nc$var[[ipar]]$dim[[nc$var[[ipar]]$ndims]]$units, " since "))[2], format="%Y-%m-%d")
  }
  if (is.na(origin)) {
    stop(paste("Cannot read time origin: ", time.var.units),call.=F)
  }
  # if(as.numeric(strsplit(as.character(origin),split="-")[[1]][1])== 0){
  #   stop(paste("Cannot read time origin: ", time.var.units),call.=F)
  # }
  ### read units time  ----------->
  
  units <- gsub(" ","",unlist(strsplit(nc$var[[ipar]]$dim[[nc$var[[ipar]]$ndims]]$units," since "))[[1]])
  
  if ((units =="seconds") || (units =="Seconds") || (units =="second") || (units =="Second") || (units =="SECS") ) {
    datepar <- as.POSIXct(nc$var[[ipar]]$dim[[nc$var[[ipar]]$ndims]]$vals, origin= unlist(strsplit(nc$var[[ipar]]$dim[[nc$var[[ipar]]$ndims]]$units,"since"))[[2]]) 
  }
  
  if ((units =="days") || (units =="Days") || (units =="day") || (units =="Day") ) {
    datepar <- as.POSIXct((nc$var[[ipar]]$dim[[nc$var[[ipar]]$ndims]]$vals)*86400, origin= unlist(strsplit(nc$var[[ipar]]$dim[[nc$var[[ipar]]$ndims]]$units,"since"))[[2]]) 
    
  }
  
  if ((units =="hours") || (units =="Hours") || (units =="hour") || (units =="Hour")) {
    datepar <- as.POSIXct((nc$var[[ipar]]$dim[[nc$var[[ipar]]$ndims]]$vals)*3600, origin= unlist(strsplit(nc$var[[ipar]]$dim[[nc$var[[ipar]]$ndims]]$units,"since"))[[2]]) 
    
  }
  
  
  if (((units !="seconds") && (units !="Seconds") && (units !="second") && (units !="Second") && (units !="SECS") )
      && ((units !="days") && (units !="Days") && (units !="day") && (units !="Day") )
      && ((units !="hours") && (units !="Hours") && (units !="hour") && (units !="Hour")))
  {
    stop(paste("Cannot read time units: ", units),call.=F)
  }
  
  ## ----------------read vals longitude-latitude from the netCDF ------------------##
  
  
  
  ilon <- 0
  ilat <- 0
  for ( i in 1:nc$var[[ipar]]$ndims) {
    if (ncatt_get(nc,nc$var[[ipar]]$dim[[i]]$name, "standard_name")$value == longitudeName || ncatt_get(nc,nc$var[[ipar]]$dim[[i]]$name, "axis")$value == "X" || nc$var[[ipar]]$dim[[i]]$name==longitudeName){
      vals_longitude <- nc$var[[ipar]]$dim[[i]]$vals
      ilon <- i
    } 
    
    if (ncatt_get(nc,nc$var[[ipar]]$dim[[i]]$name, "standard_name")$value == latitudeName || ncatt_get(nc,nc$var[[ipar]]$dim[[i]]$name, "axis")$value == "Y"  || nc$var[[ipar]]$dim[[i]]$name==latitudeName){
      vals_latitude <- nc$var[[ipar]]$dim[[i]]$vals 
      ilat <- i
    } 
  }
  
  if ((ilon==0)||(ilat==0)) {
    stop(paste("Cannot read lonlat_standard_name  (!= longitude_latitude)."),call.=F)
  }
  
  resX <- abs(vals_longitude[1]-vals_longitude[2])
  resY <- abs(vals_latitude[1]-vals_latitude[2])
  
  Xmin <- min(vals_longitude)-(resX/2); Xmax <- max(vals_longitude)+(resX/2)
  
  
  Ymin <- min(vals_latitude)-(resY/2); Ymax <- max(vals_latitude)+(resY/2)
  
  ## ----------------- for the depth dimension -------------------------##
  
  idepth <- 0
  for ( iz in 1:nc$var[[ipar]]$ndims) {
    if (ncatt_get(nc,nc$var[[ipar]]$dim[[iz]]$name, "standard_name")$value == "depth" || 
        ncatt_get(nc,nc$var[[ipar]]$dim[[iz]]$name, "standard_name")$value == "altitude" || 
        ncatt_get(nc,nc$var[[ipar]]$dim[[iz]]$name, "axis")$value == "Z"  ){
      vals_depth <- nc$var[[ipar]]$dim[[iz]]$vals
      idepth <- iz
    }}
  if((depth==-1) && (idepth != 0)){ind_depth <- 1}
  
  if((depth != -1) && (idepth != 0)){ind_depth <- min(which(abs(vals_depth-depth)==min(abs(vals_depth-depth))))}
  
  if(( depth != -1) && (idepth==0)){ind_depth <- 1}
  
  
  ## -------------------------------some default Columns to add to the Obs data --------------------------------##
  # mean and standard deviation 
  longname <- gsub(" ","_",nc$var[[ipar]]$longname)
  if (idepth != 0) {longname <- paste(longname,"_Depth_", vals_depth[ind_depth],sep="")}
  if ((idepth == 0)&&(depth != -1)) {longname <- paste(longname,"_Depth_", 0,"]",sep="")}
  dataObservation[[longname]] <- NA
  sd <- paste("sd",longname,sep="_")
  if(class(dataObservation)=="SpatialPolygonsDataFrame"){dataObservation[[sd]] <- NA}
  
  if (verbose) {
    cat("Processing ... \n")
  }
  
  # test to allow processing ------>
  if(class(dataObservation)!="SpatialPointsDataFrame" && class(dataObservation)!="SpatialPolygonsDataFrame" && class(dataObservation)!="SpatialLinesDataFrame" ){
    
    stop(paste("Type ", class(dataObservation)[1], " not yet implmented."), sep="",call.=F)
  }
  
  box <- bbox(dataObservation)
  
  if((round(box[1],3)<Xmin) || (round(box[3],3)>Xmax) || (round(box[2],3)<Ymin) || (round(box[4],3)>Ymax)) {
    stop(paste("environmental data don't cover the global observation areas."),call.=F)
  }
  
  ## ----------------  Processing -------------------------##
  
  for (i in 1:length(dataObservation)){
    print(paste(i,'sur',length(dataObservation),sep=' '))
    start <- rep(1,(nc$var[[ipar]]$ndims))
    count <- rep(1,(nc$var[[ipar]]$ndims))
    count[c(ilon,ilat)] <- -1
    if (idepth != 0) {start[idepth] <- ind_depth}
    
    ## ------- Default Time method : closest time  
    
    if (timeMethod =="ClosestTime"){
      if (nchar(nameTime)>0){
        ind <- min(which(abs(datepar-dataObservation[[nameTime]][i])==min(abs(datepar-dataObservation[[nameTime]][i]))))
        start[nc$var[[ipar]]$ndims] <- ind
        par <- ncvar_get(nc,parName,start,count)
      }
      
      if (nchar(InsertTime)>0){
        ind <- min(which(abs(datepar-InsertTime)==min(abs(datepar-InsertTime))))
        start[nc$var[[ipar]]$ndims] <- ind
        par <- ncvar_get(nc,parName,start,count)
      }
    }
    ## ---------Optional Time Method  : buffer time  (after-before-around) ----- b##
    
    if (timeMethod=="TemporalBuffer"){
      if (nchar(nameTime)>0){
        if(TemporalOperator =="after"){
          ind <- which( 0 < datepar-dataObservation[[nameTime]][i]  & datepar-dataObservation[[nameTime]][i] <valTBuffer)}
        if(TemporalOperator =="before"){
          ind <- which( 0 > datepar-dataObservation[[nameTime]][i]  & datepar-dataObservation[[nameTime]][i] >-valTBuffer)}
        if(TemporalOperator =="around"){
          ind <-  which( abs(datepar-dataObservation[[nameTime]][i]) <valTBuffer/2)}
      }
      
      if (nchar(InsertTime)>0){
        if(TemporalOperator =="after"){
          ind <- which( 0 < datepar-InsertTime  & datepar-InsertTime <valTBuffer)}
        if(TemporalOperator =="before"){
          ind <- which( 0 > datepar-InsertTime  & datepar-InsertTime >-valTBuffer)}
        if(TemporalOperator =="around"){
          ind <-  which( abs(datepar-InsertTime) <valTBuffer/2)}
      }
      if(length(ind)==0) {stop(paste("it has no environmental data for this time buffer"),call.=F)
      } else { sumlayer <- 0
      for(j in 1:length(ind)){start[nc$var[[ipar]]$ndims] <- ind[j]
      layer <- get.var.ncdf(nc,parName,start,count)
      sumlayer <- sumlayer+layer            
      }
      par <- sumlayer/length(ind)
      }
      print(paste("there are", length(ind) ,"different time for this buffer time: ", datepar[ind], sep=" "))
      
    }  
    ##-----
    
    
    par[par >= abs(nc$var[[ipar]]$missval)]<- NA 
    parTr <- t(par)
    data <- list(x=seq(Xmin,Xmax),y=seq(Ymin,Ymax),z=parTr[nrow(parTr):1,])
    dataraster <- raster(data$z, xmn=range(data$x)[1], xmx=range(data$x)[2],ymn=range(data$y)[1], ymx=range(data$y)[2])
    
    xres <- xres(dataraster)
    yres <- yres(dataraster)
    
    
    ##--------------- Default Spatial method
    
    options(warn=-1)
    if(SpatialMethod=="default"){
      
      ## ----------------- cas de polygones:
      
      if(class(dataObservation)=="SpatialPolygonsDataFrame"){
        isub <- rep(F,length(dataObservation))
        isub[i] <- T
        cell <- cellFromPolygon(dataraster,subset(dataObservation, isub),weights=T)
        
        if (as.character(cell)=="NULL") {
          #environmental data don't cover the  observation areas. return -999999
          
          dataObservation[[longname]][i] <- -999999
          
          if (timeMethod =="ClosestTime"){
            dataObservation[[paste("time",longname,sep="_")]][i] <- as.character(datepar[ind])
            
            if(nchar(nameTime)>0){
              dataObservation$diff_time[i] <- paste(round(abs(difftime(datepar[ind],dataObservation[[nameTime]][i],units="days")),2),"day (s)", sep=" ")
            }
            if(nchar(InsertTime)>0){
              dataObservation$diff_time[i] <- paste(round(abs(difftime(datepar[ind],InsertTime,units="days")),2),"day (s)", sep=" ")
            }
          }
          if (timeMethod=="TemporalBuffer"){dataObservation[[paste("buffer_time",longname,sep="_")]][i] <- paste("there are", length(ind) ,"different time for this buffer time")}  
          
          
          dataObservation[[sd]][i] <- -9999999
          
          p.c.DataInArea <- -9999999
          
          dataObservation$p.c.DataInArea[i] <- p.c.DataInArea 
          
        }
        
        if (as.character(cell)!="NULL") {
          
          indNA <- which(is.na(dataraster[cell[[1]][,1]]))
          coefNA <- sum(cell[[1]][indNA,2])
          dataObservation[[longname]][i] <- round(sum(cell[[1]][,2]*dataraster[cell[[1]][,1]],na.rm=T)/(sum(cell[[1]][,2])-coefNA),4)
          
          if (timeMethod =="ClosestTime"){
            dataObservation[[paste("time",longname,sep="_")]][i] <- as.character(datepar[ind])
            
            if(nchar(nameTime)>0){
              dataObservation$diff_time[i] <- paste(round(abs(difftime(datepar[ind],dataObservation[[nameTime]][i],units="days")),2),"day (s)", sep=" ")
            }
            if(nchar(InsertTime)>0){
              dataObservation$diff_time[i] <- paste(round(abs(difftime(datepar[ind],InsertTime,units="days")),2),"day (s)", sep=" ")
            }
          }
          if (timeMethod=="TemporalBuffer"){dataObservation[[paste("buffer_time",longname,sep="_")]][i] <- paste("there are", length(ind) ,"different time for this buffer time")}  
          
          if(length(cell[[1]][,1]) > 1){
            dataObservation[[sd]][i] <- round(sd(dataraster[cell[[1]][,1]], na.rm=T),2)
          }
          if(length(which(!(is.na(dataraster[cell[[1]][,1]])))) == 1){
            dataObservation[[sd]][i] <- -1
          }
          
          p.c.DataInArea <- paste(round(((sum(cell[[1]][,2])-coefNA)/sum(cell[[1]][,2]))*100,2),"%",sep=" ")
          
          dataObservation$p.c.DataInArea[i] <- p.c.DataInArea
        }
      }
      #### cas de lignes
      if(class(dataObservation)=="SpatialLinesDataFrame"){
        isub <- rep(F,length(dataObservation))
        isub[i] <- T
        cell <- cellFromLine(dataraster,subset(dataObservation, isub))
        
        if (as.character(cell)=="NULL") {
          #environmental data don't cover the  observation areas. return -999999
          
          dataObservation[[longname]][i] <- -999999
          
          if (timeMethod =="ClosestTime"){
            dataObservation[[paste("time",longname,sep="_")]][i] <- as.character(datepar[ind])
            
            if(nchar(nameTime)>0){
              dataObservation$diff_time[i] <- paste(round(abs(difftime(datepar[ind],dataObservation[[nameTime]][i],units="days")),2),"day (s)", sep=" ")
            }
            if(nchar(InsertTime)>0){
              dataObservation$diff_time[i] <- paste(round(abs(difftime(datepar[ind],InsertTime,units="days")),2),"day (s)", sep=" ")
            }
          }
          if (timeMethod=="TemporalBuffer"){dataObservation[[paste("buffer_time",longname,sep="_")]][i] <- paste("there are", length(ind) ,"different time for this buffer time")}  
          
          
          dataObservation[[sd]][i] <- -9999999
          
          p.c.DataInArea <- -9999999
          
          dataObservation$p.c.DataInArea[i] <- p.c.DataInArea 
          
        }
        
        if (as.character(cell)!="NULL") {
          
          dataObservation[[longname]][i] <- round(mean(dataraster[cell[[1]]], na.rm=T),4)
          if (timeMethod =="ClosestTime"){
            dataObservation[[paste("time",longname,sep="_")]][i] <- as.character(datepar[ind])
            if(nchar(nameTime)>0){
              dataObservation$diff_time[i] <- paste(round(abs(difftime(datepar[ind],dataObservation[[nameTime]][i],units="days")),2),"day (s)", sep=" ")
            }
            if(nchar(InsertTime)>0){
              dataObservation$diff_time[i] <- paste(round(abs(difftime(datepar[ind],InsertTime,units="days")),2),"day (s)", sep=" ")
            }    }
          if (timeMethod=="TemporalBuffer"){dataObservation[[paste("buffer_time",longname,sep="_")]][i] <- paste("there are", length(ind) ,"different time for this buffer time")}  
          
          dataObservation[[sd]][i] <- round(sd(dataraster[cell[[1]]], na.rm=T),2)
          if(is.na(abs(nc$var[[ipar]]$missval))){
          dataObservation[[sd]][i] <- round(sd(dataraster[cell[[1]]][dataraster[cell[[1]]] < abs(nc$var[[ipar]]$missval) ], na.rm=T),2)}
          p.c.DataInArea <- paste(round((length(which(! is.na(dataraster[cell[[1]]])))/length(dataraster[cell[[1]]]))*100,2),"%",sep=" ")
          
          dataObservation$p.c.DataInArea[i] <- p.c.DataInArea
        }
      }
      
      #### cas de points
      
      if(class(dataObservation)=="SpatialPointsDataFrame"){
        
        isub <- rep(F,length(dataObservation))
        isub[i] <- T
        
        
        cell <- cellFromXY(dataraster,subset(dataObservation, isub))
        
        if (as.character(cell)=="NULL") {
          #environmental data don't cover the  observation areas. return -999999
          
          dataObservation[[longname]][i] <- -999999
          
          if (timeMethod =="ClosestTime"){
            dataObservation[[paste("time",longname,sep="_")]][i] <- as.character(datepar[ind])
            
            if(nchar(nameTime)>0){
              dataObservation$diff_time[i] <- paste(round(abs(difftime(datepar[ind],dataObservation[[nameTime]][i],units="days")),2),"day (s)", sep=" ")
            }
            if(nchar(InsertTime)>0){
              dataObservation$diff_time[i] <- paste(round(abs(difftime(datepar[ind],InsertTime,units="days")),2),"day (s)", sep=" ")
            }
          }
          if (timeMethod=="TemporalBuffer"){dataObservation[[paste("buffer_time",longname,sep="_")]][i] <- paste("there are", length(ind) ,"different time for this buffer time")}  
          
          
          dataObservation[[sd]][i] <- -9999999
          
          p.c.DataInArea <- -9999999
          
          dataObservation$p.c.DataInArea[i] <- p.c.DataInArea 
          
        }
        
        if (as.character(cell)!="NULL") {
          #
          nadata <- which(is.na(as.numeric(dataraster[cell])))
          #     if (length(nadata)!=0){ for(j in 1:length(nadata)){dataraster[cell[nadata]] <- mean(dataraster[fourCellsFromXY(dataraster,df[nadata,]@coords,duplicates=T)[j,]],na.rm=T)}  }
          
          
          dataObservation[[longname]][i] <-round(dataraster[cell],4)
          
          if (timeMethod =="ClosestTime"){  
            dataObservation[[paste("time",longname,sep="_")]][i] <- as.character(datepar[ind])
            
            if(nchar(nameTime)>0){
              dataObservation$diff_time[i] <- paste(round(abs(difftime(datepar[ind],dataObservation[[nameTime]][i],units="days")),2),"day (s)", sep=" ")
            }
            if(nchar(InsertTime)>0){
              dataObservation$diff_time[i] <- paste(round(abs(difftime(datepar[ind],InsertTime,units="days")),2),"day (s)", sep=" ")
            } 
          }
          if (timeMethod=="TemporalBuffer"){dataObservation[[paste("buffer_time",longname,sep="_")]][i] <- paste("there are", length(ind) ,"different time for this buffer time")}  
          
        }
      }
    }
    ##--------------- Optional spatial method -------------------------------------------##  
    if(SpatialMethod == "SpatialBuffer"){
      
      ##----------------- 
      
      if((class(dataObservation)=="SpatialPointsDataFrame") || (class(dataObservation)=="SpatialPolygonsDataFrame") || (class(dataObservation)=="SpatialLinesDataFrame")){
        
        isub <- rep(F,length(dataObservation))
        isub[i] <- T
        sub <- subset(dataObservation, isub)
        buff_sub <- gBuffer(sub,width=valSBuffer)
        cell <- cellFromPolygon(dataraster,buff_sub,weights=T)
        
        if (as.character(cell)=="NULL") {
          #environmental data don't cover the  observation areas. return -999999
          
          dataObservation[[longname]][i] <- -999999
          
          if (timeMethod =="ClosestTime"){
            dataObservation[[paste("time",longname,sep="_")]][i] <- as.character(datepar[ind])
            
            if(nchar(nameTime)>0){
              dataObservation$diff_time[i] <- paste(round(abs(difftime(datepar[ind],dataObservation[[nameTime]][i],units="days")),2),"day (s)", sep=" ")
            }
            if(nchar(InsertTime)>0){
              dataObservation$diff_time[i] <- paste(round(abs(difftime(datepar[ind],InsertTime,units="days")),2),"day (s)", sep=" ")
            }
          }
          if (timeMethod=="TemporalBuffer"){dataObservation[[paste("buffer_time",longname,sep="_")]][i] <- paste("there are", length(ind) ,"different time for this buffer time")}  
          
          
          dataObservation[[sd]][i] <- -9999999
          
          p.c.DataInArea <- -9999999
          
          dataObservation$p.c.DataInArea[i] <- p.c.DataInArea 
          
        }
        
        if (as.character(cell)!="NULL") {
          
          
          indNA <- which(is.na(dataraster[cell[[1]][,1]]))
          coefNA <- sum(cell[[1]][indNA,2])
          ## spatial Operator F (see docs)
          dataObservation[[longname]][i] <-round(sum(cell[[1]][,2]*dataraster[cell[[1]][,1]],na.rm=T)/(sum(cell[[1]][,2])-coefNA),4)
          
          if (timeMethod =="ClosestTime"){ 
            dataObservation[[paste("time",longname,sep="_")]][i] <- as.character(datepar[ind])
            if(nchar(nameTime)>0){
              dataObservation$diff_time[i] <- paste(round(abs(difftime(datepar[ind],dataObservation[[nameTime]][i],units="days")),2),"day (s)", sep=" ")
            }
            if(nchar(InsertTime)>0) {
              dataObservation$diff_time[i] <- paste(round(abs(difftime(datepar[ind],InsertTime,units="days")),2),"day (s)", sep=" ")
            }
          }
          if (timeMethod=="TemporalBuffer"){dataObservation[[paste("buffer_time",longname,sep="_")]][i] <- paste("there are", length(ind) ,"different time for this buffer time")}  
          
          
          #     dataObservation[[sd]][i] <- sd(dataraster[cell[[1]][,1]][dataraster[cell[[1]][,1]] < abs(nc$var[[ipar]]$missval) ], na.rm=T)
          
          if(length(cell[[1]][,1]) > 1){
            dataObservation[[sd]][i] <- round(sd(dataraster[cell[[1]][,1]], na.rm=T),2)
          }
          if(length(which(!(is.na(dataraster[cell[[1]][,1]])))) == 1){
            dataObservation[[sd]][i] <- -1
          }
          
          p.c.DataInArea <- paste(round(((sum(cell[[1]][,2])-coefNA)/sum(cell[[1]][,2]))*100,2),"%",sep=" ")
          
          dataObservation$p.c.DataInArea[i] <- p.c.DataInArea
        }
        
      }
      
    } }
  
  ##simple visualization of a layer.
  # pal <- hsv(seq(0.6,0, length.out=256), 1, 1)
  # plot(dataraster,col=pal)
  # plot(dataObservation,add=T)
  # spplot(dataObservation,"sea_surface_temperature",main=paste("sea_surface_temperature - :",InsertTime),sub="Example")
  
  return(dataObservation) 
  
}
