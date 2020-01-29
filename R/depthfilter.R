#' @aliases depthfilter
#' @title Filter locations by water depth
#' @description Function to filter locations according to bathymetry and tide.
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' The function filters the input data by the unique "id". 
#' "DateTime" is date & time in class \code{\link[base]{POSIXct}}. 
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' "qi" is the numerical quality index associated with each location fix where the greater number indicates the higher accuracy 
#' (e.g. number of GPS satellites used for estimation).
#' @param bathymetry A RasterLayer object containing bathymetric data in metres. 
#' Negative and positive values indicate below and above the water respectively. 
#' Geographic coordinate system is WGS84.
#' @param extract Method to extract cell values from raster layer as inherited from \code{\link[raster]{extract}} function of the raster package. 
#' Default is bilinear.
#' @param tide A data frame containing columns with the following headers: "tideDT", "reading", "standard.port". 
#' "tideDT" is date & time in class \code{\link[base]{POSIXct}} at each observation. "reading" is the observed tidal height in metres. 
#' "standard.port" is the identifier of each tidal station.
#' @param qi An integer specifying a threshold of quality index. 
#' \emph{depthfilter} does not filter a location that is associated with a quality index higher than this threshold. 
#' Default is 4.
#' @param type The type of water depth considered in the filtering process.
#' "exp" is for the water depth experienced by the animal at the time.
#' This option may be applicable to animals that remains under water all the time (e.g. dugongs, dolphins, etc).
#' "HT" is for the water depth at the nearest high tide (default). 
#' This option is useful for animals that use inter-tidal zones at high tide and may remain there even after the tide drops (e.g. some sea turtles).
#' @param height A numerical value to adjust the water depth an animal is likely to use. Default is 0 m. 
#' This parameter is useful if the minimum water depth used by the animal is known. 
#' For example, a dugong unlikely uses the water shallower than its body height (e.g. <0.6 m)
#' so it may be sensible to consider the fix is an error if the estimated water depth is shallower than its body height.
#' A negative value indicates below the water surface. 
#' For the dugong example, to remove locations for which the water depth was <0.6 m, 
#' it should be specified as; height = -0.6.
#' By supplying the body height to this argument, all the locations recorded at waters shallower than its body will be removed. 
#' @param tidal.plane A data frame containing columns with the following headers: 
#' "standard.port", "secondary.port", "lat", "lon", "timeDiff", "datumDiff". 
#' "standard.port" is the identifier for a tidal observation station. 
#' "secondary.port" is the identifier for a station at which tide is only predicted using tidal records observed at the related standard port. 
#' "lat" and "lon" are the latitude and longitude of each secondary port in decimal degrees. 
#' "timeDiff" is the time difference between standard port and its associated secondary port. 
#' "datumDiff" is the baseline difference in metres between bathymetry and tidal observations/predictions 
#' if each data uses different datum (e.g. LAT and MSL). 
#' @param filter Default is TRUE. 
#' If FALSE, the function does not filter locations but returns estimates of the water depth experienced by the animal at each location.
#' @import sp
#' @importFrom data.table data.table
#' @importFrom raster pointDistance
#' @importFrom raster extract
#' @export
#' @details The function examines each location according to the water depth experienced by the animal or the water depth at the nearest high tide. 
#' The function looks for the closest match between each fix and tidal observations or predictions in temporal and spatial scales. 
#' When \emph{filter} is disabled, the function does not filter locations but returns the estimated water depth of each location with 
#' the tide effect considered (bathymetry + tide). 
#' @return When \emph{filter} option is enabled, this function filters the input data and returns with two additional columns; "depth.exp", "depth.HT". 
#' "depth.exp" is the estimated water depth at each location at the time of location fixing. 
#' "depth.HT" is the estimated water depth at the nearest high tide at each location. 
#' @author Takahiro Shimada
#' @note The input data must not contain temporal or spatial duplicates.
#' @references Shimada T, Limpus C, Jones R, Hazel J, Groom R, Hamann M (2016) 
#' Sea turtles return home after intentional displacement from coastal foraging areas. 
#' \emph{Marine Biology} 163:1-14 doi:\href{http://doi.org/10.1007/s00227-015-2771-0}{10.1007/s00227-015-2771-0}
#' @references Beaman, R.J. (2010) Project 3DGBR: A high-resolution depth model for the Great Barrier Reef and Coral Sea. 
#' \emph{Marine and Tropical Sciences Research Facility (MTSRF) Project 2.5i.1a Final Report}, MTSRF, Cairns, Australia, pp. 13 plus Appendix 1.
#' @seealso \code{\link{dupfilter}}, \code{\link{ddfilter}}
#' @examples
#' 
#' #### Load data sets
#' ## Fastloc GPS data obtained from a green turtle
#' data(turtle)
#' 
#' ## Bathymetry model developed by Beaman (2010)
#' data(bathymodel)
#' 
#' ## A tidal plane for the example site
#' data(tidalplane)
#' 
#' ## Tidal observations and predictions for the example site
#' data(tidedata)
#' 
#' ## Maps for the example site
#' data(SandyStrait)
#' 
#' 
#' #### Remove temporal and/or spatial duplicates
#' turtle.dup <- dupfilter(turtle)
#' 
#' 
#' #### Remove biologically unrealistic fixes 
#' turtle.dd <- ddfilter(turtle.dup, vmax=9.9, qi=4, ia=90, vmaxlp=2.0)
#'
#'
#' #### Apply depthfilter
#' turtle.dep <- depthfilter(sdata=turtle.dd, 
#'                           bathymetry=bathymodel, 
#'                           tide=tidedata, 
#'                           tidal.plane=tidalplane)
#' 
#' 
#' #### Plot data removed or retained by depthfilter
#' plotMap(turtle.dd, bgmap=SandyStrait, point.bg = "red", point.size = 2, line.size = 0.5, 
#'         axes.lab.size = 0, title.size=0, sb.distance=10, multiplot = FALSE)[[1]] + 
#' geom_point(aes(x=lon, y=lat), data=turtle.dep, size=2, fill="yellow", shape=21)+
#' geom_point(aes(x=x, y=y), data=data.frame(x=c(152.68, 152.68), y=c(-25.3, -25.34)), 
#'            size=3, fill=c("yellow", "red"), shape=21) + 
#' annotate("text", x=c(152.7, 152.7), y=c(-25.3, -25.34), label=c("Retained", "Removed"), 
#'          colour="black", size=4, hjust = 0)




depthfilter<-function(sdata, bathymetry, extract="bilinear", qi=4, tide, tidal.plane, type = "HT", height=0, filter=TRUE) {
  
  OriginalSS<-nrow(sdata)
  
  ### Sort data in alphabetical and chronological order
  # Animal data
  sdata<-with(sdata, sdata[order(id, DateTime),])
  row.names(sdata)<-1:nrow(sdata)
  
  # tidal data
  tide<-with(tide, tide[order(standard.port, tideDT),])
  row.names(tide)<-1:nrow(tide)
  
  # tidal plane
  tidal.plane<-with(tidal.plane, tidal.plane[order(standard.port, secondary.port),])
  row.names(tidal.plane)<-1:nrow(tidal.plane) 
  
  
  ### Set lat and lon as "SpatialPoints"
  # Animal data
  LatLong<-data.frame(Y=sdata$lat, X=sdata$lon)
  sp::coordinates(LatLong)<-~X+Y
  sp::proj4string(LatLong)<-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  
  # tidal datat
  LatLong.tide<-data.frame(Y=tidal.plane$lat, X=tidal.plane$lon)
  sp::coordinates(LatLong.tide)<-~X+Y
  sp::proj4string(LatLong.tide)<-sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  
  
  ### extract bathymetry at each animal location
  sdata$bathy<-raster::extract(bathymetry, LatLong, method=extract)
  
  
  ### Find the nearest tidal station for each animal location  
  # Get nearest tidal ports
  distance.to.ports<-raster::pointDistance(LatLong, LatLong.tide, lonlat=T)
  if(nrow(tidal.plane)==1){
    np.index<-1 
  } else {
    np.index<-apply(distance.to.ports, 1, which.min)    
  }
  
  sdata$nearest.port<-tidal.plane[np.index, "secondary.port"]
  np.list<-levels(factor(sdata$nearest.port))
  
  
  # Get the names of standard ports associated to the nearest ports
  s.index<-match(sdata$nearest.port, tidal.plane$secondary.port)
  sdata$standard.port<-tidal.plane[s.index, "standard.port"]
  
  
  ### Subset tidal data according to the range of animal data
  sp.list<-levels(factor(sdata$standard.port))
  tide<-tide[tide$standard.port %in% sp.list,]
    
  trimTide<-function(j){
      MaxInterval<-max(diff(tide[tide$standard.port %in% j, "tideDT"]))
      units(MaxInterval)<-"mins"
      minDateTime<-min(sdata[sdata$standard.port %in% j, "DateTime"])-MaxInterval
      maxDateTime<-max(sdata[sdata$standard.port %in% j, "DateTime"])+MaxInterval
      with(tide, tide[tideDT>=minDateTime & tideDT<=maxDateTime & standard.port %in% j,])
  }
  
  tide.list<-lapply(sp.list, trimTide)
  tide<-do.call(rbind, lapply(tide.list, data.frame, stringsAsFactors=FALSE))
  
  
  ### Organize tidal data
  # Calculate difference in tide to a subsequent record
  tide$range<-unlist(tapply(tide$reading, tide$standard.port, function(x) c(diff(x),NA)))   
  
  
  # Calculate tidal height increment per minute
  Interval<-unlist(tapply(tide$tideDT, tide$standard.port, function(x) c(as.numeric(diff(x)),NA)))
  tide$increment<-tide$range/Interval
  
  
  
  ## Estimate tide at secondary ports
  GetTideSP<-function(j){
      pri.port<-with(tidal.plane, tidal.plane[secondary.port %in% j, "standard.port"])
      sec.port<-with(tidal.plane, tidal.plane[secondary.port %in% j, "secondary.port"])
      tide.sub<-tide[tide$standard.port %in% pri.port,]
      
      timeDiff <- with(tidal.plane[tidal.plane$secondary.port %in% sec.port,], timeDiff)
      tide.sub$tideDT <- tide.sub$tideDT+timeDiff*60
      tide.sub$secondary.port<-sec.port
      tide.sub
  }
      
  tideS.list<-lapply(np.list, GetTideSP)
  tide.s<-do.call(rbind, lapply(tideS.list, data.frame, stringsAsFactors=FALSE))
  
  
  
  #### Estimate water depth with the tide accounted for
  ### Actual water depth
  # Estimate tidal height at each location 
  tide.s$tideDT1<-tide.s$tideDT
  sdata$DateTime1<-sdata$DateTime
  
  DateTime<-NULL; increment<-NULL; reading<-NULL; rm2<-NULL; rm3<-NULL; tideDT<-NULL
  
  tidalHeight<-function(j){
      ## Make data tables
      tide.dt<-data.table::data.table(tide.s[tide.s$secondary.port %in% j,], key="tideDT1")
      sdata.dt<-data.table::data.table(sdata[sdata$nearest.port %in% j,], key="DateTime1")
      
      ## Get the differences in time between each location and tidal reading at secondary ports
      estTide<-tide.dt[sdata.dt, list(DateTime, tideDT, reading, increment), roll="nearest"]
      estTide$gap.time<-with(estTide, as.numeric(difftime(tideDT, DateTime, units = "mins")))
      
      ## Estimate tidal adjustment for each location
      sdata.dt$adj.reading<-with(estTide, reading+increment*gap.time)
      
      as.data.frame(sdata.dt)
  }
  
      
  sdataDT.list<-lapply(np.list, tidalHeight)
  sdata<-do.call(rbind, lapply(sdataDT.list, data.frame, stringsAsFactors=FALSE))
  
  
  # Estimate actual depth of each location with tide effect
  d.index<-match(sdata$nearest.port, tidal.plane$secondary.port)
  adj.datum<-tidal.plane[d.index, "datumDiff"]
  sdata$depth.exp<-with(sdata, bathy+adj.datum-adj.reading)
  
  
  if(isTRUE(filter) && type %in% "HT"){
      ### Water depth at closest high tide
      ## Organize tidal data
      #Estimate high tide at each port: (0=High, 1=others)
      peakTide<-function(j){
          tide.temp<-tide.s[tide.s$secondary.port %in% j,]
          Interval<-as.numeric(with(tide.temp, difftime(tide.temp[2, "tideDT"], tide.temp[1, "tideDT"], units="mins")))
          ncell.lookup<-120/Interval
          
          find.peakHL<-function(i) {
              if(tide.temp$reading[i]>=max(tide.temp$reading[(i+1):(i+ncell.lookup)]) & tide.temp$reading[i]>=max(tide.temp$reading[(i-1):(i-ncell.lookup)])){
                  0
              } else {
                  1
              }
          }
          
          peak<-unlist(lapply(ncell.lookup:(nrow(tide.temp)-ncell.lookup-1), find.peakHL))
          tide.temp$peak<-c(rep(NA, ncell.lookup-1), peak, rep(NA, ncell.lookup+1))
          tide.temp[tide.temp$peak == 0,]
      }
      
      tide.s<-with(tide.s, tide.s[order(secondary.port, tideDT),])
      row.names(tide.s)<-1:nrow(tide.s)
      
      peakTide.list<-lapply(np.list, peakTide)
      Htide<-do.call(rbind, lapply(peakTide.list, data.frame, stringsAsFactors=FALSE))
      Htide<-Htide[!(is.na(Htide$reading)),]
      
      
      ## Estimate height of high tide at each turtle location
      HtideHeight<-function(j){
          Htide.dt<-data.table::data.table(Htide[Htide$secondary.port %in% j,], key="tideDT1")
          sdata.dt<-data.table::data.table(sdata[sdata$nearest.port %in% j,], key="DateTime1")
          estHtide<-Htide.dt[sdata.dt, list(DateTime, tideDT, reading, increment), roll="nearest"]
          estHtide$adj.reading<-with(estHtide, reading)  
          sdata.dt$adj.reading.HT<-estHtide$adj.reading
          
          as.data.frame(sdata.dt)
      }
      
      sdataHTH.list<-lapply(np.list, HtideHeight)
      sdata<-do.call(rbind, lapply(sdataHTH.list, data.frame, stringsAsFactors=FALSE))
      
      
      ## Estimate actual depth of a location at the nearest high tide
      d.index<-match(sdata$nearest.port, tidal.plane$secondary.port)
      adj.datum<-tidal.plane[d.index, "datumDiff"]
      sdata$depth.HT<-with(sdata, bathy+adj.datum-adj.reading.HT)
      
      
      ### Remove locations according to water depth at high tide
      sdata<-with(sdata, sdata[!(qi<=qi & depth.HT>height),])
      
      
      #### Re-order data
      sdata<-with(sdata, sdata[complete.cases(lat),])
      sdata<-with(sdata, sdata[order(id, DateTime),])
      row.names(sdata)<-1:nrow(sdata)
      
      
      #### Report the summary of filtering
      FilteredSS<-nrow(sdata)
      RemovedSamplesN<-OriginalSS-FilteredSS
      RemovedSamplesP<-round((1-(FilteredSS/OriginalSS))*100,2)
      DepthEst<-nrow(sdata[!(is.na(sdata$depth.exp)),])
      
      cat("\n")
      cat("Input data:", OriginalSS, "locations.", fill = TRUE)
      cat("Filtered data:", FilteredSS, "locations.", fill = TRUE)
      cat("depthfilter removed ", RemovedSamplesN, " locations (", RemovedSamplesP, "% of original data).", sep="", fill = TRUE)
      cat("Experienced water depth (bathymetry + tide) was estiamted for", DepthEst, "of", OriginalSS, "locations.", fill = TRUE)
      cat("\n")
      
      
      #### Delete working columns and return the output
      drops<-c("nearest.port", "bathy", "standard.port", "adj.reading", "adj.reading.HT", "DateTime1")
      sdata<-sdata[,!(names(sdata) %in% drops)] 
      return(sdata)
      
      
  } else if(isTRUE(filter) && type %in% "exp") {
    #### Remove locations according to water depth
    sdata <- with(sdata, sdata[!(qi <= qi & depth.exp > height),])
    
    
    #### Re-order data
    sdata<-with(sdata, sdata[order(id, DateTime),])
    row.names(sdata)<-1:nrow(sdata)
    
    
    #### Report the summary of filtering
    FilteredSS<-nrow(sdata)
    RemovedSamplesN<-OriginalSS-FilteredSS
    RemovedSamplesP<-round((1-(FilteredSS/OriginalSS))*100,2)
    DepthEst<-nrow(sdata[!(is.na(sdata$depth.exp)),])
    
    cat("\n")
    cat("Input data:", OriginalSS, "locations.", fill = TRUE)
    cat("Filtered data:", FilteredSS, "locations.", fill = TRUE)
    cat("depthfilter removed ", RemovedSamplesN, " locations (", RemovedSamplesP, "% of original data).", sep="", fill = TRUE)
    cat("Experienced water depth (bathymetry + tide) was estiamted for", DepthEst, "of", OriginalSS, "locations.", fill = TRUE)
    cat("\n")
    
    
    #### Delete working columns and return the output
    drops<-c("nearest.port", "bathy", "standard.port", "adj.reading", "DateTime1")
    sdata<-sdata[,!(names(sdata) %in% drops)] 
    return(sdata)

  } else {
      
      #### Re-order data
      sdata<-with(sdata, sdata[order(id, DateTime),])
      row.names(sdata)<-1:nrow(sdata)
      
      
      #### Report the summary of filtering
      DepthEst<-nrow(sdata[!(is.na(sdata$depth.exp)),])

      cat("\n")
      message("No location was removed (filter option was desabled).")
      cat("Experienced water depth (bathymetry + tide) was estiamted for", DepthEst, "of", OriginalSS, "locations.", fill = TRUE)
      cat("\n")
      
      
      #### Delete working columns and return the output
      drops<-c("nearest.port", "bathy", "standard.port", "adj.reading", "DateTime1")
      sdata<-sdata[,!(names(sdata) %in% drops)] 
      return(sdata)
         
  }
}
