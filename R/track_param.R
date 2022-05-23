#' @aliases track_param
#' @title Calculate parameters between locations 
#' @description Calculate time, distance, speed, and inner angle between successive locations
#' @param sdata A data.frame or a list of data.frames containing columns with the following headers: "id", "DateTime", "lat", "lon". 
#' The function calculates each movement parameter by a unique "id" (e.g. transmitter number, identifier for each animal) 
#' if the input is a data.frame, or by each element of the list if the input is a list.  
#' "DateTime" is the GMT date & time of each location in class \code{\link[base:DateTimeClasses]{POSIXct}} 
#' or \code{\link[base]{character}} with the following format "2012-06-03 01:33:46".
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' @param param A string or vector specifying movement parameters to be calculated.
#' Options are 'time', 'distance', 'speed', 'angle', 'mean speed' and 'mean angle'.
#' See \emph{details}.
#' @param days A numeric value specifying the number of days to calculate mean speeds and angles.
#' This argument is only used when 'mean speed' and/or 'mean angle' are selected in \emph{param}.  
#' @importFrom sf st_as_sf st_distance
#' @importFrom lubridate days
#' @importFrom dplyr bind_rows
#' @importFrom geosphere bearingRhumb
#' @export
#' @details This function calculates various parameters of tracks. 
#' time (h), distance (km), speed (km/h) and inner angle (degrees) are calculated from each pair of successive locations.
#' mean speed (km/h) and angle (degrees) are calculated from locations over a specified number of days.
#' @return The input data is returned with new columns containing the requested parameters. 
#' "pTime" and "sTime" are hours from a previous and to a subsequent fix respectively. 
#' "pDist" and "sDist" are straight distances in kilometres from a previous and to a subsequent fix respectively. 
#' "pSpeed" and "sSpeed" are linear speed (km/h) from a previous and to a subsequent fix respectively. 
#' "inAng" is the degree between the bearings of lines joining successive location points.
#' "meanSpeed" and "meanAngle" are the mean speed and degree over a specified number of days.
#' @author Takahiro Shimada
#' @examples
#' #### Load turtle tracking data
#' data(turtle)
#' 
#' 
#' #### Filter temporal and/or spatial duplicates
#' turtle.dup <- dupfilter(turtle, step.time=5/60, step.dist=0.001)
#' 
#' 
#' #### ddfilter
#' turtle.dd <- ddfilter(turtle.dup, vmax=9.9, qi=4, ia=90, vmaxlp=2.0)
#' 
#' 
#' #### Mean speed over 2 days
#' mean.speed <- track_param(turtle.dd, param = c('speed', 'mean speed'), days=2)
#' 
#' 
#' #### Plot data
#' ggplot(data = mean.speed, aes(x=lon, y=lat)) +
#' geom_path(colour = 'grey') +
#' geom_point(aes(colour=meanSpeed))


track_param <- function (sdata, param = c('time', 'distance', 'speed', 'angle', 'mean speed', 'mean angle'), days = 2){
    
  #### Organize data
  if(inherits(sdata, 'data.frame')){
 
    ## Get the number of data groups
    IDs <- levels(factor(sdata$id))
    
    ## convert to list
    sdata_list <- lapply(IDs, function(j){
      sdata[sdata$id %in% j,]
    })
  } else {
    sdata_list <- sdata
  }
  
  # exclude data with insufficient locations
  nloc <- lapply(sdata_list, nrow)
  if(any(param %in% c('time', 'distance', 'speed', 'mean speed')) & any(nloc < 2)){
    exclude <- sdata_list[which(nloc < 2)]
    exclude.id <- sapply(exclude, function(x) x$id)
    warning('ignored ', paste(unique(exclude.id), collapse=", "), '\nNot enough data for calculation.')
    sdata_list <- sdata_list[-which(nloc < 2)]
  } 
  rm(nloc)
  n <- length(sdata_list)
  
  
  ## Date & time
  for(i in 1:n){
      sdata_list[[i]]$DateTime <- as.POSIXct(sdata_list[[i]]$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
      
      # Sort data in alphabetical and chronological order
      sdata_list[[i]] <- with(sdata_list[[i]], sdata_list[[i]][order(DateTime),])
  }

  
  #### Hours from a previous and to a subsequent location (pTime & sTime)
  if(any(param %in% c("time", "speed", "mean speed"))){
    for(i in 1:n){
      timeDiff <- diff(sdata_list[[i]]$DateTime)
      units(timeDiff) <- "hours"
      sdata_list[[i]]$pTime <- c(NA, as.numeric(timeDiff))
      sdata_list[[i]]$sTime <- c(as.numeric(timeDiff), NA)
    }
  }
  
  #### Distance from a previous and to a subsequent location (pDist & sDist)
  if(any(param %in% c('distance', 'speed', 'mean speed'))){
    for(i in 1:n){
      # pts <- data.matrix(sdata_list[[i]][, c('lon', 'lat')])
      # Dist <- terra::distance(pts, lonlat = TRUE, sequential = TRUE)/1000
      pts1 <- sf::st_as_sf(sdata_list[[i]][-nrow(sdata_list[[i]]),], coords = c('lon', 'lat'), crs = 4326)
      pts2 <- sf::st_as_sf(sdata_list[[i]][-1,], coords = c('lon', 'lat'), crs = 4326)
      Dist <- sf::st_distance(pts1, pts2, by_element = TRUE)
      Dist <- as.numeric(Dist)/1000
      # sdata_list[[i]]$pDist1 <- c(NA, Dist[-1])
      # sdata_list[[i]]$sDist1 <- c(Dist[-1], NA)
      sdata_list[[i]]$pDist <- c(NA, as.numeric(Dist))
      sdata_list[[i]]$sDist <- c(as.numeric(Dist), NA)
    }
  }
  
  

  ## Speed from a previous and to a subsequent location in km/h
  if(any(param %in% c('speed', 'mean speed'))){
    for(i in 1:n){
      sdata_list[[i]]$pSpeed <- with(sdata_list[[i]], pDist/pTime)
      sdata_list[[i]]$sSpeed <- with(sdata_list[[i]], sDist/sTime)
    }
  }
  

  #### Calculate inner angle in degree
  if(any(param %in% c('angle', 'mean angle'))){
    for(i in 1:n){
      if(nrow(sdata_list[[i]]) < 3){
        
        ## Exclude data with less than 3 locations
        sdata_list[[i]]$inAng <- NA
        cat('\n', unique(sdata_list[[i]]$id), 'is ignored. Not enought data for calculation.')
        
      } else {
         
        ## inner angle
        # LatLong <- data.frame(Y=sdata1$lat, X=sdata1$lon, tms=sdata1$DateTime, id=sdata1$id)
        # sp::coordinates(LatLong) <- ~X+Y
        # sp::proj4string(LatLong) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
        # tr <- trip::trip(LatLong, c("tms", "id"))
        # sdata1$inAng <- trip::trackAngle(tr)
        m <- data.matrix(sdata_list[[i]][,c("lon", "lat")])
        b <- rep(0, nrow(m)-1)
        for(j in 2:nrow(m)){
          b[j-1] <- geosphere::bearingRhumb(p1 = m[j-1,], p2 = m[j,])
        }
        
        b1 <- b[-length(b)]; b2 <- b[-1]
        sdata_list[[i]]$inAng <- c(NA, abs(180-(abs(b2-b1))), NA)
      }
    }
  }

  
  #### Mean speed and angle over n days
  if(any(c('mean speed', 'mean angle') %in% param)){
    for(i in 1:n){
      sdata.temp <- sdata_list[[i]]
      sdata.temp$cumDays <- with(sdata.temp, difftime(DateTime, DateTime[1], units = "days"))
      sdata.temp$cumDaysBack <- with(sdata.temp, difftime(DateTime[nrow(sdata.temp)], DateTime, units = "days"))
      sdata.temp2 <- with(sdata.temp, sdata.temp[cumDays >= days/2 & cumDaysBack >= days/2, ])
      
      templist <- list()
      templist$spd <- templist$ang <- rep(0, nrow(sdata.temp2))
      
      for(j in 1:nrow(sdata.temp2)){
        DT.temp <- sdata.temp2[j, "DateTime"]
        min.DT <- DT.temp - as.numeric(lubridate::days(days))/2
        max.DT <- DT.temp + as.numeric(lubridate::days(days))/2
        
        sdata.temp3 <- with(sdata.temp, sdata.temp[DateTime >= min.DT & DateTime <= max.DT,])
        time.vec <- sum(sdata.temp3[-1, "pTime"])
        dist.vec <- sum(sdata.temp3[-1, "pDist"])
        templist$spd[j] <- dist.vec/time.vec
        templist$ang[j] <- mean(sdata.temp3[c(-1, -nrow(sdata.temp3)), "inAng"], na.rm = TRUE)
      }
      
      na1 <- length(which(sdata.temp$cumDays < days/2))
      na2 <- length(which(sdata.temp$cumDaysBack < days/2))
      
      if('mean speed' %in% param){
        sdata_list[[i]]$meanSpeed <- c(rep(NA, na1), templist$spd, rep(NA, na2))
      } 
      
      if('mean angle' %in% param){
        sdata_list[[i]]$meanAngle <- c(rep(NA, na1), templist$ang, rep(NA, na2))
      }
    }
  }

 
  ## bring back excluded data
  if(exists('exclude') && length(exclude) > 0){
    sdata_list <- c(exclude, sdata_list)
  }
  
  ## Return the output
  if(inherits(sdata, 'data.frame')){
    sdata_list <- dplyr::bind_rows(sdata_list)
  }
  return(sdata_list)
}
