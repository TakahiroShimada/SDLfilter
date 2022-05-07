#' @aliases track_param
#' @title Calculate parameters between locations 
#' @description Calculate time, distance, speed, and inner angle between successive locations
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon". 
#' The function calculates each movement parameter by a unique "id" (e.g. transmitter number, identifier for each animal). 
#' "DateTime" is the GMT date & time of each location in class \code{\link[base:DateTimeClasses]{POSIXct}} 
#' or \code{\link[base]{character}} with the following format "2012-06-03 01:33:46".
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' @param param A string or vector specifying movement parameters to be calculated.
#' Options are 'time', 'distance', 'speed', 'angle', 'mean speed' and 'mean angle'.
#' See \emph{details}.
#' @param days A numeric value specifying the number of days to calculate mean speeds and angles.
#' This argument is only used when 'mean speed' and/or 'mean angle' are selected in \emph{param}.  
#' @importFrom terra distance
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
  ## Date & time
  sdata$DateTime <- with(sdata, as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
  
  ## Sort data in alphabetical and chronological order
  sdata <- with(sdata, sdata[order(id, DateTime),])
  row.names(sdata) <- 1:nrow(sdata)
  
  
  #### Get Id of each animal
  IDs <- levels(factor(sdata$id))
  
  
  #### Hours from a previous and to a subsequent location (pTime & sTime)
  if(any(param %in% c("time", "speed", "mean speed"))){
    sdata <- dplyr::bind_rows(lapply(IDs, function(j){
      sdata.temp <- sdata[sdata$id %in% j,]
      timeDiff <- diff(sdata.temp$DateTime)
      units(timeDiff) <- "hours"
      sdata.temp$pTime <- c(NA, as.numeric(timeDiff))
      sdata.temp$sTime <- c(as.numeric(timeDiff), NA)
      return(sdata.temp)
    })) 
  }
  
  
  #### Distance from a previous and to a subsequent location (pDist & sDist)
  if(any(param %in% c('distance', 'speed', 'mean speed'))){
    sdata <- dplyr::bind_rows(lapply(IDs, function(j){
      sdata.temp <- sdata[sdata$id %in% j,]
      # LatLong <- sf::st_as_sf(sdata.temp, coords = c("lon", "lat"))
      # LatLong <- sf::st_set_crs(LatLong, 4326)
      # Dist <- raster::pointDistance(LatLong[-nrow(LatLong),], LatLong[-1,], lonlat=T)/1000
      pts <- data.matrix(sdata.temp[, c('lon', 'lat')])
      Dist <- terra::distance(pts, lonlat = TRUE, sequential = TRUE)/1000
      Dist <- Dist[-1]
      sdata.temp$pDist <- c(NA, Dist)
      sdata.temp$sDist <- c(Dist, NA)
      return(sdata.temp)
    }))
  }
    
  
  ## Speed from a previous and to a subsequent location in km/h
  if(any(param %in% c('speed', 'mean speed'))){
    sdata$pSpeed <- sdata$pDist/sdata$pTime
    sdata$sSpeed <- sdata$sDist/sdata$sTime
  }
  
  
  #### Calculate inner angle in degree
  if(any(param %in% c('angle', 'mean angle'))){
    ## Locations less than 3
    nloc <- aggregate(lat ~ id, data = sdata, FUN = length)
    exclude <- nloc[nloc$lat<3,'id']
    sdata1 <- with(sdata, sdata[!id %in% exclude,])
    
    ## others
    sdata2 <- with(sdata, sdata[id %in% exclude,])
    
    ## inner angle
    # LatLong <- data.frame(Y=sdata1$lat, X=sdata1$lon, tms=sdata1$DateTime, id=sdata1$id)
    # sp::coordinates(LatLong) <- ~X+Y
    # sp::proj4string(LatLong) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
    # tr <- trip::trip(LatLong, c("tms", "id"))
    # sdata1$inAng <- trip::trackAngle(tr)
    m <- data.matrix(sdata1[,c("lon", "lat")])
    b <- rep(0, nrow(m)-1)
    for(i in 2:nrow(m)){
      b[i-1] <- geosphere::bearingRhumb(p1 = m[i-1,], p2 = m[i,])
    }
    
    b1 <- b[-length(b)]; b2 <- b[-1]
    inAng <- abs(180-(abs(b2-b1)))
    sdata1$inAng <- c(NA, inAng, NA)

    ## Bring back excluded data
    sdata <- dplyr::bind_rows(sdata1, sdata2)
    sdata <- with(sdata, sdata[order(id, DateTime),])
    row.names(sdata) <- 1:nrow(sdata)
  }
  
  
  #### Mean speed and angle over n days
  if(any(c('mean speed', 'mean angle') %in% param)){
    mean_spd_ang <- lapply(IDs, function(j){
      sdata.temp <- with(sdata, sdata[id %in% j,])
      sdata.temp$cumDays <- with(sdata.temp, difftime(DateTime, DateTime[1], units = "days"))
      sdata.temp$cumDaysBack <- with(sdata.temp, difftime(DateTime[nrow(sdata.temp)], DateTime, units = "days"))
      sdata.temp2 <- with(sdata.temp, sdata.temp[cumDays >= days/2 & cumDaysBack >= days/2, ])
      
      spd.ang <- lapply(1:nrow(sdata.temp2), function(z){
        DT.temp <- sdata.temp2[z, "DateTime"]
        min.DT <- DT.temp - as.numeric(lubridate::days(days))/2
        max.DT <- DT.temp + as.numeric(lubridate::days(days))/2
        
        sdata.temp3 <- with(sdata.temp, sdata.temp[DateTime >= min.DT & DateTime <= max.DT,])
        time.vec <- sum(sdata.temp3[-1, "pTime"])
        dist.vec <- sum(sdata.temp3[-1, "pDist"])
        spd <- dist.vec/time.vec
        ang <- mean(sdata.temp3[c(-1, -nrow(sdata.temp3)), "inAng"], na.rm = TRUE)
        # ang.vec <- sum(sdata.temp3[c(-1, -nrow(sdata.temp3)), "inAng"])
        # ang <- ang.vec/(nrow(sdata.temp3)-2)
        # spd <- mean(sdata.temp3[-1, "pSpeed"], na.rm = TRUE)
        # ang <- mean(sdata.temp3[c(-1, -nrow(sdata.temp3)), "inAng"], na.rm = TRUE)
        return(data.frame(spd, ang))
      })
      
      spd.ang.df <- dplyr::bind_rows(spd.ang)
      
      na1 <- nrow(sdata.temp[sdata.temp$cumDays < days/2,])
      na2 <- nrow(sdata.temp[sdata.temp$cumDaysBack < days/2,])
      
      if('mean speed' %in% param){
        sdata.temp$meanSpeed <- c(rep(NA, na1), spd.ang.df$spd, rep(NA, na2))
      } 
      
      if('mean angle' %in% param){
        sdata.temp$meanAngle <- c(rep(NA, na1), spd.ang.df$ang, rep(NA, na2))
      }

      return(sdata.temp)
    })
    
    sdata <- dplyr::bind_rows(mean_spd_ang)
  
  } 


  ## Delete working columns and return the output
  drops <- c('cumDays', 'cumDaysBack')
  sdata <- sdata[,!(names(sdata) %in% drops)] 
  return(sdata)
}
