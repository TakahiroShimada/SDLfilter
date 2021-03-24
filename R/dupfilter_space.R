#' @aliases dupfilter_space
#' @title Filter spatial duplicates
#' @description Function to filter spatial duplicates in tracking data.
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' See the data \code{\link{turtle}} for an example.
#' The function filters the input data by a unique "id" (e.g. transmitter number, identifier for each animal). 
#' "DateTime" is the GMT date & time of each location in class \code{\link[base:DateTimeClasses]{POSIXct}} or \code{\link[base]{character}} with the following format "2012-06-03 01:33:46".
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' "qi" is the quality index associated with each location fix. 
#' The input values can be either the number of GPS satellites or Argos Location Classes. 
#' Argos Location Classes will be converted to numerical values, where "A", "B", "Z" will be replaced with "-1", "-2", "-3" respectively.
#' The greater number indicates a higher accuracy. 
#' @param step.time Consecutive locations less than or equal to \emph{step.time} apart are considered temporal duplicates.
#' Default is 0 hours.
#' @param step.dist Consecutive locations less than or equal to \emph{step.dist} apart are considered spatial duplicates.
#' Default is 0 kilometres. 
#' @param conditional If TRUE, spatial duplicates are filtered only if they are less than or equal to \emph{step.time} apart. 
#' Default is FALSE.
#' @import sp
#' @importFrom raster pointDistance
#' @importFrom plyr rbind.fill
#' @export
#' @details This function is a partial component of \code{\link{dupfilter}}, although works as a stand-alone function.
#' First it identifies spatial duplicates by searching for consecutive fixes that were located within \emph{step.dist}.
#' For each group of spatial duplicates, the function then retains a single fix that is nearest from a previous and to a subsequent location.
#' @note A minimum of two locations per id is required.
#' @return The input data frame is returned with spatial duplicates removed. 
#' @author Takahiro Shimada
#' @references Shimada T, Limpus C, Jones R, Hazel J, Groom R, Hamann M (2016) 
#' Sea turtles return home after intentional displacement from coastal foraging areas. 
#' \emph{Marine Biology} 163:1-14 \doi{10.1007/s00227-015-2771-0}
#' @seealso \code{\link{dupfilter}}, \code{\link{dupfilter_exact}}, \code{\link{dupfilter_time}}, \code{\link{dupfilter_qi}}, \code{\link{track_param}}


dupfilter_space <- function(sdata, step.time=0, step.dist=0, conditional=FALSE){
  
  ## Original columns
  headers <- names(sdata)
  
  ## Original sample size
  OriginalSS <- nrow(sdata)
  
  ## qi format
  sdata <- within(sdata, {
    qi[qi %in% "A"] <- "-1"
    qi[qi %in% "B"] <- "-2"
    qi[qi %in% "Z"] <- "-3"
    qi <- as.numeric(as.character(qi))
  })
  
  ## Date & time
  sdata$DateTime <- with(sdata, as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
  
  
  ## Get movement parameters
  sdata <- track_param(sdata, param = c('time', 'distance'))
  
  
  #### Filter successive locations with the exactly same coordinates
  if(isTRUE(conditional)){
    while(any(sdata$pTime <= step.time & sdata$pDist == 0, na.rm = TRUE)){
      ## temporal duplicates
      sdata1 <- with(sdata, sdata[which((pDist == 0 | sDist == 0) & (pTime <= step.time | sTime <= step.time)),])

      ## other data
      sdata2 <- dplyr::anti_join(sdata, sdata1, by = c('id', 'DateTime', 'lat', 'lon', 'qi'))

      #### Group temporal duplicates
      sdata1 <- track_param(sdata1, param = 'distance')
      index <- 0
      for(i in 1:nrow(sdata1)){
        if(any(is.na(sdata1[i, 'pDist']) | sdata1[i, 'sDist'] == 0, na.rm = TRUE)){
          index <- index + 1
          sdata1[i, 'group'] <- index
        } else {
          sdata1[i, 'group'] <- index
        }
      }
      
      #### Filter successive locations with exactly same coordinates
      sdata1 <- dplyr::distinct(sdata1, .data$id, .data$lat, .data$lon, .data$group, .keep_all = TRUE)
      
      #### Combine
      sdata <- plyr::rbind.fill(sdata1, sdata2)
      
      #### Recalculate movement parameters
      sdata <- track_param(sdata, param = c('time', 'distance'))
    }
  } else {
    while(any(sdata$pDist == 0, na.rm = TRUE)){
      ## temporal duplicates
      sdata1 <- with(sdata, sdata[which(pDist == 0 | sDist == 0),])
      
      ## other data
      sdata2 <- dplyr::anti_join(sdata, sdata1, by = c('id', 'DateTime', 'lat', 'lon', 'qi'))
      
      #### Group temporal duplicates
      sdata1 <- track_param(sdata1, param = 'distance')
      index <- 0
      for(i in 1:nrow(sdata1)){
        if(any(is.na(sdata1[i, 'pDist']) | sdata1[i, 'sDist'] == 0, na.rm = TRUE)){
          index <- index + 1
          sdata1[i, 'group'] <- index
        } else {
          sdata1[i, 'group'] <- index
        }
      }
      
      #### Filter successive locations with exactly same coordinates
      sdata1 <- dplyr::distinct(sdata1, .data$id, .data$lat, .data$lon, .data$group, .keep_all = TRUE)
      
      #### Combine
      sdata <- plyr::rbind.fill(sdata1, sdata2)
      
      #### Recalculate movement parameters
      sdata <- track_param(sdata, param = c('time', 'distance'))
    }
  }
  
  
  #### Function to filter spatial duplicates of different coordinates
  dup.location <- function(sdata=sdata, step.time=step.time, step.dist=step.dist, conditional=conditional){

    #### Subset data 
    ## temporal duplicates
    if(isTRUE(conditional)){
      sdata1 <- with(sdata, sdata[which((pDist <= step.dist | sDist <= step.dist) & (pTime <= step.time | sTime <= step.time)),])
    } else {
      sdata1 <- with(sdata, sdata[which(pDist <= step.dist | sDist <= step.dist),])
    }
    
    ## other data
    sdata2 <- dplyr::anti_join(sdata, sdata1, by = c('id', 'DateTime', 'lat', 'lon', 'qi'))
    
    
    #### Group temporal duplicates
    sdata1 <- track_param(sdata1, param = 'distance')
    index <- 0
    for(i in 1:nrow(sdata1)){
      if(any(is.na(sdata1[i, 'pDist']) | (sdata1[i, 'sDist'] <= step.dist), na.rm = TRUE)){
        index <- index + 1
        sdata1[i, 'group'] <- index
      } else {
        sdata1[i, 'group'] <- index
      }
    }
    
    ## group with more than 1 locations
    nloc <- aggregate(lat ~ group, data = sdata1, FUN = length)
    nloc_gp <- unique(nloc[nloc$lat>1, 'group'])
    sdata3 <- with(sdata1, sdata1[!group %in% nloc_gp,])
    sdata1 <- with(sdata1, sdata1[group %in% nloc_gp,])
    
   
    #### Find the location which is the closest to the previous and/or successive locations
    sdata1 <- plyr::rbind.fill(lapply(nloc_gp, function(i){
      dup_temp <- with(sdata1, sdata1[group %in% i,])
      minDT <- min(dup_temp$DateTime)
      maxDT <- max(dup_temp$DateTime)
      dup_id <- unique(dup_temp$id)
      
      ## locations immediately before
      loc.before <- with(sdata, sdata[id %in% dup_id & DateTime < minDT,])
      if(nrow(loc.before) > 0){
        maxDT_before <- max(loc.before$DateTime)
        loc.before <- loc.before[loc.before$DateTime >= maxDT_before,]
      }
      
      ## locations immediately after
      loc.after <- with(sdata, sdata[id %in% dup_id & DateTime > maxDT,])
      if(nrow(loc.after) > 0){
        minDT_after <- min(loc.after$DateTime)
        loc.after <- loc.after[loc.after$DateTime <= minDT_after,]
      }
      
      
      #### Calculate distances
      if(nrow(loc.before) > 0 & nrow(loc.after) > 0){
        dist.before <- raster::pointDistance(dup_temp[,c('lon', 'lat')], loc.before[,c('lon', 'lat')], lonlat = TRUE, allpairs = TRUE)
        dist.after <- raster::pointDistance(dup_temp[,c('lon', 'lat')], loc.after[,c('lon', 'lat')], lonlat = TRUE, allpairs = TRUE)
        dist.all <- cbind(dist.before, dist.after)
        dist.sum <- rowSums(dist.all)
        dist.min <- which.min(dist.sum)[1]
      } else if(nrow(loc.before) > 0){
        dist.before <- raster::pointDistance(dup_temp[,c('lon', 'lat')], loc.before[,c('lon', 'lat')], lonlat = TRUE, allpairs = TRUE)
        dist.sum <- rowSums(as.matrix(dist.before))
        dist.min <- which.min(dist.sum)[1]
      } else if(nrow(loc.after) > 0){
        dist.after <- raster::pointDistance(dup_temp[,c('lon', 'lat')], loc.after[,c('lon', 'lat')], lonlat = TRUE, allpairs = TRUE)
        dist.sum <- rowSums(as.matrix(dist.after))
        dist.min <- which.min(dist.sum)[1]
      } else {
        dist.min <- 1
      }
      
      #### Return the location which is the closest to the previous and successive locations
      return(dup_temp[dist.min,])
    }))
    
    
    #### Combine
    sdata <- plyr::rbind.fill(sdata1, sdata2, sdata3)
    sdata$group <- NULL
    
    ## Re-calculate
    sdata <- track_param(sdata, param = c('time', 'distance'))
    
    #### Return
    return(sdata)
  }
  
  
  #### Run the function until no locations can be removed by this filter
  if(isTRUE(conditional)){
    if(any(sdata$pTime <= step.time & sdata$pDist <= step.dist, na.rm = TRUE)){
      sdata <- dup.location(sdata=sdata, step.time=step.time, step.dist=step.dist, conditional=conditional)    
      while(any(sdata$pTime <= step.time & sdata$pDist <= step.dist, na.rm = TRUE)){
        sdata <- dup.location(sdata=sdata, step.time=step.time, step.dist=step.dist, conditional=conditional)
      }
    }
  } else {
    if(any(sdata$pDist <= step.dist, na.rm = TRUE)){
      sdata <- dup.location(sdata=sdata, step.time=step.time, step.dist=step.dist, conditional=conditional)    
      while(any(sdata$pDist <= step.dist, na.rm = TRUE)){
        sdata <- dup.location(sdata=sdata, step.time=step.time, step.dist=step.dist, conditional=conditional)
      }
    }
  }

 
  ## Filtered data
  FilteredSS<-nrow(sdata)
  RemovedSamplesN<-OriginalSS-FilteredSS
  
  
  ## Print report
  cat("dupfilter_space removed", RemovedSamplesN, "of", OriginalSS, "locations.", fill = TRUE)
  
  
  #### Delete working columns and return the output
  sdata<-sdata[,headers] 
  return(sdata)
}
