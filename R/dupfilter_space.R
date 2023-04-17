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
#' @param no.cores An integer specifying the number of cores used for parallel computing. 
#' Alternatively, type in 'detect' to use the maximum number of available cores minus one.
#' @importFrom sf st_as_sf st_distance
#' @importFrom dplyr bind_rows
#' @importFrom parallel makeCluster detectCores parLapply stopCluster
#' @export
#' @details This function is a partial component of \code{\link{dupfilter}}, although works as a stand-alone function.
#' First it identifies spatial duplicates by searching for consecutive fixes that were located within \emph{step.dist}.
#' For each group of spatial duplicates, the function then retains a single fix that is nearest from a previous and to a subsequent location.
#' @note A minimum of two locations per id is required.
#' @return The input data frame is returned with spatial duplicates removed.
#' The following columns are added: "pTime", "sTime", "pDist", "sDist". 
#' "pTime" and "sTime" are hours from a previous and to a subsequent fix respectively. 
#' "pDist" and "sDist" are straight distances in kilometres from a previous and to a subsequent fix respectively. 
#' @author Takahiro Shimada
#' @references Shimada T, Limpus C, Jones R, Hazel J, Groom R, Hamann M (2016) 
#' Sea turtles return home after intentional displacement from coastal foraging areas. 
#' \emph{Marine Biology} 163:1-14 \doi{10.1007/s00227-015-2771-0}
#' @seealso \code{\link{dupfilter}}, \code{\link{dupfilter_exact}}, \code{\link{dupfilter_time}}, \code{\link{dupfilter_qi}}, \code{\link{track_param}}


dupfilter_space <- function(sdata, step.time=0, step.dist=0, conditional=FALSE, no.cores = 1){
  
  ## Original sample size
  OriginalSS <- nrow(sdata)
  
  ## qi format
  if(!is.numeric(sdata$qi)){
    sdata <- within(sdata, {
      qi[qi %in% "A"] <- "-1"
      qi[qi %in% "B"] <- "-2"
      qi[qi %in% "Z"] <- "-3"
      qi <- as.numeric(as.character(qi))
    })
  }
  
  ## Date & time
  if(!any(class(sdata$DateTime) %in% "POSIXct")){
    sdata$DateTime <- with(sdata, as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
  }
  
  
  ## Get movement parameters
  if(nrow(sdata) > 1){
    sdata <- track_param(sdata, param = c('time', 'distance'))
  }
  
  
  #### Filter successive locations with the exactly same coordinates
  if(isTRUE(conditional)){
    while(any(sdata$pTime <= step.time & sdata$pDist == 0, na.rm = TRUE)){
      ## temporal duplicates
      sdata1 <- with(sdata, sdata[which((pDist == 0 | sDist == 0) & (pTime <= step.time | sTime <= step.time)),])

      ## other data
      sdata2 <- dplyr::anti_join(sdata, sdata1, by = c('id', 'DateTime', 'lat', 'lon', 'qi'))

      #### Group temporal duplicates
      if(nrow(sdata1) > 1){
        sdata1 <- track_param(sdata1, param = 'distance')
      }
      index <- 0; g <- rep(0, nrow(sdata1))
      for(i in 1:nrow(sdata1)){
        if(any(is.na(sdata1[i, 'pDist']) | sdata1[i, 'sDist'] == 0, na.rm = TRUE)){
          index <- index + 1
        } 
        g[i] <- index
      }
      sdata1$group <- g
      rm(g, index)
      
      
      #### Filter successive locations with exactly same coordinates
      sdata1 <- dplyr::distinct(sdata1, .data$id, .data$lat, .data$lon, .data$group, .keep_all = TRUE)
      
      #### Combine
      sdata <- dplyr::bind_rows(sdata1, sdata2)
      rm(sdata1, sdata2)
      
      #### Recalculate movement parameters
      if(nrow(sdata) > 1){
        sdata <- track_param(sdata, param = c('time', 'distance'))
      }
    }
  } else {
    while(any(sdata$pDist == 0, na.rm = TRUE)){
      ## temporal duplicates
      sdata1 <- with(sdata, sdata[which(pDist == 0 | sDist == 0),])
      
      ## other data
      sdata2 <- dplyr::anti_join(sdata, sdata1, by = c('id', 'DateTime', 'lat', 'lon', 'qi'))
      
      #### Group temporal duplicates
      if(nrow(sdata1) > 1){
        sdata1 <- track_param(sdata1, param = 'distance')
      }
      index <- 0; g <- rep(0, nrow(sdata1))
      for(i in 1:nrow(sdata1)){
        if(any(is.na(sdata1[i, 'pDist']) | sdata1[i, 'sDist'] == 0, na.rm = TRUE)){
          index <- index + 1
        } 
        g[i] <- index
      }
      sdata1$group <- g
      rm(g, index)
      
      
      #### Filter successive locations with exactly same coordinates
      sdata1 <- dplyr::distinct(sdata1, .data$id, .data$lat, .data$lon, .data$group, .keep_all = TRUE)
      
      #### Combine
      sdata <- dplyr::bind_rows(sdata1, sdata2)
      rm(sdata1, sdata2)
      
      #### Recalculate movement parameters
      if(nrow(sdata) > 1){
        sdata <- track_param(sdata, param = c('time', 'distance'))
      }
    }
  }
  
  
  #### Function to filter spatial duplicates of different coordinates
  dup.location <- function(sdata=sdata, step.time=step.time, step.dist=step.dist, conditional=conditional, no.cores = no.cores){

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
    if(nrow(sdata1) > 1){
      sdata1 <- track_param(sdata1, param = 'distance')
    }
    index <- 0; g <- rep(0, nrow(sdata1))
    for(i in 1:nrow(sdata1)){
      if(any(is.na(sdata1[i, 'pDist']) | (sdata1[i, 'sDist'] <= step.dist), na.rm = TRUE)){
        index <- index + 1
      } 
      g[i] <- index
    }
    sdata1$group <- g
    rm(g, index)
    
    
    ## group with more than 1 locations
    nloc <- aggregate(lat ~ group, data = sdata1, FUN = length)
    nloc_gp <- unique(nloc[nloc$lat>1, 'group'])
    sdata3 <- with(sdata1, sdata1[!group %in% nloc_gp,])
    sdata1 <- with(sdata1, sdata1[group %in% nloc_gp,])
    nloc_gp1 <- unique(sdata1$group)
    rm(nloc, nloc_gp)
   
    #### Find the location which is the closest to the previous and/or successive locations
    ## function to plug in
    select_rows <- function(i){
      dup_temp <- with(sdata1, sdata1[group %in% i,])
      minDT <- min(dup_temp$DateTime)
      maxDT <- max(dup_temp$DateTime)
      dup_id <- unique(dup_temp$id)
      # dup_xy <- data.matrix(dup_temp[,c('lon', 'lat')])
      # dup_xy <- st_as_sf(dup_temp, coords = c('lon', 'lat'), crs = 4326)
      dup_xy <- dup_temp[, c('lon', 'lat')]
      dup_xy <- sf::st_as_sf(dup_xy, coords = 1:2, crs = 4326)
      
      ## locations immediately before
      loc.before <- with(sdata, sdata[id %in% dup_id & DateTime < minDT,])
      if(nrow(loc.before) > 0){
        maxDT_before <- max(loc.before$DateTime)
        # loc.before <- loc.before[loc.before$DateTime >= maxDT_before,]
        # loc.before_xy <- data.matrix(loc.before[,c('lon', 'lat')])
        loc.before <- loc.before[loc.before$DateTime >= maxDT_before, c('lon', 'lat')]
        loc.before_xy <- sf::st_as_sf(loc.before, coords = 1:2, crs = 4326)
      }
      
      ## locations immediately after
      loc.after <- with(sdata, sdata[id %in% dup_id & DateTime > maxDT,])
      if(nrow(loc.after) > 0){
        minDT_after <- min(loc.after$DateTime)
        loc.after <- loc.after[loc.after$DateTime <= minDT_after,]
        # loc.after_xy <- data.matrix(loc.after[,c('lon', 'lat')])
        # loc.after_xy <-st_as_sf(loc.after, coords = c('lon', 'lat'), crs = 4326)
        loc.after <- loc.after[loc.after$DateTime <= minDT_after, c('lon', 'lat')]
        loc.after_xy <- sf::st_as_sf(loc.after, coords = 1:2, crs = 4326)
      }
      
      
      #### Calculate distances
      if(nrow(loc.before) > 0 & nrow(loc.after) > 0){
        # dist.before <- raster::pointDistance(dup_temp[,c('lon', 'lat')], loc.before[,c('lon', 'lat')], lonlat = TRUE, allpairs = TRUE)
        # dist.after <- raster::pointDistance(dup_temp[,c('lon', 'lat')], loc.after[,c('lon', 'lat')], lonlat = TRUE, allpairs = TRUE)
        dist.before <- sf::st_distance(dup_xy, loc.before_xy)
        dist.after <- sf::st_distance(dup_xy, loc.after_xy)
        # dist.before <- terra::distance(dup_xy, loc.before_xy, lonlat = TRUE)
        # dist.after <- terra::distance(dup_xy, loc.after_xy, lonlat = TRUE)
        dist.all <- cbind(dist.before, dist.after)
        dist.sum <- rowSums(dist.all)
        dist.min <- which.min(dist.sum)[1]
      } else if(nrow(loc.before) > 0){
        # dist.before <- raster::pointDistance(dup_temp[,c('lon', 'lat')], loc.before[,c('lon', 'lat')], lonlat = TRUE, allpairs = TRUE)
        # dist.before <- terra::distance(dup_xy, loc.before_xy, lonlat = TRUE)
        dist.before <- sf::st_distance(dup_xy, loc.before_xy)
        dist.sum <- rowSums(as.matrix(dist.before))
        dist.min <- which.min(dist.sum)[1]
      } else if(nrow(loc.after) > 0){
        # dist.after <- raster::pointDistance(dup_temp[,c('lon', 'lat')], loc.after[,c('lon', 'lat')], lonlat = TRUE, allpairs = TRUE)
        # dist.after <- terra::distance(dup_xy, loc.after_xy, lonlat = TRUE)
        dist.after <- sf::st_distance(dup_xy, loc.after_xy)
        dist.sum <- rowSums(as.matrix(dist.after))
        dist.min <- which.min(dist.sum)[1]
      } else {
        dist.min <- 1
      }
      
      #### Return the location which is the closest to the previous and successive locations
      return(dup_temp[dist.min,])
    }
    
    
    ## Run the function
    if(no.cores > 1){
      # using multiple CPU cores
      d <- parallel::parLapply(cl, X = nloc_gp1, fun = select_rows)
    } else {
      # using a single CPU
      d <- lapply(nloc_gp1, select_rows)
    }
    sdata1 <- dplyr::bind_rows(d)
    rm(d)

    
    #### Combine
    sdata <- dplyr::bind_rows(sdata1, sdata2, sdata3)
    rm(sdata1, sdata2, sdata3)
    
    ## Re-calculate
    if(nrow(sdata) > 1){
      sdata <- track_param(sdata, param = c('time', 'distance'))
    }
    
    #### Return
    return(sdata)
  }
  
  
  #### Run the function until no locations can be removed by this filter
  ## set parameters for parallel processing
  if(no.cores > 1 | no.cores %in% 'detect'){
    
    if(no.cores %in% 'detect'){
      no.cores <- parallel::detectCores() - 1
    }
    
    if(.Platform$OS.type %in% 'windows'){
      cl <- parallel::makeCluster(no.cores, type="PSOCK")
    } else {
      cl <- parallel::makeCluster(no.cores, type="FORK")
    }
  }
  
  
  ## run the function
  if(isTRUE(conditional)){
    if(any(sdata$pTime <= step.time & sdata$pDist <= step.dist, na.rm = TRUE)){
      sdata <- dup.location(sdata=sdata, step.time=step.time, step.dist=step.dist, conditional=conditional, no.cores = no.cores)    
      while(any(sdata$pTime <= step.time & sdata$pDist <= step.dist, na.rm = TRUE)){
        sdata <- dup.location(sdata=sdata, step.time=step.time, step.dist=step.dist, conditional=conditional, no.cores = no.cores)
      }
    }
  } else {
    if(any(sdata$pDist <= step.dist, na.rm = TRUE)){
     sdata <- dup.location(sdata=sdata, step.time=step.time, step.dist=step.dist, conditional=conditional, no.cores = no.cores)    
      while(any(sdata$pDist <= step.dist, na.rm = TRUE)){
        sdata <- dup.location(sdata=sdata, step.time=step.time, step.dist=step.dist, conditional=conditional, no.cores = no.cores)
      }
    }
  }
  
  ## stop parallel
  if(no.cores > 1){
    parallel::stopCluster(cl)
  }
  
 
  ## Filtered data
  FilteredSS <- nrow(sdata)
  RemovedSamplesN <- OriginalSS - FilteredSS
  
  
  ## Print report
  cat("dupfilter_space removed", RemovedSamplesN, "of", OriginalSS, "locations", fill = TRUE)
  
  
  #### Delete working columns and return the output
  drop.vars <- c("pSpeed", "sSpeed", "inAng", "meanSpeed", "meanAngle", 'group')
  sdata <- sdata[,!(names(sdata) %in% drop.vars)] 
  return(sdata)
}
