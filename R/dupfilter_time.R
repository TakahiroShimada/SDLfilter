#' @aliases dupfilter_time
#' @title Filter temporal duplicates
#' @description Function to filter temporal duplicates that are associated with the same quality index.
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' See the data \code{\link{turtle}} for an example.
#' The function filters the input data by a unique "id" (e.g. transmitter number, identifier for each animal). 
#' "DateTime" is the GMT date & time of each location in class \code{\link[base:DateTimeClasses]{POSIXct}} 
#' or \code{\link[base]{character}} with the following format "2012-06-03 01:33:46".
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' "qi" is the quality index associated with each location fix. 
#' The input values can be either the number of GPS satellites or Argos Location Classes. 
#' Argos Location Classes will be converted to numerical values, where "A", "B", "Z" will be replaced with "-1", "-2", "-3" respectively.
#' The greater number indicates a higher accuracy. 
#' @param step.time Consecutive locations less than or equal to \emph{step.time} apart are considered temporal duplicates.
#' Default is 0 hours.
#' @param no.cores An integer specifying the number of cores used for parallel computing. 
#' Default ('detect') uses the maximum number of available cores minus one.
#' @importFrom terra distance
#' @importFrom dplyr anti_join bind_rows
#' @importFrom parallel makeCluster detectCores parLapply stopCluster
#' @export
#' @details This is a partial component of \code{\link{dupfilter}}, although works as a stand-alone function.
#' First it identifies temporal duplicates by searching for consecutive locations that were obtained within \emph{step.time}.
#' For each group of temporal duplicates, the function then retains a single fix that is nearest from a previous and to a subsequent location.
#' @return The input data frame is returned with temporal duplicates removed.
#' The following columns are added: "pTime", "sTime", "pDist", "sDist". 
#' "pTime" and "sTime" are hours from a previous and to a subsequent fix respectively. 
#' "pDist" and "sDist" are straight distances in kilometres from a previous and to a subsequent fix respectively. 
#' @author Takahiro Shimada
#' @references Shimada T, Limpus C, Jones R, Hazel J, Groom R, Hamann M (2016) 
#' Sea turtles return home after intentional displacement from coastal foraging areas. 
#' \emph{Marine Biology} 163:1-14 \doi{10.1007/s00227-015-2771-0}
#' @seealso \code{\link{dupfilter}}, \code{\link{dupfilter_exact}}, \code{\link{dupfilter_qi}}, \code{\link{dupfilter_space}}, \code{\link{track_param}}



dupfilter_time <- function (sdata, step.time = 0, no.cores = 'detect') {
  
  ## Original columns
  # headers <- names(sdata)
  
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
  
  
  #### Get step time
  sdata <- track_param(sdata, param = c('time', 'distance'))
  
  
  #### Filter successive locations with the exactly same coordinates
  while(any(sdata$pTime <= step.time & sdata$pDist == 0, na.rm = TRUE)){
      ## temporal duplicates
      sdata1 <- with(sdata, sdata[which(sdata$pTime <= step.time & pDist == 0 | sdata$sTime <= step.time & sDist == 0),])

      ## other data
      sdata2 <- dplyr::anti_join(sdata, sdata1, by = c('id', 'DateTime', 'lat', 'lon', 'qi'))

      #### Group temporal duplicates
      sdata1 <- track_param(sdata1, param = 'distance')
      index <- 0; g <- rep(0, nrow(sdata1))
      for(i in 1:nrow(sdata1)){
        if(any(is.na(sdata1[i, 'pDist']) | sdata1[i, 'sDist'] == 0, na.rm = TRUE)){
          index <- index + 1
          g[i] <- index
        } 
        g[i] <- index
      }
      sdata1$group <- g
      

      #### Filter successive locations with exactly same coordinates
      sdata1 <- dplyr::distinct(sdata1, .data$id, .data$lat, .data$lon, .data$group, .keep_all = TRUE)

      #### Combine
      sdata <- dplyr::bind_rows(sdata1, sdata2)

      #### Recalculate movement parameters
      sdata <- track_param(sdata, param = c('time', 'distance'))
  }


  #### Function to filter temporal duplicates with different coordinates
  dup.timing <- function(sdata = sdata, step.time = step.time, no.cores = no.cores) {

    #### Subset data 
    ## temporal duplicates
    sdata1 <- with(sdata, sdata[which(pTime <= step.time | sTime <= step.time),])
    
    ## other data
    sdata2 <- dplyr::anti_join(sdata, sdata1, by = c('id', 'DateTime', 'lat', 'lon', 'qi'))
    
    
    #### Group temporal duplicates
    sdata1 <- track_param(sdata1, param = 'time')
    index <- 0; g <- rep(0, nrow(sdata1))
    for(i in 1:nrow(sdata1)){
      if(any(is.na(sdata1[i, 'pTime']) | (sdata1[i, 'sTime'] <= step.time), na.rm = TRUE)){
        index <- index + 1
        g[i] <- index
      } 
      g[i] <- index
    }
    sdata1$group <- g

    
    ## group with more than 1 locations
    nloc <- aggregate(lat ~ group, data = sdata1, FUN = length)
    nloc_gp <- unique(nloc[nloc$lat>1, 'group'])
    sdata3 <- with(sdata1, sdata1[!group %in% nloc_gp,])
    sdata1 <- with(sdata1, sdata1[group %in% nloc_gp,])
    
   
    #### Find the location which is the closest to the previous and/or successive locations
    # system.time({
    #   
    # sdata_list <- dplyr::bind_rows(lapply(nloc_gp, function(i){
    #   dup_temp <- with(sdata1, sdata1[group == i,])
    #   dup_id <- unique(dup_temp$id)
    # 
    #   minDT <- min(dup_temp$DateTime)
    #   maxDT <- max(dup_temp$DateTime)
    #   dup_xy <- data.matrix(dup_temp[,c('lon', 'lat')])
    # 
    #   ## locations immediately before
    #   loc.before <- with(sdata, sdata[id %in% dup_id & DateTime < minDT,])
    #   if(nrow(loc.before) > 0){
    #     maxDT_before <- max(loc.before$DateTime) - step.time*3600
    #     loc.before <- loc.before[loc.before$DateTime >= maxDT_before,]
    #     loc.before_xy <- data.matrix(loc.before[,c('lon', 'lat')])
    #   }
    #   
    #   ## locations immediately after
    #   loc.after <- with(sdata, sdata[id %in% dup_id & DateTime > maxDT,])
    #   if(nrow(loc.after) > 0){
    #     minDT_after <- min(loc.after$DateTime) + step.time*3600
    #     loc.after <- loc.after[loc.after$DateTime <= minDT_after,]
    #     loc.after_xy <- data.matrix(loc.after[,c('lon', 'lat')])
    #   }
    # 
    #   #### Calculate distances
    #   if(nrow(loc.before) > 0 & nrow(loc.after) > 0){
    #     dist.before <- terra::distance(dup_xy, loc.before_xy, lonlat = TRUE)
    #     dist.after <- terra::distance(dup_xy, loc.after_xy, lonlat = TRUE)
    #     dist.all <- cbind(dist.before, dist.after)
    #     dist.sum <- rowSums(dist.all)
    #     dist.min <- which.min(dist.sum)[1]
    #   } else if(nrow(loc.before) > 0){
    #     # dist.before <- raster::pointDistance(dup_temp[,c('lon', 'lat')], loc.before[,c('lon', 'lat')], lonlat = TRUE, allpairs = TRUE)
    #     dist.before <- terra::distance(dup_xy, loc.before_xy, lonlat = TRUE)
    #     dist.sum <- rowSums(as.matrix(dist.before))
    #     dist.min <- which.min(dist.sum)[1]
    #   } else if(nrow(loc.after) > 0){
    #     # dist.after <- raster::pointDistance(dup_temp[,c('lon', 'lat')], loc.after[,c('lon', 'lat')], lonlat = TRUE, allpairs = TRUE)
    #     dist.after <- terra::distance(dup_xy, loc.after_xy, lonlat = TRUE)
    #     dist.sum <- rowSums(as.matrix(dist.after))
    #     dist.min <- which.min(dist.sum)[1]
    #   } else {
    #     dist.min <- 1
    #   }
    #   
    #   #### Return the location which is the closest to the previous and successive locations
    #   dup_temp[dist.min,]
    # }))
    # })
    
    ## function to plug in
    select_rows <- function(i){
      dup_temp <- with(sdata1, sdata1[group == i,])
      dup_id <- unique(dup_temp$id)
      minDT <- min(dup_temp$DateTime)
      maxDT <- max(dup_temp$DateTime)
      dup_xy <- data.matrix(dup_temp[,c('lon', 'lat')])
      # dup_xy <- st_as_sf(dup_temp, coords = c('lon', 'lat'), crs = 4326)
      
      ## locations immediately before
      loc.before <- with(sdata, sdata[id %in% dup_id & DateTime < minDT,])
      if(nrow(loc.before) > 0){
        maxDT_before <- max(loc.before$DateTime) - step.time*3600
        loc.before <- loc.before[loc.before$DateTime >= maxDT_before,]
        loc.before_xy <- data.matrix(loc.before[,c('lon', 'lat')])
        # loc.before_xy <- st_as_sf(loc.before, coords = c('lon', 'lat'), crs = 4326)
      }
      
      ## locations immediately after
      loc.after <- with(sdata, sdata[id %in% dup_id & DateTime > maxDT,])
      if(nrow(loc.after) > 0){
        minDT_after <- min(loc.after$DateTime) + step.time*3600
        loc.after <- loc.after[loc.after$DateTime <= minDT_after,]
        loc.after_xy <- data.matrix(loc.after[,c('lon', 'lat')])
        # loc.after_xy <-st_as_sf(loc.after, coords = c('lon', 'lat'), crs = 4326)
      }
      
      #### Calculate distances
      if(nrow(loc.before) > 0 & nrow(loc.after) > 0){
        dist.before <- terra::distance(dup_xy, loc.before_xy, lonlat = TRUE)
        dist.after <- terra::distance(dup_xy, loc.after_xy, lonlat = TRUE)
        # dist.before <- sf::st_distance(dup_xy, loc.before_xy)
        # dist.after <- sf::st_distance(dup_xy, loc.after_xy)
        dist.all <- cbind(dist.before, dist.after)
        dist.sum <- rowSums(dist.all)
        dist.min <- which.min(dist.sum)[1]
      } else if(nrow(loc.before) > 0){
        dist.before <- terra::distance(dup_xy, loc.before_xy, lonlat = TRUE)
        # dist.before <- sf::st_distance(dup_xy, loc.before_xy)
        dist.sum <- rowSums(as.matrix(dist.before))
        dist.min <- which.min(dist.sum)[1]
      } else if(nrow(loc.after) > 0){
        dist.after <- terra::distance(dup_xy, loc.after_xy, lonlat = TRUE)
        # dist.after <- sf::st_distance(dup_xy, loc.after_xy)
        dist.sum <- rowSums(as.matrix(dist.after))
        dist.min <- which.min(dist.sum)[1]
      } else {
        dist.min <- 1
      }
      
      #### Return the location which is the closest to the previous and successive locations
      dup_temp[dist.min,]
    }
    
    
    ## Run the function using multiple CUP cores
    parallel::clusterExport(cl, list('sdata1', 'sdata', 'step.time'))#, envir = globalenv()
    d <- parallel::parLapply(cl, X = nloc_gp, fun = select_rows)
    sdata1 <- dplyr::bind_rows(d)

    
    #### Combine
    sdata <- dplyr::bind_rows(sdata1, sdata2, sdata3)
    sdata$group <- NULL
    
    
    ## Re-calculate
    sdata <- track_param(sdata, param = 'time')

    #### Return
    return(sdata)
  }

  
  #### Run the function until no locations can be removed by this filter
  ## set parameters for parallel processing
  if(!is.numeric(no.cores)){
    no.cores <- parallel::detectCores() - 1
  }
 
  if(.Platform$OS.type %in% 'windows'){
    cl <- parallel::makeCluster(no.cores, type="PSOCK")
  } else {
    cl <- parallel::makeCluster(no.cores, type="FORK")
  }
  
  
  ## run the function
  if(any(sdata$pTime <= step.time, na.rm = TRUE)){
    sdata <- dup.timing(sdata=sdata, step.time=step.time, no.cores = no.cores)    
    while(any(sdata$pTime <= step.time, na.rm = TRUE)){
      sdata <- dup.timing(sdata=sdata, step.time=step.time, no.cores = no.cores)
    }
  }
  
  ## stop parallel
  parallel::stopCluster(cl)
  
  
  ## Filtered data
  FilteredSS <- nrow(sdata)
  RemovedSamplesN <- OriginalSS-FilteredSS
  
  
  ## Print report
  cat("dupfilter_time removed", RemovedSamplesN, "of", OriginalSS, "locations", fill = TRUE)

  
  #### Delete working columns and return the output
  drop.vars <- c("pSpeed", "sSpeed", "inAng", "meanSpeed", "meanAngle")
  sdata <- sdata[,!(names(sdata) %in% drop.vars)] 
  return(sdata)
}
