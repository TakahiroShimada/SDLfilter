#' @aliases ddfilter_speed
#' @title Filter locations by speed 
#' @description A partial component of \code{\link{ddfilter}}, although works as a stand-alone function. 
#' This function removes locations by a given threshold speed as described in Shimada et al. (2012).
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' See the data \code{\link{turtle}} for an example.
#' The function filters the input data by a unique "id" (e.g. transmitter number, identifier for each animal). 
#' "DateTime" is the GMT date & time of each location in class \code{\link[base:DateTimeClasses]{POSIXct}} or \code{\link[base]{character}} with the following format "2012-06-03 01:33:46".
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' "qi" is the quality index associated with each location fix. 
#' The input values can be either the number of GPS satellites or Argos Location Classes. 
#' Argos Location Classes will be converted to numerical values, where "A", "B", "Z" will be replaced with "-1", "-2", "-3" respectively.
#' The greater number indicates a higher accuracy. 
#' @param vmax A numeric value specifying a threshold of speed from a previous and/or to a subsequent fix. 
#' Default is 8.9km/h. 
#' If this value is unknown, it can be estimated from \emph{sdata} using the function \code{\link{vmax}}.
#' @param method An integer (1 or 2) specifying how locations should be filtered with \emph{vmax}. 
#' Default is 1 (both way) and removes a location if the speed from a previous AND to a subsequent location exceeds \emph{vmax}. 
#' Select 2 (one way) to remove a location if the speed from a previous OR to a subsequent location exceeds \emph{vmax}. 
#' For the latter, the filter examines successive suspect locations (i.e. the speed from a previous and/or to a subsequent location exceeds \emph{vmax}) 
#' and retain one location that is associated with the minimum speed from a previous and/or to a subsequent location.
#' @importFrom dplyr bind_rows
#' @export
#' @details This function removes locations if the speed from a previous and/or to a subsequent location exceeds a given threshold speed. 
#' When method = 2 is selected (one way), 
#' If \emph{vmax} is unknown, it can be estimated using the function \code{\link{vmax}}.
#' @return The input data is returned without locations identified by this filter. 
#' The following columns are added: "pTime", "sTime", "pDist", "sDist", "pSpeed", "sSpeed". 
#' "pTime" and "sTime" are hours from a previous and to a subsequent fix respectively. 
#' "pDist" and "sDist" are straight distances in kilometres from a previous and to a subsequent fix respectively. 
#' "pSpeed" and "sSpeed" are linear speed from a previous and to a subsequent fix respectively. 
#' @author Takahiro Shimada
#' @references Shimada T, Jones R, Limpus C, Hamann M (2012) 
#' Improving data retention and home range estimates by data-driven screening.
#' \emph{Marine Ecology Progress Series} 457:171-180 \doi{10.3354/meps09747}
#' @seealso \code{\link{ddfilter}}, \code{\link{ddfilter_loop}}, \code{\link{vmax}}, \code{\link{track_param}}



ddfilter_speed <- function (sdata, vmax = 8.9, method = 1){
  
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
  
  
  ## Exclude data with less than 4 locations
  ndata <- table(sdata$id)
  id.exclude <- names(ndata[as.numeric(ndata) < 4])
  excluded.data <- sdata[sdata$id %in% id.exclude,]
  sdata <- sdata[!(sdata$id %in% id.exclude),]
  
  
  ## Speed filter
  if(nrow(sdata) > 0){
    ## Get movement parameters
    sdata <- track_param(sdata, param = 'speed')
    
    
    ## speed filter
    if(method == 1){
      
      # Remove locations at which the speed from a previous AND to a subsequent location exceeds maximum linear traveling speed (Vmax)
      sdata1 <- with(sdata, sdata[is.na(pSpeed) | is.na(sSpeed) | pSpeed <= vmax | sSpeed <= vmax,])
      
      while(!(nrow(sdata) == nrow(sdata1)))
      {
        sdata1 <- track_param(sdata1, param = 'speed')
        sdata <- with(sdata1, sdata1[is.na(pSpeed) | is.na(sSpeed) | pSpeed <= vmax | sSpeed <= vmax,])
      }
      
    } else if (method == 2){
      
      # Remove locations at which the speed from a previous OR to a subsequent location exceeds maximum linear traveling speed (Vmax)
      sdata1 <- with(sdata, sdata[(!is.na(pSpeed) & pSpeed > vmax) | (!is.na(sSpeed) & sSpeed > vmax),])
 
      while(nrow(sdata1) > 0){
        # group successive locations to assess
        index <- 0; g <- rep(0, nrow(sdata1))
        for(i in 2:nrow(sdata1)){
          if(sdata1[i, 'pSpeed'] != sdata1[i-1, 'sSpeed']){
            index <- index + 1
          } 
          g[i] <- index
        }
        sdata1$group <- g
        
        # Remove/retain locations based on the mean speed from a previous and to a subsequent location
        sdata1$meanSpeed <- rowMeans(sdata1[, c('pSpeed', 'sSpeed')], na.rm = TRUE)
        for(i in unique(sdata1$group)){
          if(length(sdata1[sdata1$group %in% i, 'group']) > 1){
            max_meanSpd <- max(with(sdata1, sdata1[group %in% i, 'meanSpeed']))
            rm_row <- with(sdata1, sdata1[group %in% i & meanSpeed %in% max_meanSpd,])
            sdata <- with(sdata, sdata[!(id %in% unique(rm_row$id) & DateTime %in% rm_row$DateTime),])
          }
        }
        
        sdata <- track_param(sdata, param = 'speed')
        sdata1 <- with(sdata, sdata[(!is.na(pSpeed) & pSpeed > vmax) | (!is.na(sSpeed) & sSpeed > vmax),])
      }
    }
  }
  
  
  ## Bring back excluded data
  if(nrow(excluded.data) > 0){
    sdata <- dplyr::bind_rows(sdata, excluded.data)
  } else {
    sdata <- sdata
  }
  
    
  #### Report the summary of filtering
  ## Data excluded from filtering
  ndata <- table(as.character(sdata$id))
  id.exclude <- names(ndata[as.numeric(ndata) < 4])
    
  ## Filtered data
  FilteredSS <- nrow(sdata)
  RemovedSamplesN <- OriginalSS - FilteredSS
  
  ## Print report
  cat("ddfilter_speed removed", RemovedSamplesN, "of", OriginalSS, "locations", fill = TRUE)
  if(length(id.exclude) > 0){
    message('Warning: insufficient data to apply ddfilter_speed to:')
    message(paste(id.exclude, collapse = ', '))
  }

  return(sdata)
}
