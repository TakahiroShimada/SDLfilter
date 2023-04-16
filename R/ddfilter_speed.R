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
#' @param method An integer (1 or 2) specifying how locations should be filtered. 
#' Default is 1 and removes a location if the speed from a previous AND to a subsequent location exceeds \emph{vmax}. 
#' Select 2 to remove a location if the speed from a previous OR to a subsequent location exceeds \emph{vmax}. 
#' For the latter, the filter examines successive suspect locations (i.e. the speed from a previous and/or to a subsequent location exceeds \emph{vmax}) 
#' and retain one location that is associated with the minimum speed from a previous and to a subsequent location.
#' @importFrom dplyr bind_rows
#' @export
#' @details This function removes locations if the speed from a previous and/or to a subsequent location exceeds a given threshold speed. 
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
      sdata$row <- seq(1, nrow(sdata))
      sdata1 <- with(sdata, sdata[(!is.na(pSpeed) & pSpeed > vmax) | (!is.na(sSpeed) & sSpeed > vmax), c('pSpeed', 'sSpeed', 'row')])
      
      while(nrow(sdata1) > 0){
        # Select working columns
        pSpd <- sdata1$pSpeed
        sSpd <- sdata1$sSpeed
        rows <- sdata1$row
        rm(sdata1)
        
        # group successive locations to assess
        index <- 1; g <- rep(1, length(rows))
        for(i in 2:length(rows)){
          if(is.na(pSpd[i]) | pSpd[i] != sSpd[i-1] | is.na(sSpd[i-1])){
            index <- index + 1
          }
          g[i] <- index
        }
        
        
        # Calculate mean speed
        mean_spd <- rowMeans(cbind(pSpd, sSpd), na.rm = TRUE)

        # Remove/retain locations based on the mean speed from a previous and to a subsequent location
        row_rm <- rep(0, length(unique(g)))
        for(i in unique(g)){
          rows_g <- which(g == i)
          max_spd <- which.max(mean_spd[rows_g])
          row_rm[i] <- rows[rows_g[max_spd]]
        }
  
        sdata <- sdata[!sdata$row %in% row_rm,]
        sdata <- track_param(sdata, param = 'speed')
        sdata1 <- with(sdata, sdata[(!is.na(pSpeed) & pSpeed > vmax) | (!is.na(sSpeed) & sSpeed > vmax), c('pSpeed', 'sSpeed', 'row')])
      }
      sdata$row <- NULL
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
