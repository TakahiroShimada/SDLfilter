#' @aliases dupfilter_qi
#' @title Filter temporal duplicates by quality index
#' @description Function to filter temporal duplicates in tracking data by quality index.
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "qi". 
#' See the data \code{\link{turtle}} for an example.
#' The function filters the input data by a unique "id" (e.g. transmitter number, identifier for each animal). 
#' "DateTime" is the GMT date & time of each location in class \code{\link[base:DateTimeClasses]{POSIXct}} or \code{\link[base]{character}} with the following format "2012-06-03 01:33:46".
#' "qi" is the quality index associated with each location fix. 
#' The input values can be either the number of GPS satellites or Argos Location Classes. 
#' Argos Location Classes will be converted to numerical values, where "A", "B", "Z" will be replaced with "-1", "-2", "-3" respectively.
#' The greater number indicates a higher accuracy. 
#' @param step.time Consecutive locations less than or equal to \emph{step.time} apart are considered temporal duplicates.
#' Default is 0 hours.
#' @importFrom dplyr bind_rows
#' @export
#' @details This function is a partial component of \code{\link{dupfilter}}, although works as a stand-alone function. 
#' It looks for temporal duplicates and retains a fix with the highest quality index.
#' @return The input data frame is returned with temporal duplicates removed by the quality index.
#' The following columns are added: "pTime", "sTime". 
#' "pTime" and "sTime" are hours from a previous and to a subsequent fix respectively. 
#' @author Takahiro Shimada
#' @references Shimada T, Limpus C, Jones R, Hazel J, Groom R, Hamann M (2016) 
#' Sea turtles return home after intentional displacement from coastal foraging areas. 
#' \emph{Marine Biology} 163:1-14 \doi{10.1007/s00227-015-2771-0}
#' @seealso \code{\link{dupfilter}}, \code{\link{dupfilter_exact}}, \code{\link{dupfilter_time}}, \code{\link{dupfilter_space}}, \code{\link{track_param}}



dupfilter_qi <- function(sdata = sdata, step.time = 0){
  
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
  
  
  #### Prepare data for filtering
  IDs <- levels(factor(sdata$id))
  
  ## Sort data in alphabetical and chronological order
  sdata <- with(sdata, sdata[order(id, DateTime),])
  
  
  ## lagged date and time
  sdata <- dplyr::bind_rows(lapply(IDs, function(j){
    sdata.temp <- sdata[sdata$id %in% j,]
    timeDiff <- diff(sdata.temp$DateTime)
    units(timeDiff) <- "hours"
    sdata.temp$pTime <- c(NA, as.numeric(timeDiff))
    sdata.temp$sTime <- c(as.numeric(timeDiff), NA)
    sdata.temp$pQI <- c(NA, sdata.temp$qi[-nrow(sdata.temp)])
    sdata.temp$sQI <- c(sdata.temp$qi[-1], NA)
    return(sdata.temp)
  }))
  

  #### Function to filter data by quality index
  dup.qi <- function(sdata = sdata, step.time = step.time){
    ## Extract data within step.time AND different qi
    sdata1 <- with(sdata, sdata[which((pTime <= step.time & qi != pQI) | (sTime <= step.time & qi != sQI)),])
    
    ## Other data
    if(nrow(sdata1)>0){
      sdata2 <- dplyr::anti_join(sdata, sdata1, by = c('id', 'DateTime', 'lat', 'lon', 'qi'))
    } else {
      sdata2 <- sdata 
    }
    
    
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
    
    ## Sort data by id, time and quality index
    sdata1 <- with(sdata1, sdata1[order(qi, decreasing = TRUE),])
    sdata1 <- with(sdata1, sdata1[order(group),])
    
    
    #### Filter temporal duplicates by quality index
    sdata1 <- dplyr::distinct(sdata1, .data$group, .keep_all = TRUE)
    
    
    #### Bring back the excluded data
    sdata <- dplyr::bind_rows(sdata1, sdata2, sdata3)
    sdata$group <- NULL
    
    #### Reorganise
    sdata <- with(sdata, sdata[order(id, DateTime),])
    IDs <- levels(factor(sdata$id))

    ## lagged date and time
    sdata <- dplyr::bind_rows(lapply(IDs, function(j){
      sdata.temp <- sdata[sdata$id %in% j,]
      timeDiff <- diff(sdata.temp$DateTime)
      units(timeDiff) <- "hours"
      sdata.temp$pTime <- c(NA, as.numeric(timeDiff))
      sdata.temp$sTime <- c(as.numeric(timeDiff), NA)
      sdata.temp$pQI <- c(NA, sdata.temp$qi[-nrow(sdata.temp)])
      sdata.temp$sQI <- c(sdata.temp$qi[-1], NA)
      return(sdata.temp)
    }))
    return(sdata)
  }


  #### Repeat the function until no locations can be removed by this filter
  if(any((sdata$pTime <= step.time & sdata$qi != sdata$pQI) | (sdata$sTime <= step.time & sdata$qi != sdata$sQI), na.rm = TRUE)){
    sdata <- dup.qi(sdata=sdata, step.time=step.time)    
    while(any((sdata$pTime <= step.time & sdata$qi != sdata$pQI) | (sdata$sTime <= step.time & sdata$qi != sdata$sQI), na.rm = TRUE)){
      sdata <- dup.qi(sdata=sdata, step.time=step.time)
    }
  }

  #### Report the summary of filtering
  ## Filtered data
  FilteredSS<-nrow(sdata)
  RemovedSamplesN<-OriginalSS-FilteredSS
  
  ## Print report
  cat("dupfilter_qi removed", RemovedSamplesN, "of", OriginalSS, "locations", fill = TRUE)


  #### Delete working columns and return the output
  drop.vars <- c("pQI", "sQI", "pDist", "sDist", "pSpeed", "sSpeed", "inAng", "meanSpeed", "meanAngle")
  sdata <- sdata[,!(names(sdata) %in% drop.vars)] 
  return(sdata)
}
