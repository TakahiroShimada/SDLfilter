#' @aliases ddfilter_loop
#' @title Filter locations by quality index, inner angle, and speed
#' @description A partial component of \code{\link{ddfilter}}, although works as a stand-alone function. 
#' This function removes locations by speed, inner angle, and quality index as described in Shimada et al. (2012).
#' @param sdata A data frame containing columns with the following headers: "id", "DateTime", "lat", "lon", "qi". 
#' See the data \code{\link{turtle}} for an example.
#' The function filters the input data by a unique "id" (e.g. transmitter number, identifier for each animal). 
#' "DateTime" is the GMT date & time of each location in class \code{\link[base:DateTimeClasses]{POSIXct}} or \code{\link[base]{character}} with the following format "2012-06-03 01:33:46".
#' "lat" and "lon" are the latitude and longitude of each location in decimal degrees. 
#' "qi" is the quality index associated with each location fix. 
#' The input values can be either the number of GPS satellites or Argos Location Classes. 
#' Argos Location Classes will be converted to numerical values, where "A", "B", "Z" will be replaced with "-1", "-2", "-3" respectively.
#' The greater number indicates a higher accuracy. 
#' @param vmaxlp A numeric value specifying a threshold of speed, which is used to evaluate the locations of loop trips. 
#' Default is 1.8 km/h.
#' If this value is unknown, it can be estimated from \emph{sdata} using the function \code{\link{vmaxlp}}.
#' @param qi An integer specifying a threshold of quality index, which is used to evaluate the locations of loop trips. 
#' Default is 4.
#' @param ia An integer specifying a threshold of inner angle, which is used to evaluate the locations of loop trips. 
#' Default is 90 degrees.
#' @importFrom dplyr bind_rows
#' @export
#' @details This function removes locations if all of the following criteria apply: 
#' the number of source satellites are less than or equal to \emph{qi}, 
#' the inner angle is less than and equal to \emph{ia} and the speed either from a previous or to a subsequent location exceeds \emph{vmaxlp}. 
#' If \emph{vmaxlp} is unknown, it can be estimated using the function \code{\link{vmaxlp}}.
#' @return The input data is returned without locations identified by this filter.
#' The following columns are added: "pTime", "sTime", "pDist", "sDist", "pSpeed", "sSpeed", "inAng".
#' "pTime" and "sTime" are hours from a previous and to a subsequent fix respectively.
#' "pDist" and "sDist" are straight distances in kilometres from a previous and to a subsequent fix respectively.
#' "pSpeed" and "sSpeed" are linear speed from a previous and to a subsequent fix respectively.
#' "inAng" is the degree between the bearings of lines joining successive location points.
#' @author Takahiro Shimada
#' @references Shimada T, Jones R, Limpus C, Hamann M (2012) Improving data retention and home range estimates by data-driven screening. 
#' \emph{Marine Ecology Progress Series} 457:171-180 \doi{10.3354/meps09747}
#' @seealso \code{\link{ddfilter}}, \code{\link{ddfilter_speed}}, \code{\link{vmaxlp}}



# Hierarchical screening
ddfilter_loop<-function(sdata, qi=4, ia=90, vmaxlp=1.8){
  
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

  for(k in 1:qi) {

      LP.filter <- function(sdata = sdata, qi = qi, ia = ia, vmaxlp = vmaxlp){
  
          #### Loop filter
          rm.maxLPSPD <- function(sdata = sdata, qi = qi, ia = ia, vmaxlp = vmaxlp) {
            
            #### Exclude data with less than 4 locations
            ndata <- table(sdata$id)
            id.exclude <- names(ndata[as.numeric(ndata) < 4])
            excluded.data <- sdata[sdata$id %in% id.exclude,]
            sdata <- sdata[!(sdata$id %in% id.exclude),]
            
            if(nrow(sdata) > 0){
              #### Organize data
              ## Sort data in alphabetical and chronological order
              sdata <- with(sdata, sdata[order(id, DateTime),])
              row.names(sdata)<-1:nrow(sdata)
              
              
              ## Get Id of each animal
              IDs<-levels(factor(sdata$id))
              
              ## Get movement parameters
              sdata <- track_param(sdata, param = c('time', 'distance', 'speed', 'angle'))
              
              
              ### Remove location according to qi, inner angle and max LP speed
              ##function to identify location to remove: (0 = remove, 1 = keep)
              pick.rm<-function(i){
                if(sdata$qi[i]<=qi && sdata$inAng[i]<ia && sdata$pSpeed[i]>vmaxlp &&
                   (!is.na(sdata$qi[i])) && (!is.na(sdata$inAng[i])) && (!is.na(sdata$pSpeed[i]))){
                  0
                } else if(sdata$qi[i]<=qi && sdata$inAng[i]<ia && sdata$sSpeed[i]>vmaxlp &&
                          (!is.na(sdata$qi[i])) && (!is.na(sdata$inAng[i])) && (!is.na(sdata$sSpeed[i]))){
                  0
                } else {
                  1  
                }
              }
              
              ## Apply the above function to each data set separately
              set.rm<-function(j){
                start<-as.numeric(rownames(sdata[sdata$id %in% j,][2,]))
                end<-as.numeric(rownames(sdata[sdata$id %in% j,][1,]))+(nrow(sdata[sdata$id %in% j,])-2)
                rm<-unlist(lapply(start:end, pick.rm))
                c(1, rm, 1)
              }
              
              sdata$overLpMax<-unlist(lapply(IDs, set.rm))
              
              sdata<-sdata[sdata$overLpMax==1,]
              
            }

            #### Bring back excluded data
            if(nrow(excluded.data)>0){
              sdata <- dplyr::bind_rows(sdata, excluded.data)
            } else {
              sdata <- sdata
            }
            
            return(sdata)
          }


        #### Repeat the function until no locations can be removed by this filter
        sdata2<-rm.maxLPSPD(sdata = sdata, qi = qi, ia = ia, vmaxlp = vmaxlp)
        sdata3<-rm.maxLPSPD(sdata = sdata2, qi = qi, ia = ia, vmaxlp = vmaxlp)
        
        while(!(nrow(sdata2) == nrow(sdata3)))
        {
          sdata3<-rm.maxLPSPD(sdata = sdata2, qi = qi, ia = ia, vmaxlp = vmaxlp)
          sdata2<-rm.maxLPSPD(sdata = sdata3, qi = qi, ia = ia, vmaxlp = vmaxlp)
        }
          
        sdata<-sdata3
      }
  
    sdata<-LP.filter(sdata = sdata, qi=k, ia = ia, vmaxlp = vmaxlp)
  }
  
  
  #### Report the summary of filtering
  ## Data excluded from filtering
  ndata<-table(as.character(sdata$id))
  id.exclude<-names(ndata[as.numeric(ndata)<4])
  
  ## Filtered data
  FilteredSS<-nrow(sdata)
  RemovedSamplesN<-OriginalSS-FilteredSS
  
  ## Print report
  cat("ddfilter_loop removed", RemovedSamplesN, "of", OriginalSS, "locations", fill = TRUE)
  # cat("\n")
  if(length(id.exclude)>0){
      message('Warning: insufficient data to apply ddfilter_loop to;')
      message(paste(id.exclude, collapse = ', '))
  }
  

  #### Delete working columns and return the output
  drop.vars <- c("overLpMax", "meanSpeed", "meanAngle")
  sdata <- sdata[,!(names(sdata) %in% drop.vars)] 
  # sdata$overLpMax <- NULL
  return(sdata)
}
