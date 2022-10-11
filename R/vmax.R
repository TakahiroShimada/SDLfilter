#' @aliases vmax
#' @title Maximum linear speed
#' @description Function to estimate the maximum linear speed between two consecutive locations.
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
#' @param qi An integer specifying the lowest quality index of a location that is qualified to be used in the estimation. 
#' Default is 5 (e.g. 5 GPS satellite or more).
#' @param method Available options are "sample" (i.e. sample quantile - see \code{\link[stats:quantile]{quantile}})
#' and "ML" (maximum likelihood estimation - see details). Default is "ML".
#' @param prob A quantile value (0 to 1). 
#' This value specifies the upper limit of a sample quantile or a probability distribution of linear speed, 
#' from which maximum linear speed is determined. Default is 0.99. See details.
#' @param ... Extra arguments passed to \code{\link{dupfilter}}.
#' @importFrom stats pgamma dgamma optim var
#' @export
#' @details The function first calculates the linear speed between each pair of two consecutive locations. 
#' Some of the calculated linear speed can be inaccurate when input data contains inaccurate locations (e.g. outliers).
#' To exclude implausible outliers, the function discards extreme values based on the specified quantile or 
#' an estimated probability distribution for the loop trip speed, depending on the selected \emph{method}. 
#' If the "ML" method is selected, a Gamma distribution is assumed and the shape and scale parameters are estimated via maximum likelihood estimation 
#' using the \code{\link[stats:optim]{optim}} function. 
#' The maximum value within a given probability range (e.g. 0.99) represents the maximum linear speed at which 
#' an animal would travel between two consecutive locations.   
#' @return Maximum linear speed (vmax) estimated from the input data. The unit is km/h. 
#' @author Takahiro Shimada
#' @references Shimada T, Jones R, Limpus C, Hamann M (2012) 
#' Improving data retention and home range estimates by data-driven screening. 
#' \emph{Marine Ecology Progress Series} 457:171-180 \doi{10.3354/meps09747}
#' @seealso \code{\link{ddfilter}}, \code{\link{ddfilter_speed}}, \code{\link{track_param}}, \code{\link{dupfilter}}


vmax <- function(sdata, qi=5, method = 'ML', prob=0.99, ...){
  
  #### Organize data
  ## qi format
  sdata <- within(sdata, {
    qi[qi %in% "A"] <- "-1"
    qi[qi %in% "B"] <- "-2"
    qi[qi %in% "Z"] <- "-3"
    qi <- as.numeric(as.character(qi))
  })
  
  ## Date & time
  sdata$DateTime <- with(sdata, as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"))
  
  
  ## Subset data by quality index
  sdata<-sdata[sdata$qi >= qi,]
  
  
  ## Filter duplicate locations
  sdata <- dupfilter(sdata, ...)
  
  
  ## Get movement parameters
  sdata <- track_param(sdata, param = c('time', 'distance', 'speed'))
  v <- with(sdata, sdata[!(is.na(pSpeed)) & pSpeed > 0, 'pSpeed'])
  
  
  #### Maximum speed 
  if(method == "ML"){
    
    ## Maximum likelihood estimation

        # likelihood function for normal distribution with two unknowns
    # v <- log(speed)
    # neg_log_lik_gaussian <- function(mu,sigma) {
    #   -sum(dnorm(v, mean=mu, sd=sigma, log=TRUE))
    # }
    # 
    # gaussian_fit <- stats4::mle(neg_log_lik_gaussian, start=list(mu=1, sigma=1), method="L-BFGS-B") #, method="L-BFGS-B"
    # mle_mean <- gaussian_fit@coef['mu']
    # ml_sd <- gaussian_fit@coef['sigma']
    # v_vec <- seq(min(v), max(v), by = 0.001)
    # p.norm <- pnorm(v_vec, m=mle_mean, sd=ml_sd)
    
    ## use Gamma distribution
    # maximum likelihood estimation of gamma distribution parameters (shape, scale)
    
    alpha.start <- mean(v)^2 / stats::var(v)
    lambda.start <- mean(v) / stats::var(v)
    theta.start <- c(alpha.start, lambda.start)
    
    mlogl <- function(theta, x) {
      alpha <- theta[1]
      lambda <- theta[2]
      return(- sum(stats::dgamma(x, shape = alpha, rate = lambda, log = TRUE)))
    }
    
    # para <- nlm(mlogl, theta.start, x = v, hessian = TRUE,
    #            fscale = length(v))
    
    if(suppressWarnings({
      inherits(try(stats::optim(par = theta.start, fn = mlogl, x = v), silent = TRUE), "try-error")
      })){
      message('There is not enough data to estimate Vmax')
      return(NA)
    } else {
      suppressWarnings({
        para <- stats::optim(par = theta.start, fn = mlogl, x = v)
      })
      
      p <- prob + (1 - prob)/2
      v_vec <- seq(min(v), max(v), by = 0.001)
      p.gamma <- stats::pgamma(v_vec, shape=para$par[1], scale=para$par[2])
      p <- prob + (1 - prob)/2
      Vmax <- v_vec[max(which(p.gamma < p))]
    } 
    
   } else {
     ## Given x percentile considered to exclude outliers
     Vmax <- stats::quantile(v, prob)
   }
    
  #### Report the results
  SampleSize <- round(nrow(sdata)*prob)
  cat("\n")
  cat("The maximum linear speed (Vmax) was estimated using", SampleSize, "locations.", fill = TRUE)
  cat("Vmax:", Vmax, "km/h", fill = TRUE)
  
  
  #### Maximum speed given # percentile considered outliers
  return(Vmax)
}

