#' @aliases percent_vol
#' @title UD percent volume  
#' @description Function to calculate a percent volume on a utilisation distribution (UD)
#' @param x A vector containing the probability density. 
#' @param percent An integer specifying the percent volume of a UD to be considered.
#' @export
#' @details This function calculates a percent volume on a UD. The probability beyond the specified range will be assigned with a zero value.
#' @return A vector containing the specified percent volume.
#' @author Takahiro Shimada


percent_vol <- function(x, percent = 100){
    y <- sort(x[!is.na(x)], decreasing = TRUE)
    z <- cumsum(as.numeric(y))
    if(any(z <= percent/100)){
        m <- max(z[z <= percent/100])
        i <- which(z == m)[1]
        
        if(m < 1){
            d <- ifelse(x >= y[i], x, 0)
        } else {
            d <- ifelse(x > y[i], x, 0)
        }
        return(d)
    } else {
        stop(paste0("cannot calculate ", percent, "% UD. Increase the resolution of the UDs (i.e. use smaller grid cells)."), call. = FALSE)
    }
}

