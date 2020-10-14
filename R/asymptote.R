#' @aliases asymptote
#' @title Horizontal asymptotes of rational functions
#' @description Function to find horizontal asymptotes of a rational function.
#' @param data An output object from \code{\link{boot_overlap}} or \code{\link{boot_area}}.
#' @param x,y Numeric vectors of independent (x) and dependent (y) variables. 
#' These parameters will be ignored if \emph{data} is supplied.
#' @param d1,d2 Maximal degrees of numerator (d1) and denominator (d2) of a rational function.
#' @param threshold Threshold value for considering an asymptote. 
#' Once the y value reaches the threshold, it is considered that an asymptote is reached.
#' @param proportional If TRUE (default), a threshold is calculated as \emph{estimated asymptote * threshold}. 
#' If FALSE, the value specified in \emph{threshold} is used in the analysis.
#' @importFrom pracma rationalfit
#' @importFrom pracma polyval
#' @export
#' @details This function fits a rational function to the input data. 
#' When an output object from \code{\link{boot_overlap}} or \code{\link{boot_area}} is supplied, 
#' a rational function is fit to the means of the bootstrap results (e.g. mean overlap probability) as a function of \emph{x} (e.g. sample size).
#' It then estimates horizontal asymptotes and identifies the sample size when an asymptote is considered.
#' @return A list containing a data frame (rational function fit associated with x values), an estimated horizontal asymptote, 
#' and the minimum sample size if an asymptote is reached.
#' @author Takahiro Shimada
#' @references Press, W. H., S. A. Teukolsky, W. T. Vetterling, and B. P. Flannery (2007). 
#' \emph{Numerical Recipes: The Art of Numerical Computing}. Third Edition, Cambridge University Press, New York.
#' @references Shimada T, Thums M, Hamann M, Limpus CJ, Hays GC, FitzSimmons N, Wildermann NE, Duarte CD, Meekan MG (in press) 
#' Optimising sample sizes for animal distribution analysis using tracking data. 
#' \emph{Methods in Ecology and Evolution} n/a(n/a) doi:\href{http://doi.org/10.1111/2041-210X.13506}{10.1111/2041-210X.13506}
#' @seealso \code{\link{boot_overlap}}, \code{\link{boot_area}}



asymptote <- function(data = NULL, x = NULL, y = NULL, d1 = 1, d2 = 1, threshold = 0.95, proportional = TRUE){
    if(!is.null(data)){
        x <- data$summary$N
        y <- data$summary$mu
    } 
    
    ## Fit rational function
    RF <- pracma::rationalfit(x, y, d1 = d1, d2 = d2)
    
    p1 <- RF$p1; p2 <- RF$p2
    ys <- pracma::polyval(p1, x) / pracma::polyval(p2, x)
    overlap_values <- data.frame(x, value = ys)
    
    asymp <- p1[1]/p2[1]
    
    #### Minimum sample size to achieve 95% of asymptote
    if(isTRUE(proportional)){
        above.asymp <- overlap_values$value > asymp * threshold
    } else {
        above.asymp <- overlap_values$value > threshold
    }
    
    
    #### Report
    cat("\n")
    if(any(above.asymp)){
        min.n <- min(overlap_values[above.asymp, "x"])
        cat('Asymptote reached at x =', min.n, fill = TRUE)
    } else {
        message('Asymptote not reached')
        min.n <- NA
    }
    
    cat('Estimated Horizontal asymptote ~', asymp, fill = TRUE)
    
    return(list(results = data.frame(ys, x), h.asymptote = asymp, min.n=min.n))
}
