#' @aliases asymptote
#' @title Horizontal asymptotes of rational functions
#' @description Function to find horizontal asymptotes of a rational function.
#' @param data An output object from \code{\link{boot_overlap}}, \code{\link{combn_overlap}}, or \code{\link{boot_area}}.
#' @param max.asymptote The maximum limit of an expected asymptote. Default is 1 (i.e. maximum probability). 
#' If it is unknown, set as NA (e.g. max.asymptote = NA).
#' @param x,y Numeric vectors of independent (x) and dependent (y) variables. 
#' These parameters will be ignored if \emph{data} is supplied.
#' @param degree The default 'optim' option selects the maximal degree of numerator and denominator of a rational function 
#' that minimises the mean squared error. 
#' Alternatively, an integer can be used to specify the maximal degree.
#' The 'optim' option is recommended unless there is a strong reason that a maximal degree should be specified.
#' @param upper.degree The upper limit of the maximal degree to be assessed when the 'optim' option is selected. 
#' Default is 10, meaning the "optimal" degree is searched from 1 and 10. 
#' The default usually gives good results. If the fit does not look good, a larger value may result in a better fit.
#' @param d1,d2 (Deprecated) Maximal degrees of numerator (d1) and denominator (d2) of a rational function. d1 and d2 must be equal. 
#' Use \emph{degree} instead.
#' @param threshold Threshold value for considering an asymptote. 
#' Once the y value reaches the threshold, it is considered that an asymptote is reached.
#' @param proportional If TRUE (default), a threshold is calculated as \emph{estimated asymptote * threshold}. 
#' If FALSE, the value specified in \emph{threshold} is used in the analysis.
#' @param estimator Method used to estimate the mean or predicted \emph{y} relative to \emph{x} (e.g. sample size).
#' Available options are 'mean' using arithmetic means and 'glm' using the \code{\link[stats]{glm}} function.
#' @param ci.level Confidence level for the mean or predicted \emph{y}, which will be used to assess if/when an asymptote has been reached.
#' If NULL, only the mean and predicted \emph{y} are used for the assessment (see details).   
#' @param ... Optional arguments passed to \code{\link[stats]{glm}}.
#' @importFrom pracma rationalfit
#' @importFrom pracma polyval
#' @importFrom emmeans emmeans
#' @importFrom stats glm qnorm
#' @export
#' @details This function fits a rational function to the input data. 
#' When an output object from \code{\link{boot_overlap}}, \code{\link{combn_overlap}} or \code{\link{boot_area}} is supplied, 
#' a rational function is fit to the means or predicted values of the bootstrap results (e.g. mean overlap probability) 
#' as a function of \emph{x} (e.g. sample size).
#' It then estimates horizontal asymptotes and identifies the sample size when an asymptote is considered.
#' If ci.level = NULL and threshold = 0.95, 
#' an asymptote is considered when the mean or predicted y value reaches above 95% of the estimated horizontal asymptote.
#' If ci.level is specified (e.g. 0.95) and threshold = 0.95,
#' an asymptote is considered when the mean or predicted y value AND the confidence interval are above 95% of the estimated horizontal asymptote.
#' When the "PHR" method was used in \code{\link{boot_overlap}}, 
#' binomial is generally a sensible \code{\link[stats]{family}} object for the GLM.
#' gaussian and Gamma are often good options when the maximum \emph{y} value exceeds 1 (e.g. area size).
#' Please caution if estimated horizontal asymptote is very different from the expected asymptote. 
#' For example, the estimated horizontal asymptote should be around 1 when overlaps between UDs are calculated using the "PHR" method.
#' see \code{\link{boot_overlap}}.
#' @return A list containing a data frame (rational function fit associated with x values), an estimated horizontal asymptote, 
#' the minimum sample size if an asymptote is reached, and the estimated optimal degree of numerator and denominator of the rational function.
#' @author Takahiro Shimada
#' @references Shimada T, Thums M, Hamann M, Limpus CJ, Hays GC, FitzSimmons N, Wildermann NE, Duarte CD, Meekan MG (2021) 
#' Optimising sample sizes for animal distribution analysis using tracking data. 
#' \emph{Methods in Ecology and Evolution} 12(2):288-297 \doi{10.1111/2041-210X.13506}
#' @references Press, W. H., S. A. Teukolsky, W. T. Vetterling, and B. P. Flannery (2007). 
#' \emph{Numerical Recipes: The Art of Numerical Computing}. Third Edition, Cambridge University Press, New York.
#' @seealso \code{\link{boot_overlap}}, \code{\link{combn_overlap}}, \code{\link{boot_area}}



asymptote <- function(data = NULL, x = NULL, y = NULL, degree = 'optim', upper.degree = 10, 
                      d1 = NA, d2 = NA, threshold = 0.95, proportional = TRUE, max.asymptote = 1,
                      estimator = 'glm', ci.level = 0.95, ...){
    
    if(!is.null(data)){
        x <- data$summary$N
        
        if(estimator == 'mean'){
            y <- data$summary$mu
            if(is.null(ci.level)){
                y_lwr <- NA
                y_upr <- NA
             } else {
                me <- stats::qnorm((1+ci.level)/2)
                y_lwr <- y - data$summary$sem*me
                y_upr <- y + data$summary$sem*me
            }
            
        } else if(estimator == 'glm'){
            glm_x <- factor(as.character(data$data$N), levels = as.character(x))
            glm_y <- data$data[,3]
            suppressWarnings(
                m <- stats::glm(glm_y ~ glm_x, ...)
            )
            
            # predict
            nd <- data.frame(glm_x = factor(as.character(x), levels = as.character(x)))
            if(is.null(ci.level)){
                nd <- summary(emmeans::emmeans(emmeans::ref_grid(m, nd), ~glm_x), type = "response")
                y_lwr <- NA
                y_upr <- NA
            } else {
                nd <- summary(emmeans::emmeans(emmeans::ref_grid(m, nd), ~glm_x), type = "response", level = ci.level)
                y_lwr <- nd[,5]
                y_upr <- nd[,6]
            }
            y <- nd[,2]
        }
    } 

    
    ## d1, d2 to degree
    if(!any(is.na(d1), is.na(d2))){
        warnings('The arguments "d1" and "d2" have been deprecated. The "degree" argement is used instead.')
    }
    
    
    ## Find optimal degree based on mean squared error
    if(degree == 'optim'){
        msq <- asymp <- rep(0, upper.degree)
        overlap_values <- list()
        for(i in 1:upper.degree){
            
            # Fit rational function
            RF <- pracma::rationalfit(x, y, d1 = i, d2 = i)
            RF <- lapply(RF, Re)
            
            p1 <- RF$p1; p2 <- RF$p2
            ys <- pracma::polyval(p1, x) / pracma::polyval(p2, x)
            overlap_values[[i]] <- data.frame(x, ys)
            asymp[i] <- p1[1]/p2[1]
            
            # mean square error
            msq[i] <- sum((ys - y)^2)/length(y)
            
            # exclude if there are decrements
            if(any(diff(ys)<0)){
                msq[i] <- NA
            }
            
            # exclude if asymp < 0
            if(asymp[i] < 0){
                msq[i] <- NA
            }
        }
        
        degrees <- data.frame(degree = 1:upper.degree, asymp, msq)
        if(!is.na(max.asymptote)){
            degrees <- with(degrees, degrees[asymp <= max.asymptote,])
        }
        degrees <- with(degrees, degrees[order(msq),])
        
        # Optimal degree for a rational function
        degree <- degrees[1,'degree']
        
        # Estimated asymptote
        asymp <- degrees[1,'asymp']
        
        # Estimated overlap values
        overlap_values <- overlap_values[[degree]]
        
        if(all(is.na(degrees[,'asymp']))){
            stop('Decrements were detected in the rational fit. Try a larger upper.degree. 
                 \nIf the issue persists, re-run boot_overlap/boot_area with a larger number of iterations (R).\nR = sample size x 100 is a good start.')
        }
    } else {
        ## Fit rational function
        RF <- pracma::rationalfit(x, y, d1 = degree, d2 = degree)
        RF <- lapply(RF, Re)
        
        p1 <- RF$p1; p2 <- RF$p2
        ys <- pracma::polyval(p1, x) / pracma::polyval(p2, x)
        overlap_values <- data.frame(x, ys)
        
        asymp <- p1[1]/p2[1]
        if(asymp < 0){
            stop('Unable to estimate horizontal asymptotes. The sample size is probably too small.')
        } 
    }
    
    cat('Estimated horizontal asymptote: ~', asymp, fill = TRUE, sep = '')
    

    #### Minimum sample size to achieve x% of asymptote as specified by the 'threshold' argument
    if(isTRUE(proportional)){
        
        threshold <- asymp * threshold
        
        if(is.null(ci.level)){
            above.asymp <- overlap_values$ys > threshold
        } else {
            above.asymp <- y_lwr > threshold
        }
        
    } else {
        if(is.null(ci.level)){
            above.asymp <- overlap_values$ys > threshold
        } else {
            above.asymp <- y_lwr > threshold
        }
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
    
    cat('Asymptote threshold used: y =', threshold, fill = TRUE)
    cat('Confidence level used:', ci.level, fill = TRUE)
    
    if(is.null(ci.level)){
        if(estimator == 'mean'){
            return(list(results = data.frame(x, y, ys), h.asymptote = asymp, min.n=min.n, optimal.degree = degree))
        } else {
            return(list(results = data.frame(x, y, ys), h.asymptote = asymp, min.n=min.n, optimal.degree = degree, glm.results = m))
        }
    } else {
        if(estimator == 'mean'){
            return(list(results = data.frame(x, y, y_lwr, y_upr, ys), h.asymptote = asymp, min.n=min.n, optimal.degree = degree))
        } else {
            return(list(results = data.frame(x, y, y_lwr, y_upr, ys), h.asymptote = asymp, min.n=min.n, optimal.degree = degree, glm.results = m))
        }
    }
}
