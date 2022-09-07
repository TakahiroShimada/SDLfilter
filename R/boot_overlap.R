#' @aliases boot_overlap
#' @title Bootstrap overlaps between Utilisation Distributions (UDs) 
#' @description Function to calculate overlaps between UDs relative to sample size by bootstrapping.
#' @param data A matrix or list of RasterLayer/SpatRaster objects. 
#' Each row of the matrix or each RasterLayer/SpatRaster object contains a utilisation distribution 
#' (or other statistics that sums to 1 - e.g. proportion of time spent).
#' \bold{The grid size and geographical extent must be consistent across each row of the matrix or each RasterLayer/SpatRaster object.}
#' The function assumes that each column of the matrix is associated with a unique geographical location or 
#' that each RasterLayer/SpatRaster has exactly the same geographical extent and resolution. 
#' @param R An integer specifying the number of iterations. A larger \emph{R} is required when the sample size is large.
#' R > sample size x 100 is recommended (e.g. R > 1000 for a sample size 10).
#' @param method The overlap quantification method. 
#' "HR" is for the proportion of an individual's home range overlapped by the known habitats of other individuals. 
#' "PHR" is for the probability of an individual to be within the known habitats of other individuals. 
#' "VI", "BA" and "UDOI" quantify overlap between UDs using the full probabilistic properties as described in Fieberg and Kochanny (2005). 
#' For the latter three options, the function calculates overlaps between each additional UD and a collective UD. 
#' To generate a collective UD, each UD is overlaid and averaged at each grid cell so the probability density of the collective UD sums up to 1.
#' @param percent An integer specifying the percent volume of each UD to be considered in the analysis. 
#' @param quantiles A vector or a number to specify the quantiles to be calculated in the summary of the results. 
#' @importFrom stars st_as_stars
#' @importFrom stats aggregate sd quantile
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
#' @details This function calculates and bootstraps overlap between UDs based on the areas ("HR"), 
#' areas of collective UDs and the probability distribution of each individual ("PHR"), 
#' or the probability distribution of an individual and an averaged probability distribution of collective individuals ("VI", "BA", "UDOI").
#' @return A list containing two data frames - raw results and summary (mean, sd, sem and quantiles at each sample size).
#' @author Takahiro Shimada
#' @references Shimada T, Thums M, Hamann M, Limpus CJ, Hays GC, FitzSimmons N, Wildermann NE, Duarte CD, Meekan MG (2021) 
#' Optimising sample sizes for animal distribution analysis using tracking data. 
#' \emph{Methods in Ecology and Evolution} 12(2):288-297 \doi{10.1111/2041-210X.13506}
#' @references Fieberg J & Kochanny CO (2005) Quantifying home-range overlap: The importance of the utilization distribution. 
#' \emph{The Journal of Wildlife Management}, 69(4), 1346–1359. \doi{10.2193/0022-541x(2005)69[1346:Qhotio]2.0.Co;2}
#' @seealso \code{\link{combn_overlap}}, \code{\link{boot_area}}
#' @examples
#' \dontrun{
#' 
#' #1 Utilisation uistributions of flatback turtles (n = 15).
#' data(ud_matrix)
#' 
#' #2 Calculate overlap probability from 2000 random permutation.
#' overlap <- boot_overlap(ud_matrix, R = 2000, method = "PHR")
#' 
#' #3 Find the minimum sample size required to estimate the general distribution.
#' a <- asymptote(overlap, upper.degree = 10, estimator = 'glm', family = binomial)
#' 
#' #4 Plot the mean probability and rational function fit relative to the sample sizes.
#' ggplot(data = a$results, aes(x = x))+
#'   geom_pointrange(aes(y = y, ymin = y_lwr, ymax = y_upr)) + 
#'   geom_hline(yintercept = a$h.asymptote*0.95, linetype = 2) +
#'   scale_x_continuous(breaks = seq(0, 15, 3), limits = c(2,15), name = "Animals tracked (n)") +
#'   scale_y_continuous(limits = c(0.5,1), name = "Overlap probability")
#' }




boot_overlap <- function(data, R = 1000, method = "PHR", percent = 100, quantiles = c(0.25, 0.5, 0.75)) {
  
  #### Process time
  start_time <- Sys.time()
  
  
  #### Input data
  if(inherits(data[[1]], 'stars')){
    
    dens_all_list <- lapply(data, function(x){
      c(x[[1]])
    })
    
    if(is.null(names(data))){
      names(dens_all_list) <- 1:length(data)
    } else {
      names(dens_all_list) <- names(data)
    }
    dens_all <- do.call(rbind, dens_all_list)
    
  } else if (inherits(data[[1]], c("RasterLayer", "SpatRaster"))){
    
    dens_all_list <- lapply(data, function(x){
      c(stars::st_as_stars(x)[[1]])
    })
    
    if(is.null(names(data))){
      names(dens_all_list) <- 1:length(data)
    } else {
      names(dens_all_list) <- names(data)
    }
    dens_all <- do.call(rbind, dens_all_list)
  } else {
    dens_all <- data
  }

  
  #### Names
  nam <- rownames(dens_all)
  
  #### Crop to the area of interest
  dens_all_sum <- colSums(dens_all)
  dens_all <- dens_all[,which(dens_all_sum > 0)]

  
  #### Standardise UD values (i.e. density sums to 1)
  row_sum <- rowSums(dens_all)
  l <- lapply(1:nrow(dens_all), function(x) {dens_all[x,] / row_sum[x]})
  dens_all <- do.call(rbind, l)
  
  
  #### empty vectors to save results
  nR <- (nrow(dens_all) - 1) * R
  iteration <- rep(0, nR)
  N <- rep(0, nR)
  overlap <- rep(0, nR)
  new_data <- rep('', nR)
  existing_data <- rep('', nR)

  
  #### Quantify overlap for R times
  rn <- 0
  pb <- utils::txtProgressBar(min = 0, max = R, width = 50, style = 3)
  
  for(i in 1:R){
    ## Random order of the input layers
    n <- sample(nrow(dens_all), nrow(dens_all))
    
    ## Randomly selected HR to compare with
    dens_a <- dens_all[n[1],]
    
    ## Overlap between dens_a and collective UD
    for(k in 2:nrow(dens_all)){
      rn <- rn + 1
      
      ## combined (mean) ud of the subsets
      dens_sub <- dens_all[n[2:k],]
      
      ## Areas under xx% volume (only for collective areas)
      if(percent < 100){
        if(is.matrix(dens_sub)){
          l <- lapply(1:nrow(dens_sub), function(x) percent_vol(dens_sub[x,], percent))
          dens_sub <- do.call(rbind, l)
        } else {
          dens_sub <- percent_vol(dens_sub, percent)
        }
      } 
      
      if(is.matrix(dens_sub)){
        if(method %in% c("HR", "PHR")){
          dens_sub <- colSums(dens_sub)
        } else {
          dens_sub <- colMeans(dens_sub)
        }
      }
      
      ## Calculate overlaps
      if(method == "HR") {
        goi <- which(dens_a*dens_sub > 0)
        index <- length(dens_a[goi]) / length(dens_a[dens_a > 0])
      } else if(method == "PHR") {
        goi <- which(dens_a*dens_sub > 0)
        index <- sum(dens_a[goi])
      } else if (method == "VI"){
        goi <- which(dens_a*dens_sub > 0)
        min.val <- pmin(dens_a[goi], dens_sub[goi])
        index <- sum(min.val)
      } else if(method == "BA") {
        index <- sum(sqrt(dens_a*dens_sub))
      } else if(method == "UDOI") {
        goi <- which(dens_a*dens_sub > 0)
        val <- dens_a[goi]*dens_sub[goi]
        index <- sum(val) * length(goi)
      }
      
      ## Results
      iteration[rn] <- i
      N[rn] <- k
      overlap[rn] <- index
      new_data[rn] <- nam[n[1]]
      existing_data[rn] <- paste(nam[n[2:k]], collapse=',')
    }
    

    #### progress bar
    Sys.sleep(0.1)
    utils::setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  # id_df <- as.data.frame(do.call(cbind, id_list))
  overlap_df <- data.frame(iteration, N, overlap, new_data, existing_data)


  #### Summary stats
  summary.data <- stats::aggregate(overlap ~ N, data = overlap_df, FUN = function(x) {
    c(mu = mean(x),
      std = stats::sd(x),
      sem = stats::sd(x)/sqrt(length(x)),
      stats::quantile(x, probs = quantiles))
  })
  summary.data <- cbind(N = summary.data[,1], as.data.frame(summary.data[,-1]))
  quantile_header <- quantiles * 100
  colnames(summary.data) <- c("N", "mu", "std", "sem", paste0("Qu", quantile_header))
  
  #### Running time
  run.time <- as.numeric(difftime(Sys.time(), start_time, units = 'min'))
  M <- floor(run.time); S <- round((run.time - M) * 60)
  message('Runtime: ', M, ' minutes ', S, ' seconds')
  
  #### Outputs
  list(data = overlap_df, summary = summary.data)
}

