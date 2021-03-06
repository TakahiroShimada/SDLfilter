#' @aliases flatback
#' @title Flatback turtle tracking data
#' @description Satellite tracking data of 15 flatback turtles (\emph{Natator depressus}) that nested in Curtis Island, Australia.
#' This sample data is a subset of the tracking data used in \href{https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13506}{Shimada et al. (2021)}.
#' @format A data frame with 1020 rows and 4 variables:
#' \describe{
#'   \item{id}{identifier for each animal.}
#'   \item{DateTime}{GMT date & time of each location in class \code{\link[base:DateTimeClasses]{POSIXct}}.}
#'   \item{x}{longitude in UTM.}
#'   \item{y}{latitude in UTM.}
#' }
#' @source Shimada T, Thums M, Hamann M, Limpus CJ, Hays GC, FitzSimmons N, Wildermann NE, Duarte CD, Meekan MG (2021) 
#' Optimising sample sizes for animal distribution analysis using tracking data. 
#' \emph{Methods in Ecology and Evolution} 12(2):288-297 \doi{10.1111/2041-210X.13506}
"flatback"