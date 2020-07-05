#' @aliases turtle
#' @title Green turtle tracking data
#' @description A dataset containing Fastloc GPS locations of a green turtle tracked in Sandy Strait, Australia.
#' @format A data frame with 429 rows and 5 variables:
#' \describe{
#'   \item{id}{identifier for each animal.}
#'   \item{DateTime}{GMT date & time of each location in class \code{\link[base:DateTimeClasses]{POSIXct}}.}
#'   \item{lat}{latitude in decimal degrees.}
#'   \item{lon}{longitude in decimal degrees.}
#'   \item{qi}{quality index associated with each location fix. 
#' The input values can be either the number of GPS satellites or Argos Location Classes. 
#' Argos Location Classes will be converted to numerical values, where "A", "B", "Z" will be replaced with "-1", "-2", "-3" respectively.
#' The greater number indicates a higher accuracy.}
#' }
#' @source Shimada T, Jones R, Limpus C, Groom R, Hamann M (2016) 
#' Long-term and seasonal patterns of sea turtle home ranges in warm coastal foraging habitats: Implications for conservation. 
#' \emph{Marine Ecology Progress Series} 562:163-179. doi:\href{http://doi.org/10.3354/meps11972}{10.3354/meps11972}
"turtle"