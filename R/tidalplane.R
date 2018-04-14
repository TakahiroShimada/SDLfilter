#' @aliases tidalplane
#' @title Tidal plane table for Sandy Strait, Australia
#' @description A semidiurnal tidal plane table containing the height of the mean tidal planes and the average tide time differences 
#' at different places within Sandy Strait.
#' @format A data frame with 2 rows and 6 variables:
#' \describe{
#'   \item{secondary.port}{identifier for a station at which tide is only predicted using tidal records observed at the related standard port}
#'   \item{standard.port}{identifier for a tidal observation station}
#'   \item{lat}{latitude in decimal degrees}
#'   \item{lon}{longitude in decimal degrees}
#'   \item{datumDiff}{baseline difference in meters between bathymetry and tidal observations/predictions 
#'   if each data uses different datum (e.g. LAT and MSL)}
#'   \item{timeDiff}{time difference between standard port and its associated secondary port}
#' }
#' @source The State of Queensland (Department of Transport and Main Roads), Tidal planes.
"tidalplane"