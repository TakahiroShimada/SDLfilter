---
editor_options:
  chunk_output_type: inline
output:
  html_document:
    df_print: paged
---
News for Package 'SDLfilter'
=========

#### Version 2.2.1.9001 (2022-05-29)

* improved the estimation of vmax of vmaxlp by implementing Maximum Likelihood estimation.
* fixed minor bugs in the ddfilter, dupfilter and track_param functions.


#### Version 2.2.1 (2022-05-15)

* added a parallel computing option to the dupfilter_time, dupfilter_space, and dupfilter functions.
* removed the kml_track function as the dependent package (plotKML) has been removed from the CRAN repository.
* updated to use the sf and stars packages instead of the sp and raster packages. The updated functions are boot_area, boot_overlap, combn_overlap, depthfilter, dupfilter_space, track_param, dupfilter_time, and map_track
* updated to use the dplyr package instead of the plyr package. The updated functions are boot_area, ddfilter_loop, ddfilter_speed, dupfilter_qi, dupfilter_space, dupfilter_time, and track_param.
* updated the track_param function to use the geosphere package instead of the trip package.
* The map_track function has been replaced with the "to_map" function.


#### Version 2.2.0 (2022-01-10)

* added an option in the asymptote function to estimate the confidence intervals for bootstrapped overlap estimates. The new default is to use the CIs when considering an asymptote.
* added a new function 'combn_overlap' as an alternative to 'boot_overlap'. This new function calculates overlaps between all possible combination of UDs relative to sample size.
* improved the processing speed of 'boot_overlap'.

#### Version 2.1.2 (2021-10-31)

* fixed a minor bug related with the max.asymptote argument within the asymptote function.

#### Version 2.1.1 (2021-07-04)

* amended the ddfilter, dupfilter, and their depending functions (e.g. ddfilter_speed) to return movement parameters used in each function.

#### Version 2.1.0 (2021-04-02)
* improved the asymptote function.
* added a new citation for functions 'boot_overlap', 'boot_area' and 'asymptote'. Shimada T, Thums M, Hamann M, Limpus CJ, Hays GC, FitzSimmons NN, Wildermann NE, Duarte CD, Meekan MG (2021) Optimising sample sizes for animal distribution analysis using tracking data. Methods in Ecology and Evolution, 12(2):288-297. doi:10.1111/2041-210X.13506
* updated the README file.
* improved the R documentations of functions asymptote, boot_overlap, and boot_area.
* renamed the example data "curtis" to "flatback".


#### Version 2.0.1 (2020-07-04)

* fixed bugs in the track_param function. 
* The input qi values can be now either the number of GPS satellites or Argos Location Classes. Argos Location Classes will be converted to numerical values, where "A", "B", "Z" will be replaced with "-1", "-2", "-3" respectively.
* The input DateTime (GMT) can be now either in class POSIXct or Character with the following format "2012-06-03 01:33:46".
* improved the R documentations.

#### Version 2.0.0 (2020-05-04)

* added four new functions (boot_overlap, boot_area, asymptote, percent_vol) to assess sample sizes of animal tracking data.
* added a new function (track_param) to calculate some parameters of animal tracking data (i.e. time, distance, speed, angle between locations).
* added a new function (kml_track) to visualise the tracking data.
* added a new example data "curtis".
* added a new argument "type" in the depthfilter function.
* renamed some functions (ddfilter.loop -> ddfilter_loop; ddfilter.speed -> ddfilter_speed; dupfilter.exact -> dupfilter_exact; dupfilter.qi -> dupfilter_qi; dupfilter.space -> dupfilter_space; dupfilter.time -> dupfilter_time, distantfilter -> distfilter).
* renamed an argument in ddfilter (maxvlp -> vmaxlp).
* renamed the plotMap to map_track.
* improved the map_track function.
* improved the R documentations.
* improved the processing speed of the dupfilter function and its dependent functions.
* updated the vmax and maxvlp functions. In the previous version, when input data contain duplicate locations, the function ceased with error messages. These errors are now avoided by removing duplicate locations using the dupfilter function prior to calculation of vmax and maxvlp.


#### Version 1.2.1 (2019-02-19)

* amended "plotMap" in accordance with the ggsn update to version 0.5.0. 

#### Version 1.2.0 (2019-02-18)

* added a mew function "distance_filter". This function removes fixes located beyond a given threshold distance.
* fixed bugs in the example codes.
* amended "plotMap" in accordance with the ggmap update to version 3.0.0. 

#### Version 1.1.1 (2018-06-10)

* fixed minor bugs in the est.maxvlp function. The values estimated using the previous version would not have been affected by the changes. In the previous version, when sample size is too small (i.e. <6 fixes) or inner angles could not be estimated (e.g. two consecutive locations with the same coordinates), the function ceased with warning messages. These errors are now avoided by exuding those fixes from the estimation process.

#### Version 1.1.0 (2018-04-27)

* added a new function (plotMap) to plot locations.
* updated vignette.

#### Version 1.0.0 (2018-02-08)

* First official release.





