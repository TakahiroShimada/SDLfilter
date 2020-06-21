---
editor_options:
  chunk_output_type: inline
output:
  html_document:
    df_print: paged
---
News for Package 'SDLfilter'
=========

#### Version 2.0.1 (2020-05-16)

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





