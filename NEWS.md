---
editor_options:
  chunk_output_type: inline
output:
  html_document:
    df_print: paged
---
News for Package 'SDLfilter'
=========

#### Version 1.2.1.9017 (2020-03-21)

* added four new functions (boot_overlap, boot_area, asymptote, percent_vol) to assess sample sizes of animal tracking data.
* added a new function (track_param) to calculate some parameters of animal tracking data (i.e. time, distance, speed, angle between locations).
* added a new function (kml_track) to visualise the tracking data.
* added a new example data "curtis".
* added a new argument "type" in the depthfilter function.
* renamed some functions (ddfilter.loop -> ddfilter_loop; ddfilter.speed -> ddfilter_speed; dupfilter.exact -> dupfilter_exact; dupfilter.qi -> dupfilter_qi; dupfilter.space -> dupfilter_space; dupfilter.time -> dupfilter_time, distantfilter -> distfilter).
* renamed an argument in ddfilter (maxvlp -> vmaxlp).
* renamed the plotMap to map_track
* improved the map_track function.
* improved the R documentations.

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





