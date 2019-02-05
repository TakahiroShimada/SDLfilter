---
output: html_notebook
editor_options: 
  chunk_output_type: inline
---
## News for Package 'SDLfilter'


#### Version 1.2.0 (2019-02-05)

* A mew function "distance_filter" is added. This function removes fixes located beyond a given threshold distance.
* fixed bugs in the example codes.
* "plotMap" amended in accordance with the ggmap update to version 3.0.0. 

#### Version 1.1.1 (2018-06-10)

* fixed minor bugs in the est.maxvlp function. The values estimated using the previous version would not have been affected by the changes. In the previous version, when sample size is too small (i.e. <6 fixes) or inner angles could not be estimated (e.g. two consecutive locations with the same coordinates), the function ceased with warning messages. These errors are now avoided by exuding those fixes from the estimation process.

#### Version 1.1.0 (2018-04-27)

* added a new function (plot.map) to plot locations.
* updated vignette.

#### Version 1.0.0 (2018-02-08)

* First official release.





