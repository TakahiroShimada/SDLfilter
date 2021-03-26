<!-- README.md is generated from README.Rmd. Please edit that file -->

SDLfilter
=========

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3631115.svg)](https://doi.org/10.5281/zenodo.3631115)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/SDLfilter)](https://cran.r-project.org/package=SDLfilter)

Overview
--------

SDLfilter contains a variety of functions to screen GPS/Argos locations
and to assess the adequacy of sample size of tracking data for animal
distribution analysis.

Installation
------------

``` r
# The official version from CRAN:
install.packages("SDLfilter")

# Or the development version from GitHub:
install.packages("devtools")
devtools::install_github("TakahiroShimada/SDLfilter")
```

Usage
-----

``` r
library(SDLfilter)
```

### 1. Location filtering

There are three main filtering functions.

-   *dupfilter* filters temporal and spatial duplicates.  
-   *ddfilter* filters locations with high error.  
-   *depthfilter* filters locations by water depth.

<!-- <p>&nbsp;</p> -->

##### 1-1. Load tracking data

``` r
data(turtle)
```

##### 1-2. Remove temporal and spatial duplicates

``` r
turtle.dup <- dupfilter(turtle)
```

##### 1-3. Remove biologically unrealistic fixes

``` r
## Calculate the maximum linear speed between two consecutive locations
V <- vmax(turtle.dup)  

## Calculate the maximum one-way linear speed of a loop trip
VLP <- vmaxlp(turtle.dup) 

## Run ddfilter
turtle.dd <- ddfilter(turtle.dup, vmax=V, vmaxlp=VLP)
```

##### 1-4. Plot data

``` r
 # Entire area
 p1 <- map_track(turtle.dup, bgmap=Australia, point.size = 2, line.size = 0.5, axes.lab.size = 0, 
             sb.distance=200, multiplot = FALSE, point.bg = "red",
             title.size=15, title="Entire area")[[1]] + 
   geom_point(aes(x=lon, y=lat), data=turtle.dd, size=2, fill="yellow", shape=21)+
   geom_point(aes(x=x, y=y), data=data.frame(x=c(154, 154), y=c(-22, -22.5)), 
              size=3, fill=c("yellow", "red"), shape=21) + 
   annotate("text", x=c(154.3, 154.3), y=c(-22, -22.5), label=c("Retained", "Removed"), 
            colour="black", size=4, hjust = 0)

 # Zoomed in
 p2 <- map_track(turtle.dup, bgmap=SandyStrait, xlim=c(152.7, 153.2), ylim=(c(-25.75, -25.24)), 
             axes.lab.size = 0, sb.distance=10, point.size = 2, point.bg = "red", line.size = 0.5, 
             multiplot = FALSE, title.size=15, title="Zoomed in")[[1]] + 
 geom_path(aes(x=lon, y=lat), data=turtle.dd, size=0.5, colour="black", linetype=1) + 
 geom_point(aes(x=lon, y=lat), data=turtle.dd, size=2, colour="black", shape=21, fill="yellow")
 
 ## plot
 gridExtra::grid.arrange(p1, p2, layout_matrix=cbind(1,2))
```

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

### 2. Assessing sample sizes

#### Probability-based approach

##### 2-1. Load utilisation distributions of flatback turtles (n = 29).

The input data can be either a matrix or a list of RasterLayer objects.
Each row of the matrix or each RasterLayer object contains the
probability distribution of an animal. The function assumes that each
column of a matrix is associated with a unique geographical location,
therefore it is critical that the grid size and geographical extent are
consistent across UDs. In this example, the grid size was 1km and the
geographical extent was 1901789, 1972789, -2750915, -2653915 (EPSG:3577)
across all 29 layers.

``` r
data(curtis)
```

##### 2-2. Calculate overlap probability from 3000 random permutation (\~sample size x 100).

It will take some time to run this code depending on the number of
iterations and the machine specs. The runtime was about 7 minutes for
3000 iterations on a linux machine (Intel i7-8650U CPU @ 1.90GHz, 32GB
RAM).

``` r
overlap <- boot_overlap(curtis, R = 3000, method = "PHR")
```

##### 2-3. Find the minimum sample size required to estimate the general distribution.

As described in the main text, an asymptote was considered once the mean
overlap probability exceeded 95% of the estimated horizontal asymptote.
The sample size linked to this value was deemed to be the minimum sample
size required to represent the general distribution of the group.

``` r
a <- asymptote(overlap, upper.degree = 10)
```

##### 2-4. Plot the mean probability and rational function fit relative to the sample sizes (n).

``` r
ggplot(data = overlap$summary)+
  geom_point(aes(x = N, y = mu), alpha = 0.5) + 
  geom_path(data = a$results, aes(x = x, y = ys)) + 
  geom_vline(xintercept = a$min.n, linetype = 2) +
  scale_x_continuous(breaks = seq(0, 30, 5), limits = c(2,29), name = "Animals tracked (n)") +
  scale_y_continuous(limits = c(0,1), name = "Overlap probability")
```

![](README_files/figure-markdown_github/unnamed-chunk-10-1.png)

> Please see the package help pages and Shimada et al. (2012, 2016,
> 2021) for more details.

References
----------

If you use the function *ddfilter*, please cite

Shimada T, Jones R, Limpus C, Hamann M (2012) Improving data retention
and home range estimates by data-driven screening. *Mar Ecol Prog Ser*
457:171-180 doi:
[10.3354/meps09747](http://dx.doi.org/10.3354/meps09747)

If you use the functions *dupfilter* or *depthfilter*, please cite

Shimada T, Limpus C, Jones R, Hazel J, Groom R, Hamann M (2016) Sea
turtles return home after intentional displacement from coastal foraging
areas. *Mar Biol* 163:1-14 doi:
[10.1007/s00227-015-2771-0](http://dx.doi.org/10.1007/s00227-015-2771-0)

If you use the functions *boot\_overlap* or *boot\_area*, please cite

Shimada, T, Thums, M, Hamann, M, et al. (2021) Optimising sample sizes
for animal distribution analysis using tracking data. *Methods Ecol
Evol* 12(2):288-297 doi:
[10.1111/2041-210X.13506](https://doi.org/10.1111/2041-210X.13506)

Current version
---------------

2.0.1.0009 (26 March 2021)
