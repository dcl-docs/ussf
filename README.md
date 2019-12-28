# U.S. Boundaries

U.S. boundaries for states, counties, and commuting zones as sf objects. Derived from U.S. Census Bureau 2018 cartographic boundaries.

You can install this R package with:

``` r
# install.packages("devtools")
devtools::install_github("dcl-docs/ussf")
```

Boundaries are available for each combination of the following:

* Geometry:
  * State
  * County
  * Commuting zone
* Resolution:
  * 1:20,000,000
  * 1:5,000,000
  * 1:500,000
* Projection:
  * Albers equal area with Alaska and Hawaii scaled and below contiguous states
  * Longitude and latitude
