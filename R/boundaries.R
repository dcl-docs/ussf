#' Boundaries for U.S. states, counties, or commuting zones
#'
#' Boundaries for U.S. states, counties, or commuting zones as sf object.
#'
#' @param geography  One of:
#'   * `"state"` (Default)
#'   * `"county"`
#'   * `"cz"` Commuting zone.
#' @param resolution  One of:
#'   * `"20m"` 1:20,000,000 (Default)
#'   * `"5m"` 1:5,000,000
#'   * `"500k"` 1:500,000
#' @param projection  One of:
#'   * `"albers"` Albers equal area with Alaska and Hawaii scaled and below
#'     contiguous states. (Default)
#'   * `"longlat"` Longitude and latitude.
#'   
#' @details The default 1:20,000,000 resolution is suitable for maps of the
#' entire U.S. The other two higher resolutions are suitable for maps of smaller
#' areas. The default Albers equal area projection is suitable for maps of the
#' entire U.S. The longitude and latitude projection is suitable for maps of
#' smaller areas.
#' 
#' @return  An sf object.
#'
#' @export
#'
#' @examples
#' counties <- boundaries(geography = "county")
#'
#' @source U.S. Census Bureau. \href{https://www2.census.gov/geo/tiger/GENZ2018/shp/}{2018 Cartographic Boundary Shapefiles.}
#' @source USDA Economic Research Service.
#' \href{https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas/}{1990 Commuting Zones.}
boundaries <- function(
  geography = c("state", "county", "cz"),
  resolution = c("20m", "5m", "500k"),
  projection = c("albers", "longlat")
) {
  geography <- match.arg(geography)
  resolution <- match.arg(resolution)
  projection <- match.arg(projection)
  
  get(str_glue("{geography}_{resolution}_{projection}"))
}
