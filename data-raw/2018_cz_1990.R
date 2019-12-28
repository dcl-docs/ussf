# Create boundaries of US 1990 commuting zones in R sf format using US Census
# Bureau shapefiles

# Author: Bill Behrman
# Version: 2019-12-27

# Libraries
library(tidyverse)
library(readxl)
library(sf)
library(stringr)

# Parameters
  # Boundary year
year <- "2018"
  # URL for US Census Bureau shapefiles
url_cb <- str_glue("http://www2.census.gov/geo/tiger/GENZ{year}/shp")
  # URL for commuting zone county partition using 1990 counties
url_cz <- "https://www.ers.usda.gov/webdocs/DataFiles/48457/czlma903.xls?v=40961"
  # Boundary resolutions
resolutions <- c("20m", "5m", "500k")
  # FIPS codes for US states and District of Columbia
fips_states <-
  c(
    "01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", "15",
    "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27",
    "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
    "40", "41", "42", "44", "45", "46", "47", "48", "49", "50", "51", "53",
    "54", "55", "56"
  )
  # Tranformation parameters for Alaska and Hawaii
alaska_crop <- c(xmin = -172, xmax = -125, ymin = 50, ymax = 75)
alaska_scale <- 0.35
alaska_x_offset <- 0.095
alaska_y_offset <- 0.150
hawaii_crop <- c(xmin = -165, xmax = -150, ymin = 15, ymax = 25)
hawaii_scale <- 1
hawaii_x_offset <- 0.305
hawaii_y_offset <- 0.065
  # Coordinate reference systems
longlat <- "+proj=longlat +datum=WGS84 +no_defs"
albers_contig <-
  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +no_defs"
albers_alaska <-
  "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=WGS84 +no_defs"
albers_hawaii <-
  "+proj=aea +lat_1=8 +lat_2=18 +lat_0=13 +lon_0=-157 +x_0=0 +y_0=0 +datum=WGS84 +no_defs"
  # FIPS codes for counties deleted since 1990 census
counties_deleted <-
  c(
    "02201", "02231", "02270", "02280", "12025", "30113", "46113", "51515",
    "51560", "51780"
  )
  # FIPS codes for counties added since 1990 census with 1990 commuting zone
counties_added <-
  tribble(
    ~fips_county, ~cz_1990,
    "02068", "34115",
    "02105", "34109",
    "02158", "34112",
    "02195", "34110",
    "02198", "34111",
    "02230", "34109",
    "02275", "34110",
    "02282", "34109",
    "08014", "28900",
    "12086", "07000",
    "46102", "27704"
  )
  # Rename commuting zones
rename_cz <-
  c(
    "34103" = "Valdez-Cordova"
  )
  # Output file compression
compress <- "gz"
  # Temporary directory
tmp <- str_glue("{tempdir()}/2019_cz_1990")

#===============================================================================

# Create temporary directory
if (!file.exists(tmp)) {
  dir.create(tmp, recursive = TRUE)
}

# Download commuting zones county partition
dest <- str_glue("{tmp}/czlma903.xls")
if (download.file(url = url_cz, destfile = dest, quiet = TRUE)) {
  stop("Error: Commuting zones download failed")
}

# Read commuting zone county partition, add place and state variables
cz <-
  read_excel(dest, sheet = "CZLMA903", na = ".") %>%
  select(
    fips_county = "County FIPS Code",
    cz_1990 = CZ90,
    place_state = "Name of largest place in Commuting Zone"
  ) %>%
  mutate(
    place =
      place_state %>%
      str_remove(" borough.*| CDP.*| city.*| town.*| \\(rem.*|,.*"),
    state = place_state %>% str_sub(start = -2L)
  ) %>%
  select(-place_state)

# Adjust county partition for counties added and deleted since 1990
cz <-
  counties_added %>%
  left_join(cz %>% select(-fips_county) %>% distinct(), by = "cz_1990") %>%
  bind_rows(cz, .) %>%
  filter(!fips_county %in% counties_deleted)

# Rename commuting zones
for (zone in names(rename_cz)) {
  cz$place[cz$cz_1990 == zone] <- rename_cz[zone]
}

# For resolution
for (resolution in resolutions) {

  # Boundary specification
  boundary <- str_glue("cb_{year}_us_county_{resolution}")

  # Download and unzip US Census Bureau shapefile
  url <- str_glue("{url_cb}/{boundary}.zip")
  dest <- str_glue("{tmp}/{boundary}.zip")
  if (download.file(url = url, destfile = dest)) {
    print(str_glue("Error: Download for {boundary} failed"))
    next
  }
  unzip(zipfile = dest, exdir = str_glue("{tmp}/{boundary}"))

  # sf object for commuting zones for states and District of Columbia
  us <-
    read_sf(dsn = str_glue("{tmp}/{boundary}/{boundary}.shp")) %>%
    filter(STATEFP %in% fips_states) %>%
    left_join(cz, by = c("GEOID" = "fips_county")) %>%
    group_by(cz_1990, place, state) %>%
    summarize() %>%
    ungroup() %>%
    st_cast("MULTIPOLYGON")

  # Version with longitude and latitude projection
  var <- str_glue("cz_{resolution}_longlat")
  us %>%
    st_transform(crs = longlat) %>% 
    assign(x = var, value = ., envir = .GlobalEnv)

  # Version with Albers projection and Alaska and Hawaii below other states
  contig <-
    us %>%
    filter(!state %in% c("AK", "HI")) %>%
    st_transform(crs = albers_contig)

  bbox_contig <- st_bbox(contig)

  position <- function(bbox, x_offset, y_offset) {
    st_sfc(st_point(c(
      bbox$xmin + x_offset * (bbox$xmax - bbox$xmin),
      bbox$ymin + y_offset * (bbox$ymax - bbox$ymin)
    )))
  }

  alaska <-
    us %>%
    filter(state == "AK") %>%
    st_crop(alaska_crop) %>%
    st_transform(crs = albers_alaska) %>%
    st_set_geometry(
      (st_geometry(.) - st_centroid(st_union(st_geometry(.)))) *
        alaska_scale +
        position(
          bbox = bbox_contig,
          x_offset = alaska_x_offset,
          y_offset = alaska_y_offset
        )
    ) %>%
    st_set_crs(value = albers_contig)

  hawaii <-
    us %>%
    filter(state == "HI") %>%
    st_crop(hawaii_crop) %>%
    st_transform(crs = albers_hawaii) %>%
    st_set_geometry(
      (st_geometry(.) - st_centroid(st_union(st_geometry(.)))) *
        hawaii_scale +
        position(
          bbox = bbox_contig,
          x_offset = hawaii_x_offset,
          y_offset = hawaii_y_offset
        )
    ) %>%
    st_set_crs(value = albers_contig)

  var <- str_glue("cz_{resolution}_albers")
  rbind(contig, alaska, hawaii) %>%
    assign(x = var, value = ., envir = .GlobalEnv)
}

# Remove temporary directory
if (unlink(tmp, recursive = TRUE, force = TRUE)) {
  print("Error: Remove temporary directory failed")
}
