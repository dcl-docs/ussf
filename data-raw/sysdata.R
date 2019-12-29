# Save boundaries for states, counties, and commuting zones.

# Author: Bill Behrman
# Version: 2019-12-27

# Libraries
library(tidyverse)

# Parameters
  # Script to create boundaries for states and counties
script_state_country <- here::here("data-raw/2018_state_county.R")
  # Script to create boundaries for commuting zones
script_cz <- here::here("data-raw/2018_cz_1990.R")
  # Variable pattern
var_pattern <- "^(state|county|cz)_(20m|5m|500k)_(albers|longlat)$"
  # Output file
file_out <- here::here("R/sysdata.rda")

#===============================================================================

source(script_state_country)
source(script_cz)

stopifnot(length(ls(pattern = var_pattern)) == 18)

save(
  list = ls(pattern = var_pattern),
  file = file_out,
  version = 2,
  compress = "xz"
)
