library(tidyverse)
library(sf)
library(lubridate)
library(viridis)
library(raster)
library(png)
library(grid)
library(tidylog)
library(magrittr)
library(readxl)
library(purrr)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

`%!in%` <- Negate(`%in%`)
