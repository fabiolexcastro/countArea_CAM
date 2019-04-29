

# Load data ---------------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse)

# Initial setup -----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

utm_15n <- '+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs' 
utm_16n <- '+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs' 
wgs_84 <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
lbl_aez <- data.frame(category = 1:9, 
                      clase = c('No idoneo', 'No idoneo', 'AEZ 1', 'AEZ 2', 'AEZ 3', 'AEZ 4', 'AEZ 5', 'Limitaciones', 'Aptitud incierta'))

# Functions to use --------------------------------------------------------
makeAreas <- function(cnt, cls, prd){
  # cnt <- 'gtm'; cls <- 'aez'; prd <- 'crn'
  rst <- grep(cnt, fls, value = T) %>%
    grep(cls, ., value = T) %>% 
    grep(prd, ., value = T) %>% 
    raster()
  crs(rst) <- wgs_84
  rst_prj <- projectRaster(rst, crs = utm_15n, method = 'ngb')
  area <- res(rst_prj)[1] * res(rst_prj)[2] / 10000
  rslt <- rasterToPoints(rst_prj) %>% 
    as.data.frame() %>%
    as_tibble() %>%
    setNames(c('x', 'y', 'category')) %>%
    group_by(category) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(has = count * area,
           miles = round(has / 1e3, 0) * 1000)
  print('Done!')
  return(rslt)
}

# Load data ---------------------------------------------------------------
fls <- list.files('../tif', full.names = T, pattern = '.tif$') 

# Guatemala
gtm_crn_aez <- makeAreas(cnt = 'gtm', cls = 'aez', prd = 'crn')
gtm_crn_aez <- inner_join(gtm_crn_aez, lbl_aez, by = 'category')
sum(gtm_crn_aez$count)



