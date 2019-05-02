
# Load data ---------------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse)

# Initial setup -----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

utm_15n <- '+proj=utm +zone=15 +datum=WGS84 +units=m +no_defs' 
utm_16n <- '+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs' 
utm_17n <- '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs'
utm_19n <- '+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs' 
wgs_84 <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
lbl_aez <- data.frame(category = 1:9, 
                      clase = c('No idoneo', 'No idoneo', 'AEZ 1', 'AEZ 2', 'AEZ 3', 'AEZ 4', 'AEZ 5', 'Limitaciones', 'Aptitud incierta'))
sst_utm <- data.frame(countries = c('Guatemala', 'Nicaragua', 'El Salvador', 'Honduras', 'Costa Rica', 'Belice', 'Republica Dominicana'),
                      sistema = c('utm_16n', 'utm_17n', 'utm_16n', 'utm_16n', 'utm_17n', 'utm_16n', 'utm_19n'),
                      pais = as.character(c('gtm', 'nic', 'slv', 'hnd', 'cri', 'blz', 'rdm')))
sst_utm$pais <- as.character(sst_utm$pais)

# Functions to use --------------------------------------------------------
makeAreas <- function(cnt, cls, prd){
  # cnt <- 'crc'; cls <- 'aez'; prd <- 'crn'
  print(cnt)
  rst <- grep(cnt, fls, value = T) %>%
    grep(cls, ., value = T) %>% 
    grep(prd, ., value = T) %>% 
    raster()
  crs(rst) <- wgs_84
  
  st <- data.frame(pais = as.character(cnt)) %>% mutate(pais = as.character(pais))
  st <- inner_join(st, sst_utm, by = c('pais' = 'pais')) %>% pull(3) %>% as.character()
  
  if(st == 'utm_15n'){
    rst_prj <- projectRaster(rst, crs = utm_15n, method = 'ngb')
  } else if(st == 'utm_16n'){
    rst_prj <- projectRaster(rst, crs = utm_16n, method = 'ngb')
  } else if(st == 'utm_17n'){
    rst_prj <- projectRaster(rst, crs = utm_17n, method = 'ngb')
  } else if(st == 'utm_19n'){
    rst_prj <- projectRaster(rst, crs = utm_19n, method = 'ngb')
  }
    
  area <- res(rst_prj)[1] * res(rst_prj)[2] / 10000
  rslt <- rasterToPoints(rst_prj) %>% 
    as.data.frame() %>%
    as_tibble() %>%
    setNames(c('x', 'y', 'category')) %>%
    group_by(category) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(has = count * area,
           miles = round(has / 1e3, 0) * 1000,
           country = cnt,
           clase = cls,
           periodo = prd) %>% 
    dplyr::select(category, miles, country, clase, periodo)

  write.csv(rslt, paste0('../tbl/tbl_', cnt, '_', cls, '_', prd, '.csv'), row.names = F)
  print('Done!')
  return(rslt)
}

# Load data ---------------------------------------------------------------
fls <- list.files('../tif', full.names = T, pattern = '.tif$') 

str_sub(fls, start = 12, end = 14) %>%
  table() %>%
  as.data.frame()

# Apply the function AEZ --------------------------------------------------
cl <- c('aez', 'imp')
tbl_crn <- lapply(1:length(sst_utm$pais), function(p){
  makeAreas(cnt = sst_utm$pais[p], cls = 'aez', prd = 'crn')
})
tbl_30 <- lapply(1:length(sst_utm$pais), function(p){
  lapply(1:2, function(k){
    makeAreas(cnt = sst_utm$pais[p], cls = cl[k], prd = '30')  
  })
})
tbl_50 <- lapply(1:length(sst_utm$pais), function(p){
  lapply(1:2, function(k){
    makeAreas(cnt = sst_utm$pais[p], cls = cl[k], prd = '50')  
  })
})
tbl_crn <- bind_rows(tbl_crn)
tbl_30 <- tbl_30 %>% 
  purrr::flatten() %>% 
  dplyr::bind_rows()
tbl_50 <- tbl_50 %>% 
  purrr::flatten() %>% 
  dplyr::bind_rows()
tbl_all <- rbind(tbl_crn, tbl_30, tbl_50)
write.csv(tbl_all, '../tbl/tbl_areas_all.csv', row.names = FALSE)
