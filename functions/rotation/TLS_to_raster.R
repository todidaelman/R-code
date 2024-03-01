TLS_to_raster <- function(file_names, raster) {
  
  #function to turn TLS pointclouds in a street into one combined raster (CDSM and TDSM). The raster will have the same extent as the input raster file
  
  CDSM_list <- list()
  TDSM_list <- list()
  
  
  for (file in file_names){
    las <- readLAS(file)
    CDSM_i <- grid_metrics(las, quantile(Z, probs = 0.99), res = 1)
    # CDSM_i <- grid_metrics(las, ~max(Z), res = 1)
    CDSM_i <- extend(CDSM_i, extent(DEM), NA)
    CDSM_list[[file]] <- resample(CDSM_i, DEM, method = "ngb")
    
    TDSM_i <- grid_metrics(las, ~quantile(Z, probs = 0.01), res = 1)
    # TDSM_i2 <- grid_metrics(las, ~min(Z), res = 1)
    TDSM_i <- extend(TDSM_i, extent(DEM), NA)
    TDSM_list[[file]] <- resample(TDSM_i, DEM, method = "ngb")
    
    print(file)
  }
  
  CDSM <- merge(stack(CDSM_list))
  TDSM <- merge(stack(TDSM_list))
  
  return(list(CDSM, TDSM))
  
}
