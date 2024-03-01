rasterlist_to_df <- function(raster_list){
  
  #make values_hour variable empty (delete it) so a new one can be made for every scenario
  suppressWarnings(rm(df_hour))
  
  #iterate over the j hours in each scenario
  
  for (j in 1:length(raster_list)) {
    
    raster_j <- raster_list[[j]]
    df_j <- gplot_data(raster_j)
    
    if (!exists("df_hour")) {
      #initialise values_hour dataframe
      df_hour <- data.frame(matrix(
        NA,
        nrow = length(df_j$value),
        ncol = length(raster_list)
      ))
      colnames(df_hour) <- paste0("hour_", 7:22)
    }
    
    df_hour[j] <-  df_j$value
    
  }
  
  return(df_hour)
} 
