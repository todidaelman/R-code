save_generated_input_list <- function(output_list, output_folder) {
  # Create the output folder if it doesn't exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  # Iterate over each list in the output list
  for (i in seq_along(output_list)) {
    # Create a subfolder for each list
    sublist_name <- names(output_list)[i]
    sublist_folder <- file.path(output_folder, sublist_name)
    if (!dir.exists(sublist_folder)) {
      dir.create(sublist_folder)
    }
    
    # Iterate over each raster in the sublist
    sublist <- output_list[[i]]
    for (j in seq_along(sublist)) {
      raster_file <- sublist[[j]]
      raster_name <- names(sublist)[j] # paste0("raster_", j, ".tif")  # or adjust as needed
      raster_path <- file.path(sublist_folder, raster_name)
      writeRaster(raster_file, filename = raster_path, format = "GTiff", overwrite = TRUE)
    }
  }
}
