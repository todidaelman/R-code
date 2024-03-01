mrtaverage <- function(simulation_output_folder, clip_extent, time_of_interest = "D") {
  # -----------------------------
  #simulation_output_folder: folder where simulaiton output folders are stored. automatically looks for "transveg" into these folders and "no_trees"
  #clip_extent: loaded in polygon (sf class) containing the boundaries of where to clip the results too (e.g. street)
  #time_of_interest: average to be calculated over which time frame? D = day (07h00 - 22h00) or N = night (23h00 - 06h00)
  
  time_of_interest = "D"
  
  # Get the list of all folders starting with "simulation_"
  folder_list_unfiltered <- list.dirs(path = simulation_output_folder, full.names = TRUE, recursive = FALSE)
  
  folder_list <- folder_list_unfiltered[grep("transveg", folder_list_unfiltered)]
  
  # Extract numeric values from folder names
  numeric_values <- as.numeric(sub(".*simulation_transveg_(\\d+)$", "\\1", folder_list))
  
  # Order the folder list based on numeric values
  folder_list <- folder_list[order(numeric_values)]
  
  # add basecase first
  # Find the index of the folder containing "no_trees"
  index_no_trees <- grep("no_trees", folder_list_unfiltered)
  folder_list <- c(folder_list_unfiltered[index_no_trees], folder_list)
  
  # Initialize an empty list to store the rasters
  raster_list <- list()
  
  # Initialize an empty list to store the rasters
  raster_list <- list()
  
  # Loop over each folder
  for (folder in folder_list) {
    # Get the list of .tif files ending with "D"
    
    pattern_string <- paste0("^Tmrt.*", time_of_interest, "\\.tif$")
    tif_files_variable <- list.files(path = folder, pattern = pattern_string, full.names = TRUE)
    
    print(folder)
    
    # Initialize an empty list to store individual rasters for averaging
    individual_raster_list <- list()
    
    # Loop over each .tif file ending with "D"
    for (tif_file in tif_files_variable) {
      # Read the raster file
      raster <- raster(tif_file)
      
      # Read the buildings raster file
      buildings_raster <- raster(file.path(folder, "buildings.tif"))
      
      # Set the cells corresponding to buildings (value 0) to NA
      raster[buildings_raster[] == 0] <- NA
      
      # Clip the raster to the extent of the shapefile
      masked_raster <- mask(raster, clip_extent)
      
      # Add the masked raster to the individual list
      individual_raster_list[[length(individual_raster_list) + 1]] <- masked_raster
      
    }
    
    # Calculate the average of the individual rasters
    if (length(individual_raster_list) > 0) {
      average_raster <- calc(stack(individual_raster_list), mean, na.rm = TRUE)
      
      # Add the average raster to the list
      raster_list[[length(raster_list) + 1]] <- average_raster
      print(cellStats(average_raster, stat = mean))
    }
  }
  return(raster_list)
}





# 
# 
# time = "D"
# 
# 
# # Get the list of all folders starting with "simulation_"
# folder_list_unfiltered <- list.dirs(path = simulation_output_folder, full.names = TRUE, recursive = FALSE)
# 
# folder_list <- folder_list_unfiltered[grep("transveg", folder_list_unfiltered)]
# 
# # Extract numeric values from folder names
# numeric_values <- as.numeric(sub(".*simulation_transveg_(\\d+)$", "\\1", folder_list))
# 
# # Order the folder list based on numeric values
# folder_list <- folder_list[order(numeric_values)]
# 
# # add basecase first
# # Find the index of the folder containing "no_trees"
# index_no_trees <- grep("no_trees", folder_list_unfiltered)
# folder_list <- c(folder_list_unfiltered[index_no_trees], folder_list)
# 
# # Initialize an empty list to store the rasters
# raster_list <- list()
# 
# # Initialize an empty list to store the rasters
# raster_list <- list()
# 
# # Loop over each folder
# for (folder in folder_list) {
#   # Get the list of .tif files ending with "D"
#   tif_files_D <- list.files(path = folder, pattern = paste0(time, "\\.tif$"), full.names = TRUE)
#   
#   print(folder)
#   
#   # Initialize an empty list to store individual rasters for averaging
#   individual_raster_list <- list()
#   
#   # Loop over each .tif file ending with "D"
#   for (tif_file in tif_files_D) {
#     # Read the raster file
#     raster <- raster(tif_file)
#     
#     # Read the buildings raster file
#     buildings_raster <- raster(file.path(folder, "buildings.tif"))
#     
#     # Set the cells corresponding to buildings (value 0) to NA
#     raster[buildings_raster[] == 0] <- NA
#     
#     # Clip the raster to the extent of the shapefile
#     masked_raster <- mask(raster, clip_extent)
#     
#     # Add the masked raster to the individual list
#     individual_raster_list[[length(individual_raster_list) + 1]] <- masked_raster
#     
#   }
#   
#   # Calculate the average of the individual rasters
#   if (length(individual_raster_list) > 0) {
#     average_raster <- calc(stack(individual_raster_list), mean, na.rm = TRUE)
#     
#     # Add the average raster to the list
#     raster_list[[length(raster_list) + 1]] <- average_raster
#   }
# }
# 
# # Now, raster_list contains the average rasters for each folder


