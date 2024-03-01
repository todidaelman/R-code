mrt_cropping <- function(simulation_output_folder, clip_extent, search_variable = "transveg", time_of_interest = "D") {
  
  # -----------------------------
  
  # function to clip different simulation output SOLWEIG files automatically (e.g. 10 simulations of 1 day with each a varying vegetation transmissivity): N folders with P timesteps per folder
  
  ### input:
  
  ### simulation_output_folder: folder where simulation output folders are stored. Folder names should be ending with 'simulation_transveg' or another search variable if specified. automatically looks for "transveg" into these folders and "no_trees"
  ### clip_extent: loaded in polygon (sf class) containing the boundaries of where to clip the results too (e.g. street)
  ### time_of_interest: average to be calculated over which time frame? D = day (07h00 - 22h00) or N = night (23h00 - 06h00)
  
  ### output:
  
  ### raster_list: list of rasters that are clipped to the extent of interest (this is a nested list contains N lists each containing P rasters)
  
  #------------------------------
  
  # Get the list of all folders starting with "simulation_"
  folder_list_unfiltered <- list.dirs(path = simulation_output_folder, full.names = TRUE, recursive = FALSE)
  
  folder_list <- folder_list_unfiltered[grep(search_variable, folder_list_unfiltered)]
  
  # Extract numeric values from folder names
  numeric_values <- suppressWarnings(as.numeric(sub(paste0(".*simulation_", search_variable, "_(\\d+)$"), "\\1", folder_list)))
  
  # Order the folder list based on numeric values
  folder_list <- folder_list[order(numeric_values)]
  
  # add basecase first
  # Find the index of the folder containing "no_trees"
  index_no_trees <- grep(paste0("_no_trees"), folder_list_unfiltered)
  folder_list <- c(folder_list_unfiltered[index_no_trees], folder_list)
  # folder_list <- folder_list[-length(folder_list)]
  
  # Initialize an empty list to store the rasters
  raster_list <- list()
  
  # Store the names of all simulation scenarios from the folders
  scenario_names <- unlist(lapply(folder_list, function(x) tail(strsplit(x, "/")[[1]], n = 1)))
  
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
    
    # saving all individual rasters
    if (length(individual_raster_list) > 0) {

      raster_list[[length(raster_list) + 1]] <- individual_raster_list
      
    }
    
    # if (time_of_interest == "D" && length(individual_raster_list) != 16) {
    #   warning("Warning: when selecting the 'daily' hours (7 - 22h00), 16 rasters are expected (one for every hour) but a different amount is found.")
    # }
    # stopifnot(time_of_interest == "D" && length(individual_raster_list) != 16,
    #           "Error: when selecting the 'daily' hours (7 - 22h00), 16 rasters are expected (one for every hour) but a different amount is found.")
  }
  
  names(raster_list) <- scenario_names
  
  return(raster_list)
}
