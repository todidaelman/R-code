############# explanation for script #################

## script to generate all the input for SOLWEIG: rotated rasters (as specified by the "rotation angles" array) to be used as input in SOLWEIG model

#####################################################

## input files / path to input files

source("raster_rotation/input/input_rotation_script.R")
### EXTRA TO DO: source all functions in folder "functions"!!!


## create extent files
# first input: street polygon

extent_street <- st_read(extent_file_path)

extent_i <- extent(extent_street) 

extent_square <- extent_square_fun(extent_i)
extent_crop <- upscale_extent_fun(extent_square) #the extent to what all rasters will be cropped AFTER rotation
extent_rotate <- upscale_extent_fun(upscale_extent_fun(extent_crop)) #the extent to what all rasters need to be cropped BEFORE rotation

## initialise lists for results

rotation_angles <- c(0, 30, 30 + seq(45,315,45))
street_angles <- -(rotation_angles + 15) + 360
raster_rotation_list <- list()

for (i in 1:length(rotation_angles)) {
  
  #for every rotation: list with DSM/DEM/CDSM/TDSM/landcover/street rasters
  rotation_i_list <- list()
  generated_output_names <- c("DSM", "DEM", "CDSM", "TDSM", "landcover", "street_raster")
  
  
  #base case: 0° rotation
  if (i == 1) {
    
    # basecase: no rotation to be performed
    
    DSM_vl <- raster(file.path(DSM_vl_path))
    DTM_vl <- raster(file.path(DTM_vl_path))
    
    # first crop -> larger extent for rotation later
    
    GRB_rotate <- st_crop(GRB_vl, extent_rotate)
    DSM_vl_rotate <- crop(DSM_vl, extent_rotate)
    
    #mask to cut out with a polygon mask layer, cover to fill in NA values with values of DEM
    DEM_rotate <- crop(DTM_vl, extent_rotate)
    DSM_rotate <- cover(mask(DSM_vl_rotate, GRB_rotate), DEM_rotate)
    
    landcover_rotate <- raster(ext = extent(extent_rotate), res = 1) #initialising raster
    landcover_rotate <- rasterize(GRB_rotate, landcover, fun = 'first')

    landcover_rotate[!is.na(landcover_rotate)] <- 2
    landcover_rotate[is.na(landcover_rotate)] <- 1
    
    #TO DO: function to improve!!
    
    TLS_file_names <- list.files(path = TLS_tree_folder_path, full.names = TRUE)
    
    TLS_list <- TLS_to_raster(TLS_file_names, DSM_rotate)
    CDSM_rotate <- TLS_list[[1]]
    TDSM_rotate <- TLS_list[[2]]
    
    street_rasterized <- rasterize(extent_street, DEM_rotate)
    street_raster_rotate <- mask(street_rasterized, GRB_rotate, inverse = TRUE)
    
    # second crop -> basecase at 0° rotation
    
    raster_names_list <- list(DSM_rotate, DEM_rotate, CDSM_rotate, TDSM_rotate, landcover_rotate, street_raster_rotate)
    
    #perform crop
    rotation_i_list <- lapply(raster_names_list, function(r) crop(r, extent_crop))
    names(rotation_i_list) <- generated_output_names
    
    #save results
    raster_rotation_list[[i]] <- rotation_i_list
    
  }
  
  rotation_angle <- rotation_angles[i]
  
  #perform rotation and cropping
  rotation_i_list <- lapply(raster_names_list, function(r) rotate_crop_fun(r, extent_crop, rotation_angle))
  names(rotation_i_list) <- generated_output_names
  
  #save results
  raster_rotation_list[[i]] <- rotation_i_list
  
  #visual check
  plot(rotation_i_list$DSM) + plot(rotation_i_list$street_raster, add = TRUE, col = "white")
  
  print(rotation_angle)
}

names(raster_rotation_list) <- paste0("r_", rotation_angles)

## saving the list
# saveRDS(raster_rotation_list, file = "raster_rotation/output/raster_rotation_45_list.RData")

# reading the list
# readRDS("raster_rotation/output/raster_rotation_list.RData")


# Specify the output folder
output_folder <- "../generated SOLWEIG input/kareldierickx_rotation"

save_generated_input_list(raster_rotation_list, output_folder)


###################################### extra checking (not needed) ####################################


df <- data.frame(rotation_angle = names(raster_rotation_list), 
                 street_angle = street_angles,
                 freq = names(raster_rotation_list))

# for (i in 1:length(names(raster_rotation_list))) {
#   
#   df$freq[i] <- freq(raster_rotation_list[[i]]$street_raster)[1,2]
#   
#   print(plot(raster_rotation_list[[i]]$street_raster, main = names(raster_rotation_list)[i]))
#   
# }


######################################## FUNCTIONS ##################################################

#squaring the extent 
extent_square_fun <- function(exent_i) {
  
  midpoint <- c(unlist(extent_i)[1] + (unlist(extent_i)[2] - unlist(extent_i)[1])/2, 
                unlist(extent_i)[3] + (unlist(extent_i)[4] - unlist(extent_i)[3])/2)
  
  max_dist <- max((unlist(extent_i)[2] - unlist(extent_i)[1])/2, (unlist(extent_i)[4] - unlist(extent_i)[3])/2)
  
  extent_square <- c(xmin = midpoint[1] - max_dist,
                     xmax = midpoint[1] + max_dist,
                     ymin = midpoint[2] - max_dist,
                     ymax = midpoint[2] + max_dist)
  
  extent_square <- round(extent_square)
  
  return(extent_square)
  
}


upscale_extent_fun <- function(extent_square){
  
  midpoint <- c(unlist(extent_square)[1] + (unlist(extent_square)[2] - unlist(extent_square)[1])/2, 
                unlist(extent_square)[3] + (unlist(extent_square)[4] - unlist(extent_square)[3])/2)
  
  z <- (unlist(extent_square)[2] - unlist(extent_square)[1])
  
  max_dist_big <- sqrt(z^2 + z^2)/2
  
  extent_big <- c(midpoint[1] - max_dist_big,
                  midpoint[1] + max_dist_big,
                  midpoint[2] - max_dist_big,
                  midpoint[2] + max_dist_big)
  
  names(extent_big) <- c("xmin", "xmax", "ymin", "ymax")
  
  extent_big <- round(extent_big)
  
  return(extent_big)
}








