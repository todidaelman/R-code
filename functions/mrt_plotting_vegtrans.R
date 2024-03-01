mrt_plotting_vegtrans <- function(raster_list, background_raster, control = TRUE) {
  
  # -----------------------------
  
  # function to creating a heat map of Tmrt values on a grey raster background (e.g. DSM with building height info)
  
  ### input:
  
  ### raster_list:  list containing rasters that each want to be plotted (can be different variables, timesteps, ...)
  ### background:   raster with same spatial extent and resolution that you want to display as the background in greyscale (e.g. DSM)
  ### control:      logical value, if TRUE than the first item in the list is considered as the control raster with no vegetation and title is different.
  
  ### output:
  
  ### ggplot_list: list of ggplots 
  
  #------------------------------
  
  DSM_background_df <- gplot_data(background_raster)

  source("functions/gplot_data.r")
  
  plotfun <- function(background_df, raster_i_df) {
    
    plot_temp <- ggplot(mapping = aes(x, y)) +
      geom_tile(data = dplyr::filter(background_df, !is.na(value)), aes(fill = value)) +
      scale_fill_gradientn(colours = grey.colors(100), guide = 'none') +
      theme(legend.position = "none") +
      new_scale_fill() +
      geom_tile(data = dplyr::filter(raster_i_df, !is.na(value)), aes(fill = value)) +
      scale_fill_viridis(option = "inferno",
                         limits = c(30,60)) +
      labs(fill = "Tmrt (°C)") +
      coord_equal() +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5))
    
    return(plot_temp)
  }
  
  ggplot_list <- list()
    
  for (i in 1:length(raster_list)) {
    
    raster_i_df <- gplot_data(raster_list[[i]])
    
    print(paste0(i, "/", length(raster_list)))
    
    if (control == TRUE && i == 1) {
      
      ggplot_list[[i]] <- plotfun(background_df = DSM_background_df, raster_i_df) +
        ggtitle("No vegetation") 
      
      # ggplot_list[[i]] <- ggplot(mapping = aes(x, y)) +
      #   geom_tile(data = dplyr::filter(DSM_background_df,!is.na(value)), aes(fill = value)) +
      #   scale_fill_gradientn(colours = grey.colors(100), guide = 'none') +
      #   theme(legend.position = "none") +
      #   new_scale_fill() +
      #   geom_tile(data = dplyr::filter(raster_i_df,!is.na(value)), aes(fill = value)) +
      #   scale_fill_viridis(option = "inferno",
      #                      limits = c(30,60)) +
      #   labs(fill = "Tmrt (°C)") +
      #   ggtitle("No vegetation") +
      #   theme(plot.title = element_text(hjust = 0.5)) +
      #   coord_equal() +
      #   theme_void() +
      #   scale_x_continuous(limits = c(xmin(background_raster),
      #                                 xmax(background_raster))) +
      #   scale_y_continuous(limits = c(ymin(background_raster),
      #                                 ymax(background_raster))) +
      #   theme(plot.title = element_text(hjust = 0.5))
      
    } else {
      
      ggplot_list[[i]] <- plotfun(background_df = DSM_background_df, raster_i_df) + 
        ggtitle(paste("Transmission =", (i-2) * 5 + 5, "%"))
    
    }
  }
  return(ggplot_list)
}

  
 
  
  
  
  