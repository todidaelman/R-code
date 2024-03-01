

DSM_background_df <- gplot_data(DSM_background)

# Function to display each raster
display_raster <- function(raster_image, titlevar) {
  
  raster_df <- gplot_data(raster_image)
  
  p <- ggplot(mapping = aes(x, y)) +
    geom_tile(data = dplyr::filter(DSM_background_df,!is.na(value)), aes(fill = value)) +
    scale_fill_gradientn(colours = grey.colors(100), guide = 'none') +
    theme(legend.position = "none") +
    new_scale_fill() +
    geom_tile(data = dplyr::filter(raster_df,!is.na(value)), aes(fill = value)) +
    scale_fill_viridis(option = "inferno",
                       limits = c(15, 70)) +
    labs(fill = "Tmrt (°C)") +
    theme_void() +
    ggtitle(paste0(7 + titlevar, "h00")) +
    theme(plot.title = element_text(hjust = 0.5,
                                    margin = margin(b = -5)))
  print(p)
    
}

# Save the animation as a GIF
saveGIF(
  {for (i in 1:length(individual_raster_list)) {
    display_raster(individual_raster_list[[i]], i)
    Sys.sleep(0.5)}
    },
  interval = 0.5, movie.name = "animation.gif")


