delta_hour_plotting <- function(delta_Tmrt, scenario) {
  
  delta_Tmrt %>%
    filter(scenario == scenario) %>%
    filter(mm == "shade") %>%
    ggplot(aes(x = value, y = hour, fill = stat(x))) +
    geom_density_ridges_gradient(
      aes(point_fill = stat(x),
          point_colour = stat(x)),
      rel_min_height = 0.01,
      scale = 2,
      jittered_points = TRUE,
      position = position_points_jitter(width = 0.05, height = 0, yoffset = -0.1),
      point_shape = '|',
      point_size = 1, 
      point_alpha = 0.3, 
      alpha = 0.7
    ) +
    theme_minimal() +
    scale_fill_gradientn(colours = c("blue2", "green3", "white"),
                         limits = c(-30,1),
                         aesthetics = c("colour", "fill", "point_fill", "point_colour")) -> baseplot_without_sun
  
  
  baseplot_without_sun +
    geom_point(data = delta_Tmrt %>%
                 filter(scenario == scenario) %>%
                 filter(mm == "sun") %>%
                 sample_frac(1), #downsampling if wanted
               shape = "|",
               alpha = 0.7,
               colour = "grey") +
    scale_y_discrete(labels = paste(7:22, ":00", sep = "")) +
    scale_x_continuous(limits = c(-30,1)) +
    guides(colour = "none",
           point_fill = "none",
           point_colour = "none") +
    labs(fill = expression(Delta*"T"["MRT"]),
         y = '',
         x = expression(Delta*"T"["MRT"])) -> tempplot
  
  return(tempplot)
  
}
