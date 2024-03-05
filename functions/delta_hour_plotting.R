delta_hour_plotting <- function(delta_Tmrt, scenario) {
  
  delta_Tmrt %>%
    mutate(hour = factor(hour, 
                         levels = paste("hour", 7:22, sep = "_"),
                         labels = paste(7:22, ":00", sep = ""),
                         ordered = TRUE)) %>%
    filter(scenario == scenario) -> delta_Tmrt_scenario 
  
  delta_Tmrt_scenario %>%
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
    scale_fill_gradientn(colours = c("blue2", "green3", "lightgrey"),
                         limits = c(-30,1),
                         aesthetics = c("colour", "fill", "point_fill", "point_colour"))  +
    scale_y_discrete(drop = FALSE) -> baseplot_without_edge_effect
  
  
  baseplot_without_edge_effect +
    geom_point(data = delta_Tmrt_scenario %>%
                 filter(mm == "edge_effect") %>%
                 mutate(value = value) %>%
                 sample_frac(1), #downsampling if wanted
               aes(x = value, y = hour, colour = value),
               shape = "|",
               alpha = 0.7) +
    scale_x_continuous(limits = c(-30,1)) +
    guides(colour = "none",
           point_fill = "none",
           point_colour = "none") +
    labs(fill = expression(Delta*"T"["MRT"]),
         y = '',
         x = expression(Delta*"T"["MRT"])) -> tempplot
  
  return(tempplot)
  
}
