saving_of_ggplot_figures <- function(ggplot_list, folder_name = "animations", animationname = "animation"){
  
  width = 10
  height = 10
  
  for (i in 1:length(ggplot_list)) {
    ggsave(
      filename = paste0(folder_name, "/", animationname, "_", i, ".png"),
      plot = ggplot_list[[i]],
      width = width,
      height = height,
      bg = "white",
      dpi = 300
    )
    Sys.sleep(0.5)
  }

}
