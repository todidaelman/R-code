raster_list_to_delta_df <- function(raster_list_full, scenario){
  
  ## inputs
  ### raster_list_full:     list with all simulated Tmrt values of the scenarios
  ### scenario:             name of the scenario you want to generate a dataframe of
  
  ## outputs:
  ### delta_Tmrt_shade_hour: a df that contains, for a given scenario, all non-zero delta values (Tmrt_scenario - Tmrt_notree) per hour
  
  #select index of scenario
  index <- grep(scenario, names(raster_list_full))[1]

  raster_list_scenario <- raster_list_full[[index]]
  raster_list_notree <- raster_list_full[[1]]
  
  raster_list_scenario_delta <- list()
  
  for (i in 1:length(raster_list_scenario)) {
    raster_list_scenario_delta[[i]] <- raster_list_scenario[[i]] - raster_list_notree[[i]]
  }
  
  
  delta_df_hour <- rasterlist_to_df(raster_list_scenario_delta)
  
  shade_list_output <- suppressMessages(shade_analyser(delta_df_hour, scenario_name = scenario))
  
  delta_df <- shade_list_output[[1]]
  
  return(delta_df)
}
