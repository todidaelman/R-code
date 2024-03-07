raster_list_to_delta_df <- function(raster_list_full, scenario, summary_index = 1){
  
  ## inputs
  ### raster_list_full:     list with all simulated Tmrt values of the scenarios
  ### scenario:             name of the scenario you want to generate a dataframe of
  ### summaryornot:         default is 1 (no summary), if 1 a summary of the delta per hour is given, if 3 a summary for the whole shade of the day is given. if 4 whole list is given as output
  
  ## outputs:
  ### delta_Tmrt_shade_hour: a df that contains, for a given scenario, all non-zero delta values (Tmrt_scenario - Tmrt_notree) per hour
  
  
  ## uses functions:
  ### rasterlist_to_df
  ### shade_analyser
  
  #select index of scenario
  index <- grep(scenario, names(raster_list_full))[1]

  raster_list_scenario <- raster_list_full[[index]]
  raster_list_notree <- raster_list_full[[1]]
  
  raster_list_scenario_delta <- list()
  
  for (i in 1:length(raster_list_scenario)) {
    raster_list_scenario_delta[[i]] <- raster_list_scenario[[i]] - raster_list_notree[[i]]
  }
  
  
  delta_df_hour <- rasterlist_to_df(raster_list_scenario_delta)
  
  #reference: Tmrt on the street with no vegetation as reference (as was clipped before to the specified street_extent)
  Tmrt_df_hour <- rasterlist_to_df(raster_list_notree)
  
  shade_list_output <- suppressMessages(shade_analyser(delta_df_hour, Tmrt_df_hour, scenario_name = scenario))
  
  if (summary_index == 1){
    delta_df <- shade_list_output[[summary_index]]
    return(delta_df)
  } 
  if (summary_index == 2){
    delta_Tmrt_shade_hour <- shade_list_output[[summary_index]]
    return(delta_Tmrt_shade_hour)
  } 
  if (summary_index == 3){
    delta_Tmrt_shade_day <- shade_list_output[[summary_index]]
    return(delta_Tmrt_shade_day)
  } 
  
  if (summary_index == 4){
    return(shade_list_output)
  } 
  
}
