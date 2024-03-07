shade_analyser <- function(delta_df_hour, Tmrt_df_hour, scenario_name){
    
  ## input:
  ### delta_df_hour:      df with deltas of all pixels per hour
  ### scenario name:      name of scenario you want to give, will be a column in the final df
  
  
  ## 3 returns (in 1 list): 
  ### delta_df: dataframe of all labeled raster values (edge effect or shade)
  ### delta_Tmrt_shade_hour: summary dataframe of average deltaTmrt in shade for every hour
  ### delta_Tmrt_shade_day: summary dataframe of average deltaTmrt in shade globally over the day
  
  return_list <- list()
  
  set.seed(350)
  
  delta_df_hour %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = "hour",
                        values_to = "value") %>%
    filter(!is.na(value)) %>%
    mutate(hour = factor(hour, levels = paste0("hour_", 7:22), ordered = TRUE)) %>%
    arrange(hour) -> df_deltaTmrt #needs to be arranged by the categorical variable in order for numbering later to work out
  
  Tmrt_df_hour %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = "hour",
                        values_to = "value") %>%
    filter(!is.na(value)) %>%
    mutate(Tmrt = value) %>%
    mutate(hour = factor(hour, levels = paste0("hour_", 7:22), ordered = TRUE)) %>%
    arrange(hour) %>%
    select(Tmrt) -> df_reference
  
  df_deltaTmrt %>%
    mutate(Tmrt_reference = df_reference$Tmrt) -> df_clustering
  
  
  # https://stackoverflow.com/questions/40490199/r-univariate-clustering-by-group
  
  
  res2 <- tapply(df_clustering$value, INDEX = df_clustering$hour, function(x) kmeans(x,2))   
  res3 <- lapply(names(res2), function(x) data.frame(hour=x, Centers=res2[[x]]$centers, Size=res2[[x]]$size))  
  
  
  res3 <- do.call(rbind, res3)
  
  data <- cbind(df_clustering, 
                cluster = unlist(lapply(names(res2), function(x) paste(x, res2[[x]]$cluster, sep = "_"))),
                cluster_id = unlist(lapply(names(res2), function(x) res2[[x]]$cluster))) %>%
    mutate(cluster_id = factor(cluster_id))
  
  
  ## visual check pre ordering of clusters
  data %>%
    ggplot(aes(x = hour, y = value, fill = cluster_id, color = cluster_id)) +
    geom_dotplot(binaxis = 'y',
                 stackdir = 'center',
                 stackratio = 0.2,
                 dotsize = 0.5,
                 position = "jitter",
                 method = "histodot") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_discrete(labels = c(7:22)) +
    facet_wrap(~hour) -> clusterplot
  

  # Parameters
  median_diff_threshold <- 1 # Change this value to adjust the threshold
  
  data %>%
    group_by(hour, cluster_id) %>%
    summarise(median_value = median(value), .groups = 'drop') %>%
    ungroup() %>%
    group_by(hour) %>%
    mutate(rank = rank(-median_value)) %>%
    group_by(hour) %>%
    mutate(mm = case_when(
      max(median_value) - min(median_value) < median_diff_threshold ~ "edge_effect",
      rank == 2 ~ "shade",
      TRUE ~ "edge_effect"
    )) %>%
    select(-median_value, -rank) %>%
    mutate(cluster_mm = paste(hour, mm, sep = "_")) %>%
    right_join(data, by = c("hour", "cluster_id")) %>%
    select(hour, value, Tmrt_reference, cluster, cluster_id, mm, cluster_mm) -> delta_df
  
  
  ## visual check pre ordering of clusters
  delta_df %>%
    ggplot(aes(x = hour, y = value, fill = mm, color = mm)) +
    geom_dotplot(binaxis = 'y',
                 stackdir = 'center',
                 stackratio = 0.2,
                 dotsize = 0.5,
                 position = "jitter",
                 method = "histodot") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_discrete(labels = c(7:22)) +
    facet_wrap(~hour) -> cluster_ordered_plot
  
  return_list[[1]] <- delta_df
  
  ## plotting of the different groups
  
  # delta_df %>%
  #   ggplot(aes(x = hour, y = value, fill = mm, color = mm)) +
  #   geom_dotplot(binaxis = 'y',
  #                stackdir = 'center',
  #                stackratio = 0.2,
  #                dotsize = 0.5,
  #                position = "jitter",
  #                method = "histodot") +
  #   theme_minimal() +
  #   theme(plot.title = element_text(hjust = 0.5)) +
  #   scale_x_discrete(labels = c(7:22)) +
  #   facet_wrap(~hour)
  
  #summary table per hour (reference Tmrt is not yet included!!)
  
  delta_df %>%
    group_by(hour, cluster_mm, mm) %>%
    summarise(mean_delta = mean(value), #you can add more parameters here
              max_delta = min(value),
              mean_Tmrt_reference = mean(Tmrt_reference),
              pixels = n()) %>%
    ungroup() %>%
    tidyr::pivot_longer(cols = -c(hour, cluster_mm, mm),
                        names_to = "parameters",
                        values_to = "value") %>%
    select(-cluster_mm) %>%
    tidyr::pivot_wider(names_from = parameters, values_from = value) %>%
    filter(mm == "shade") %>%
    select(-mm) %>%
    right_join(data.frame(hour = paste0("hour_", 7:22)), by = "hour") %>% #add NA's for the hours with no delta
    mutate(hour = factor(hour, levels = paste0("hour_", 7:22), ordered = TRUE)) %>%
    arrange(hour) %>%
    tidyr::pivot_longer(cols = -1) %>%
    tidyr::pivot_wider(names_from = hour,
                values_from = value) %>%
    mutate(scenario = scenario_name) %>%
    select(scenario, everything()) -> delta_Tmrt_shade_hour
    
  return_list[[2]] <- delta_Tmrt_shade_hour
  
  #make one globally weighed table
  
  delta_Tmrt_mean <- mean(unlist(select(delta_Tmrt_shade_hour, starts_with("hour"))[1,]), na.rm = TRUE)
  n_shade_pixels <- sum(unlist(delta_Tmrt_shade_hour %>%
                                 filter(name == "pixels") %>%
                                 select(starts_with("hour"))), 
                        na.rm = TRUE)
  
  delta_Tmrt_shade_day <- data.frame(scenario = scenario_name, 
                                     delta_Tmrt_mean = delta_Tmrt_mean, 
                                     n_shade_pixels = n_shade_pixels)
  
  # print(delta_Tmrt_shade_day)
  
  return_list[[3]] <- delta_Tmrt_shade_day
  
  return(return_list)
  
}

