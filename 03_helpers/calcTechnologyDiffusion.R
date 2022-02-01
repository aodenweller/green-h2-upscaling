calcTechnologyDiffusion <- function(data.demand, data.input, delta.t){
 
  data.full <- full_join(data.demand, data.input) %>% 
    mutate(results = NA) %>% 
    arrange(match(region, c("Global", "EU")))   # Global first
  
  print("Starting simulation.")
  print("0 %")
  # Loop over samples
  for (i in 1:dim(data.full)[1]){
    
    # Print every 10 %
    if (i %% (dim(data.full)[1]/10) == 0){
      print(paste0(100*i/dim(data.full)[1], " %"))
    }
    
    # Simulation
    data.temp <- data.full[i,] %>% 
      select(!results) %>% 
      unnest(demand) %>% 
      # Increase temporal resolution
      group_by(region, anticipation, sample, growth, start, start.year, capex, learning) %>%
      complete(year = seq(first(start.year), 2050, delta.t)) %>%
      mutate(demand = na.approx(demand, rule = 2)) %>% 
      # Adjust growth rate accordingly
      mutate(growth = (1 + growth)**(delta.t) - 1) %>% 
      # purrr::accumulate, demand is passed as second argument (.y) to function,
      # cumulative value as .x
      mutate(forecast = purrr::accumulate(demand,
                                          ~ .x + first(growth)*(1 - .x/.y)*.x,
                                          .init = first(start))[1:n()])
    
    # Calculate specific costs via learning
    if (data.full$region[i] == "Global"){
      data.temp <- data.temp %>% 
        mutate(cost.spec = capex * (forecast/start)**(log(1 - learning/100, base = 2)))
    } else {
      # Copy specific costs from global values if necessary
      cost.spec.glob <- data.full %>%
        filter(region == "Global",
               anticipation == data.full$anticipation[i],
               sample == data.full$sample[i]) %>%
        select(-demand) %>%
        unnest(results) %>%
        pull(cost.spec)
      
      data.temp <- data.temp %>% 
        mutate(cost.spec = cost.spec.glob)
    }
    
    # Calculate yearly and total cumulative costs
    data.temp <- data.temp %>% 
      mutate(cost.yearly = (forecast - lag(forecast)) * cost.spec,  # million euros
             cost.cum = cumsum(replace_na(cost.yearly, 0))) %>%   # million euros
      nest(results = c(year, demand, forecast, cost.spec, cost.yearly, cost.cum))
    
    # Copy nested data
    data.full$results[i] <- data.temp$results[1]
  }
  
  # Nest sensitivities
  data.results <- data.full %>% 
    group_by(region, anticipation, demand) %>% 
    nest() %>% 
    ungroup() %>% 
    rename(sensitivities = data)

  return(data.results)
}  
