calcTechnologyDiffusion <- function(data.demand, data.input, delta.t){

  print("Starting simulation.")
  print("0 %")
   
  data.full <- full_join(data.demand, data.input) %>% 
    mutate(results = NA) %>% 
    arrange(match(region, c("Global", "EU")))   # Global first
  
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
      group_by(region, anticipation, sample, growth, start) %>%
      complete(year = seq(first(start.year), 2050, delta.t)) %>%
      mutate(demand = na.approx(demand, rule = 2),
             start.year = first(start.year)) %>% 
      # Adjust growth rate accordingly
      mutate(growth = (1 + growth)**(delta.t) - 1) %>% 
      # purrr::accumulate, demand is passed as second argument (.y) to function,
      # cumulative value as .x
      mutate(forecast = purrr::accumulate(demand,
                                          ~ .x + first(growth)*(1 - .x/.y)*.x,
                                          .init = first(start))[1:n()]) %>% 
      # Nest results
      nest(results = c(year, demand, forecast))
    
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
