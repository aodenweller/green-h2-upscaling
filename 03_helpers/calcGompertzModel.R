calcGompertzModel <- function(data.row, t.fix){
  # Unnest data
  data.temp <- data.row %>% 
    unnest(sensitivities) %>% 
    select(!demand) %>% 
    unnest(results) %>% 
    mutate(demand.full = max(demand))
  # Calculate Gompertz parameters
  data.gomp.param <- data.temp %>%
    group_by(region, sample) %>%
    slice(1) %>%
    do(calcGompertzParam(.$start,
                         .$growth,
                         .$demand.full,
                         t.fix,
                         startval = c(0.02, 10)))
  # Calculate Gompertz model
  data.temp.gomp <- data.temp %>%
    full_join(data.gomp.param) %>%
    group_by(sample) %>%
    mutate(forecast.gomp = demand.full * exp(-exp(-b1 * (year - t0 - 2023)))) %>% 
    select(!c("b1","t0")) %>% 
    nest(results = c(year, demand, demand.full, forecast, forecast.gomp))
  # Nest 
  data.nest <- data.row %>% 
    unnest(sensitivities)
  # Copy nested results
  data.nest$results <- data.temp.gomp$results
  # Nest again into sensitivities
  data.nest <- data.nest %>% 
    select(sample, growth, start, start.year, results) %>% 
    nest(data = everything()) %>% 
    ungroup() %>% 
    rename(sensitivities = data)
  # Data output
  data.out <- data.row
  data.out$sensitivities <- data.nest$sensitivities
  return(data.out)
}