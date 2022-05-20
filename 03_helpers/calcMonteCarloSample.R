calcMonteCarloSample <- function(N, a.status, cap.1, cumprob.1, feasibility.success.rate, slice.len, b.min){
  startyear <- t.split
  # Create data
  data.input.param <- NULL
  data.input <- NULL
  for (r in 1:2){
    # Starting value
    temp <- data.h2 %>% 
      filter(year == startyear,
             region %in% regions.h2$list[[r]]) %>%
      group_by(year, status) %>% 
      summarise(cumcap.sum = sum(cumcap.sum))
    # First condition: Lower bound
    a <- temp %>% 
      filter(status %in% a.status) %>% 
      pull(cumcap.sum) %>% 
      sum()
    # Second condition
    cap.1.sum <- temp %>%  
      filter(status %in% cap.1) %>% 
      pull(cumcap.sum) %>% 
      sum()
    # Third condition: Feasibility share
    cap.feasibility <- temp %>% 
      mutate(cumcap.sum = case_when(status == "Concept" ~ 0,
                                    status == "Feasibility study" ~ feasibility.success.rate*cumcap.sum,
                                    TRUE ~ cumcap.sum)) %>% 
      pull(cumcap.sum) %>% 
      sum()
    # Calculate mean and sd parameters of truncated normal distribution
    start.dist <- calcTruncNormParam2(a = a, b = Inf, cap.1 = cap.1.sum,
                                      cumprob.1 = cumprob.1, cap.mean = cap.feasibility)
    # Sample truncated distribution of starting value
    start.sample <- rtruncnorm(n = N, a = a, b = Inf, mean = start.dist["mean"], sd = start.dist["sd"])
    # Maximum start value for plotting
    start.max <- temp %>% pull(cumcap.sum) %>% sum()
    # Growth rate
    temp <- data.bp.fit %>% 
      filter(slice.length == slice.len,
             region == regions.bp$list[[r]])
    # Get mean and standard deviation of solar and wind
    b.mean <- mean(temp %>% pull(b))
    b.sd <- sd(temp %>% pull(b))
    # Sample truncated distribution of growth rate
    b.sample <- rtruncnorm(n = N, a = b.min, b = Inf, mean = b.mean, sd = b.sd)
    
    # Parameters of distributions
    temp.param <- tibble(
      region = regions.h2$name[[r]],
      start.min = a,
      start.mean = start.dist["mean"],  # Before truncation!
      start.sd = start.dist["sd"],  # Before truncation!
      start.sample.mean = mean(start.sample),  # After truncation (numeric)
      start.sample.sd = sd(start.sample),  # After truncation (numeric)
      start.sample.q25 = quantile(start.sample, 0.25), # After truncation
      start.sample.q75 = quantile(start.sample, 0.75), # After truncation
      start.cap.1 = cap.1.sum,
      start.cap.feasibility = cap.feasibility,  # x% of feasibility study
      start.max = start.max,  # All projects
      growth.min = b.min,  # Exogenous
      growth.mean = b.mean,  # Before truncation!
      growth.sd = b.sd,  # Before truncation!
      growth.sample.mean = mean(b.sample),
      growth.sample.sd = sd(b.sample),
      growth.sample.q25 = quantile(b.sample, 0.25),
      growth.sample.q75 = quantile(b.sample, 0.75),
      growth.solar = list(temp %>% filter(technology == "solar") %>% pull(b)),
      growth.wind = list(temp %>% filter(technology == "wind") %>% pull(b))
    )
    
    # Save parameters
    data.input.param <- bind_rows(data.input.param, temp.param)
    
    # Join samples
    temp <- tibble(b.sample, start.sample) %>% 
      mutate(sample = 1:n(),  # Numbering of sensitivities
             region = regions.h2$name[[r]],
             start.year = startyear) %>% 
      rename(growth = b.sample, start = start.sample)
    
    # Save samples for each region
    data.input <- bind_rows(data.input, temp) %>% 
      select(region, sample, growth, start, start.year)  # Reorder columns
    
  }
  
  return(list(data.input, data.input.param))
}