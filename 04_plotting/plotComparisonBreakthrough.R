plotComparisonBreakthrough <- function(regi, anticip, title) {
  
  data.plot.conv <- data.results %>% 
    filter(region == regi, anticipation == anticip) %>% 
    select(!demand) %>% 
    unnest(sensitivities) %>% 
    unnest(results) %>% 
    mutate(abs.growth = forecast - lag(forecast)) %>%
    group_by(sample) %>% 
    filter(abs.growth == max(abs.growth)) %>% 
    mutate(type = "Conventional growth")
  
  data.plot.unconv <- data.results.unconv %>% 
    filter(region == regi, anticipation == anticip) %>% 
    select(!demand) %>% 
    unnest(sensitivities) %>% 
    unnest(results) %>% 
    mutate(abs.growth = forecast - lag(forecast)) %>%
    group_by(sample) %>% 
    filter(abs.growth == max(abs.growth)) %>% 
    mutate(type = "Unconventional growth")
  
  data.plot <- bind_rows(data.plot.conv, data.plot.unconv)
  
  data.plot.median <- data.plot %>% 
    group_by(type) %>% 
    mutate(median = median(year))
  
  p.dens <- ggplot() + 
    # Density
    geom_density(data = data.plot,
                 mapping = aes(x = year, fill = type),
                 adjust = 4,
                 alpha = 0.3) +
    scale_fill_npg(name = "Growth rates") +
    # Vertical line at median
    geom_vline(data = data.plot.median,
               mapping = aes(xintercept = median, color = type),
               linetype = "dashed",
               alpha = 0.5) +
    # Vertical line at 2030 and 2040
    geom_vline(mapping = aes(xintercept = 2030),
               linetype = "dotted") +
    geom_vline(mapping = aes(xintercept = 2040),
               linetype = "dotted") + 
    xlab(NULL) + 
    xlim(2023, 2050) + 
    ylab("") +
    ggtitle(title) +
    theme(plot.title = element_text(size = font.size,
                                    hjust = 0.5),
          legend.position = "bottom",
          legend.justification = "center",
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) + 
    coord_cartesian(xlim = c(2020,2050))
  
}