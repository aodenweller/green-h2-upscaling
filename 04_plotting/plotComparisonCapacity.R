plotComparisonCapacity <-
  function(regi,
           anticip,
           title,
           final.energy,  # TWh
           time.slice,
           plot.unconv = T,
           ylim = NA) {
    
    # Legend labels
    leg.label <- c("Conventional" = "Conventional\n(like solar/wind)",
                   "Unconventional" = "Unconventional\n(emergency deployment)")
    
    # Unnest results
    data.plot.conv <- data.results %>%
      filter(region == regi, anticipation == anticip) %>% 
      unnest(sensitivities) %>%
      select(!demand) %>%
      unnest(results) %>%
      drop_na(forecast) %>% 
      mutate(type = "Conventional")
    
    if (plot.unconv == T){
      data.plot.unconv <- data.results.unconv %>%
        filter(region == regi, anticipation == anticip) %>% 
        unnest(sensitivities) %>%
        select(!demand) %>%
        unnest(results) %>%
        drop_na(forecast) %>% 
        mutate(type = "Unconventional")
      
      # Join datasets
      data.plot <- bind_rows(data.plot.conv, data.plot.unconv) %>% 
        filter(year == time.slice)
    } else {
      data.plot <- data.plot.conv %>% 
        filter(year == time.slice)
    }
     
    # Median of data
    data.plot.median <- data.plot %>% 
      group_by(type) %>% 
      summarise(median = median(forecast))
    
    # Secondary axis scaling
    sec.axis.scale <- 100*flh*eff.fe*1E-3/final.energy
    
    p.dens <- ggplot() + 
      # Density
      geom_density(data = data.plot,
                   mapping = aes(x = forecast, fill = type),
                   alpha = 0.3,
                   adjust = 2) +
      scale_fill_npg(name = "Growth rates",
                     guide = guide_legend(order = 1),
                     labels = leg.label) +
      # Vertical line at median
      geom_vline(data = data.plot.median,
                 mapping = aes(xintercept = median, color = type),
                 linetype = "dashed",
                 alpha = 0.5) +
      scale_color_npg(name = "Median",
                      guide = guide_legend(override.aes = list(size = 0.35),
                                           order = 2),
                      labels = leg.label) +
      scale_y_continuous(name = NULL) +
      ggtitle(title) + 
      # Style
      theme(plot.title = element_text(size = font.size,
                                      hjust = 0.5),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            strip.background = element_rect(fill="white"),
            legend.position = "bottom",
            legend.justification = "center") +
      coord_flip()
    
    if (!is.na(ylim)){
      p.dens <- p.dens +
        # Secondary x axis
        scale_x_continuous(name = NULL,
                           limits = c(0, ylim),
                           sec.axis = sec_axis(~.*sec.axis.scale))
    } else {
      p.dens <- p.dens +
        # Secondary x axis
        scale_x_continuous(name = NULL,
                           sec.axis = sec_axis(~.*sec.axis.scale))
    }
    
    if (regi == "EU" & time.slice == 2030){
      p.dens <- p.dens + 
        new_scale_color() + 
        geom_vline(mapping = aes(xintercept = 100),
                   linetype = "dashed") + 
        geom_text(mapping = aes(x = 110, y = 0.02, label = "REPowerEU target"),
                  size = 6/.pt)
    }
    
    if (regi == "Global" & time.slice == 2030){
      p.dens <- p.dens + 
        new_scale_color() + 
        geom_vline(mapping = aes(xintercept = 850),
                   linetype = "dashed") +
        geom_text(mapping = aes(x = 900, y = 0.01, label = "IEA NZE"),
                  size = 6/.pt)
    }
    
    return(p.dens)
  }
