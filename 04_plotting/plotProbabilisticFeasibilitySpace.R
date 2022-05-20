plotProbabilisticFeasibilitySpace <- function(data.row,
                                              data.targets,
                                              colour,
                                              plot.marginal = T,
                                              marginal.year,
                                              title,
                                              ci.80 = F,  # 80% confidence int.
                                              slices = 500,
                                              random.paths = 10,
                                              zoom.panel = T,
                                              gompertz = F) {
  
  # Unnest results
  data.plot <- data.row %>%
    unnest(sensitivities) %>%
    select(!demand) %>%
    unnest(results)
  
  # Plot Gompertz model instead of logistic if switch activated
  if (gompertz == T){
    data.plot <- data.plot %>% 
      mutate(forecast = forecast.gomp,
             demand = demand.full)
  }
  
  # Calculate median and percentiles
  if (ci.80 == T){
    quantiles <- c(0.95, 0.9, 0.5, 0.1, 0.05)
    col.quant <- c("0.95" = darken(colour, 0.1),
                  "0.9" = darken(colour, 0.3),
                  "0.5" = darken(colour, 0.5),
                  "0.1" = darken(colour, 0.3),
                  "0.05" = darken(colour, 0.1))
  } else
  {
    quantiles <- c(0.95, 0.5, 0.05)
    col.quant <- c("0.95" = darken(colour, 0.1),
                  "0.5" = darken(colour, 0.5),
                  "0.05" = darken(colour, 0.1))
  }
  quibble <- function(x, q = quantiles) {
    tibble(x = quantile(x, q), q = q)
  }
  data.stat <- data.plot %>%
    group_by(year) %>%
    summarise(quibble(forecast)) %>% 
    mutate(q = as.factor(q))
    
  # Get demand
  data.plot.demand <- data.plot %>%
    filter(sample == 1)
  
  # Select random paths
  data.plot.random <- data.plot %>%
    filter(sample %in% sample(1:max(data.plot$sample), random.paths))
  
  # Calculate density of scenarios for each year and save in raster
  data.raster <- NULL
  for (y in unique(data.plot$year)) {
    # Select
    subset <- data.plot %>%
      filter(year == y) %>%
      pull(forecast)
    # Density
    dens <- density(
      subset,
      n = slices,
      na.rm = T,
      from = 0,
      to = max(data.plot.demand$demand, na.rm = T)
    )
    # Normalise
    dens$y <- dens$y / max(dens$y)
    temp <- tibble(year = y,
                   forecast = dens$x,
                   density = dens$y)
    # Save
    data.raster <- bind_rows(data.raster, temp)
  }
  
  # Data targets
  data.plot.targets <- data.targets %>%
    filter(region == unique(data.plot$region)[[1]])
  
  # Plot using ggplot2::geom_raster
  p.facet <- ggplot() +
    # Density
    geom_raster(
      data = data.raster,
      mapping = aes(x = year, y = forecast, fill = density),
      interpolate = TRUE
    ) +
    scale_fill_gradient(name = "Probability density\n(normalised)", low = "white", high = colour) +
    # Random paths
    geom_line(
      data = data.plot.random,
      mapping = aes(
        x = year,
        y = forecast,
        group = sample,
        color = "Example path"
      ),
      lwd = 0.5,
      alpha = 0.5
    ) +
    scale_color_manual(
      values = c("Example path" = "grey"),
      name = NULL,
      guide = guide_legend(order = 3)
    ) +
    new_scale_color() +
    geom_line(
      data = data.stat,
      mapping = aes(x = year, y = x, color = q),
      size = 1
    ) +
    # Quantiles
    scale_color_manual(values = col.quant,
                       name = "Percentiles",
                       labels = c("0.95" = "95 %",
                                  "0.9" = "90 %",
                                  "0.5" = "50 % (median)",
                                  "0.1" = "10 %",
                                  "0.05" = "5 %"),
                       guide = guide_legend(order = 4)) +
    new_scale_color() +
    # Demand pull
    geom_line(
      data = data.plot.demand,
      mapping = aes(x = year, y = demand, color = "Demand pull"),
      linetype = "dotted"
    ) +
    scale_color_manual(
      values = c("Demand pull" = "black"),
      name = NULL,
      guide = guide_legend(order = 2)
    ) +
    # Targets
    new_scale_fill() +
    geom_point(
      data = data.plot.targets,
      mapping = aes(x = year, y = value, fill = "Targets"),
      shape = 21,
      color = "black",
      stroke = 1,
      size = 1
    ) +
    scale_fill_manual(
      name = NULL,
      values = c("Targets" = "white"),
      guide = guide_legend(order = 1)
    ) +
    xlab(NULL) +
    ylab("Electrolysis capacity [GW]") +
    # Vertical line at X
    #geom_vline(xintercept = marginal.year, linetype = "dashed") +
    # Title
    ggtitle(title) +
    theme(legend.position = "right",
          legend.justification = "center",
          plot.title = element_text(
            face = "bold",
            size = font.size))
  # Add optional zoom panel
  if (zoom.panel == T) {
    p.facet <- p.facet +
      facet_zoom(
        xlim = c(t.split, 2030),
        ylim = c(0, data.plot.targets %>% filter(year == 2030) %>% pull(value)),
        horizontal = FALSE,
        zoom.size = 0.5,
        show.area = FALSE
      )
  }
  
  if (plot.marginal == T){
    
    # Marginal at year X
    data.marginal <- data.raster %>%
      filter(year == marginal.year)
    
    # Marginal density plot
    p.dens <- ggplot(data = data.marginal) +
      geom_ribbon(
        mapping = aes(x = forecast, ymax = density),
        ymin = 0,
        fill = colour
      ) +
      geom_line(mapping = aes(x = forecast, y = density)) +
      geom_text(
        x = 1.03 * max(data.marginal$forecast),
        y = 0.5,
        hjust = "middle",
        label = marginal.year,
        size = (font.size - 1) / .pt
      ) +
      ylim(0, 1) +
      coord_flip() +
      theme_void()
    
    # Position of marginal density plot (manual adjustment due to facet zoom)
    if (zoom.panel == T) {
      p.dens.row <- plot_grid(NULL,
                              p.dens,
                              NULL,
                              ncol = 1,
                              rel_heights = c(0.11, 1, 0.8))
    } else {
      p.dens.row <- plot_grid(NULL,
                              p.dens,
                              NULL,
                              ncol = 1,
                              rel_heights = c(0.1, 1, 0.08))
    }
    
    # Put facet plot and marginal plot beside each other
    p.plot <- plot_grid(
      p.facet + theme(legend.position = "none"),
      p.dens.row,
      ncol = 2,
      rel_widths = c(9, 1)
    )
  } else {
    p.plot <- p.facet + theme(legend.position = "none")
  }

  # Extract legend
  p.legend <- get_legend(p.facet)
  
  return(list(p.plot, p.legend))
}