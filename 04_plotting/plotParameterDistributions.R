plotParameterDistributions <- function(data.input, data.input.param) {
  bins <- 20
  plots <- NULL
  subplots <- NULL
  for (r in 1:2) {
    # Get values
    start.min <- data.input.param$start.min[[r]]
    start.max <- 1.2 * data.input.param$start.max[[r]]
    growth.min <- data.input.param$growth.min[[r]]
    growth.max <- 1.25  # Only for plotting
    
    # Density
    data.plot.density <- tibble(
      start = seq(start.min, start.max, length.out = 100),
      start.dens = dtruncnorm(
        start,
        a = start.min,
        mean = data.input.param$start.mean[[r]],
        sd = data.input.param$start.sd[[r]]
      ),
      growth = seq(growth.min, growth.max, length.out = 100),
      growth.dens = dtruncnorm(
        growth,
        a = growth.min,
        mean = data.input.param$growth.mean[[r]],
        sd = data.input.param$growth.sd[[r]]
      )
    )
    
    # Sample drawn from distribution (starting value and growth rate)
    data.plot.sample <- data.input %>%
      filter(region == regions.h2$name[[r]])
    
    # Starting value distribution and sample histogram
    p.start.dist <- ggplot() +
      # Sample
      geom_histogram(
        data = data.plot.sample,
        mapping = aes(x = start, y = ..density.., color = "Sample"),
        binwidth = (start.max - start.min) / bins,
        boundary = start.min,
        alpha = 0.3
      ) +
      scale_color_manual(name = NULL, values = c("Sample" = "grey")) +
      # Distribution
      new_scale_color() +
      geom_line(data = data.plot.density,
                mapping = aes(x = start, y = start.dens, color = "Density")) +
      scale_color_manual(name = NULL, values = c("Density" = "black")) +
      xlab(NULL) +
      ylab("Probability density") +
      xlim(0, start.max) +
      ggtitle(regions.h2$name[[r]]) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # No x axis
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        # Only line for y axis
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        plot.title = element_text(
          hjust = 0.5,
          face = "bold",
          size = font.size
        )
      )
    
    # Save plot
    plots <- append(plots, list(p.start.dist))
    
    # Horizontal bar of capacity by status
    data.plot.cap <- data.h2 %>%
      filter(region %in% regions.h2$list[[r]],
             year == t.split) %>%
      group_by(status) %>%
      summarise(cumcap.sum = sum(cumcap.sum)) %>%
      # Add decommissioned to operational for simpler plotting
      mutate(cumcap.sum = case_when(
        status == "Operational" ~ cumcap.sum + lead(cumcap.sum),
        TRUE ~ cumcap.sum
      )) %>%
      filter(status != "Decommissioned")
    
    # Starting value horizontal bar plot
    p.start.cap <- ggplot() +
      geom_bar(
        data = data.plot.cap,
        mapping = aes(x = 1, y = cumcap.sum, fill = status),
        stat = "identity"
      ) +
      scale_fill_npg(name = "Status") +
      coord_flip() +
      xlab(NULL) +
      ylab(paste0("Capacity in ", t.split, " [GW]")) +
      ylim(0, start.max) +
      # Set negative margin to overlay plots
      theme(
        plot.margin = unit(c(-0.2, 0, 0, 0), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
    
    # Save plot
    plots <- append(plots, list(p.start.cap))
    
    # Wind and solar growth rates
    growth.solar <-
      data.input.param[r, ] %>% unnest(growth.solar) %>% pull(growth.solar)
    growth.wind <-
      data.input.param[r, ] %>% unnest(growth.wind) %>% pull(growth.wind)
    
    # Growth rate
    p.growth <- ggplot() +
      # Sample
      geom_histogram(
        data = data.plot.sample,
        mapping = aes(
          x = 100 * growth,
          y = 100 * ..density..,
          color = "Sample"
        ),
        binwidth = (100 * growth.max - 100 * growth.min) /
          bins,
        boundary = 100 * growth.min,
        alpha = 0.3
      ) +
      scale_color_manual(name = NULL, values = c("Sample" = "grey")) +
      # Distribution
      new_scale_color() +
      geom_line(
        data = data.plot.density,
        mapping = aes(
          x = 100 * growth,
          y = growth.dens,
          color = "Density"
        )
      ) +
      scale_color_manual(name = NULL, values = c("Density" = "black")) +
      scale_x_continuous(name = "Emergence growth rate [%/yr]", breaks = c(100 *
                                                                             bmin, seq(25, 150, 25))) +
      ylab("Probability density") +
      ggtitle(regions.h2$name[[r]]) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Only line for y axis
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(
          hjust = 0.5,
          face = "bold",
          size = font.size
        )
      )
    # Add solar growth rates
    for (j in seq_along(growth.solar)) {
      p.growth <- p.growth +
        geom_vline(
          xintercept = 100 * growth.solar[j],
          linetype = "dotted",
          color = color.pv
        )
    }
    # Add wind growth rates
    for (j in seq_along(growth.wind)) {
      p.growth <- p.growth +
        geom_vline(
          xintercept = 100 * growth.wind[j],
          linetype = "dotted",
          color = color.wind
        )
    }
    # Annotate
    ymax <- data.plot.density %>% pull(growth.dens) %>% max()
    p.growth <- p.growth +
      annotate(
        geom = "label",
        x = 100 * mean(growth.solar),
        y = 0.5 * ymax,
        label = "Solar PV",
        color = color.pv,
        size = font.size / .pt
      ) +
      annotate(
        geom = "label",
        x = 100 * mean(growth.wind),
        y = 0.5 * ymax,
        label = "Wind",
        color = color.wind,
        size = font.size / .pt
      )
    
    # Save plot
    plots <- append(plots, list(p.growth))
  }
  return(plots)
}