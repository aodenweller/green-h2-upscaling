plotElectrolysisProjects <-
  function(data.h2,
           regions,
           fill.by,
           title,
           leg.label) {
    data.plot <- data.h2 %>%
      filter(region %in% names(regions)) %>%
      revalue.levels(region = regions) %>%
      order.levels(region = regions) %>%
      group_by(across(all_of(c("year", fill.by)))) %>%
      summarise(cumcap.sum = sum(cumcap.sum))
    
    data.plot.1 <- data.plot %>%
      filter(year %in% seq(t.min, t.split))
    
    data.plot.2 <- data.plot %>%
      filter(year %in% seq(t.split, t.max))
    
    p1 <- ggplot() +
      geom_bar(
        data = data.plot.1,
        mapping = aes_string(x = "year", y = "cumcap.sum", fill = fill.by),
        stat = "identity",
        position = "stack"
      ) +
      scale_fill_npg(name = leg.label) +
      scale_x_continuous(name = NULL, breaks = c(seq(t.min, t.split, 5), t.split)) +
      ylab("Capacity [GW]") +
      theme(legend.position = c(0.2, 0.7),
            plot.margin = unit(c(5, -2, 5, 5), "pt"))
    
    p2 <- ggplot() +
      geom_bar(
        data = data.plot.2,
        mapping = aes_string(x = "year", y = "cumcap.sum", fill = fill.by),
        stat = "identity",
        position = "stack"
      ) +
      scale_fill_npg(name = NULL) +
      scale_x_continuous(name = NULL, breaks = c(t.split, t.max)) +
      ylab(NULL) +
      theme(legend.position = "none",
            plot.margin = unit(c(5, 5, 5,-2), "pt"))
    
    p.title <- ggdraw() +
      draw_label(title,
                 size = font.size,
                 fontface = "bold",
                 hjust = 0.5)
    
    p.row <- plot_grid(p1, p2,
                       ncol = 2,
                       rel_widths = c((t.split - t.min + 1), (t.max - t.split + 1)))
    
    p <- plot_grid(p.title,
                   p.row,
                   nrow = 2,
                   rel_heights = c(0.05, 1))
  }