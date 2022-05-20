plotIllustration <- function() {
  # Targets
  targets <- tibble(year = c(15, 30, 50, 75, 100),
                    target = c(30, 120, 400, 1000, 1000))
  
  # Start and growth rate
  s <- 0.1
  a <- 0.2
  today <- 10
  length <- 100
  
  # Linearly increasing demand pull
  data.demand <- tibble(
    year = 0:100,
    demand = approx(
      x = targets$year,
      y = targets$target,
      xout = 0:100,
      method = "linear",
      rule = 2
    )[["y"]]
  )
  
  # Simulate upscaling
  data.simulate <- data.demand %>%
    mutate(forecast = purrr::accumulate(demand,
                                        ~ .x + a * (1 - .x / .y) * .x,
                                        .init = s)[1:n()],
           case = "linear")
  
  # Random "historical" data
  set.seed(4)
  data.hist <- data.simulate %>%
    filter(year %in% seq(0, today)) %>%
    mutate(hist = pmax(forecast + rnorm(
      mean = 0, sd = 0.03, n = today
    ), 0))
  data.hist <- data.hist %>%
    mutate(hist = sort(data.hist$hist))
  
  # Error bars for starting value
  data.start <- data.simulate %>%
    filter(year %in% seq(today, today + 2)) %>%
    mutate(startmin = first(forecast),
           startmax = c(1, 2, 3) * forecast) %>%
    filter(year %in% seq(today + 1, today + 2))
  
  data.plot.simulate <- data.simulate %>%
    filter(year > today + 1)
  
  data.plot.simulate.anticipate <- data.simulate %>%
    select(year, demand) %>%
    mutate(year = year - 15) %>%
    filter(year > today + 1) %>%
    bind_rows(tibble(year = (100 - 14):100,
                     demand = 1000)) %>%
    mutate(forecast = data.plot.simulate$forecast)
  
  # Colors
  color.determinant <- pal_npg()(3)
  color.determinant[4] <- pal_npg()(5)[5]
  color.growth <- pal_npg()(4)[4]
  
  # Demand pull arrows
  data.plot.arrows <- data.plot.simulate %>%
    filter(year %% 5 == 0,
           year %in% seq(20, 80)) %>%
    mutate(from = forecast + 15,
           to = demand - 15)
  
  # Anticipation arrows
  data.plot.arrows.anticipate <- data.plot.simulate %>%
    filter(year %% 5 == 0,
           demand %in% seq(100, 900)) %>%
    mutate(from = year - 1.5,
           to = year - 13.5)
  
  # Demand pull arrows with anticipation
  data.plot.arrows.with.anticipation <-
    data.plot.simulate.anticipate %>%
    filter(year %% 5 == 0,
           year %in% seq(20, 80)) %>%
    mutate(from = forecast + 15,
           to = demand - 15)
  
  p <- ggplot() +
    # "Historical" data
    geom_point(data = data.hist,
               mapping = aes(x = year, y = hist)) +
    # Demand pull
    geom_line(
      data = data.plot.simulate,
      mapping = aes(x = year, y = demand),
      linetype = "dotted"
    ) +
    # Demand pull with anticipation
    geom_line(
      data = data.plot.simulate.anticipate,
      mapping = aes(x = year, y = demand),
      linetype = "dotted",
      alpha = 0.5
    ) +
    # Upscaling
    geom_line(
      data = data.plot.simulate,
      mapping = aes(x = year, y = forecast),
      linetype = "solid",
      size = 1,
      color = color.growth
    ) +
    # Starting value
    geom_errorbar(
      data = data.start,
      mapping = aes(x = year, ymin = startmin, ymax = startmax),
      color = color.determinant[1]
    ) +
    # Demand pull arrows
    geom_segment(
      data = data.plot.arrows,
      mapping = aes(
        x = year,
        y = from,
        xend = year,
        yend = to
      ),
      arrow = arrow(length = unit(0.15, "cm")),
      lineend = "round",
      linejoin = "round",
      size = 0.5,
      color = color.determinant[3]
    ) +
    # Demand pull label
    geom_label(
      mapping = aes(x = 68, y = 550, label = "3.1 Demand pull\nmagnitude"),
      align = "center",
      color = color.determinant[3],
      size = 8 / .pt
    ) +
    # Anticipation arrows
    geom_segment(
      data = data.plot.arrows.anticipate,
      mapping = aes(
        x = from,
        y = demand,
        xend = to,
        yend = demand
      ),
      arrow = arrow(length = unit(0.15, "cm")),
      lineend = "round",
      linejoin = "round",
      size = 0.5,
      color = color.determinant[4]
    ) +
    # Demand pull anticipation label
    geom_label(
      mapping = aes(x = 35, y = 580, label = "3.2 Demand pull\nanticipation"),
      align = "center",
      color = color.determinant[4],
      size = 8 / .pt
    ) +
    # Demand pull arrows with anticipation
    geom_segment(
      data = data.plot.arrows.with.anticipation,
      mapping = aes(
        x = year,
        y = from,
        xend = year,
        yend = to
      ),
      arrow = arrow(length = unit(0.15, "cm")),
      lineend = "round",
      linejoin = "round",
      size = 0.5,
      alpha = 0.1,
      color = color.determinant[3]
    ) +
    # Targets
    geom_point(
      data = targets,
      mapping = aes(x = year, y = target),
      shape = 21,
      color = "black",
      fill = "white",
      stroke = 1.5,
      size = 3
    ) +
    # Other labels
    geom_text(
      mapping = aes(x = 2.75, y = -25,
                    label = "Historical"),
      hjust = "center",
      vjust = "center",
      size = 8 / .pt
    ) +
    geom_text(
      mapping = aes(x = 10, y = 1050,
                    label = "Formative"),
      hjust = "center",
      vjust = "center",
      size = 8 / .pt
    ) +
    geom_text(
      mapping = aes(x = 45, y = 1050,
                    label = "Growth"),
      hjust = "center",
      vjust = "center",
      size = 8 / .pt
    ) +
    geom_text(
      mapping = aes(x = 80, y = 1050,
                    label = "Saturation"),
      hjust = "center",
      vjust = "center",
      size = 8 / .pt
    ) +
    theme_cowplot(8) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank()) +
    scale_x_continuous(name = "Time",
                       limits = c(-5, 105),
                       expand = c(0, 0)) +
    scale_y_continuous(name = "Capacity [GW]",
                       limits = c(-50, 1100),
                       expand = c(0, 0)) +
    # Shade past grey
    annotate(
      "rect",
      xmin = -Inf,
      xmax = today + 0.5,
      ymin = -Inf,
      ymax = Inf,
      alpha = 0.1
    )
  
  # Slope triangle
  slope <- tibble(
    x = c(today + 4, today + 8, today + 8),
    y = c(
      data.simulate %>% filter(year %in% c(today + 4)) %>% pull(forecast),
      data.simulate %>% filter(year %in% c(today + 4, today +
                                             8)) %>% pull(forecast)
    )
  )
  
  # Inset plot
  inset <- p +
    xlab(NULL) +
    ylab(NULL) +
    xlim(0, 2 * today) +
    ylim(0, data.simulate %>% filter(year == 2 * today) %>% pull(forecast)) +
    # Emergence growth slope triangle
    geom_line(data = slope,
              mapping = aes(x = x, y = y),
              color = color.determinant[2]) +
    # Initial capacity label
    geom_label(
      mapping = aes(
        x = today + 1.5,
        y = max(data.start$startmax) + 0.5,
        label = "1. Initial capacity"
      ),
      align = "center",
      color = color.determinant[1],
      size = 8 / .pt
    ) +
    # Emergence growth label
    geom_label(
      mapping = aes(x = 17,
                    y = 0.6,
                    label = "2. Emergence\ngrowth rate"),
      align = "center",
      color = color.determinant[2],
      size = 8 / .pt
    )
  
  p.with.inset <- ggdraw() +
    draw_plot(p) + 
    draw_plot(inset, x = 0.57, y = 0.05, width = 0.4, height = 0.3)
  
  return(p.with.inset)
}