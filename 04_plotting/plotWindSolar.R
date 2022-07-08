plotWindSolar <-
  function(data.bp,
           data.bp.fit,
           slice.len,
           regi,
           tech,
           slice.st,
           color) {
    
  # Capacity data
  data.plot <- data.bp %>% 
    filter(region == regi,
           technology == tech)
  
  # y limit of facet zoom
  ylim <- data.plot %>%
    filter(year == 2010) %>% 
    pull(capacity)
  
  # Growth rate estimates
  data.fit <- data.bp.fit %>% 
    filter(region == regi,
           technology == tech,
           slice.length == slice.len,
           slice.start >= slice.st) %>% 
    unnest(predict) %>% 
    # Only print in zoom panel
    mutate(zoom = TRUE) 
  
  # Join with BP dataset to define label position
  data.fit <- data.fit %>% 
    full_join(data.plot %>% 
                filter(year %in% seq(slice.st, max(data.fit$slice.start))) %>% 
                rename(slice.start = year)) %>% 
    # Position for text label
    mutate(ypos = capacity + 0.1*ylim)
  
  # Calculate implicit demand pull
  data.dp <- data.plot %>% 
    filter(year >= 2011) %>% 
    ungroup() %>% 
    mutate(pmax = capacity/(1 - (lead(capacity) - capacity)/(capacity * mean(data.fit$b)))) %>% 
    head(-1) %>% 
    mutate(pmax.movavg = rollapply(pmax, 5, mean, align = "center", partial = TRUE))
  
  # Predicted time series
  data.plot.fit <- NULL
  for (i in unique(data.fit$slice.start)){
    temp <- data.fit %>% 
      filter(slice.start == i) %>% 
      unnest(predict) %>% 
      mutate(year = first(slice.start):(first(slice.start)+first(slice.length)-1))
    
    data.plot.fit <- bind_rows(temp, data.plot.fit)
  }
  
  # Plot
  p <- ggplot() + 
    # Historical data
    geom_point(data = data.plot,
               mapping = aes(x = year, y = capacity/1E3, color = "Historical")) +
    scale_color_manual(name = NULL, values = c("Historical" = "black")) +
    new_scale_color() + 
    # Exponential fits up to year X
    geom_line(data = data.plot.fit,
              mapping = aes(x = year, y = predict/1E3, group = slice.start, color = "Exponential fit\n(7-year slices)"), size = 1, alpha = 0.3) +
    scale_color_manual(name = NULL, values = c("Exponential fit\n(7-year slices)" = color)) +
    # Implicit demand pull
    geom_line(data = data.dp,
              mapping = aes(x = year, y = pmax.movavg/1E3, linetype = "Implicit demand pull\n(5-year rolling mean)")) +
    scale_linetype_manual(name = NULL, values = c("Implicit demand pull\n(5-year rolling mean)" = "dotted")) +
    # Growth rates as text
    geom_text(data = data.fit,
              mapping = aes(x = slice.start, y = ypos/1E3, label = paste0(round(b,2)*100, "%")),
              size = 2) + 
    facet_zoom(xlim = c(slice.st, 2010), ylim = c(0, ylim/1E3), horizontal = FALSE, zoom.size = 1, zoom.data = zoom) +
    xlab(NULL) + 
    ylab("Capacity [GW]") + 
    ggtitle(paste0(regi, " ", tech)) +
    theme(legend.key.size = unit(1, "line"),
          plot.title = element_text(size = 7))
  
  return(p)
}