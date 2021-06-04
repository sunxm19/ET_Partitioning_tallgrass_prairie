dual_isotope_time_series <- 
  readRDS(file = here("dual_isotope", "3_output", "dual_isotope_time_series.rds")) %>% 
  filter(type != "italic(delta[Le])")
# Plot dual_isotope graph -------------------------------------------------
head(dual_isotope_time_series)
summary(dual_isotope_time_series)

dual_isotope_time_series_input <- 
  dual_isotope_time_series %>% 
  filter(time_category != "night",
         my_date != lubridate::ymd("2016-06-13")) %>% 
  # my_date != lubridate::ymd("2016-06-27")) %>% 
  mutate(my_date = format(my_date, "%b-%d"))

isotope_type <-  
  c( "italic(delta[leaf])",
    "italic(delta[soil])","italic(delta[X])","italic(delta[V])")


dual_isotope_time_series_input$type <- 
  factor(dual_isotope_time_series_input$type,
         levels = c( "italic(delta[leaf])",
                    "italic(delta[soil])",
                    "italic(delta[X])", "italic(delta[V])"),
         ordered = TRUE)

summary(dual_isotope_time_series_input$type)

my_colors <- 
  c( "Lime Green",  "Goldenrod 1", "Saddle Brown", "Deep Sky Blue" ) 



#### plot time series
dual_isotope_series_plot <- 
  ggplot(data = dual_isotope_time_series_input,
         mapping = aes(x = my_timestamp, y = delta, 
                       color = type,
                       shape = type)) +  #, shape = time_category)) +
  geom_point() +
  geom_line() +
  scale_color_manual(name = "", 
                     labels = parse_format()(isotope_type),
                     values = my_colors) +
  scale_shape_manual(name = "", 
                     labels = parse_format()(isotope_type),
                     values = c(21:25)) +
  scale_x_datetime(#breaks = date_breaks("3 hour"), 
    labels = date_format("%H"),
    expand = c(2.5/48, 2.5/48)) +  ## expand half-hour, i.e., 1/48
  facet_grid(isotope ~ my_date, labeller = label_parsed, scales = "free") +
  xlab("Hours") +
  ylab("\u03b4  (\u2030 vs VSMOW)") +
  #theme_classic() +  ## classic theme
  theme(#legend.position = c(0.8, 0.25),
    strip.text.x = element_text(face = "bold", size = 12),
    strip.text.y = element_text(face = "bold", size = 14),
    legend.title = element_text(face = "bold", size = 12), 
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0)),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA, color = "black"))

dual_isotope_series_plot_gt <- 
  ggplot_gtable(ggplot_build(dual_isotope_series_plot))

dual_isotope_series_plot_gt$widths[14] <- 4*dual_isotope_series_plot_gt$widths[14]

grid.draw(dual_isotope_series_plot_gt)

export::graph2eps(file = here("dual_isotope", "3_figure", "Figure_6_dual_isotope_time_series_plot.eps"),
                  width = 12, height = 9.55)
