## picarro data for air
picarro_data <- 
  readRDS(file = here("delta_ET", "3_output", "3_picarro_cleaned", "picarro_cleaned_study_1.rds")) %>% 
  filter(my_date %in% my_observation_dates_1)

dplyr::bind_rows(head(picarro_data), tail(picarro_data))
# Format picarro water vapor data from 9:00-7:00 each day -----------------
my_time_df <- 
  crossing(hour = c(9:19), minutes = c(":00:00", ":30:00")) %>% 
  unite(col = "day_time", sep = "", remove = TRUE)

my_sequence <- 
  crossing(date = unique(picarro_data$my_date), time = my_time_df) %>% 
  unite(col = "my_timestamp", sep = " ", remove = TRUE) %>% 
  mutate(my_timestamp = lubridate::ymd_hms(my_timestamp))


water_vapor <- 
  picarro_data %>% 
  ungroup() %>% 
  select(my_timestamp = time_label, hydrogen, oxygen) %>% 
  group_by(my_timestamp) %>% 
  summarise(hydrogen = mean(hydrogen),
            oxygen   = mean(oxygen)) %>% 
  mutate(type = "vapor") 

summary(water_vapor)


water_vapor_square_format <- 
  my_sequence %>% 
  left_join(water_vapor, by = "my_timestamp") %>% 
  mutate(type = "vapor")


# SIBS data ---------------------------------------------------------------


## sibs  data
sibs_data <- 
  readRDS(file = here("delta_E", "0_raw_data", "sibs", "period_1.rds")) %>% 
  filter(Date %in% my_observation_dates_1) %>% 
  select(my_timestamp = date_time, type = sampling, hydrogen = H2, oxygen = O18, H2_StDev, O18_StDev) %>% 
  filter(oxygen < 37) %>% #  remove an outlier in June 6 afternoon leaf data (heavier isotopic ratios)
  mutate(type = as.character(type), 
         type = if_else(type == "root", "xylem", type))

sibs_data %>% 
  rename(timestamp = my_timestamp) %>% 
write.csv("sibs_data.csv")

head(sibs_data)
summary(sibs_data)  
## combine data in two ways
## full_join 

dual_isotope_time_series <- 
  bind_rows(water_vapor_square_format, sibs_data) %>% 
  arrange(my_timestamp)


evaporation_site_ISS <-
  readRDS(file = here("delta_T", "3_output", "iss_result_brief.rds")) %>% 
  filter(my_timestamp != lubridate::ymd_hms("2016-06-06 17:00:00")) %>% 
  select(my_timestamp, isotope, evaporation_site) %>% 
  spread(key = isotope, value = evaporation_site) %>% 
  rename(hydrogen = `{ }^{2}*H`, oxygen = `{ }^{18}*O`) %>% 
  mutate(type = "evapo_site")

dual_isotope_time_series <- 
  dual_isotope_time_series %>% 
  filter(my_timestamp >= lubridate::ymd("2016-06-04"), 
         my_timestamp <= lubridate::ymd("2016-07-01")) %>% 
  bind_rows(evaporation_site_ISS) %>% 
  gather(key = isotope, value = delta, -c("my_timestamp", "type"))


date_location <- 
  read_excel(path = here( "doc", "date_location.xlsx"),
             sheet = "study1_period") %>% 
  rename(my_date = date) %>% 
  mutate(my_date = lubridate::date(my_date))

dplyr::bind_rows(head(date_location), tail(date_location))

study_1_camp_date1  <- lubridate::ymd("2016-06-03")
study_1_camp_date2  <- lubridate::ymd("2016-06-14")
study_1_camp_date3  <- lubridate::ymd("2016-06-26")
study_1_camp_date4  <- lubridate::ymd("2016-07-01")
##
dual_isotope_time_series <- 
  dual_isotope_time_series %>% 
  mutate(my_date =  lubridate::date(my_timestamp)) %>% 
  semi_join(date_location, by = "my_date") %>% 
  mutate(camp = case_when(
    between(my_date, study_1_camp_date1, study_1_camp_date2) ~ "Campaign_1",
    between(my_date, study_1_camp_date3, study_1_camp_date4) ~ "Campaign_2",
    TRUE ~ "NA"),
    isotope = if_else(isotope == "hydrogen", "{ }^{2}*H",
                      if_else(isotope == "oxygen", "{ }^{18}*O", "NA"), "NA"),
    type = if_else(type == "vapor", "italic(delta[V])",
                   if_else(type == "soil", "italic(delta[soil])", 
                           if_else(type == "xylem", "italic(delta[X])",
                                   if_else(type == "leaf", "italic(delta[leaf])",
                                           if_else(type == "evapo_site", "italic(delta[Le])", "NA")
                                           ,"NA"),"NA"),"NA"),"NA"), NULL)

dual_isotope_time_series$isotope <- factor(dual_isotope_time_series$isotope, 
                                           levels = c("{ }^{2}*H" , "{ }^{18}*O"),
                                           ordered = TRUE)

dual_isotope_time_series$camp <- 
  factor(dual_isotope_time_series$camp, 
         levels = c("Campaign_1", "Campaign_2"), 
         ordered = TRUE)

dual_isotope_time_series$my_time <- 
  strftime(dual_isotope_time_series$my_timestamp, format = "%H:%M:%S", tz = "UTC")

dual_isotope_time_series$my_time <- 
  as.POSIXct(strptime(dual_isotope_time_series$my_time, format = "%H:%M:%S"), tz = "UTC")

field_camp_duration <- 
  dual_isotope_time_series %>% 
  arrange(my_timestamp) %>% 
  group_by(my_date) %>% 
  summarise(begin = first(my_time),
            end   = last(my_time))

## table2tex(x = field_camp_duration, type = "TEX", standAlone = FALSE)

x <- as.POSIXct(strptime(c("050000","110000","110001","150000","150001",
                           "195959"),"%H%M%S"),"UTC")

dual_isotope_time_series <- 
  dual_isotope_time_series %>% 
  mutate(time_category = case_when(
    between(my_time,x[1],x[2]) ~"morning",
    between(my_time,x[3],x[4]) ~"noon",
    between(my_time,x[5],x[6]) ~"afternoon",
    TRUE ~"night")) %>% 
  select(-my_time)



saveRDS(dual_isotope_time_series, 
        here("dual_isotope", "3_output", "dual_isotope_time_series.rds"))
##
dual_isotope_time_series <- 
  readRDS(file = here("dual_isotope", "3_output", "dual_isotope_time_series.rds"))

head(dual_isotope_time_series)

# Plot dual_isotope graph -------------------------------------------------

summary(dual_isotope_time_series)

dual_isotope_time_series_input <- 
  dual_isotope_time_series %>% 
  filter(time_category != "night",
         my_date != lubridate::ymd("2016-06-13")) %>% 
        # my_date != lubridate::ymd("2016-06-27")) %>% 
  mutate(my_date = format(my_date, "%b-%d"))

isotope_type <-  
  c("italic(delta[Le])", "italic(delta[leaf])",
    "italic(delta[soil])","italic(delta[X])","italic(delta[V])")


dual_isotope_time_series_input$type <- 
  factor(dual_isotope_time_series_input$type,
         levels = c("italic(delta[Le])", "italic(delta[leaf])",
                    "italic(delta[soil])",
                    "italic(delta[X])", "italic(delta[V])"),
         ordered = TRUE)

summary(dual_isotope_time_series_input$type)

my_colors <- 
  c("darkcyan", "Lime Green",  "Goldenrod 1", "Saddle Brown", "Deep Sky Blue" ) 



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
                       expand = c(1.5/48, 1.5/48)) +  ## expand half-hour, i.e., 1/48
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
export::graph2png(file = here("dual_isotope", "3_figure", "dual_isotope_time_series_plot.png"),
                  width = 12, height = 9.55)
##########
#### done