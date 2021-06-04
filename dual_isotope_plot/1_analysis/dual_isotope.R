library(ggExtra)  ## for marginal distribution plot
# Import data for dual-isotope graph --------------------------------------

## picarro data for air
picarro_data <- 
  readRDS(file = here("delta_ET", "3_output", "3_picarro_cleaned", "picarro_cleaned_study_1.rds"))

dplyr::bind_rows(head(picarro_data), tail(picarro_data))

water_vapor <- 
  picarro_data %>% 
  ungroup() %>% 
  select(my_timestamp = time_label, hydrogen, oxygen) %>% 
  group_by(my_timestamp) %>% 
  summarise(hydrogen = mean(hydrogen),
            oxygen   = mean(oxygen)) %>% 
  mutate(type = "vapor") %>% 
  select(- my_timestamp)


summary(water_vapor)

## rain data
rain_data <- 
  readRDS(file = here("dual_isotope", "3_output", "delta_rain.rds")) %>% 
  mutate(type = "rain") %>% 
  select(hydrogen = hydrogen_mean, oxygen = oxygen_mean, type)

  

## sibs  data
sibs_data <- 
  readRDS(file = here("delta_E", "0_raw_data", "sibs", "period_1.rds")) %>% 
  filter(Date %in% my_observation_dates_1) %>% 
  select(my_timestamp = date_time, type = sampling, hydrogen = H2, oxygen = O18) %>% 
  filter(oxygen < 37) %>% #  remove an outlier in June 6 afternoon leaf data (heavier isotopic ratios)
  mutate(type = as.character(type), 
         type = if_else(type == "root", "xylem", type)) %>% 
  select(hydrogen, oxygen, type)

head(sibs_data)
summary(sibs_data)  
## combine data in two ways
## full_join 

dual_isotope <- 
  bind_rows(water_vapor, rain_data, sibs_data)

dplyr::bind_rows(head(dual_isotope), tail(dual_isotope))


dual_isotope$type <- 
  factor(dual_isotope$type, 
         levels = c("vapor", "rain", "soil", "xylem", "leaf"),
         ordered = TRUE)

saveRDS(dual_isotope,
        here("dual_isotope", "3_output", "dual_isotope.rds"))

# dual_isotope <- readRDS(file = here("dual_isotope", "3_output", "dual_isotope.rds"))
##

head(dual_isotope)

# Plot dual_isotope graph -------------------------------------------------
rain_may_june <- 
  readRDS(file = here("dual_isotope", "3_output", "delta_rain.rds")) %>% 
  mutate(month = lubridate::month(my_date)) %>% 
  filter(month >=5, month <=6)



my_colors <- c("Deep Sky Blue", "Blue","Goldenrod 1", "Saddle Brown",  "Lime Green")

met_line <- data.frame(slope = c(7.32, 8.2), intercept = c(9.50, 11.3),
                       lty =c("dotted", "dashed"), name = c("LMWL", "GMWL"))

formula <- y ~ x

dual_isotope_plot <- 
  ggplot(data = dual_isotope, aes(x = oxygen, y = hydrogen, color = type)) +
  geom_smooth(method = "lm",  se = TRUE,
              show.legend = NA, inherit.aes = TRUE) +
  geom_abline(data= met_line, 
              mapping = aes(slope=slope, intercept=intercept, linetype = factor(name)),
              col = "gray", size = 0.9) +
  geom_point(alpha = 0.8) +
  geom_point(data = rain_may_june, 
             mapping = aes(x = oxygen_mean, y = hydrogen_mean), 
             shape = 21, colour = "red",
             fill = NA,  inherit.aes = FALSE) + 
  scale_color_manual(name = "Sample Types", values = my_colors) +
  scale_linetype(name="Meteoric Lines") +
  scale_x_continuous(breaks = seq(-30, 40, 10)) +
  scale_y_continuous(breaks = seq(-150, 150, 30)) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               label.x.npc = "left", label.y.npc =  "top",
               formula = formula, parse = TRUE, size = 4) +
  #stat_fit_glance(method = "lm", 
  #                method.args = list(formula = formula),
  #                label.x = "middle",
  #                label.y = "top",
  #                aes(label = paste("italic(P)*\"-value = \"*", 
  #                                  signif(..p.value.., digits = 2), sep = "")),
  #                parse = TRUE, size = 3.5) +
  xlab(sprintf("\u03b4\ub9\u2078O  (\u2030 vs VSMOW)")) +
  ylab("\u03b4\u00b2H  (\u2030 vs VSMOW)")+
  #theme_classic() +  ## classic theme
  theme(legend.position = c(0.8, 0.25), 
        legend.title = element_text(face = "bold", size = 12), 
        legend.text = element_text(size = 11),
        axis.title.x = element_text(size = 14, margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 8, b = 0, l = 0)),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggExtra::ggMarginal(dual_isotope_plot, type = "density", margins = "both", 
           groupColour = TRUE,
           groupFill = TRUE)

ggsave(file = here("dual_isotope", "3_figure", "Figure_5_dual_isotope_plot_ggsave.eps"), 
       ggMarginal(dual_isotope_plot, type = "density", margins = "both", 
                  groupColour = TRUE,
                  groupFill = TRUE), 
       width = 9.55, height = 9.55, dpi = 1200, 
       device=cairo_ps)

export::graph2eps(file = here("dual_isotope", "3_figure", "dual_plot_isotope_export.eps"),
                  width = 8.40, height = 9.55)



###
# Format picarro water vapor data from 9:00-7:00 each day -----------------
my_time_df <- 
  crossing(hour = c(9:19), minutes = c(":00:00", ":30:00")) %>% 
  unite(col = "day_time", sep = "", remove = TRUE)

my_sequence <- 
  crossing(date = unique(picarro_data$my_date), my_time_df) %>%
  mutate(date = as.character(date)) %>% 
  unite(col = "my_timestamp", date:day_time, sep = " ", remove = TRUE) %>% 
  mutate(my_timestamp = lubridate::ymd_hms(my_timestamp))


water_vapor_square_format <- 
  my_sequence %>% 
  left_join(water_vapor, by = "my_timestamp")

# for Statistics of rainfall isotopic composition -------------------------
### for statistics
summary(rain_annual)
rain_annual_revised <- 
  rain_annual %>% 
  mutate(month = lubridate::month(my_timestamp)) %>% 
  group_by(month) %>% 
  summarise(
    hydrogen_min  = min(hydrogen),
    hydrogen_max  = max(hydrogen),
    hydrogen_mean = mean(hydrogen),
    hydrogen_sd   = sd(hydrogen),
    oxygen_min    = min(oxygen),
    oxygen_max    = max(oxygen),
    oxygen_mean   = mean(oxygen),
    oxygen_sd     = sd(oxygen)
  )

rain_annual %>% 
mutate(month = lubridate::month(my_timestamp)) %>% 
filter(month >=5, month <= 6) %>% 
summarise(
  hydrogen_min  = min(hydrogen),
  hydrogen_max  = max(hydrogen),
  hydrogen_mean = mean(hydrogen),
  hydrogen_sd   = sd(hydrogen),
  oxygen_min    = min(oxygen),
  oxygen_max    = max(oxygen),
  oxygen_mean   = mean(oxygen),
  oxygen_sd     = sd(oxygen)
)


# For Dew water (only two data, not use this time)-----------------------------------------------------------

dew_data <- 
  readRDS(file = here("dual_isotope", "0_raw_data", "dew.rds")) %>% 
  select(my_timestamp = date, hydrogen = H2, oxygen = O18) %>% 
  mutate(type = "dew")


# SIBS data ---------------------------------------------------------------





# Summary statistics ------------------------------------------------------
dplyr::bind_rows(head(dual_isotope), tail(dual_isotope))

dual_isotope_summary <- 
dual_isotope %>% 
  group_by(type) %>% 
  summarise(
    hydrogen_min  = min(hydrogen),
    hydrogen_max  = max(hydrogen),
    hydrogen_mean = mean(hydrogen),
    hydrogen_sd   = sd(hydrogen),
    oxygen_min    = min(oxygen),
    oxygen_max    = max(oxygen),
    oxygen_mean   = mean(oxygen),
    oxygen_sd     = sd(oxygen)
  )

###########################
### done 
###########################

















## below are useless
#############
# intra-seasonal and diurnal dynamics -------------------------------------------
dplyr::bind_rows(head(dual_isotope), tail(dual_isotope))


evaporation_site_ISS <-
  readRDS(file = here("delta_T", "3_output", "iss_result_brief.rds")) %>% 
  filter(my_timestamp != lubridate::ymd_hms("2016-06-06 17:00:00")) %>% 
  select(my_timestamp, isotope, evaporation_site) %>% 
  spread(key = isotope, value = evaporation_site) %>% 
  rename(hydrogen = `{ }^{2}*H`, oxygen = `{ }^{18}*O`) %>% 
  mutate(type = "evapo_site")

dual_isotope_time_series <- 
  dual_isotope %>% 
  filter(my_timestamp >= lubridate::ymd("2016-06-04"), 
         my_timestamp <= lubridate::ymd("2016-07-01")) %>% 
  filter(type != "rain") %>% 
  mutate(type = as.character(type)) %>% 
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

table2tex(x = field_camp_duration, type = "TEX", standAlone = FALSE)

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
# Plot dual_isotope graph -------------------------------------------------





###### rain data removed
summary(dual_isotope_time_series)

dual_isotope_time_series_input <- 
  dual_isotope_time_series %>% 
  filter(time_category != "night",
         my_date != lubridate::ymd("2016-06-13"),
         my_date != lubridate::ymd("2016-06-27")) %>% 
  mutate(my_date = format(my_date, "%b-%d"))

limits <- 
  crossing(date = as.character(my_observation_dates), time =  c("10:00:00", "19:00:00")) %>% 
  dplyr::unite(col = "time_limits", c("date", "time"),sep = " ", remove = TRUE) %>% 
  mutate()



my_colors <- 
  c("Deep Sky Blue", "Goldenrod 1", "Saddle Brown",  "Lime Green", "darkcyan") 

isotope_type <-  
  c("italic(delta[V])","italic(delta[soil])","italic(delta[X])",
    "italic(delta[leaf])","italic(delta[Le])")

#### plot time series

  ggplot(data = dual_isotope_time_series_input,
         mapping = aes(x = my_timestamp, y = delta, 
                     color = type,
                     shape = type)) +#, shape = time_category)) +
  geom_point() +
  geom_line() +
  scale_color_manual(name = "", 
                     labels = parse_format()(isotope_type),
                     values = my_colors) +
  scale_shape_manual(name = "", 
                     labels = parse_format()(isotope_type),
                     values = c(21:25)) +
  scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%H")) +
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
      axis.title = element_text(size = 14),
      axis.text.x = element_text(margin = margin(t = 0, r = 0, b = 8, l = 0)),
      axis.text.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 8)))


export::graph2eps(file = here("dual_isotope", "3_figure", "dual_isotope_time_series_plot.eps"),
                  width = 16, height = 9.55)
