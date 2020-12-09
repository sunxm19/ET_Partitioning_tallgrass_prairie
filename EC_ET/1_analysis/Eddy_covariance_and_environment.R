## read in eddy covariance data
getwd()

tower_south <- 
  readRDS(file = here("EC_ET", "0_raw_data", "tower_south_ultimate.rds")) %>% 
  select(my_timestamp = timestamp, 
         Rn = Rn_1_1_1, 
         PPFD = PPFD_1_1_1,
         Tair, Tsoil, RH, specific_humidity, Tdew, e, es,  vpd = VPD.x,
         h2o_molar_density, h2o_mole_fraction, h2o_mixing_ratio, water_vapor_density, 
         Ustar, wind_speed,  max_wind_speed, 
         h2o_flux, rand_err_h2o_flux, 
         air_pressure, air_density, air_heat_capacity, air_molar_volume,
         T_soil = Ts_1_1_1,
         SWC = SWC_1_1_1,
         ET = ET_result, ET_sd = ET_result_sd) %>% 
  mutate(my_timestamp = lubridate::ymd_hms(my_timestamp))

names(tower_south)
dplyr::bind_rows(head(tower_south), tail(tower_south))


#########################
rain_2016 <- 
  read_excel(here("EC_ET", "0_raw_data", "eddypro_north_biomet_2017-03-25T235617_adv.xlsx"),
             sheet = "Sheet1", col_names = TRUE) %>% 
  select(timestamp, P_1_1_1) %>% 
  filter(P_1_1_1 != -9999) %>% 
  mutate(my_year  = lubridate::year(timestamp),
         P_mm         = P_1_1_1 * 1000) %>% 
  filter(my_year == 2016) %>% 
  select(my_timestamp = timestamp, P_mm)

dplyr::bind_rows(head(rain_2016), tail(rain_2016))
sum(rain_2016$P_mm)

tower_south_rain <- 
  tower_south %>% 
  left_join(rain_2016, by = "my_timestamp") %>% 
  filter(my_timestamp >= lubridate::ymd("2016-05-01"),
         my_timestamp < lubridate::ymd("2016-09-01")) %>% 
  arrange(my_timestamp)

names(tower_south_rain)  


####  soil mositure

soil_mositure <- 
  read.csv(here("EC_ET", "0_raw_data", "soil_station_moisuture.csv"), header = TRUE) %>% 
  mutate(my_timestamp = lubridate::mdy_hm(timestamp)) %>% 
  select(my_timestamp, contains("south"))

### eddy and biometeorology data

eddy_biomet_south_rain_soil <- 
  left_join(tower_south_rain, soil_mositure, by = "my_timestamp") %>% 
  rename(timestamp = my_timestamp,
         P = P_mm,
         soil_moisture_5cm  = south_5,
         soil_moisture_20cm = south_20,
         soil_moisture_45cm = south_45,
         soil_moisture_80cm = south_80) %>% 
  select(- "T_soil") 

saveRDS(eddy_biomet_south_rain_soil, 
        here("EC_ET", "3_output", "eddy_biomet_south_rain_soil.rds"))

write.csv(eddy_biomet_south_rain_soil, 
          here("EC_ET", "3_output", "eddy_biomet_south_rain_soil.csv"))



# daily analysis ----------------------------------------------------------

dplyr::bind_rows(head(eddy_biomet_south_rain_soil), tail(eddy_biomet_south_rain_soil))

eddy_biomet_daily <- 
  eddy_biomet_south_rain_soil %>% 
  mutate(date = lubridate::date(timestamp)) %>% 
  group_by(date) %>% 
  summarise(ET_sum = sum(ET),
            ET_sd  = sqrt(sum(ET_sd^2)),
            P_mm   = sum(P),
            depth5_mean  = mean(soil_moisture_5cm),
            depth20_mean = mean(soil_moisture_20cm),
            depth45_mean = mean(soil_moisture_45cm),
            depth80_mean = mean(soil_moisture_80cm),
            depth5_intial = first(soil_moisture_5cm),
            depth5_last   = last(soil_moisture_5cm),
            depth20_intial = first(soil_moisture_20cm),
            depth20_last   = last(soil_moisture_20cm),
            depth45_intial = first(soil_moisture_45cm),
            depth45_last   = last(soil_moisture_45cm),
            depth80_intial = first(soil_moisture_80cm),
            depth80_last   = last(soil_moisture_80cm)) %>% 
  ungroup() %>% 
  mutate(change_0_10   =  (depth5_last - depth5_intial) * 100 , ## positive stands for depletion
         change_10_30  =  (depth20_last - depth20_intial) * 200, ## negative stands for recharge
         change_30_60  =  (depth45_last - depth45_intial) * 300,
         change_60_100 =  (depth80_last - depth80_intial) * 400) %>% 
  select(date, ET_sum, ET_sd, P_mm, contains("mean"), contains("change"))

head(eddy_biomet_daily)

saveRDS(eddy_biomet_daily, here("EC_ET", "3_output", "eddy_biomet_daily.rds"))

# eddy_biomet_daily <- readRDS(here("EC_ET", "3_output", "eddy_biomet_daily.rds"))
head(eddy_biomet_daily)
###############

### soil vertical plot
# eddy_biomet_south_rain_soil <- 
#  readRDS(here("EC_ET", "3_output", "eddy_biomet_south_rain_soil.rds"))
#########################
my_observation_dates_1 <- 
  c(lubridate::ymd("2016-06-04"),
    lubridate::ymd("2016-06-06"),
    lubridate::ymd("2016-06-08"),
    lubridate::ymd("2016-06-10"),
    lubridate::ymd("2016-06-12"),
    lubridate::ymd("2016-06-27"),
    lubridate::ymd("2016-06-28"),
    lubridate::ymd("2016-06-29"),
    lubridate::ymd("2016-06-30"))

names(eddy_biomet_south_rain_soil)

soil_theta_daily <- 
  eddy_biomet_south_rain_soil %>%
  select(timestamp, contains("soil_moisture")) %>% 
  mutate(Date = lubridate::date(timestamp)) %>% 
  filter(Date %in% my_observation_dates_1) %>% 
  mutate(Date = format(strptime(Date, format = "%Y-%m-%d"), "%b-%d")) %>% 
  group_by(Date) %>% 
  summarise(depth5_mean  = mean(soil_moisture_5cm),
            depth20_mean = mean(soil_moisture_20cm),
            depth45_mean = mean(soil_moisture_45cm),
            depth80_mean = mean(soil_moisture_80cm),
            depth5_sd    = sd(soil_moisture_5cm),
            depth20_sd   = sd(soil_moisture_20cm),
            depth45_sd   = sd(soil_moisture_45cm),
            depth80_sd   = sd(soil_moisture_80cm)) %>% 
  gather(key = depth_type, value, -Date) %>% 
  separate(col = "depth_type", into = c("depth", "type"), sep = "_", remove = TRUE) %>% 
  spread(key = "type", value = "value", fill = NA) %>% 
  separate(col = "depth", into = c("depth_word", "depth"), sep = "th", remove = TRUE) %>% 
  select(-"depth_word") %>% 
  mutate(depth = as.numeric(depth),
         Date = as.factor(Date)) %>% 
  arrange(Date, depth)


dplyr::bind_rows(head(soil_theta_daily), tail(soil_theta_daily))

# plot_soil_moisuture_profile <- 
ggplot(data = soil_theta_daily,
       mapping = aes(x = depth, y = mean, col = Date, shape = Date)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), 
                width = 0.3, position = position_dodge(2)) +
  geom_point(position = position_dodge(2)) +
  geom_line(position = position_dodge(2)) +
  coord_flip() +
  scale_shape_manual(values = 1:nlevels(soil_theta_daily$Date)) +
  scale_x_reverse(breaks = c(5, 20, 45, 80)) +
  ylab(expression(paste(theta[soil]~~(m^3~m^-3)))) +
  xlab("Depth (cm)") +
  #theme_classic() +
  theme(legend.position = c(0.2, 0.34),
        strip.text.x = element_text(face = "bold", size = 12),
        strip.text.y = element_text(face = "bold", size = 14),
        legend.title = element_text(face = "bold", size = 12), 
        legend.text = element_text(size = 12),
        legend.margin = margin( r = 8), 
        legend.background = element_rect(colour = "white",
                                         fill = "white",
                                         size = 4),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 8, b = 0, l = 0)),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))


export::graph2eps(file = here("EC_ET", "3_figure", "figure_4_soil_moisture_vertical_plot.eps"),
                  width = 7.25, height = 7.25)
export::graph2png(file = here("EC_ET", "3_figure", "figure_4soil_moisture_vertical_plot.eps"),
                  width = 7.25, height = 7.25)

####

# biomet dynamics --------------------------------------------------------
date_june_01 <- as.Date("2016-06-01", tz = "UTC")
date_june_13 <- as.Date("2016-06-13", tz = "UTC")
date_june_14 <- as.Date("2016-06-14", tz = "UTC")
date_july_01 <- as.Date("2016-07-01", tz = "UTC")

##################  two field camps
period_1 <- c(as.Date("2016-06-04", tz = "UTC"), 
              as.Date("2016-06-06", tz = "UTC"),
              as.Date("2016-06-08", tz = "UTC"), 
              as.Date("2016-06-10", tz = "UTC"), 
              as.Date("2016-06-12", tz = "UTC"))

period_2 <- seq(as.Date("2016-06-27", tz = "UTC"), 
                as.Date("2016-06-30", tz = "UTC"),
                by = "day")

field_camp <- c(period_1, period_2)
#############

environment_eddy_diurnal <- 
  eddy_biomet_south_rain_soil %>% 
  select(timestamp, Rn, PPFD, Tair,  Tsoil, RH,  vpd, 
         wind_speed, max_wind_speed,
         ET, ET_sd, P,
         soil_moisture_5cm, soil_moisture_20cm, 
         soil_moisture_45cm, soil_moisture_80cm) %>% 
  mutate(my_time = format(strptime(timestamp, format = "%Y-%m-%d %H:%M"), "%H:%M"))

environment_eddy_diurnal$my_time_decimal <- 
  sapply(strsplit(environment_eddy_diurnal$my_time,":"),
         function(x) {
           x <- as.numeric(x)
           x[1]+x[2]/60
         }
  )


environment_eddy_diurnal_summary <- 
  environment_eddy_diurnal %>% 
  mutate(my_date = lubridate::date(timestamp)) %>% 
  filter(my_date  %in% field_camp) %>% 
  select(timestamp, my_date, my_time_decimal,
         Rn, Tair, wind_speed,RH, vpd,
         soil_5 = soil_moisture_5cm, Tsoil) %>% 
  mutate( vpd = vpd/100,
          camp = case_when(
            between(my_date, date_june_01, date_june_13)  ~ "camp_1",
            between(my_date, date_june_14, date_july_01) ~ "camp_2",
            TRUE                                         ~  "camp_x"
          )) %>% 
  group_by(my_time_decimal, camp) %>% 
  summarise(
    Rn_mean       = mean(Rn, na.rm = TRUE),
    Rn_sd         = sd(Rn),
    tair_mean     = mean(Tair),
    tair_sd       = sd(Tair),
    wind_mean     = mean(wind_speed),
    wind_sd       = sd(wind_speed),
    hair_mean     = mean(RH),
    hair_sd       = sd(RH),
    vpd_mean      = mean(vpd),
    vpd_sd        = sd(vpd),
    thetasoil_mean = mean(soil_5),
    thetasoil_sd  = sd(soil_5),
    tsoil_mean    = mean(Tsoil),
    tsoil_sd      = sd(Tsoil)
  )

summary(environment_eddy_diurnal_summary)
factor(environment_eddy_diurnal_summary$camp)


environment_eddy_diurnal_summary$camp <- 
  factor(environment_eddy_diurnal_summary$camp, 
         levels = c("camp_1", "camp_2"), 
         ordered = TRUE)

levels(environment_eddy_diurnal_summary$camp) <-
  c(expression(Camp~1), expression(Camp~2))

head(environment_eddy_diurnal_summary)
names(environment_eddy_diurnal_summary)

environment_eddy_diurnal_summary_input <- 
  environment_eddy_diurnal_summary %>%
  gather(key, value, -c("my_time_decimal", "camp")) %>% 
  separate(key, into = c("variable", "type"),  sep = "_") %>% 
  spread(key = type, value)

###

environment_eddy_diurnal_summary_input$variable <- 
  factor(environment_eddy_diurnal_summary_input$variable, 
         levels = c("Rn", "tair", "wind", "hair", "vpd", "tsoil", "thetasoil"), 
         ordered = TRUE)

levels(environment_eddy_diurnal_summary_input$variable) <- 
  c(expression(paste(italic(R[n]), ~~(W~m^{-2}))),
    expression(paste(italic(T[air])~~(~degree~C))),
    expression(paste(italic(u),~~(m~s^{-1}))),
    expression(paste(italic(h[air]),~~("%"))),
    expression(paste(italic(VPD),~~(hPa))),
    expression(paste(italic(T[soil])~~(~degree~C))),
    expression(paste(italic(theta[0.05])~(m^{3}~m^{-3}))))

saveRDS(environment_eddy_diurnal_summary_input, 
        "environment_eddy_diurnal_summary_input.rds")



ggplot(data = environment_eddy_diurnal_summary_input, 
       mapping = aes(x = my_time_decimal, y = mean, color = variable)) +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean + sd), width=.2) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  ylab(NULL) +
  xlab("Time of Day (hours)")+
  facet_grid(variable ~ camp, scales="free_y",
             labeller = label_parsed,
             switch = "y") +
  scale_x_continuous(breaks = seq(0, 24, by = 6)) +
  scale_color_manual(values=c("orangered", "skyblue3", "turquoise1",
                              "cyan4", "royalblue","goldenrod3", "darkorange")) +
  theme(legend.title = element_blank(),#(size = 12),
        legend.position="none", #face = "bold", ), 
        axis.title = element_text(size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.text.y = element_text(size=10),# margin = unit(c(0, 4, 0, 0), "mm")),
        axis.text.x = element_text(size=10), 
        strip.background.y = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) #, colour = "orange", angle = 90)

export::graph2eps(file = "environment_eddy_diurnal.eps",
                  width = 7.25, height = 8.37)
###



# Long-term environment ---------------------------------------------------


######################################################
#####################################################
##  plot daily average (daytime) over long-term
###  rain, NDVI, 
####################################################
environment_long_term <- 
  eddy_biomet_south_rain_soil %>% 
  select(timestamp, P, Rn, PPFD, Tair, Tsoil, 
         rh = RH,  vpd, 
         wind_speed, 
         wind_max = max_wind_speed,
         soil_moisture_5cm, soil_moisture_20cm, 
         soil_moisture_45cm, soil_moisture_80cm) %>% 
  mutate(my_date = lubridate::date(timestamp),
         my_time = format(strptime(timestamp, format = "%Y-%m-%d %H:%M"), "%H:%M")) %>% 
  mutate(vpd_hpa = vpd/100)

bio_metero_grassland <- 
  readRDS(file = here("EC_ET","0_raw_data","grassland_em50.rds")) %>% 
  rename(my_timestamp = date_time)

head(bio_metero_grassland)

environment_long_term <- 
  environment_long_term %>% 
  as_tibble() %>% 
  left_join(bio_metero_grassland, by = c("timestamp" = "my_timestamp"))

names(environment_long_term)

environment_long_term$my_time_decimal <- 
  sapply(strsplit(environment_long_term$my_time,":"),
         function(x) {
           x <- as.numeric(x)
           x[1]+x[2]/60
         }
  )


#### precipitation and leaf wetness
precipitation_leaf_wetness_daily <- 
  environment_long_term %>% 
  select(my_date, P, LWS_mean) %>% 
  group_by(my_date) %>% 
  summarise(P_daily  = sum(P, na.rm = TRUE),
            leaf_wet_duration = sum(LWS_mean, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(wet_leaf_ratio = leaf_wet_duration/(24*60))

summary(precipitation_leaf_wetness_daily)


#### meteorological and climate in daytime

meterorological_daytime <- 
  environment_long_term %>% 
  select(my_date, my_time_decimal, Rn, PPFD = PAR_mean, Tair, Tsoil,
         wind_speed, wind_max,
         rh, vpd_hpa, 
         Tsoil, contains("soil_moisture")) %>% 
  filter(my_time_decimal>= 9 & my_time_decimal <= 19) %>% 
  group_by(my_date) %>% 
  summarise(Rn_mean    = mean(Rn, na.rm = TRUE),
            Rn_sd      = sd(Rn, na.rm = TRUE),
            PPFD_mean  = mean(PPFD, na.rm = TRUE),
            PPFD_sd    = sd(PPFD, na.rm = TRUE),
            Tair_mean  = mean(Tair, na.rm = TRUE),
            Tair_sd    = sd(Tair, na.rm = TRUE),
            Tsoil_mean = mean(Tsoil, na.rm = TRUE),
            Tsoil_sd   = sd(Tsoil, na.rm = TRUE),
            wind_mean  = mean(wind_speed, na.rm = TRUE),
            wind_sd    = sd(wind_speed, na.rm = TRUE),
            windmax_mean  = mean(wind_max, na.rm = TRUE),
            windmax_sd    = sd(wind_max, na.rm = TRUE),
            rh_mean    = mean(rh, na.rm = TRUE),
            rh_sd      = sd(rh, na.rm = TRUE),
            vpd_mean   = mean(vpd_hpa, na.rm = TRUE),
            vpd_sd     = sd(vpd_hpa, na.rm = TRUE),
            theta5_mean= mean(soil_moisture_5cm, na.rm = TRUE),
            theta5_sd  = sd(soil_moisture_5cm, na.rm = TRUE),
            theta20_mean= mean(soil_moisture_20cm, na.rm = TRUE),
            theta20_sd  = sd(soil_moisture_20cm, na.rm = TRUE),
            theta45_mean= mean(soil_moisture_45cm, na.rm = TRUE),
            theta45_sd  = sd(soil_moisture_45cm, na.rm = TRUE),
            theta80_mean= mean(soil_moisture_80cm, na.rm = TRUE),
            theta80_sd  = sd(soil_moisture_80cm, na.rm = TRUE)) %>% 
  ungroup()



ndvi_daytime <- 
  environment_long_term %>% 
  select(my_date, my_time_decimal, NDVI_mean) %>% 
  filter(my_time_decimal>=12 & my_time_decimal <= 14) %>% 
  group_by(my_date) %>% 
  summarise(ndvi_mean  = mean(NDVI_mean, na.rm = TRUE),
            ndvi_sd    = sd(NDVI_mean, na.rm = TRUE)) %>% 
  ungroup()

environment_daily <- 
  left_join(precipitation_leaf_wetness_daily,
            meterorological_daytime,
            by = "my_date") %>% 
  left_join(ndvi_daytime, by = "my_date") %>% 
  filter(my_date >= as.Date("2016-05-20", tz = "UTC"),
         my_date <= as.Date("2016-06-30", tz = "UTC"))

saveRDS(environment_daily, "environment_daily.rds")
# environment_daily <- readRDS(file = "environment_daily.rds")
# Start to draw -----------------------------------------------------------

dplyr::bind_rows(head(environment_daily), tail(environment_daily))

date_breaks <- 
  c(as.Date("2016-05-20", tz = "UTC"),
    as.Date("2016-06-04", tz = "UTC"),
    as.Date("2016-06-12", tz = "UTC"),
    as.Date("2016-06-20", tz = "UTC"),
    as.Date("2016-06-27", tz = "UTC"),
    as.Date("2016-06-30", tz = "UTC"))

camp_period <- 
  data.frame(xmin = c(as.Date("2016-06-04", tz = "UTC"),
                      as.Date("2016-06-27", tz = "UTC")),
             xmax = c(as.Date("2016-06-12", tz = "UTC"),
                      as.Date("2016-06-30", tz = "UTC")),
             ymin = c(-Inf, -Inf),
             ymax = c(Inf, Inf))

rain_lws_data<- 
  environment_daily %>% 
  select(my_date, P_daily, wet_leaf_ratio) 

camp_period_2 <- 
  camp_period %>% 
  tidyr::crossing(rain_lws_data %>% 
                    summarise(x1 = min(my_date),
                              x2 = max(my_date)) %>% 
                    ungroup) %>% 
  filter(xmin >= x1 & xmax <= x2) %>% 
  ungroup()



rain_plot <- 
  ggplot(data = rain_lws_data, mapping = aes(x = my_date, y = P_daily)) + 
  geom_rect(data = camp_period_2, inherit.aes = FALSE,
            mapping = aes(xmin = xmin, xmax = xmax, 
                          ymin = ymin, ymax = ymax),
            fill = 'red', alpha = 0.1) +
  geom_col(fill = "blue") +
  scale_y_reverse() + 
  annotate("text", x = as.Date("2016-07-02"), y = 0, label = "(a)") +
  labs(y = expression(atop(italic(P), paste((mm~day^-1))))) +
  scale_x_date(breaks = date_breaks, date_labels = "%b-%d", expand = c(0, 1)) +
  theme(axis.ticks.length.x = unit(0, "cm"),
        axis.text.y = element_text(size=10),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=12,  
                                    margin = margin(t = 0, r = 5, b = 0, l = 10)),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))


lws_plot <- 
  ggplot(data = rain_lws_data, mapping = aes(x = my_date, y = wet_leaf_ratio*100)) + 
  geom_rect(data = camp_period_2, inherit.aes = FALSE,
            mapping = aes(xmin = xmin, xmax = xmax, 
                          ymin = ymin, ymax = ymax),
            fill = 'red', alpha = 0.1) +
  geom_line(color = "green4") +
  geom_point(color = "green4") +
  annotate("text", x = as.Date("2016-07-02"), y = 43, label = "(b)") +
  labs(y = expression(atop(italic(LWR), paste(("%"))))) +
  scale_x_date(breaks = date_breaks, date_labels = "%b-%d", expand = c(0, 1)) +
  theme(axis.ticks.length.x = unit(0, "cm"),
        axis.text.y = element_text(size=10),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=12,
                                    margin = margin(t = 0, r = 5, b = 0, l = 10)),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))


###############################################
names(environment_daily)

environment_daily_2 <- 
  environment_daily %>% 
  select(- c("P_daily", "leaf_wet_duration", "wet_leaf_ratio")) %>% 
  gather(key, value, -c("my_date")) %>% 
  separate(key, into = c("variable", "type"),  sep = "_") %>% 
  spread(key = type, value)

head(environment_daily_2)


### radiation plot

radiation_data <- 
  environment_daily_2 %>%
  filter(variable == "Rn" | variable == "PPFD") %>% 
  mutate(mean = if_else(variable == "PPFD", mean*0.327, 
                        if_else(variable == "Rn", mean, NULL)),
         sd = if_else(variable == "PPFD", sd*0.327, 
                      if_else(variable == "Rn", sd, NULL)))

summary(radiation_data)
summary(environment_daily$Rn_mean)
summary(environment_daily$PPFD_mean)
summary(radiation_data$mean)

head(radiation_data)

camp_period_3 <- 
  camp_period %>% 
  tidyr::crossing(radiation_data %>%
                    summarise(x1 = min(my_date),
                              x2 = max(my_date)) %>% 
                    ungroup) %>% 
  filter(xmin >= x1 & xmax <= x2) %>% 
  ungroup()

my_labels_1 <- c("italic(PPFD)" ,"italic(R[n])" )
my_values_1 <- c("deepskyblue", "orangered")

my_col_1 <- c("italic(PPFD)" = "deepskyblue", 
              "italic(R[n])" =  "orangered")


radiation_plot <- 
  ggplot(data = radiation_data, mapping = aes(x = my_date, y = mean, 
                                              shape = variable, color = variable)) +
  geom_rect(data = camp_period_3, inherit.aes = FALSE,
            mapping = aes(xmin = xmin, xmax = xmax, 
                          ymin = ymin, ymax = ymax),
            fill = 'red', alpha = 0.1) +
  #geom_smooth(method = "auto", se = TRUE, aes(fill = variable), 
  #            fullrange=FALSE, level=0.95,  alpha = 0.1) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  scale_x_date(breaks = date_breaks, date_labels = "%m-%d", expand = c(0, 1)) +
  annotate("text", x = as.Date("2016-07-02"), y = 500, label = "(c)") +
  labs(y = expression(atop(Radiation, paste((W~m^{-2}))))) +
  scale_colour_manual(values = my_values_1,
                      labels = c(parse(text = my_labels_1[1]),
                                 parse(text = my_labels_1[2]))) +
  scale_fill_manual(values = my_values_1,
                    labels = c(parse(text = my_labels_1[1]),
                               parse(text = my_labels_1[2]))) +
  scale_shape_manual(values = c(15:16),
                     labels = c(parse(text = my_labels_1[1]),
                                parse(text = my_labels_1[2]))) +
  theme(axis.ticks.length.x = unit(0, "cm"),
        legend.title = element_blank(),#(size = 12),
        legend.position = "right",
        legend.text = element_text(face = "italic"),
        legend.text.align = 0.5,
        axis.text.y = element_text(size=10),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12,
                                    margin = margin(t = 0, r = 5, b = 0, l = 10)),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))






## PPFD unit conversion comes from 
## https://www.apogeeinstruments.com/conversion-ppfd-to-watts/

###


### 
head(environment_daily_2)


temp_data <- 
  environment_daily_2 %>%
  filter(variable == "Tair" | variable == "Tsoil") 


camp_period_4 <- 
  camp_period %>% 
  tidyr::crossing(temp_data %>% 
                    summarise(x1 = min(my_date),
                              x2 = max(my_date)) %>% 
                    ungroup) %>% 
  filter(xmin >= x1 & xmax <= x2) %>% 
  ungroup()


head(temp_data)


my_labels_2 <- c("italic(T[air])" ,"italic(T[soil])" )
my_values_2 <- c("blue3", "goldenrod3")

my_col_2 <- c("italic(T[air])"  = "blue3", 
              "italic(T[soil])" =  "goldenrod3")


temp_plot <- 
  ggplot(data = temp_data, 
         mapping = aes(x = my_date, 
                       y = mean, 
                       color = variable, 
                       shape = variable)) +
  geom_rect(data = camp_period_4, inherit.aes = FALSE,
            mapping = aes(xmin = xmin, xmax = xmax, 
                          ymin = ymin, ymax = ymax),
            fill = 'red', alpha = 0.1) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  scale_x_date(breaks = date_breaks, date_labels = "%m-%d", expand = c(0, 1)) +
  annotate("text", x = as.Date("2016-07-02"), y = 33.5, label = "(d)") +
  labs(y = expression(atop(Temperature,paste((degree~C))))) +
  scale_colour_manual(values = my_values_2,
                      labels = c(parse(text = my_labels_2[1]),
                                 parse(text = my_labels_2[2]))) +
  scale_fill_manual(values = my_values_2,
                    labels = c(parse(text = my_labels_2[1]),
                               parse(text = my_labels_2[2]))) +
  scale_shape_manual(values = c(15:16),
                     labels = c(parse(text = my_labels_2[1]),
                                parse(text = my_labels_2[2]))) +
  theme(axis.ticks.length.x = unit(0, "cm"),
        legend.title = element_blank(),#(size = 12),
        legend.position = "right",
        legend.text = element_text(face = "italic"),
        axis.text.y = element_text(size=10),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=12,
                                    margin = margin(t = 0, r = 5, b = 0, l = 10)),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))



####
water_vapor_data <- 
  environment_daily_2 %>%
  filter(variable == "rh" | variable == "vpd") %>% 
  mutate(mean_1 = if_else(variable == "vpd", mean*2, 
                          if_else(variable == "rh", mean, NULL)))
camp_period_5 <- 
  camp_period %>% 
  tidyr::crossing(water_vapor_data %>% 
                    summarise(x1 = min(my_date),
                              x2 = max(my_date)) %>% 
                    ungroup) %>% 
  filter(xmin >= x1 & xmax <= x2) %>% 
  ungroup()

head(water_vapor_data)
my_labels_3 <- c("italic(h[air])" ,"italic(VPD)" )
my_values_3 <- c("cyan4", "royalblue")

my_col_3 <- c("italic(h[air])"  = "cyan4", 
              "italic(VPD)" =  "royalblue")


water_vapor_plot <- 
  ggplot(data = water_vapor_data, mapping = aes(x = my_date, y = mean_1, color = variable)) +
  geom_rect(data = camp_period_5, inherit.aes = FALSE,
            mapping = aes(xmin = xmin, xmax = xmax, 
                          ymin = ymin, ymax = ymax),
            fill = 'red', alpha = 0.1) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  scale_x_date(breaks = date_breaks, date_labels = "%b-%d", expand = c(0, 1)) +
  scale_y_continuous(sec.axis = sec_axis(~.*0.5, 
                                         name = expression(atop(italic(VPD), paste((hPa)))))) +
  labs(y = expression(atop(italic(h[air]), paste(("%"))))) +  
  annotate("text", x = as.Date("2016-07-02"), y = max(water_vapor_data$mean_1)-5, label = "(f)") +
  scale_colour_manual(values = my_values_3,
                      labels = c(parse(text = my_labels_3[1]),
                                 parse(text = my_labels_3[2]))) +
  scale_fill_manual(values = my_values_3,
                    labels = c(parse(text = my_labels_3[1]),
                               parse(text = my_labels_3[2]))) +
  scale_shape_manual(values = c(0:1),
                     labels = c(parse(text = my_labels_3[1]),
                                parse(text = my_labels_3[2]))) +
  theme(axis.ticks.length.x = unit(0, "cm"),
        legend.title = element_blank(),#(size = 12),
        legend.position = "right",
        legend.text = element_text(face = "italic"),
        axis.text.y = element_text(size=10),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=12,
                                    margin = margin(t = 0, r = 5, b = 0, l = 10)),
        axis.line.y.left = element_line(color = "cyan4"), 
        axis.ticks.y.left = element_line(color = "cyan4"),
        axis.line.y.right = element_line(color = "royalblue"), 
        axis.ticks.y.right = element_line(color = "royalblue"),
        axis.title.y.left =  element_text(color = "cyan4"),
        axis.title.y.right = element_text(color = "royalblue"),
        legend.text.align = 0.5,
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

##
##############
###  wind plot

head(environment_daily_2, n = 20)


wind_data <- 
  environment_daily_2 %>%
  filter(variable == "wind" | variable == "windmax") 

camp_period_7 <- 
  camp_period %>% 
  tidyr::crossing(wind_data %>% 
                    summarise(x1 = min(my_date),
                              x2 = max(my_date)) %>% 
                    ungroup) %>% 
  filter(xmin >= x1 & xmax <= x2) %>% 
  ungroup()

head(wind_data)
my_labels_5 <- c("italic(u)" ,"italic(u[max])") 
my_values_5 <- c("turquoise1", "lightslateblue")

my_col_5 <- c("italic(u)" = "turquoise1", 
              "italic(u[max])" = "lightslateblue")

wind_plot <- 
  ggplot(data = wind_data, 
         mapping = aes(x = my_date, y = mean, color = variable, shape = variable)) +
  geom_rect(data = camp_period_7, inherit.aes = FALSE,
            mapping = aes(xmin = xmin, xmax = xmax, 
                          ymin = ymin, ymax = ymax),
            fill = 'red', alpha = 0.1) +
  #geom_smooth(method = "auto", se = TRUE,
  #            fullrange=FALSE, level=0.95, aes(fill = variable), alpha = 0.1) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  scale_x_date(breaks = date_breaks, date_labels = "%b-%d", expand = c(0, 1)) +
  labs(y = expression(atop(Wind, paste((m~s^{-1}))))) +
  annotate("text", x = as.Date("2016-07-02"), y = 13, label = "(e)") +
  ylim(0, 13) +
  scale_colour_manual(values = my_values_5,
                      labels = c(parse(text = my_labels_5[1]),
                                 parse(text = my_labels_5[2]) )) +
  scale_fill_manual(values = my_values_5,
                    labels = c(parse(text = my_labels_5[1]),
                               parse(text = my_labels_5[2]) )) +
  scale_shape_manual(values = c(15:16),
                     labels = c(parse(text = my_labels_5[1]),
                                parse(text = my_labels_5[2]))) +
  theme(axis.ticks.length.x = unit(0, "cm"),
        legend.title = element_blank(),#(size = 12),
        legend.position = "right" ,
        legend.text = element_text(face = "italic"),
        axis.text.y = element_text(size=10),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12,  margin = margin(t = 0, r = 5, b = 0, l = 10)),
        legend.text.align = 0.5,
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))  
############
names(environment_daily_2)
head(environment_daily_2$variable, n =10)


soil_data <- 
  environment_daily %>%
  select(my_date, theta5_mean,theta20_mean,theta45_mean, theta80_mean) %>% 
  gather(key = variable, value = mean, -c("my_date"))

camp_period_6 <- 
  camp_period %>% 
  tidyr::crossing(soil_data %>%
                    summarise(x1 = min(my_date),
                              x2 = max(my_date)) %>% 
                    ungroup) %>% 
  filter(xmin >= x1 & xmax <= x2) %>% 
  ungroup()

soil_data$variable <- 
  factor(soil_data$variable, 
         levels = c("theta5_mean", "theta20_mean", 
                    "theta45_mean", "theta80_mean"),
         ordered = TRUE)


levels(soil_data$variable) <- 
  c(expression(italic(theta[5])),
    expression(italic(theta[20])),
    expression(italic(theta[45])),
    expression(italic(theta[80])))


my_labels_4 <- c("italic(theta[5])" ,"italic(theta[20])",
                 "italic(theta[45])", "italic(theta[80])" )
my_values_4 <- c("darkorange", "azure4", "violet", "saddlebrown")

my_col_4 <- c("italic(theta[5])" = "darkorange", 
              "italic(theta[20])" = "azure4", 
              "italic(theta[45])" = "violet",
              "italic(theta[80])" = "saddlebrown")


summary(soil_data)

soil_plot <- 
  ggplot(data = soil_data, mapping = aes(x = my_date, y = mean, 
                                         color = variable, shape = variable)) +
  geom_rect(data = camp_period_6, inherit.aes = FALSE,
            mapping = aes(xmin = xmin, xmax = xmax, 
                          ymin = ymin, ymax = ymax),
            fill = 'red', alpha = 0.1) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  scale_x_date(breaks = date_breaks, date_labels = "%b-%d", expand = c(0, 1)) +
  labs(y = expression(atop(italic(theta[soil]), paste((m^{3}~m^{-3})))),
       x = "Date") + 
  annotate("text", x = as.Date("2016-07-02"), y = 0.375, label = "(g)") +
  scale_colour_manual(values = my_values_4,
                      labels = c(parse(text = my_labels_4[1]),
                                 parse(text = my_labels_4[2]),
                                 parse(text = my_labels_4[3]),
                                 parse(text = my_labels_4[4])))+
  scale_shape_manual(values = c(15:18),
                     labels = c(parse(text = my_labels_4[1]),
                                parse(text = my_labels_4[2]),
                                parse(text = my_labels_4[3]),
                                parse(text = my_labels_4[4]))) +
  theme(legend.title = element_blank(),#(size = 12),
        legend.position = "right" ,
        legend.text = element_text(face = "italic"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        axis.title.x  = element_text(size = 14,  margin = margin(t = 3, r = 0, b = 3, l = 0)),
        axis.title.y = element_text(size = 12,  margin = margin(t = 0, r = 5, b = 0, l = 10)),
        legend.text.align = 0.5,
        legend.spacing.x = unit(0.2, 'cm'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))





########
rain_plot + lws_plot + radiation_plot + 
  temp_plot + wind_plot + water_vapor_plot +
  soil_plot + 
  plot_layout(ncol = 1) 



export::graph2eps(file = here("EC_ET", "3_figure", "Figure_3_daily_environment_long_term.eps"),
                  width = 10.93, height = 9.85)

###
### graphs are finished above
