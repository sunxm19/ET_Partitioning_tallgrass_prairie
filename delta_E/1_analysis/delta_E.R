### calculate delta_E: isotopic composition of soil evaporation
getwd()


source(here("delta_E","source", "craig_gordon_model.R"))
# Import SIBS data --------------------------------------------------------
sibs <- 
  readRDS(file = here("delta_E", "0_raw_data", "sibs", "period_1.rds") ) %>%
  filter(site == "south", sampling == "soil") %>% 
  select(my_timestamp = date_time, date = Date, 
         time_category = new_time_category,
         soil_hydrogen_value = H2,  soil_hydrogen_sd = H2_StDev,
         soil_oxygen_value   = O18, soil_oxygen_sd   = O18_StDev
  ) %>% 
  filter(date %in% my_observation_dates_1) %>% 
  filter(time_category != "night")

dplyr::bind_rows(head(sibs), tail(sibs))


# picarro data preparation ------------------------------------------------
picarro_cleaned <- 
  readRDS(here("delta_ET", "3_output", "3_picarro_cleaned", "picarro_cleaned_study_1.rds")) %>% 
  select(time_label, my_date, time_decimal, height, water, hydrogen, oxygen) %>% 
  mutate(water_reciprocal = 1/water,
         time_label = as.character(time_label))

picarro_for_cg <- 
  picarro_cleaned %>% 
  group_by(time_label, height) %>% 
  summarise(air_sample_size = n(),
            air_water_mean = mean(water),
            air_water_sd = sd(water),
            air_hydrogen_mean = mean(hydrogen),
            air_hydrogen_sd   = sd(hydrogen),
            air_oxygen_mean   = mean(oxygen),
            air_oxygen_sd     = sd(oxygen)) %>% 
  ungroup() %>% 
  mutate(my_timestamp = lubridate::ymd_hms(time_label)) %>% 
  select(- "time_label") %>% 
  select(my_timestamp, colnames(.))

dplyr::bind_rows(head(picarro_for_cg), tail(picarro_for_cg))
saveRDS(picarro_for_cg, 
        here("delta_E", "3_output", "picarro_for_cg.rds" ))


# import picarro data -----------------------------------------------------

cg_input_picarro <- 
  readRDS(file = here("delta_E","3_output", "picarro_for_cg.rds")) %>% 
  mutate(my_time =  format(strptime(my_timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
         "%H:%M")) %>% 
  select(my_timestamp, my_time, height, contains("air")) 
  

cg_input_picarro$time_decimal <- 
  sapply(strsplit(cg_input_picarro$my_time,":"),
         function(x) {
           x <- as.numeric(x)
           x[1]+x[2]/60
         }
  )

head(cg_input_picarro)
summary(cg_input_picarro$air_sample_size)

cg_input_picarro <- 
  cg_input_picarro %>% 
  rename_at(vars(starts_with("air_")), 
            list(~ str_replace(., pattern = "air_", replacement = ""))) %>%
  gather(key, value, -c("my_timestamp", "my_time","time_decimal", "height")) %>% 
  unite(new_key, key, height) %>% 
  spread(new_key, value)

names(cg_input_picarro)

cg_input_picarro <- 
  cg_input_picarro %>% 
  mutate(air_hydrogen_value = if_else(!is.na(hydrogen_mean_Low), hydrogen_mean_Low,
                                if_else(!is.na(hydrogen_mean_Middle), hydrogen_mean_Middle,
                                        hydrogen_mean_High)),
         air_hydrogen_sd    = if_else(!is.na(hydrogen_sd_Low), hydrogen_sd_Low,
                                if_else(!is.na(hydrogen_sd_Middle), hydrogen_sd_Middle,
                                        hydrogen_sd_High)),
         air_oxygen_value = if_else(!is.na(oxygen_mean_Low), oxygen_mean_Low,
                                      if_else(!is.na(oxygen_mean_Middle), oxygen_mean_Middle,
                                              oxygen_mean_High)),
         air_oxygen_sd    = if_else(!is.na(oxygen_sd_Low), oxygen_sd_Low,
                                      if_else(!is.na(oxygen_sd_Middle), oxygen_sd_Middle,
                                              oxygen_sd_High))
  ) %>% 
  select(my_timestamp, my_time, time_decimal,  contains("air"))

head(cg_input_picarro)
names(cg_input_picarro)
summary(cg_input_picarro)

saveRDS(cg_input_picarro, here("delta_E","3_output", "cg_input_picarro.rds"))
# write.csv(cg_input_picarro, here("delta_E","3_output", "cg_input_picarro_1.csv"))

### remeber!
### note this CSV needs to be manaully aligned according to sibs data
### borrow data from nearby time slot
### time at the beginning and end of the day are mannually adjusted
head(cg_input_picarro$my_timestamp)

cg_input_picarro <- 
  read.csv(here("delta_E","3_output", "cg_input_picarro_1.csv"), 
           header = TRUE) %>% 
  mutate(my_timestamp = lubridate::mdy_hm(my_timestamp),
         time_decimal = as.numeric(time_decimal)) %>% 
  tibble::as_tibble()


sibs_picarro <- 
  left_join(sibs, cg_input_picarro[, -1], by = "my_timestamp") %>% 
  drop_na()

saveRDS(sibs_picarro, here("delta_E","3_output", "sibs_picarro.rds"))

###


# join sibs_picarro with eddy covariance data -----------------------------
#  sibs_picarro <- readRDS(here("delta_E","3_output", "sibs_picarro.rds"))
eddy_biomet <- readRDS(here("EC_ET", "3_output", "eddy_biomet_south_rain_soil.rds"))

dplyr::bind_rows(head(sibs_picarro), tail(sibs_picarro))
names(sibs_picarro)
names(eddy_biomet)
dplyr::bind_rows(head(eddy_biomet), tail(eddy_biomet))


sibs_picarro_eddy <- 
  sibs_picarro %>% 
  left_join(eddy_biomet[, c("timestamp", "Tair", "Tsoil", "RH", "soil_moisture_5cm")], 
            by = c("my_timestamp" = "timestamp")) %>% 
  mutate(T_air = Tair + 273.15, 
         T_soil = Tsoil + 273.15) %>% 
  rename(RH_air = "RH", theta_soil = "soil_moisture_5cm") %>% 
  select(-c("Tair", "Tsoil"))

write.csv(names(sibs_picarro_eddy), here("delta_E","3_output", "sibs_picarro_eddy_description.csv"))
saveRDS(sibs_picarro_eddy, here("delta_E","3_output", "sibs_picarro_eddy.rds"))
write.csv(sibs_picarro_eddy, here("delta_E","3_output", "sibs_picarro_eddy.csv"))
# apply craig_gordon ------------------------------------------------------
## input T_air
head(sibs_picarro_eddy)

cg_results <- 
  sibs_picarro_eddy %>% 
  rename(delta_L_2H = soil_hydrogen_value, delta_L_18O = soil_oxygen_value,
         delta_air_2H = air_hydrogen_value, delta_air_18O = air_oxygen_value) %>%
  select(delta_L_2H, delta_L_18O, T_soil, T_air, RH_air,  
         theta_soil, delta_air_2H, delta_air_18O) %>% 
  mutate(!!!invoke(CG_model_advanced, unname(.)))

head(cg_results)  


### join and calculate the uncertainity, Error Propagation
str()
cg <- 
  sibs_picarro_eddy %>% 
  rename(delta_L_2H = soil_hydrogen_value, delta_L_18O = soil_oxygen_value,
         delta_air_2H = air_hydrogen_value, delta_air_18O = air_oxygen_value) %>%
  left_join(cg_results,
            by =  c("delta_L_2H", "delta_L_18O", "delta_air_2H", 
                    "delta_air_18O", "RH_air", "theta_soil", "T_air", "T_soil")) %>% 
  rename(my_date = date, 
         soil_hydrogen_value = delta_L_2H,
         soil_oxygen_value   = delta_L_18O,
         delta_air_hydrogen  = delta_air_2H,
         delta_air_oxygen    = delta_air_18O,
         alpha_equilibrium_hydrogen = alpha_equilibrium_2H,
         epsilon_equilibrium_hydrogen = `1000 * epsilon_equilibrium_2H`,
         alpha_equilibrium_oxygen = alpha_equilibrium_18O,
         epsilon_equilibrium_oxygen = `1000 * epsilon_equilibrium_18O`,
         epsilon_kinetic_hydrogen = `1000 * epsilon_k_2H`,
         epsilon_kinetic_oxygen =`1000 * epsilon_k_18O`,
         delta_E_hydrogen_value = `1000 * delta_E_2H_advanced`,
         delta_E_oxygen_value = `1000 * delta_E_18O_advanced`) %>% 
  mutate(delta_E_hydrogen_sd = sqrt(soil_hydrogen_sd^2/alpha_equilibrium_hydrogen^2 + RH_normal^2 * air_hydrogen_sd^2),
         delta_E_oxygen_sd   = sqrt(soil_oxygen_sd^2/alpha_equilibrium_oxygen^2     + RH_normal^2 * air_oxygen_sd^2))


  
glimpse(cg)
## separate three campaigns in study 1
study_1_camp_date1  <- lubridate::ymd("2016-06-01")
study_1_camp_date2  <- lubridate::ymd("2016-06-14")
study_1_camp_date3  <- lubridate::ymd("2016-06-30")


cg <- 
  cg %>% 
  mutate(camp = case_when(
    between(my_date, study_1_camp_date1, study_1_camp_date2) ~ "Campaign_1",
    between(my_date, study_1_camp_date2, study_1_camp_date3) ~ "Campaign_2",
    TRUE ~ "NA"
  ))

summary(cg$camp)

cg$camp <- 
  factor(cg$camp, 
         levels = c("Campaign_1", "Campaign_2"), 
         ordered = TRUE)

names(cg)
###
saveRDS(cg, here("delta_E","3_output", "cg.rds"))
write.csv(cg, here("delta_E","3_output", "cg.csv"))

# Analysis on delta E value -----------------------------------------------
names(cg)
delta_evaporation <- 
  cg %>% 
  filter(my_date != lubridate::date("2016-06-27")) %>% 
  select(my_date, camp, contains("delta_E_")) %>% 
  summarise(
    delta_E_hydrogen_mean    = mean(delta_E_hydrogen_value),
    delta_E_hydrogen_mean_sd = sd(delta_E_hydrogen_value),
    delta_E_hydrogen_sd_mean = mean(delta_E_hydrogen_sd),
    delta_E_hydrogen_sd_sd   = sd(delta_E_hydrogen_sd),
   #delta_E_hydrogen_sd = sqrt(sum(delta_E_hydrogen_sd^2))/n(),
    delta_E_hydrogen_min     = min(delta_E_hydrogen_value),
    delta_E_hydrogen_max     = max(delta_E_hydrogen_value),
    delta_E_oxygen_mean      = mean(delta_E_oxygen_value),
    delta_E_oxygen_mean_sd  = sd(delta_E_oxygen_value),
   # delta_E_oxygen_sd = sqrt(sum(delta_E_oxygen_sd^2))/n(),
    delta_E_oxygen_min       = min(delta_E_oxygen_value),
    delta_E_oxygen_max       = max(delta_E_oxygen_value),
    delta_E_oxygen_sd_mean   = mean(delta_E_oxygen_sd),
    delta_E_oxygen_sd_sd     = sd(delta_E_oxygen_sd)) %>% 
  gather(key, value)


summary(cg$delta_E_hydrogen_sd)
sd(cg$delta_E_hydrogen_sd)

summary(cg$delta_E_oxygen_sd)
sd(cg$delta_E_oxygen_sd)

dplyr::bind_rows(head(delta_evaporation), tail(delta_evaporation))
# visualize craig-gordon results ------------------------------------------
ggplot(data = cg, 
       aes( x = my_timestamp, y = delta_E_hydrogen_value, color = time_category)) +
  geom_point() +
  facet_wrap(~camp, scales = 'free_x')

ggplot(data = cg, 
       aes( x = my_timestamp, y = delta_E_oxygen_value, color = time_category)) +
  geom_point() +
  facet_wrap(~camp, scales = 'free_x')

#  comparison between epsilon_k and epsilon_eq
names(cg)

fractionation_data <- 
  cg %>% 
  filter(my_date != lubridate::date("2016-06-27")) %>% 
  select(my_timestamp, my_date, camp, time_category,
         epsilon_kinetic_hydrogen, epsilon_equilibrium_hydrogen, 
         epsilon_kinetic_oxygen, epsilon_equilibrium_oxygen) %>% 
  gather(key, epsilon, -c(my_timestamp, my_date, camp, time_category)) %>% 
  separate(col = key, into = c("value", "type", "isotope"), remove = TRUE) %>% 
  spread(key = "type", value = "epsilon") %>% 
  mutate(isotope = if_else(isotope == "hydrogen", "{ }^{2}*H",
                           if_else(isotope == "oxygen", "{ }^{18}*O", "NA"), "NA")) 
  

# unique(fractionation_data$isotope)
fractionation_data$isotope <- factor(fractionation_data$isotope, 
                                      levels = c("{ }^{2}*H" , "{ }^{18}*O"),
                                      ordered = TRUE)
fractionation_data$time_category <- factor(fractionation_data$time_category, 
                                     levels = c("morning", "noon", "afternoon"), 
                                     ordered = TRUE)

# Plot epsilon_eq VS epsilon_k --------------------------------------------
saveRDS(fractionation_data, here("delta_E","3_output", "fractionation_data.rds"))
#  fractionation_data <- readRDS(here("delta_E","3_output", "fractionation_data.rds"))
##
##
summary(fractionation_data)
summary(fractionation_data$time_category)
str(fractionation_data$time_category)
#fractionation_data$time_category <- as.character(fractionation_data$time_category)
camp_labels <- c("Campaign 1", "Campaign 2")
names(camp_labels) <- c("Campaign_1", "Campaign_2")

formula <- y ~ x

annotation_text <- 
  tibble(kinetic = 12, equilibrium = 10.8, lab = "1:1 Line",
        isotope = factor("{ }^{18}*O"), shape = NA,
        levels = c("{ }^{2}*H" , "{ }^{18}*O"), time_category = NA)


ggplot(data = fractionation_data, 
       aes(x = kinetic,
           y = equilibrium, 
           color = time_category, shape = time_category)) +
  geom_abline(data = NULL, mapping = aes(slope = 1, intercept = 0), colour = "#555555",
              linetype = "dashed",
              alpha = 0.5, show.legend = FALSE) +
  geom_text(data = annotation_text, label = "1:1 Line",  colour = "#555555",  size = 4) +
  #geom_smooth(method = "lm", formula = formula, se = FALSE) +
  stat_ellipse(alpha = 0.2, level = 0.8, type = "norm", geom = "polygon") +
  geom_point() +
  #geom_convexhull(alpha = 0.2) +
  scale_x_continuous(breaks = seq(from = 1, to = 15, by = 3)) + 
  scale_colour_manual(name = "Time category", 
                      breaks = c("morning", "noon", "afternoon"), 
                      values = c("#00AFBB", "#FC4E07", "#E7B800")) + 
  scale_shape_manual(name = "Time category", 
                     breaks = c("morning", "noon", "afternoon"),
                     values = c( 15, 16, 17)) +
  labs(x =  expression(paste(italic(epsilon[k]),scriptstyle(" (\u2030 VSMOW)"))),
       y =  expression(paste(italic(epsilon[eq]),scriptstyle(" (\u2030 VSMOW)"))), 
       color =  "Time category", shape =  "Time category") +
  #labs(x = "\u03B5\u2096 (\u2030)", #italic(epsilon[k])~, #TeX("$\\epsilon_{k} (\textperthousand)$"),
  #    y = "\u03B5 (\u2030)") + #italic(epsilon[eq])~) + #TeX("$\\epsilon_{eq} (\textperthousand)$")) +
  facet_grid(isotope~camp, 
             labeller = labeller(isotope = label_parsed, camp = camp_labels), 
             scales = "free_y") +
  theme(legend.position = "right",
        strip.text = element_text(size = 12), #face = "bold"),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title = element_text(size=19),
        axis.title.x = element_text(size = 14, margin = margin(t = 8, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 8, b = 0, l = 0)),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))

ggsave(file = here("delta_E","3_figure", "Figure_9_revised_fractionation_plot_export.eps"),
       width = 9.5, height = 9.5,  
       fallback_resolution = 800, device = cairo_ps, family="DejaVu Sans")


## summary of the data

summary(fractionation_data)
fractionation_data %>% 
  group_by(isotope) %>% 
  summarise(
    equilibrium_min  = min(equilibrium),
    equilibrium_max  = max(equilibrium),
    equilibrium_mean = mean(equilibrium),
    equilibrium_sd   = sd(equilibrium)
  )

fractionation_data %>% 
  group_by(isotope) %>% 
  summarise(
    kinetic_min  = min(kinetic),
    kinetic_max  = max(kinetic),
    kinetic_mean = mean(kinetic),
    kinetic_sd   = sd(kinetic)
  )


fractionation_data %>% 
  group_by(camp, isotope) %>% 
  summarise(
    equilibrium_min  = min(equilibrium),
    equilibrium_max  = max(equilibrium),
    equilibrium_mean = mean(equilibrium),
    equilibrium_sd   = sd(equilibrium)
  )

fractionation_data %>% 
  group_by(camp, isotope) %>% 
  summarise(
    kinetic_min  = min(kinetic),
    kinetic_max  = max(kinetic),
    kinetic_mean = mean(kinetic),
    kinetic_sd   = sd(kinetic)
  )





#####################
study_1_camp_date1  <- lubridate::ymd("2016-06-03")
study_1_camp_date2  <- lubridate::ymd("2016-06-14")

eddy_biomet_south_for_delta_E <- 
  readRDS(here("EC_ET", "3_output", "eddy_biomet_south_rain_soil.rds")) %>% 
  tibble::as_tibble() %>% 
  mutate(my_date =  lubridate::date(timestamp)) %>% 
  filter(my_date %in% my_observation_dates) %>% 
  mutate(camp = case_when(
    between(my_date, study_1_camp_date1, study_1_camp_date2) ~ "Campaign_1",
    TRUE ~ "Campaign_2"
  ))


names(eddy_biomet_south_for_delta_E)

daily_vpd_sum <- 
  eddy_biomet_south_for_delta_E%>%
  select(timestamp, my_date, camp, vpd) %>% 
  right_join(cg, by = c("timestamp" = "my_timestamp" , "my_date", "camp")) %>% 
  group_by(my_date) %>% 
  summarise(vpd_sum = sum(vpd))

names(daily_vpd_sum)

delta_E_daily <- 
  cg %>% 
  select("my_timestamp", "my_date", "delta_E_hydrogen_value", "delta_E_hydrogen_sd",
          "delta_E_oxygen_value","delta_E_oxygen_sd") %>% 
  left_join(eddy_biomet_south_for_delta_E, by = c("my_timestamp" = "timestamp","my_date")) %>% 
  left_join(daily_vpd_sum, by = "my_date") %>% 
  mutate(weight = vpd/vpd_sum, 
         weighted_hydrogen_value = weight * delta_E_hydrogen_value,
         weighted_oxygen_value   = weight * delta_E_oxygen_value,
         weighted_hydrogen_variance = (weight * delta_E_hydrogen_sd)^2, 
         weighted_oxygen_variance   = (weight * delta_E_oxygen_sd)^2) %>% 
  group_by(my_date) %>% 
  summarise(
    daily_E_hydrogen_value       = sum(weighted_hydrogen_value),
    daily_E_oxygen_value         = sum(weighted_oxygen_value),
    daily_E_hydrogen_variance    = sum(weighted_hydrogen_variance),
    daily_E_oxygen_variance      = sum(weighted_oxygen_variance)
  ) %>% 
  ungroup() %>%
  rename(daily_E_hydrogen = daily_E_hydrogen_value,
         daily_E_oxygen   = daily_E_oxygen_value) %>% 
  mutate(hydrogen_sd = sqrt(daily_E_hydrogen_variance),
         oxygen_sd = sqrt(daily_E_oxygen_variance)) %>% 
  filter(my_date != lubridate::ymd("2016-06-27")) %>% 
  select(-contains("_sd"))

names(delta_E_daily)  


saveRDS(delta_E_daily, file = here("delta_E", "3_output", "delta_E_daily.rds"))

write.csv(delta_E_daily, file = here("delta_E", "3_output", "delta_E_daily.csv") )

###################
### Done here #####
###################





















