
# load packages and data-----------------------------------------------------------

library(gganimate)
conflict_prefer("View", "utils")
getwd()


picarro_cleaned_study <- readRDS(
        here("delta_ET", "3_output", "3_picarro_cleaned", "picarro_cleaned_study_1.rds"))

# visualization -----------------------------------------------------------


## for animation
picarro_study_1_viz <- 
  picarro_cleaned_study %>% 
  select(my_date, time_decimal, hydrogen, oxygen, height) %>% 
  gather(key = isotope, value = delta, c(hydrogen, oxygen)) %>% 
  mutate(isotope = if_else(isotope == "hydrogen", "{ }^{2}*H",
                         if_else(isotope == "oxygen", "{ }^{18}*O", "NA"), "NA"))

unique(picarro_study_1_viz$isotope)

picarro_study_1_viz$isotope <- factor(picarro_study_1_viz$isotope, 
                                      levels = c("{ }^{2}*H" , "{ }^{18}*O"))

unique(picarro_study_1_viz$my_date)

picarro_study_1_viz <- 
  ggplot(picarro_study_1_viz, aes(x= time_decimal, y = delta, color = height)) + 
  geom_jitter(alpha = 0.4) + 
  scale_x_continuous(breaks = seq(0, 24, 1), limits = c(10,18)) +
  ylab(expression(paste(delta," (\u2030)"))) +
  xlab("Time") +
  facet_wrap(~ isotope, labeller = label_parsed, scales = "free_y") +
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14, angle = 0),
        axis.title.x = element_text(size=18, margin = unit(c(4, 0, 0, 0), "mm")),
        axis.title.y = element_text(size=20, margin = unit(c(0, 4, 0, 0), "mm")),
        plot.title = element_text(size = 20, face = "bold")) +
  transition_states(
    states = my_date,
    transition_length = 0,
    state_length = 15
  ) +
  labs(title = "Date: {closest_state}") +#, subtitle = 'Frame {frame} of {nframes}') +
  #labs(title = 'my_date: {frame_time}', x = 'Time', y = 'delta 18O')+
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

library(gapminder)
library(gifski)

animate(picarro_study_1_viz, nframes = 150, fps = 10, duration = 10,
        width= 754, height=796/2, renderer = gifski_renderer())  

anim_save(here("delta_ET", "3_figure", "picarro_data_viz", "study_period_animation.gif"),
          fps = 1)

#########################
##### Keeling_plot regression
##
library(broom)

picarro_cleaned <- 
  readRDS(here("delta_ET", "3_output", "3_picarro_cleaned", "picarro_cleaned_study_1.rds")) %>% 
  filter(my_date >= lubridate::ymd("2016-06-01")) %>% 
  select(time_label, my_date, time_decimal, height, water, hydrogen, oxygen) %>% 
  mutate(time_label = lubridate::ceiling_date(time_label, "hour"),
         water_reciprocal = 1/water,
         time_label = as.character(time_label))

# hydrogen ----------------------------------------------------------------
keeling_hydrogen_analysis <- 
  picarro_cleaned %>% 
  split(.$time_label) %>% 
  map(~ lm(hydrogen ~ water_reciprocal, data = .)) %>%
  map(broom::tidy) %>%  # https://broom.tidyverse.org/reference/tidy.lm.html
  map_df(tibble::rownames_to_column, 'var', .id = 'name') %>% 
  select(-c("statistic", "p.value")) %>% 
  gather(key = result_type, value = value,
         c("estimate", "std.error")) %>% 
  unite(col = "new_key", c("term", "result_type"), remove = TRUE) %>%
  select(- "var") %>% 
  spread(key = "new_key", value = value) %>% 
  rename(intercept_estimate = "(Intercept)_estimate",
         intercept_se       = "(Intercept)_std.error",
         slope_estimate     = "water_reciprocal_estimate",
         slope_se           = "water_reciprocal_std.error")


###
keeling_hydrogen_statistics <-  
  picarro_cleaned %>% 
  split(.$time_label) %>% 
  map(~ lm(hydrogen ~ water_reciprocal, data = .)) %>%
  map(glance) %>%  # https://www.rdocumentation.org/packages/broom/versions/0.5.2/topics/glance.lm
  map_df(tibble::rownames_to_column, 'var', .id = 'name') %>%
  rename(F_statistic = statistic, F_test_p.value = p.value) %>% 
  select(name, r.squared, F_statistic,  F_test_p.value, AIC, BIC)


keeling_hydrogen <- 
  left_join(keeling_hydrogen_analysis, keeling_hydrogen_statistics, by = "name") %>% 
  mutate(date_time = ymd_hms(name)) %>%
  rename_at(vars(- c("name", "date_time")), ~ paste0(., "_hydrogen"))

head(keeling_hydrogen)
tail(keeling_hydrogen)
# oxygen ------------------------------------------------------------------

keeling_oxygen_analysis <- 
  picarro_cleaned %>% 
  split(.$time_label) %>% 
  map(~ lm(oxygen ~ water_reciprocal, data = .)) %>%
  map(tidy) %>% 
  map_df(tibble::rownames_to_column, 'var', .id = 'name') %>% 
  select(-c("statistic", "p.value")) %>% 
  gather(key = result_type, value = value,
         c("estimate", "std.error")) %>% 
  unite(col = "new_key", c("term", "result_type"), remove = TRUE) %>%
  select(- "var") %>% 
  spread(key = "new_key", value = value) %>% 
  rename(intercept_estimate = "(Intercept)_estimate",
         intercept_se       = "(Intercept)_std.error",
         slope_estimate     = "water_reciprocal_estimate",
         slope_se           = "water_reciprocal_std.error")

keeling_oxygen_statistics <-  
  picarro_cleaned %>% 
  split(.$time_label) %>% 
  map(~ lm(oxygen ~ water_reciprocal, data = .)) %>%
  map(glance) %>% 
  map_df(tibble::rownames_to_column, 'var', .id = 'name') %>%
  rename(F_statistic = statistic, F_test_p.value = p.value) %>% 
  select(name, r.squared, F_statistic,  F_test_p.value, AIC, BIC)


keeling_oxygen <- 
  left_join(keeling_oxygen_analysis, keeling_oxygen_statistics, by = "name") %>% 
  mutate(date_time = ymd_hms(name)) %>% 
  #rename(delta_ET = estimate) %>% 
  rename_at(vars(- c("name", "date_time")), ~ paste0(., "_oxygen"))

head(keeling_oxygen)
tail(keeling_oxygen)
# Combine together --------------------------------------------------------
names(keeling_hydrogen)
names(keeling_oxygen)
head(keeling_hydrogen)
head(keeling_oxygen)
tail(keeling_hydrogen)
tail(keeling_oxygen)

keeling_plot_results <- 
  left_join(keeling_hydrogen, keeling_oxygen, by = c("date_time", "name")) %>% 
  mutate(my_timestamp  = lubridate::ymd_hms(name),
         my_date       = lubridate::date(my_timestamp),
         my_time       = format(strptime(name, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                                "%H:%M")) %>% 
  select(- c("date_time", "name"))
names(keeling_plot_results)
tail(keeling_plot_results)

keeling_plot_results$time_decimal <- 
  sapply(strsplit(keeling_plot_results$my_time,":"),
         function(x) {
           x <- as.numeric(x)
           x[1]+x[2]/60
         }
  )

names(keeling_plot_results)

keeling_plot_results <- 
  keeling_plot_results %>% 
  select(names(keeling_plot_results)[19:22], names(keeling_plot_results)[1:18])

picarro_cleaned_data_description <- 
  picarro_cleaned %>% 
  group_by(time_label) %>% 
  summarise(air_sample_size = n(),
            air_water_mean = mean(water),
            air_water_sd = sd(water),
            air_hydrogen_mean = mean(hydrogen),
            air_hydrogen_sd   = sd(hydrogen),
            air_oxygen_mean   = mean(oxygen),
            air_oxygen_sd     = sd(oxygen)) %>% 
  mutate(my_timestamp = lubridate::ymd_hms(time_label)) %>% 
  select(- time_label) 


keeling_plot_results <- 
  keeling_plot_results %>% 
  left_join(picarro_cleaned_data_description, by = "my_timestamp")

#####
camp_1_start <- lubridate::ymd("2016-06-04")
camp_1_end    <- lubridate::ymd("2016-06-14")
camp_2_end   <- lubridate::ymd("2016-06-30")

keeling_plot_results <- 
  keeling_plot_results %>% 
  filter(my_date %in% my_observation_dates_1) %>% 
  mutate(camp = case_when(
    between(my_date, camp_1_start, camp_1_end) ~ "Campaign_1",
    TRUE ~ "Campaign_2"
  ))


keeling_plot_results$camp <- 
  factor(keeling_plot_results$camp, 
         levels = c("Campaign_1", "Campaign_2"), 
         ordered = TRUE)



###
saveRDS(keeling_plot_results, 
        here("delta_ET","3_output", "4_keeling_plot" , "keeling_plot_results_hourly.rds"))
write.csv(keeling_plot_results, 
          here("delta_ET","3_output", "4_keeling_plot" , "keeling_plot_results_hourly.csv"))

summary(keeling_plot_results)
head(keeling_plot_results)
mean(keeling_plot_results$air_sample_size)
sd(keeling_plot_results$air_sample_size)
write.csv(names(keeling_plot_results), 
          here("delta_ET", "3_output", "4_keeling_plot","keeling_plot_description.csv"))

keeling_plot_results <- readRDS( 
       here("delta_ET","3_output", "4_keeling_plot" , "keeling_plot_results_hourly.rds"))


#########
### statistics on keeling-plot result
## removed data during night 

keeling_plot_results <- 
  keeling_plot_results %>% 
  filter(time_decimal >= 9, time_decimal <= 19)

dplyr::bind_rows(head(keeling_plot_results), tail(keeling_plot_results))
names(keeling_plot_results)
summary(keeling_plot_results$r.squared_hydrogen)
## sumamry on the statistics
keeling_plot_results %>% 
  group_by(camp) %>% 
  summarise(mean_rsqaure_hydrogen = mean(r.squared_hydrogen),
            sd_rsqaure_hydrogen   = sd(r.squared_hydrogen),
            mean_rsqaure_oxygen   = mean(r.squared_oxygen),
            sd_rsqaure_oxygen     = sd(r.squared_oxygen))
summary(keeling_plot_results$camp)
########
# test on R^2_keeling -----------------------------------------------------

## 1. compare the r square between 2H and 18O
##  assumption 1: are the two samples paired?
########" yes, there are left-and-right type, not before-and-after type
##  assumption 2: smaple size large enough?
########  yes, >30
##  assumption 3: normality
########  
diff <- 
  keeling_plot_results %>% 
  mutate(diff = r.squared_hydrogen - r.squared_oxygen) %>% 
  select(diff)
shapiro.test(diff$diff)


###
## because p > 0.05, we need to use parametric test
summary(keeling_plot_results$camp)

test_input_camp_1 <- 
  keeling_plot_results %>% 
  filter(camp == "Campaign_1") %>% 
  select(r.squared_hydrogen, r.squared_oxygen) %>% 
  gather(key = isotope, value = r_square)

t.test(r_square ~ isotope, data = test_input_camp_1, 
       paired = TRUE,  alternative = "greater")

test_input_camp_2 <- 
  keeling_plot_results %>% 
  filter(camp == "Campaign_2") %>% 
  select(r.squared_hydrogen, r.squared_oxygen) %>% 
  gather(key = isotope, value = r_square)


t.test(r_square ~ isotope, data = test_input_camp_2, 
       paired = TRUE, alternative = "greater")
####################
keeling_plot_results %>% 
  group_by(camp) %>% 
  summarise(number = n())

keeling_plot_results %>% 
  filter(F_test_p.value_hydrogen <=0.05) %>% 
  group_by(camp) %>% 
  summarise(number = n())

keeling_plot_results %>% 
  filter(F_test_p.value_oxygen <=0.05) %>% 
  group_by(camp) %>% 
  summarise(number = n())


###
# Creat data frame for analysis -------------------------------------------
## eddy data
camp_date1  <- lubridate::ymd("2016-06-03")
camp_date2  <- lubridate::ymd("2016-06-14")

eddy_biomet_south <- 
  readRDS(here("EC_ET", "3_output", "eddy_biomet_south_rain_soil.rds")) %>% 
  tbl_df() %>% 
  mutate(my_date =  lubridate::date(timestamp)) %>% 
  filter(my_date %in% my_observation_dates) %>% 
  mutate(camp = case_when(
    between(my_date, camp_date1, camp_date2) ~ "Campaign_1",
    TRUE ~ "Campaign_2"
  )) %>% 
  select(timestamp, my_date, camp, Rn, Tair, RH, specific_humidity, vpd, wind_speed,
         max_wind_speed, ET, P, soil_moisture_5cm) %>% 
  mutate(my_timestamp_hourly = lubridate::ceiling_date(timestamp, "hour")) %>% 
  group_by(my_timestamp_hourly) %>% 
  summarise(
    my_timestamp      = last(timestamp),
    my_date           = first(my_date),
    camp              = last(camp),
    Rn                = mean(Rn),
    Tair              = mean(Tair),
    RH                = mean(RH),
    specific_humidity = mean(specific_humidity),
    vpd               = mean(vpd),
    wind_speed        = mean(wind_speed),
    max_wind_speed    = max(max_wind_speed),
    ET                = sum(ET),
    P_mm              = sum(P),
    south_5           = mean(soil_moisture_5cm)) %>%
  ungroup()


eddy_biomet_south$camp <- 
  factor(eddy_biomet_south$camp, 
         levels = c("Campaign_1", "Campaign_2"), 
         ordered = TRUE)

head(eddy_biomet_south$camp)
tail(eddy_biomet_south$camp)
dplyr::bind_rows(head(eddy_biomet_south), tail(eddy_biomet_south))
names(eddy_biomet_south)
head(eddy_biomet_south$SWC)
head(eddy_biomet_south$south_5)

eddy_biomet_south %>% 
  group_by(my_date) %>% 
  summarise(sum = sum(ET),
            camp_1 = dplyr::first(camp),
            camp_2 = dplyr::last(camp)) %>% 
  group_by(camp_1) %>% 
  summarise(et_mean = mean(sum),
            et_sd   = sd(sum) )



# Join eddy data with keeling-plot result ---------------------------------
keeling_eddy <- 
  keeling_plot_results %>% 
  select(my_timestamp, my_date, camp, my_time, time_decimal,
         delta_et_value_hydrogen = "intercept_estimate_hydrogen", 
         delta_et_se_hydrogen = "intercept_se_hydrogen",
         "slope_estimate_hydrogen",
         "r.squared_hydrogen",  "F_test_p.value_hydrogen", 
         delta_et_value_oxygen = "intercept_estimate_oxygen",   
         delta_et_se_oxygen = "intercept_se_oxygen" , 
         "slope_estimate_oxygen" ,
         "r.squared_oxygen" , "F_test_p.value_oxygen", "air_sample_size",
         "air_water_mean",             
         "air_water_sd", "air_hydrogen_mean",  "air_hydrogen_sd",            
         "air_oxygen_mean", "air_oxygen_sd") %>%   
  left_join(eddy_biomet_south, by = c("my_timestamp", "my_date", "camp"))

#############
names(keeling_eddy)
write.csv(keeling_eddy, file = here("delta_ET", "3_output", "5_keeling_eddy_biomet", "keeling_eddy.csv"))
keeling_eddy %>% 
  summarise(
    delta_et_hydrogen_mean    = mean(delta_et_value_hydrogen),
    delta_et_hydrogen_sd      = sd(delta_et_value_hydrogen),
    delta_et_hydrogen_max     = max(delta_et_value_hydrogen),
    delta_et_hydrogen_min     = min(delta_et_value_hydrogen),
    delta_et_hydrogen_sd_mean = mean(delta_et_se_hydrogen),
    delta_et_hydrogen_sd_sd   = sd(delta_et_se_hydrogen),
    delta_et_oxygen_mean      = mean(delta_et_value_oxygen),
    delta_et_oxygen_sd        = sd(delta_et_value_oxygen),
    delta_et_oxygen_max       = max(delta_et_value_oxygen),
    delta_et_oxygen_min       = min(delta_et_value_oxygen),
    delta_et_oxygen_sd_mean   = mean(delta_et_se_oxygen),
    delta_et_oxygen_sd_sd     = sd(delta_et_se_oxygen)
  ) %>% 
  gather(key, value)

######
# analysis ----------------------------------------------------------------
library(PerformanceAnalytics)

  keeling_eddy %>% 
  select(-c(my_timestamp, my_date, camp, my_time, time_decimal,my_timestamp_hourly,  
            P_mm, south_5, ET)) %>%  
  chart.Correlation(., histogram = TRUE, pch = 19)

table.Correlation()


export::graph2png(file = here("delta_ET", "3_figure", "keeling_eddy_correlation.png"),
                  width = 30, height = 30)
######

names(keeling_eddy)
summary(keeling_eddy$air_sample_size)


### test normality 
qqnorm(keeling_eddy$r.squared_oxygen)
qqnorm(keeling_eddy$r.squared_hydrogen)
shapiro.test(keeling_eddy$r.squared_oxygen)
shapiro.test(keeling_eddy$r.squared_hydrogen)
## because p < 0.001, we need to use non-parametric test

matric <- 
  keeling_eddy %>% 
  select(-c(my_timestamp, my_timestamp_hourly , my_date, camp, my_time, time_decimal, P_mm,
            south_5, ET)) %>% 
  as.matrix(.) %>% 
  Hmisc::rcorr(., type = "spearman")

summary(matric)
# snippt for format correlation matric ------------------------------------

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
#### 
flattenCorrMatrix(matric$r, matric$P) %>%
  #mutate(cor  = abs(cor)) %>% 
  filter(p <= 0.05, abs(cor) > 0.5) %>% 
  write.csv(., file = here("delta_ET", "3_output", "keeling_corr_output.csv"))
#rcorr(.)

# analysis on environmental factors ---------------------------------------
names(keeling_eddy)
summary(keeling_eddy$time_decimal)

keeling_eddy %>% 
  select(my_date, "Rn","Tair" , "RH" , "specific_humidity", "vpd", "wind_speed",
         "max_wind_speed", "ET" ,  "P_mm",  "south_5") %>% 
  group_by(my_date) %>% 
  summarise(
    Rn_mean   = mean(Rn),                  Rn_sd     = sd(Rn),
    Tair_mean = mean(Tair),                Tair_sd       = sd(Tair),
    vpd_mean  = mean(vpd),                 vpd_sd        = sd(vpd),
    wind_mean = mean(wind_speed),          wind_sd       = sd(wind_speed),
    max_wind_mean = mean(max_wind_speed),  max_wind_sd = sd(max_wind_speed), 
    ET        = sum(ET),
    P_mm      = sum(P_mm)
  )



# Filtering data ----------------------------------------------------------

names(keeling_eddy)

keeling_eddy_filtered <- 
  keeling_eddy %>% 
  filter(F_test_p.value_hydrogen <= 0.05, r.squared_hydrogen > 0.4) %>% 
  filter(delta_et_value_hydrogen <= -10)
#filter(F_test_p.value_oxygen <= 0.05) %>% 
#filter(my_timestamp != lubridate::ymd_hm("2016-06-12 15:00"),
#       my_timestamp != lubridate::ymd_hm("2016-06-12 16:00"),
#       my_timestamp != lubridate::ymd_hm("2016-06-08 11:00"),
#       my_date      != lubridate::ymd("2016-06-27"))

keeling_eddy %>% 
  group_by(camp) %>% 
  summarise(row = n())

keeling_eddy_filtered %>% 
  group_by(camp) %>% 
  summarise(row = n())
20/35
14/30

### save file 
saveRDS(object = keeling_eddy_filtered, 
        file  = here("delta_ET", "3_output", "5_keeling_eddy_biomet", "keeling_eddy_filtered.rds"))

write.csv(keeling_eddy_filtered, 
        file  = here("delta_ET", "3_output", "5_keeling_eddy_biomet", "keeling_eddy_filtered.csv"))
######

####################
# daily_delta_ET ----------------------------------------------------------
####################
names(keeling_eddy_filtered)

daily_ET_sum <- 
  keeling_eddy_filtered %>% 
  group_by(my_date) %>% 
  summarise(daily_ET = sum(ET))


delta_ET_daily <- 
  keeling_eddy_filtered %>% 
  select("my_timestamp", "my_date", "time_decimal", "camp", "ET",
         "delta_et_value_hydrogen", "delta_et_se_hydrogen",
         "delta_et_value_oxygen",   "delta_et_se_oxygen") %>% 
  left_join(daily_ET_sum, by = "my_date") %>% 
  mutate(weight                      = ET/daily_ET,
         weighted_hydrogen_value     = weight * delta_et_value_hydrogen,
         weighted_oxygen_value       = weight * delta_et_value_oxygen,
         weighted_hydrogen_variance  = (weight * delta_et_se_hydrogen)^2,
         weighted_oxygen_variance    = (weight * delta_et_se_oxygen)^2
  ) %>% 
  group_by(my_date) %>% 
  summarise(
    daily_ET_hydrogen_value       = sum(weighted_hydrogen_value),
    daily_ET_oxygen_value         = sum(weighted_oxygen_value),
    daily_ET_hydrogen_variance    = sum(weighted_hydrogen_variance),
    daily_ET_oxygen_variance      = sum(weighted_oxygen_variance)
  ) %>% 
  ungroup() %>%
  rename(daily_ET_hydrogen = daily_ET_hydrogen_value,
         daily_ET_oxygen   = daily_ET_oxygen_value) %>% 
  mutate(hydrogen_sd = sqrt(daily_ET_hydrogen_variance),
         oxygen_sd = sqrt(daily_ET_oxygen_variance))

names(delta_ET_daily)
view(delta_ET_daily)
#  filter(my_date != lubridate::ymd("2016-02-28"),
#         my_date != lubridate::ymd("2016-06-27"))

saveRDS(delta_ET_daily, 
        here("delta_ET", "3_output", "6_daily_delta_ET", "delta_ET_daily.rds"))

#############################################
################## done #####################
#############################################