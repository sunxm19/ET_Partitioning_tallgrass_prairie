# Import data from SIBS data ----------------------------------------------
conflict_prefer("legend", "graphics")

sibs_period_1 <- 
  readRDS(here("delta_E", "0_raw_data", "sibs", "period_1.rds")) %>% 
  filter(site == "south", sampling != "soil") %>% 
  select(-"site") %>% 
  rename(my_timestamp = date_time, my_date = Date, 
         time_category = new_time_category,
         hydrogen_value = H2,  hydrogen_sd = H2_StDev,
         oxygen_value   = O18, oxygen_sd   = O18_StDev) %>% 
  filter(my_date %in% my_observation_dates_1)

dplyr::bind_rows(head(sibs_period_1), tail(sibs_period_1))

delta_T_daily <- 
  sibs_period_1 %>% 
  filter(sampling == "root", time_category == "noon")

# delta_T_daily <- readRDS(file = here("delta_T", "3_output", "delta_T_daily.rds") )
dplyr::bind_rows(head(delta_T_daily), tail(delta_T_daily))
delta_T_daily %>% 
  summarise(
    hydrogen_mean = mean(hydrogen_value),
    hydrogen_mean_sd   = sd(hydrogen_value),
    hydrogen_max  = max(hydrogen_value),
    hydrogen_min  = min(hydrogen_value),
    hydrogen_sd_mean = mean(hydrogen_sd),
    hydrogen_sd_sd   = sd(hydrogen_sd, na.rm = TRUE),
    oxygen_mean   = mean(oxygen_value),
    oxygen_mean_sd     = sd(oxygen_value),
    oxygen_max    = max(oxygen_value),
    oxygen_min    = min(oxygen_value),
    oxygen_sd_mean = mean(oxygen_sd),
    oxygen_sd_sd   = sd(oxygen_sd, na.rm = TRUE)) %>% 
  gather(key, value)

# save delta_T results for later daily ET partitioning analysis
saveRDS(delta_T_daily, 
        here("delta_T", "3_output", "delta_T_daily.rds"))

delta_T_xylem <- 
  sibs_period_1 %>% 
  filter(sampling == "root")

