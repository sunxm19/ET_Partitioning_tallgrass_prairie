
# Import standard data for calibration ------------------------------------
here()

standard <- read_excel(
  path  = here("dual_isotope", "0_raw_data", "rain.xlsx"),
  sheet = "international_standard_simple" ) %>% 
  group_by(standard) %>% 
  summarise(oxygen_mean   = mean(oxygen),
            hydrogen_mean = mean(hydrogen))

dplyr::bind_rows(head(standard), tail(standard))


true_standard <- 
  tibble(standard      = c("GISP", "VSMOW", "SLAP"),
         oxygen_true   = c(-24.76, 0, -55.5),
         hydrogen_true = c(-189.5, 0, -427.5))


combine_standard <- 
  left_join(standard, true_standard, by = "standard")

combine_standard


lm(hydrogen_true ~ hydrogen_mean, data = combine_standard)
lm(oxygen_true   ~ oxygen_mean, data = combine_standard)


# Apply correction --------------------------------------------------------

rain_data <- 
  read_excel(
    path = here("dual_isotope", "0_raw_data", "rain.xlsx"),
    sheet = "rain"
  ) %>% 
  select(my_date, type, hydrogen, oxygen) %>% 
  mutate(hydrogen = hydrogen * 1.014 + 7.438, 
         oxygen   = oxygen   * 1.008 - 5.566) %>% 
  filter(type == "rain") %>% 
  mutate(my_date = lubridate::ymd(my_date)) %>% 
  arrange(my_date)

saveRDS(object = rain_data,
        file = here("dual_isotope", "3_output", "delta_rain.rds"))


# Comparison bewteen Picarro and IRMS -------------------------------------
options(pillar.sigfig = 5)

picarro_standard <- 
  read_excel(
    path = here("dual_isotope", "0_raw_data", "rain.xlsx"),
    sheet = "rain"
  ) %>% 
  select(my_date, type, hydrogen, oxygen) %>% 
  mutate(hydrogen = hydrogen * 1.014 + 7.438, 
         oxygen   = oxygen   * 1.008 - 5.566) %>% 
  filter(type == "standard") %>% 
  group_by(my_date) %>% 
  summarise(hydrogen_mean = mean(hydrogen), 
            oxygen_mean   = mean(oxygen)) %>% 
  ungroup() %>% 
  rename(type = my_date)


## 
IRMS <- tibble(
  type     = c("high", "low"),
  hydrogen_irms = c(-7.13, -213.84),
  oxygen_irms   = c(-6.76, -28.11)
)
  


## picarro vs IRMS
picarro_irms <- 
  left_join(picarro_standard, IRMS, by = "type")

picarro_irms


lm(hydrogen_irms ~ hydrogen_mean, data = picarro_irms)
lm(oxygen_irms   ~ oxygen_mean, data = picarro_irms)
###

# All in all --------------------------------------------------------------

rain_data <- 
  read_excel(
    path = here("dual_isotope", "0_raw_data", "rain.xlsx"),
    sheet = "rain"
  ) %>% 
  select(my_date, type, hydrogen, oxygen) %>% 
  mutate(hydrogen = hydrogen * 1.014 + 7.438, 
         oxygen   = oxygen   * 1.008 - 5.566) %>% 
  filter(type == "rain") %>% 
  mutate(my_date = lubridate::ymd(my_date)) %>% 
  arrange(my_date) %>% 
  mutate(hydrogen = hydrogen * 1.009 - 4.012,
         oxygen   = oxygen * 0.9931  + 0.1146) %>% 
  group_by(my_date) %>% 
  summarise(hydrogen_mean = mean(hydrogen), 
            hydrogen_sd   = sd(hydrogen),
            oxygen_mean   = mean(oxygen),
            oxygen_sd     = sd(oxygen)) %>% 
  ungroup()

saveRDS(object = rain_data,
        file = here("dual_isotope", "3_output", "delta_rain.rds"))


readRDS(file = here("dual_isotope", "3_output", "delta_rain.rds")) %>% 
  rename(date = my_date) %>% 
  write.csv("delta_rain.csv")
###
### done


## for high sample, hydrogen
-3.0914 - (-7.13)

## for low sample, hydrogen
-208.02  - (-213.84) 

#### for hydrogen 
((-3.0914 - (-7.13)) + (-208.02  - (-213.84)))/2
#########


## for high sample, oxygen
-6.9223 -(-6.76)

## for low sample, oxygen
-28.420 -(-28.11)



# Statistics of delta Rain ------------------------------------------------

rain_data <- 
  readRDS(file = here("dual_isotope", "3_output", "delta_rain.rds")) %>% 
  mutate(month = lubridate::month(my_date)) %>% 
  filter(month >=5, month <=6) %>% 
  summarise(hydrogen_min = min(hydrogen_mean),
            hydrogen_max = max(hydrogen_mean),
            oxygen_min   = min(oxygen_mean),
            oxygen_max   = max(oxygen_mean))

dplyr::bind_rows(head(rain_data), tail(rain_data))

