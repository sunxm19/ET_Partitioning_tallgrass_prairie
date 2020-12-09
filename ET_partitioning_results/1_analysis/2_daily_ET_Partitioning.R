### Source code for partitioning formula and uncertainity formula
source(here("ET_partitioning_results", "1_analysis", 
            "source_ET_partitioning_uncertainity_function.R"))



# Import data -------------------------------------------------------------

delta_ET_daily <- 
  readRDS(here("delta_ET", "3_output", "6_daily_delta_ET", "delta_ET_daily.rds")) %>% 
  select(-contains("_sd")) %>% 
  rename(daily_ET_hydrogen_value = daily_ET_hydrogen,
         daily_ET_oxygen_value   = daily_ET_oxygen)

dim(delta_ET_daily)
summary(delta_ET_daily)

head(delta_ET_daily, n = 8)

## no data in June-27 

## daily delta E
summary(delta_E_daily)
names(delta_E_daily)
delta_E_daily <- 
  readRDS(file = here("delta_E", "3_output", "delta_E_daily.rds")) %>% 
  rename(daily_E_hydrogen_value = daily_E_hydrogen, 
         daily_E_oxygen_value   = daily_E_oxygen)

head(delta_E_daily, n = 10)
delta_T
## daily delta T
delta_T_daily <- 
  delta_T_daily %>% 
  rename(daily_T_hydrogen_value = hydrogen_value,
         daily_T_oxygen_value   = oxygen_value) %>%
  mutate(daily_T_hydrogen_variance = hydrogen_sd^2,
         daily_T_oxygen_variance   = oxygen_sd^2) %>% 
  select(my_date, daily_T_hydrogen_value,  daily_T_hydrogen_variance,
         daily_T_oxygen_value,  daily_T_oxygen_variance)

dplyr::bind_rows(head(delta_T_daily), tail(delta_T_daily))


# Join data together as inputs for isotopic ET partitioning ---------------

delta_daily_data <- 
  delta_ET_daily %>% 
  left_join(delta_E_daily, by = "my_date") %>% 
  left_join(delta_T_daily, by = "my_date") %>% 
  gather(key = "delta_and_variance", value = "value", - "my_date") %>% 
  separate(col = delta_and_variance, 
           into = c("temporal_resolution", "flux", "isotope", "data_type"),
           sep = "_",
           remove = TRUE) %>% 
  unite(col = "label", c("flux", "data_type"), sep = "_", remove = TRUE) %>% 
  spread(key = "label", value = "value") %>% 
  select(-"temporal_resolution" )

dplyr::bind_rows(head(delta_daily_data), tail(delta_daily_data))



# Apply isotopic ET partitioning at daily interval  --------

delta_daily_data <- 
  delta_daily_data %>% 
  dplyr::mutate(
    f_T_ET = purrr::pmap(
      list(delta_ET = ET_value, delta_E = E_value, delta_T = T_value),
      ET_partitioning_function),
    f_variance = purrr::pmap(
      list(delta_ET_variance = ET_variance, 
           delta_E_variance  = E_variance, 
           delta_T_variance  = T_variance, 
           f_T_ET            = f_T_ET, 
           delta_E           = E_value, 
           delta_T           = T_value),
      ET_partitioning_variance)
  ) %>% 
  tidyr::unnest(cols = c(f_T_ET, f_variance)) %>% 
  mutate(f_sd = sqrt(f_variance))


dplyr::bind_rows(head(delta_daily_data), tail(delta_daily_data))
# format ET partitioning results ---------------------------------------
delta_daily_data %>% 
  group_by(isotope) %>% 
  summarise(f_sd_mean = mean(f_sd),
            f_sd_sd   = sd(f_sd))

study_1_camp_date1  <- lubridate::ymd("2016-06-01")
study_1_camp_date2  <- lubridate::ymd("2016-06-13")
study_1_camp_date3  <- lubridate::ymd("2016-06-14")
study_1_camp_date4  <- lubridate::ymd("2016-07-01")
##

delta_daily_data <- 
  delta_daily_data %>% 
  mutate(camp = case_when(
    between(my_date, study_1_camp_date1, study_1_camp_date2) ~ "Campaign_1",
    between(my_date, study_1_camp_date3, study_1_camp_date4) ~ "Campaign_2",
    TRUE ~ "NA"),
    isotope = if_else(isotope == "hydrogen", "{ }^{2}*H",
                      if_else(isotope == "oxygen", "{ }^{18}*O", "NA"), "NA"))


delta_daily_data$camp <- 
  factor(delta_daily_data$camp, 
         levels = c("Campaign_1", "Campaign_2"), 
         ordered = TRUE)

delta_daily_data$isotope <- 
  factor(delta_daily_data$isotope, 
         levels = c("{ }^{2}*H" , "{ }^{18}*O"), ordered = TRUE)

saveRDS(delta_daily_data,
        here("ET_partitioning_results", "2_output", "delta_daily_data.rds"))

## delta_daily_data <- 
##      readRDS(file = here("ET_partitioning_results", "2_output", "delta_daily_data.rds"))
# Analysis on daily ET partitioning data  ---------------------------------
head(delta_daily_data)
summary(delta_daily_data)
delta_daily_data %>% 
  select(my_date, isotope, f_T_ET, f_sd, camp) %>% 
  gather(key = key, value = value, -c(my_date, isotope, camp)) %>% 
  unite(col = "new_key", c("isotope", "key"), sep = " ", remove = TRUE) %>%
  spread(key = "new_key", value = value) %>% 
  rename(f_18O_sd = `{ }^{18}*O f_sd`, f_18O_value = `{ }^{18}*O f_T_ET`,
         f_2H_sd = `{ }^{2}*H f_sd`, f_2H_value = `{ }^{2}*H f_T_ET`) %>% 
  group_by(camp) %>% 
  summarise(my_number    = n(),
            f_2h_mean    = mean(f_2H_value),
            f_2h_sd      = sqrt(sum(f_2H_sd^2))/my_number,
            f_2h_sd_mean = mean(f_2H_sd),
            f_2h_sd_sd   = sd(f_2H_sd),
            f_2h_min     = min(f_2H_value),
            f_2h_max     = max(f_2H_value),
            f_18O_mean   = mean(f_18O_value),
            f_18O_sd     = sqrt(sum(f_18O_sd^2))/my_number,
            f_18O_sd_mean= mean(f_18O_sd),
            f_18O_sd_sd  = sd(f_18O_sd),
            f_18O_min    = min(f_18O_value),
            f_18O_max    = max(f_18O_value))

# Visulization of ET partitioning results ---------------------------------
dplyr::bind_rows(head(delta_daily_data), tail(delta_daily_data))
my_date_breaks <- unique(delta_daily_data$my_date)
my_breaks <-  c("2016-06-04", "2016-06-06", "2016-06-08",
                "2016-06-10", "2016-06-12",
                "2016-06-28", "2016-06-29", "2016-06-30")
camp_labels <- c("Campaign 1", "Campaign 2")
names(camp_labels) <- c("Campaign_1", "Campaign_2")

##################################

my_colors <- c("#00BFC4", "#F8766D") 
isotope_type <-  c("{ }^{2}*H", "{ }^{18}*O")

#### plot time series
f_t_et_plot <- 
  ggplot(data = delta_daily_data,
         mapping = aes(x = my_date, y = f_T_ET, color = isotope, shape = isotope)) +
  geom_errorbar(aes(ymin = f_T_ET - f_sd, ymax = f_T_ET + f_sd), 
                width = 0.2, position = position_dodge(0.2), size = 0.7) +
  geom_line(position = position_dodge(0.2), size = 0.8) + 
  geom_point(position = position_dodge(0.2), size = 3) +
  scale_color_discrete(name = "", labels = c(as.expression(bquote({ }^{2}*"H")), as.expression(bquote({ }^{18}*"O"))))+
  scale_shape_discrete(name = "", labels = c(as.expression(bquote({ }^{2}*"H")), as.expression(bquote({ }^{18}*"O")))) +
  facet_grid(~ camp, 
             labeller = labeller(isotope = label_parsed, camp = camp_labels),
             scales = "free", space = "free_x") +
  xlab("Date") +
  ylab(expression(italic("T/ET"))) +
  scale_x_date(breaks = as.Date(my_breaks), date_labels = "%b-%d") + 
  scale_y_continuous(breaks = c(-0.2, 0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2)) +
  #theme_classic() +  ## classic theme
  theme(#legend.position = c(0.8, 0.25),
    strip.text.x = element_text( size = 12),
    legend.title = element_text( size = 12), 
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 12), #angle = 45, hjust = 1),
    axis.title.x = element_text(size = 14, #face = "bold", 
                                margin = margin(t = 8, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 14, #face = "bold", 
                                margin = margin(t = 0, r = 8, b = 0, l = 0)),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA, color = "black"))







## https://stackoverflow.com/questions/49123019/add-space-between-specific-facets-in-ggplot2-facet-grid
f_t_et_plot_gt <-  ggplotGrob(f_t_et_plot)

f_t_et_plot_gt$widths[6] <- 2*f_t_et_plot_gt$widths[6]

grid.draw(f_t_et_plot_gt)

export::graph2eps(file = here("ET_partitioning_results", "3_figure", "Figure_10_1_isotopic_et_partitioning_daily.eps"),
                  width = 9.30, height = 7.96)
export::graph2png(file = here("ET_partitioning_results", "3_figure", "isotopic_et_partitioning_daily_agu.png"),
                  width = 9.30, height = 7.96)


##################

