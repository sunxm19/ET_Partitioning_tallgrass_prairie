
# Import delta_ET and standarize the time range ---------------------------
delta_ET_eddy_biomet <- 
  readRDS(file = here("delta_ET", "3_output", 
                      "5_keeling_eddy_biomet", 'keeling_eddy_filtered.rds')) %>% 
  select("my_timestamp", "my_date", "my_time", "camp", "time_decimal", 
         delta_ET_hydrogen_value = "delta_et_value_hydrogen",
         delta_ET_oxygen_value = "delta_et_value_oxygen",
         "delta_et_se_hydrogen", "delta_et_se_oxygen" 
  ) %>% 
  rename(delta_ET_hydrogen_sd = delta_et_se_hydrogen ,
         delta_ET_oxygen_sd   = delta_et_se_oxygen) 

names(delta_ET_eddy_biomet)
unique(delta_ET_eddy_biomet$my_date)
delta_ET_eddy_biomet %>% 
  arrange(my_timestamp) %>% 
  group_by(my_date) %>% 
  summarise(
    begin = dplyr::first(my_time),
    end   = dplyr::last(my_time)
  )


# Format picarro water vapor data from 9:00-7:00 each day -----------------
my_time_df <- 
  crossing(hour = c(09:19), minutes = c(":00:00", ":30:00")) %>% 
  unite(col = "day_time", sep = "", remove = TRUE) %>% 
  slice(- c(length(day_time)))

my_sequence <- 
  crossing(date = unique(delta_ET_eddy_biomet$my_date), my_time_df) %>% 
  unite(col = "my_timestamp", sep = " ", remove = TRUE) %>% 
  mutate(my_timestamp = lubridate::ymd_hms(my_timestamp))

head(my_sequence)
class(my_sequence)

delta_ET <- 
  readRDS(file = here("delta_ET", "3_output", 
                      "5_keeling_eddy_biomet", "keeling_eddy_filtered.rds")) %>% 
  select("my_timestamp",
         delta_ET_hydrogen_value = "delta_et_value_hydrogen",
         delta_ET_oxygen_value = "delta_et_value_oxygen",
         "delta_et_se_hydrogen", "delta_et_se_oxygen" 
  ) %>% 
  rename(delta_ET_hydrogen_sd = delta_et_se_hydrogen ,
         delta_ET_oxygen_sd   = delta_et_se_oxygen) 


delta_ET <- 
  left_join(my_sequence, delta_ET, by = "my_timestamp") 

# Import delta_E and delta_T data ---------------------------------------------------------
## time range for delta_E
readRDS(file = here("delta_E", "3_output", "cg.rds")) %>% 
  select("my_timestamp", "my_date", "my_time",
         "delta_E_hydrogen_value", "delta_E_hydrogen_sd",
         "delta_E_oxygen_value","delta_E_oxygen_sd") %>% 
  arrange(my_timestamp) %>% 
  group_by(my_date) %>% 
  summarise(begin = first(my_time),
            end   = last(my_time))

delta_E <- 
  readRDS(file = here("delta_E", "3_output", "cg.rds")) %>% 
  select("my_timestamp", 
         "delta_E_hydrogen_value", "delta_E_hydrogen_sd",
         "delta_E_oxygen_value","delta_E_oxygen_sd") %>% 
  arrange(my_timestamp)



names(keeling_eddy_biomet)


delta_T <- 
  readRDS(file = here("delta_T", "3_output", "delta_T.rds")) %>% 
  select("my_timestamp", 
         delta_T_hydrogen_value = hydrogen_value,
         delta_T_hydrogen_sd    = hydrogen_sd,
         delta_T_oxygen_value   = oxygen_value,
         delta_T_oxygen_sd      = oxygen_sd)


# examine the data
dplyr::bind_rows(head(delta_E), tail(delta_E))
dplyr::bind_rows(head(delta_ET), tail(delta_ET))
dplyr::bind_rows(head(delta_T), tail(delta_T))

names(delta_E)
names(delta_ET)
names(delta_T)
# Combine all data by full_join -------------------------------------------

delta_ET_E_T <- 
  delta_ET %>% 
  left_join(delta_E,  by = c("my_timestamp")) %>%
  left_join(delta_T,  by = c("my_timestamp")) %>% 
  arrange(my_timestamp) %>% 
  gather(key = data_type, value = delta_sd, 
         -c("my_timestamp")) %>% 
  separate(col = "data_type", into = c("delta", "flux", "isotope", "type"), remove = TRUE) %>% 
  select(- "delta") %>% 
  mutate(flux = if_else(flux == "ET", "italic(delta[ET])",
                        if_else(flux == "E", "italic(delta[E])",
                                if_else(flux == "T", "italic(delta[T])", "NA"))),
         isotope = if_else(isotope == "hydrogen", "{ }^{2}*H",
                           if_else(isotope == "oxygen", "{ }^{18}*O", "NA"), "NA")) %>% 
  spread(key = "type", value = "delta_sd")

dplyr::bind_rows(head(delta_ET_E_T), tail(delta_ET_E_T))

delta_ET_E_T$flux <- 
  factor(delta_ET_E_T$flux,
         levels = c("italic(delta[ET])",  "italic(delta[E])",  "italic(delta[T])"),
         ordered = TRUE)
# format the data frame__factor -------------------------------------------

study_1_camp_date1  <- lubridate::ymd("2016-06-01")
study_1_camp_date2  <- lubridate::ymd("2016-06-13")
study_1_camp_date3  <- lubridate::ymd("2016-06-14")
study_1_camp_date4  <- lubridate::ymd("2016-07-01")
##
delta_ET_E_T <- 
  delta_ET_E_T %>% 
  mutate(my_date =  lubridate::date(my_timestamp)) %>% 
  mutate(camp = case_when(
    between(my_date, study_1_camp_date1, study_1_camp_date2) ~ "Campaign_1",
    between(my_date, study_1_camp_date3, study_1_camp_date4) ~ "Campaign_2",
    TRUE ~ "NA"))

delta_ET_E_T$isotope <- factor(delta_ET_E_T$isotope, 
                               levels = c("{ }^{2}*H" , "{ }^{18}*O"),
                               ordered = TRUE)
delta_ET_E_T$camp <- 
  factor(delta_ET_E_T$camp, 
         levels = c("Campaign_1", "Campaign_2"), 
         ordered = TRUE)

delta_ET_E_T$my_time <- 
  strftime(delta_ET_E_T$my_timestamp, format = "%H:%M:%S", tz = "UTC")

delta_ET_E_T$my_time <- 
  as.POSIXct(strptime(delta_ET_E_T$my_time, format = "%H:%M:%S"), tz = "UTC")

x <- as.POSIXct(strptime(c("050000","110000","110001","150000","150001",
                           "195959"),"%H%M%S"),"UTC")

delta_ET_E_T <- 
  delta_ET_E_T %>% 
  mutate(time_category = case_when(
    between(my_time,x[1],x[2]) ~"morning",
    between(my_time,x[3],x[4]) ~"noon",
    between(my_time,x[5],x[6]) ~"afternoon",
    TRUE ~"night")) %>% 
  select(-my_time) %>% 
  filter(my_date != lubridate::ymd("2016-06-13")) %>% 
  mutate(my_date = format(my_date, "%b-%d")) 


saveRDS(delta_ET_E_T, 
        file = here("ET_partitioning_results", "2_output", "delta_ET_E_T_se_for_ET.rds"))

# 
delta_ET_E_T <- readRDS(file = here("ET_partitioning_results", "2_output", "delta_ET_E_T_se_for_ET.rds"))
# visulization ------------------------------------------------------------

dev.off()

dplyr::bind_rows(head(delta_ET_E_T), tail(delta_ET_E_T)) 

plot_delta_ET_E_T <- 
  ggplot(data = delta_ET_E_T,
         mapping = aes(x = my_timestamp, y = value, color = flux, shape = flux)) +
  geom_line(data = delta_ET_E_T[!is.na(delta_ET_E_T$value), ]) + 
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd), width = 0.2,
                position = position_dodge(0.05)) +
  geom_point() +
  scale_color_manual(name = "", 
                     labels = parse_format()(c("italic(delta[ET])", "italic(delta[E])", "italic(delta[T])")),
                     values = c("#00BFC4", "#F8766D", "#006400")) +
  scale_shape_manual(name = "", 
                     labels = parse_format()(c("italic(delta[ET])", "italic(delta[E])", "italic(delta[T])")),
                     values = c(15, 16, 17)) + 
  scale_x_datetime(#breaks = date_breaks("3 hour"), 
    labels = date_format("%H")) +
  #expand = c(0, 1/48)) +
  facet_grid(isotope ~ my_date, labeller = label_parsed, scales = "free",  space = "free_x") +
  xlab("Hours") +
  ylab("\u03b4  (\u2030 vs VSMOW)") +
  #theme_classic() +  ## classic theme
  theme(#legend.position = c(0.8, 0.25),
    strip.text.x = element_text(face = "bold", size = 12),
    strip.text.y = element_text(face = "bold", size = 14),
    legend.title = element_text(face = "bold", size = 12), 
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title.x = element_text(size = 14, margin = margin(t = 8, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 8, b = 0, l = 0)),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA, color = "black"))


plot_delta_ET_E_T_gt <- 
  ggplot_gtable(ggplot_build(plot_delta_ET_E_T))

plot_delta_ET_E_T_gt$widths[14] <- 4*plot_delta_ET_E_T_gt$widths[14]

grid.draw(plot_delta_ET_E_T_gt)

export::graph2eps(file = here("ET_partitioning_results", "3_figure", "Figure_8_delta_ET_E_T_subdaily_with_SE.eps"),
                  width = 10.93, height = 9.55)

#########################
