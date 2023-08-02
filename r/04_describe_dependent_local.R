#-------------------------------------------------------------------------------
# Summarise dependent variables at local authority level 

# Local authority distribution by year
# Local authority estimates with confidence intervals and national value line in key year
# Categorise trends over time for each local authority
# Descriptive statistics of local authority values
# Identify missing values
# Detect outliers
# Detect variability in longitudinal data: method 1 and 2
# Distribution of dependent variable = normal?
# Map dependent variable
#-------------------------------------------------------------------------------


# Local authority distribution by year ------------------------------------

# Distribution NT recalculated rate and ONS rate

ggplot(infant_mortality_outcome_long %>%
         group_by(variable_new, year_num) %>%
         mutate(q25               = quantile(value, na.rm = TRUE, probs = 0.25)
                , q75             = quantile(value, na.rm = TRUE, probs = 0.75)
                , iqr             = IQR(value, na.rm = TRUE)
                , outlier         = case_when((value <= (q25 - (1.5 * iqr)) & value > (q25 - (3 * iqr))) 
                                              | (value >= (q75 + (1.5 * iqr)) & value < (q75 + (3 * iqr))) ~ value)
                , extreme_outlier = case_when(value <= (q25 - (3 * iqr)) 
                                              | value >= (q75 + (3 * iqr)) ~ value))
       , aes(x = as.factor(year_num), y = value)) +
  stat_boxplot(geom = "errorbar", width = 0.5, coef = 1.5, na.rm = TRUE) +
  geom_boxplot(outlier.colour = NA, na.rm = TRUE) +
  geom_point(aes(x = as.factor(year_num), y = outlier), colour = "red", shape = 16, na.rm = TRUE) +
  geom_point(aes(x = as.factor(year_num), y = extreme_outlier), colour = "blue", shape = 4, na.rm = TRUE) +
  facet_wrap(~ factor(variable_new
                      , levels=c("u1mortality_nt_", "u1mortality_ons_")
                      , labels = c("NT rate", "ONS Rate"))) +
  scale_x_discrete(breaks = c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016)
                   , labels = c("2000", "2002", "2004", "2006", "2008"
                                , "2010", "2012", "2014", "2016")) +
  scale_y_continuous(limits = c(0, NA)
                     , expand = c(0, 1)) +
  labs(y = "Rate per 1,000 Live Births"
       , x = "Year"
       , title = "Local authority distribution of the Infant Mortality Rate by year"
       , caption = "Red dot = outlier (1.5*IQR)\nBlue cross = extreme outlier (3*IQR)") +
  NT_style() +
  theme(panel.spacing = unit(1,"lines"))
ggsave("output/descriptives_dependent/plots/Dist_all_infant_mortality_utla.pdf"
       , width = 13.7, height = 10, units = "cm", dpi = 600)

# Distribution NT rate

ggplot(infant_mortality_outcome_long %>%
         filter(variable_new == "u1mortality_nt_") %>%
         group_by(variable_new, year_num) %>%
         mutate(q25               = quantile(value, na.rm = TRUE, probs = 0.25)
                , q75             = quantile(value, na.rm = TRUE, probs = 0.75)
                , iqr             = IQR(value, na.rm = TRUE)
                , outlier         = case_when((value <= (q25 - (1.5 * iqr)) & value > (q25 - (3 * iqr))) 
                                              | (value >= (q75 + (1.5 * iqr)) & value < (q75 + (3 * iqr))) ~ value)
                , extreme_outlier = case_when(value <= (q25 - (3 * iqr)) | value >= (q75+(3 * iqr)) ~ value))
       , aes(x = as.factor(year_num), y = value)) +
  stat_boxplot(geom = "errorbar", width = 0.5, coef = 1.5) +
  geom_boxplot(outlier.colour = NA, na.rm = TRUE) +
  geom_point(aes(x = as.factor(year_num), y = outlier), colour = "red", shape = 16, na.rm = TRUE) +
  geom_point(aes(x = as.factor(year_num), y = extreme_outlier), colour = "blue", shape = 4, na.rm = TRUE) +
  scale_x_discrete(breaks = c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016)
                   , labels = c("2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2016")) +
  scale_y_continuous(limits = c(0, NA)
                     , expand = c(0, 1)) +
  labs(y = "Rate per 1,000 Live Births"
       , x = "Year"
       , title = "Local authority distribution of the Infant Mortality Rate by year"
       , caption = "Red dot = outlier (1.5*IQR)\nBlue cross = extreme outlier (3*IQR)") +
  NT_style()
ggsave("output/descriptives_dependent/plots/Dist_infant_mortality_nt_utla.pdf"
       , width = 13.7, height = 10, units = "cm", dpi = 600)


# Local authority estimates (CIs and national line) -----------------------

# Use Byar's method for counts > 10 - otherwise use the exact method

infant_mortality_count_data <- read_csv("data/Infant_Mortality_Counts_UTLA.csv") %>%
  mutate(value = (numerator / denominator) * 1000
         , lcl = case_when(numerator < 10 ~ ((qchisq(p = 0.025, df = (numerator * 2))) 
                                             / denominator) * 1000
                           , TRUE ~ ((numerator * ((1 - (1 / (9 * numerator)) 
                                                    - (1.96 / (3 * (sqrt(numerator))))) ^ 3))
                                     / denominator) * 1000)
         , ucl = case_when(numerator < 10 ~ ((qchisq(p = 0.975, df = ((numerator * 2) + 2))) 
                                             / denominator) * 1000
                           , TRUE ~ (((numerator + 1) * ((1 - (1 / (9 * (numerator + 1)))
                                                          + (1.96 / (3 * (sqrt(numerator + 1))))) ^ 3)) 
                                     / denominator) * 1000
                           )
         )

infant_mortality_E_17 <- read_csv("output/descriptives_dependent/Summary_Infant_Mortality_uk.csv") %>% 
  filter(country == "E" & year_num == 2017)

ggplot(infant_mortality_count_data %>%
         filter(year_num == 2017)
       , aes(x = reorder(utla17cd, value), y = value)) +
  geom_point(size = 0.5) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), size = 0.2) +
  geom_hline(aes(yintercept = infant_mortality_E_17$value)
             , color = "mediumpurple2", size = 0.2) +
  geom_rect(aes( ymin = infant_mortality_E_17$lcl, ymax = infant_mortality_E_17$ucl
                 , xmin = 0, xmax = Inf)
            , alpha = 1/ 150, color = NA, fill = "mediumpurple2") +
  labs(y = "Rate per 1,000 Live Births"
       , x = "Upper tier local authority (n = 150)"
       , title = "Infant Mortality Rate by local authority in 2017"
       , caption = "Isles of Scilly combined with Cornwall\nCity of London combined with Hackney") +
  scale_y_continuous(limits = c(0, 25)
                     , expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x=element_blank()
        , axis.ticks.x=element_blank()
  )
ggsave("output/descriptives_dependent/plots/Dist_la_eng_infant_mortality_utla_17.pdf")

ggplot(infant_mortality_count_data %>%
         filter(year_num == 2017)
       , aes(x = reorder(utla17cd, value), y = value)) +
  geom_bar(stat = "identity", fill = "#9F67FF", colour = NA) +
  #geom_errorbar(aes(ymin = lcl, ymax = ucl), size = 0.2) +
  geom_hline(aes(yintercept = infant_mortality_E_17$value), color = "#271544", size = 0.5) +
  #geom_rect(aes( ymin = infant_mortality_E_17$lcl, ymax = infant_mortality_E_17$ucl, xmin = 0, xmax = Inf)
  #          , alpha = 1/ 150, color = NA, fill = "mediumpurple2") +
  labs(y = "Rate per 1,000 live births"
       , x = "Upper tier local authority (n = 150)"
       , caption = "Isles of Scilly combined with Cornwall\nCity of London combined with Hackney") +
  scale_y_continuous(limits = c(0, 8)
                     , breaks = seq(0, 8, 1)
                     , expand = c(0, 0)) +
  NT_style() +
  theme(axis.text.x=element_blank()
        , axis.ticks.x=element_blank())
ggsave("output/descriptives_dependent/plots/UTLA_IM_England_2017.png"
       , width = 13.7, height = 10, units = "cm", dpi = 600)


# Categorise trends over time for each LA ---------------------------------

# Change between earliest and latest data point for each LA - higher, lower, stable

trends_la <- infant_mortality_count_data %>%
  group_by(utla17cd) %>%
  filter(year_num == 2008) %>%
  dplyr::select(utla17cd, year_num, year_label, value, numerator, denominator) %>%
  rename(start_year_num = year_num
         , start_year_label = year_label
         , start_value = value
         , start_numerator = numerator
         , start_denominator = denominator) %>%
  left_join(infant_mortality_count_data %>% 
              group_by(utla17cd) %>%
              filter(year_num == max(year_num)) %>%
              dplyr::select(utla17cd, year_num, year_label, value, numerator, denominator)
            , by = "utla17cd") %>%
  rename(end_year_num = year_num
         , end_year_label = year_label
         , end_value = value
         , end_numerator = numerator
         , end_denominator = denominator) %>%
  mutate(diff = end_value-start_value
         , perc_diff = (diff/start_value)*100
         , p_value = prop.test(x = c(start_numerator, end_numerator)
                               , n = c(start_denominator, end_denominator))$p.value
         , trend_char = case_when(diff > 0 & p_value < 0.05 ~ "Higher"
                                  , p_value >= 0.05 ~ "Stable"
                                  , diff < 0 & p_value < 0.05 ~ "Lower"))

write_csv(trends_la,"output/descriptives_dependent/Trends_la_infant_mortality_utla.csv")

# Create a summary table of trends across all LAs

trends <- trends_la %>%
  group_by(trend_char) %>%
  summarise(n = n()
            , avg_diff = mean(diff)
            , min_diff = min(diff)
            , max_diff = max(diff)
            , avg_perc_diff = mean(perc_diff)
            , min_perc_diff = min(perc_diff)
            , max_perc_diff = max(perc_diff))


write_csv(trends,"output/descriptives_dependent/Trends_infant_mortality_utla.csv")


# Descriptive statistics of LA values -------------------------------------

infant_mortality_desc <- as_tibble(psych::describeBy(infant_mortality_outcome_long$value
                                                     , group = list(infant_mortality_outcome_long$year_num
                                                                    , infant_mortality_outcome_long$variable_new)
                                                     , mat = TRUE, IQR = TRUE, quant = c(.25, .75))) %>%
  rename("variable_new" = "group2"
         , "year_char" = "group1")

write_csv(infant_mortality_desc,"output/descriptives_dependent/Summary_infant_mortality_utla.csv")


# Identify missing values -------------------------------------------------

infant_mortality_missing <- infant_mortality_outcome_long %>%
  group_by(variable_new, year_num) %>%
  summarise(missing = sum(is.na(value))) %>%
  filter(missing > 0)

write_csv(infant_mortality_missing, "output/descriptives_dependent/Missing_Infant_Mortality_utla.csv")

infant_mortality_missing_la <- infant_mortality_outcome_long %>%
  filter(is.na(value)) %>%
  dplyr::select(utla17cd, variable_new, year_num, year_label)

write_csv(infant_mortality_missing_la, "output/descriptives_dependent/Missing_la_Infant_Mortality_utla.csv")


# Detect outliers ---------------------------------------------------------

# outliers = mean +/- (3 * sd) or Q1/3 +/- (1.5* IQR)
# extreme outliers = Q1/3 +/- (3 * IQR)

infant_mortality_desc_limits <- infant_mortality_desc %>%
  dplyr::select(variable_new, year_char, mean, sd, Q0.25, Q0.75, IQR) %>%
  mutate(loweriqr_1.5 = Q0.25 - (IQR * 1.5)
         , upperiqr_1.5 = Q0.75 + (IQR * 1.5)
         , lowersd_3 = mean - (sd * 3)
         , uppersd_3 = mean + (sd * 3)
         , loweriqr_3 = Q0.25 - (IQR * 3)
         , upperiqr_3 = Q0.75 + (IQR * 3)
  )

infant_mortality_outcome_long_stat <- infant_mortality_outcome_long %>%
  left_join(infant_mortality_desc_limits, by = c("variable_new", "year_char")) %>%
  mutate(iqr_outlier_tot_1.5 = case_when(value > upperiqr_1.5 | value < loweriqr_1.5 ~ 1,
                                         TRUE ~ 0)
         , iqr_outlier_high_1.5 = case_when(value > upperiqr_1.5 ~ 1,
                                            TRUE ~ 0)
         , iqr_outlier_low_1.5 = case_when(value < loweriqr_1.5 ~ 1,
                                           TRUE ~ 0)
         , sd_outlier_tot_3 = case_when(value > uppersd_3 | value < lowersd_3 ~ 1,
                                        TRUE ~ 0)
         , sd_outlier_high_3 = case_when(value > uppersd_3 ~ 1,
                                         TRUE ~ 0)
         , sd_outlier_low_3 = case_when(value < lowersd_3 ~ 1,
                                        TRUE ~ 0)
         , iqr_outlier_tot_3 = case_when(value > upperiqr_3 | value < loweriqr_3 ~ 1,
                                         TRUE ~ 0)
         , iqr_outlier_high_3 = case_when(value > upperiqr_3 ~ 1,
                                          TRUE ~ 0)
         , iqr_outlier_low_3 = case_when(value < loweriqr_3 ~ 1,
                                         TRUE ~ 0)  
  )

infant_mortality_outliers <- infant_mortality_outcome_long_stat %>%
  group_by(variable_new, year_num) %>%
  summarise(countiqr_tot_1.5 = sum(iqr_outlier_tot_1.5, na.rm = TRUE)
            , countiqr_high_1.5 = sum(iqr_outlier_high_1.5, na.rm = TRUE)
            , countiqr_low_1.5 = sum(iqr_outlier_low_1.5, na.rm = TRUE)
            , countsd_tot_3 = sum(sd_outlier_tot_3, na.rm = TRUE)
            , countsd_low_3 = sum(sd_outlier_low_3, na.rm = TRUE)
            , countsd_high_3 = sum(sd_outlier_high_3, na.rm = TRUE)
            , countiqr_tot_3 = sum(iqr_outlier_tot_3, na.rm = TRUE)
            , countiqr_high_3 = sum(iqr_outlier_high_3, na.rm = TRUE)
            , countiqr_low_3 = sum(iqr_outlier_low_3, na.rm = TRUE)
  )

write_csv(infant_mortality_outliers,"output/descriptives_dependent/Outliers_infant_mortality_utla.csv")

infant_mortality_outliers_la <- infant_mortality_outcome_long_stat %>%
  filter(sd_outlier_tot_3 == 1 | iqr_outlier_tot_1.5 == 1) %>%
  dplyr::select(year_num, utla17cd, variable
                , iqr_outlier_tot_1.5
                , sd_outlier_tot_3, iqr_outlier_tot_3
  )

write_csv(infant_mortality_outliers_la,"output/descriptives_dependent/Outliers_la_infant_mortality_utla.csv")


# Detect longitudinal variability -----------------------------------------

## Method 1
#	At each time point, compare the average value across all local authorities 
# to each individual local authority value

m1_infant_mortality_se_avg <- infant_mortality_outcome_long %>%
  group_by(variable_new, year_num) %>%
  summarise(avg_value = mean(value, na.rm = TRUE))

m1_infant_mortality_outcome_long_avg <- infant_mortality_outcome_long %>%
  left_join(m1_infant_mortality_se_avg
            , by = c("variable_new", "year_num")) %>%
  mutate(diff = avg_value - value) 

m1_infant_mortality_outcome_avg_diff <- m1_infant_mortality_outcome_long_avg %>%
  group_by(variable_new, utla17cd) %>%
  summarise(total_diff = sum(diff, na.rm = TRUE)
            , avg_diff = mean(diff, na.rm = TRUE)
            , sd_diff = sd(diff, na.rm = TRUE)) %>%
  mutate(abs_total_diff = abs(total_diff)
         , abs_avg_diff = abs(avg_diff)) %>%
  filter(!is.na(sd_diff))
# If sd is NA then not enough time points so excluded

m1_variability_limits <- m1_infant_mortality_outcome_avg_diff %>%
  group_by(variable_new) %>%
  summarise(q25 = quantile(sd_diff, 0.25, na.rm = TRUE)
            , q75 = quantile(sd_diff, 0.75, na.rm = TRUE)
            , iqr = q75-q25
            , low1.5 = q25 - (1.5 * iqr)
            , low3 = q25 - (3 * iqr)
            , high1.5 = q75 + (1.5 * iqr)
            , high3 = q75 + (3 * iqr)
  )

ggplot(m1_infant_mortality_outcome_avg_diff, aes(x = tidytext::reorder_within(utla17cd,sd_diff, variable_new)
                                                 , y = sd_diff)) +
  geom_point(size = 0.2) +
  geom_hline(data = m1_variability_limits, mapping = aes(yintercept = high1.5), colour = "blue") +
  geom_hline(data = m1_variability_limits, mapping = aes(yintercept = high3), colour = "red") +
  geom_hline(data = m1_variability_limits, mapping = aes(yintercept = low1.5), colour = "blue") +
  geom_hline(data = m1_variability_limits, mapping = aes(yintercept = low3), colour = "red") +
  facet_wrap(~ factor(variable_new
                      , levels = c("u1mortality_nt_", "u1mortality_ons_")
                      , labels = c("NT Rate", "ONS Rate"))
             , scales = "fixed") +
  tidytext::scale_x_reordered() +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Local authority"
       , y = "Standard deviation of difference"
       , title = "Standard deviation of difference to all local authorities yearly means"
       , caption = "Blue line = 1.5 x IQR\nRed line = 3 x IQR") +
  theme_bw() +
  theme(panel.spacing = unit(1, "lines")
        , plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")
        , axis.ticks.x = element_blank()
        , axis.text.x = element_blank()
        )
ggsave("output/descriptives_dependent/plots/Variability_m1_sd_diff_infant_mortality_utla.pdf")

ggplot(m1_infant_mortality_outcome_avg_diff, aes(y = sd_diff)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8) +
  facet_wrap(~ factor(variable_new
                      , levels = c("u1mortality_nt_", "u1mortality_ons_")
                      , labels = c("NT Rate", "ONS Rate"))
             , scales = "fixed") +
  labs(y = "Standard deviation of difference"
       , title = "Boxplot of standard deviation of difference to all local authorities yearly means"
       , caption = "Red outliers = greater than 1.5 x IQR") +
  theme_bw() +
  theme(panel.spacing = unit(1, "lines")
        , plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")
        , axis.ticks.x = element_blank()
        , axis.text.x = element_blank()
        , axis.title.x = element_blank()
  )
ggsave("output/descriptives_dependent/plots/Variability_m1_box_infant_mortality_utla.pdf")

m1_variability_outliers_summary <- m1_infant_mortality_outcome_avg_diff %>%
  left_join(m1_variability_limits, by = "variable_new") %>%
  mutate(outlier_1.5 = case_when(sd_diff > high1.5 ~ 1
                                 , TRUE ~ 0)
         , outlier_3 = case_when(sd_diff > high3 ~ 1
                                 , TRUE ~ 0)) %>%
  group_by(variable_new) %>%
  summarise(outliers_1.5 = sum(outlier_1.5)
            , outliers_3 = sum(outlier_3)) 

write_csv(m1_variability_outliers_summary, "output/descriptives_dependent/Variability_outliers_m1_infant_mortality_utla.csv")

m1_variability_la_outliers_summary <- m1_infant_mortality_outcome_avg_diff %>%
  left_join(m1_variability_limits, by = "variable_new") %>%
  mutate(outlier_1.5 = case_when(sd_diff > high1.5 ~ 1
                                 , TRUE ~ 0)
         , outlier_3 = case_when(sd_diff > high3 ~ 1
                                 , TRUE ~ 0)) %>%
  filter(outlier_1.5 == 1) %>%
  dplyr::select(variable_new, utla17cd, outlier_1.5, outlier_3)

write_csv(m1_variability_la_outliers_summary, "output/descriptives_dependent/Variability_outliers_m1_la_infant_mortality_utla.csv")

ggplot(m1_infant_mortality_outcome_long_avg %>%
         left_join(m1_variability_outliers_summary, by = "variable_new") %>%
         inner_join(m1_variability_la_outliers_summary
                    , by = c("utla17cd", "variable_new"))
       , aes(group = utla17cd, colour = utla17cd)) +
  geom_point(aes(x = factor(year_num), y = value), size = 1) +
  geom_line(aes(x = factor(year_num), y = value)) +
  facet_wrap(~ factor(variable_new
                      , levels=c("u1mortality_nt_", "u1mortality_ons_")
                      , labels = c("NT Rate", "ONS Rate"))
             , scales = "fixed") +
  labs(x = "Year"
       , y = "Rate per 1,000 Live Births"
       , title = "Local authorities with more than 1.5*IQR in difference to all local authorities yearly means") +
  scale_x_discrete(breaks = c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016)
                   , labels = c("2000", "2002", "2004", "2006", "2008"
                                , "2010", "2012", "2014", "2016")) +
  scale_y_continuous(limits = c(0, NA)
                     , expand = c(0, 1)) +
  scale_colour_manual("Local authority"
                      , values = (colorRampPalette(RColorBrewer::brewer.pal(12,"Paired"))(n_distinct(m1_variability_la_outliers_summary$utla17cd)))) +
  theme_bw() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")
        , panel.spacing = unit(1, "lines")
        , legend.position = "bottom"
  )
ggsave("output/descriptives_dependent/plots/Variability_outliers_m1_la_infant_mortality_utla.pdf")

#######################################################

## Method 2
# At each time point, use linear regression to estimate LA value and compare to actual value

m2_fitted_models <- infant_mortality_outcome_long %>%
  group_by(variable_new, utla17cd) %>%
  do(model = lm(value ~ year_num, data = .)) %>%
  mutate(a = summary(model)$coefficients[1]
         , b = summary(model)$coefficients[2]) %>%
  dplyr::select(-model)

m2_infant_mortality_outcome_long_avg <- infant_mortality_outcome_long %>%
  left_join(m2_fitted_models
            , by = c("variable_new", "utla17cd")) %>%
  mutate(fit = a + (b * year_num)
         , diff = value - fit) 

m2_infant_mortality_outcome_avg_diff <- m2_infant_mortality_outcome_long_avg %>%
  group_by(variable_new, utla17cd) %>%
  summarise(total_diff = sum(diff, na.rm = TRUE)
            , avg_diff = mean(diff, na.rm = TRUE)
            , sd_diff = sd(diff, na.rm = TRUE)) %>%
  mutate(abs_total_diff = abs(total_diff)
         , abs_avg_diff = abs(avg_diff))

m2_variability_limits <- m2_infant_mortality_outcome_avg_diff %>%
  group_by(variable_new) %>%
  summarise(q25 = quantile(sd_diff, 0.25, na.rm = TRUE)
            , q75 = quantile(sd_diff, 0.75, na.rm = TRUE)
            , iqr = q75-q25
            , low1.5 = q25 - (1.5*iqr)
            , low3 = q25 - (3*iqr)
            , high1.5 = q75 + (1.5*iqr)
            , high3 = q75 + (3*iqr)
  )

ggplot(m2_infant_mortality_outcome_avg_diff, aes(x = tidytext::reorder_within(utla17cd,sd_diff,variable_new), y = sd_diff)) +
  geom_point(size = 0.2) +
  geom_hline(data = m2_variability_limits, mapping = aes(yintercept = high1.5), colour = "blue") +
  geom_hline(data = m2_variability_limits, mapping = aes(yintercept = high3), colour = "red") +
  geom_hline(data = m2_variability_limits, mapping = aes(yintercept = low1.5), colour = "blue") +
  geom_hline(data = m2_variability_limits, mapping = aes(yintercept = low3), colour = "red") +
  facet_wrap(~ factor(variable_new
                      , levels=c("u1mortality_nt_", "u1mortality_ons_")
                      , labels = c("NT Rate", "ONS Rate"))
             , scales = "fixed") +
  tidytext::scale_x_reordered() +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Local authority"
       , y = "Standard deviation of difference"
       , title = "Standard deviation of difference to yearly fitted values"
       , caption = "Blue line = 1.5 x IQR\nRed line = 3 x IQR") +
  theme_bw() +
  theme(panel.spacing = unit(1, "lines")
        , plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")
        , axis.ticks.x = element_blank()
        , axis.text.x = element_blank()
  )
ggsave("output/descriptives_dependent/plots/Variability_m2_sd_diff_infant_mortality_utla.pdf")

ggplot(m2_infant_mortality_outcome_avg_diff, aes(y = sd_diff)) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8) +
  facet_wrap(~ factor(variable_new
                      , levels=c("u1mortality_nt_", "u1mortality_ons_")
                      , labels = c("NT Rate", "ONS Rate"))
             , scales = "fixed") +
  labs(y = "Standard deviation of difference"
       , title = "Boxplot of standard deviation of difference to yearly fitted values"
       , caption = "Red outliers = greater than 1.5 x IQR") +
  theme_bw() +
  theme(panel.spacing = unit(1, "lines")
        , plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")
        , axis.ticks.x = element_blank()
        , axis.text.x = element_blank()
        , axis.title.x = element_blank()
  )
ggsave("output/descriptives_dependent/plots/Variability_m2_box_infant_mortality_utla.pdf")

m2_variability_outliers_summary <- m2_infant_mortality_outcome_avg_diff %>%
  left_join(m2_variability_limits, by = "variable_new") %>%
  mutate(outlier_1.5 = case_when(sd_diff > high1.5 ~ 1
                                 , TRUE ~ 0)
         , outlier_3 = case_when(sd_diff > high3 ~ 1
                                 , TRUE ~ 0)) %>%
  group_by(variable_new) %>%
  summarise(outliers_1.5 = sum(outlier_1.5)
            , outliers_3 = sum(outlier_3)) 

write_csv(m2_variability_outliers_summary, "output/descriptives_dependent/Variability_outliers_m2_infant_mortality_utla.csv")

m2_variability_la_outliers_summary <- m2_infant_mortality_outcome_avg_diff %>%
  left_join(m2_variability_limits, by = "variable_new") %>%
  mutate(outlier_1.5 = case_when(sd_diff > high1.5 ~ 1
                                 , TRUE ~ 0)
         , outlier_3 = case_when(sd_diff > high3 ~ 1
                                 , TRUE ~ 0)) %>%
  filter(outlier_1.5 == 1) %>%
  dplyr::select(variable_new, utla17cd, outlier_1.5, outlier_3)

write_csv(m2_variability_la_outliers_summary, "output/descriptives_dependent/Variability_outliers_m2_la_infant_mortality_utla.csv")

ggplot(m2_infant_mortality_outcome_long_avg %>%
         left_join(m2_variability_outliers_summary, by = "variable_new") %>%
         inner_join(m2_variability_la_outliers_summary, by = c("utla17cd", "variable_new")), aes(group = utla17cd, colour = utla17cd)) +
  geom_point(aes(x = factor(year_num), y = value), size = 1) +
  geom_line(aes(x = factor(year_num), y = value)) +
  facet_wrap(~ factor(variable_new
                      , levels=c("u1mortality_nt_", "u1mortality_ons_")
                      , labels = c("NT Rate", "ONS Rate"))
             , scales = "fixed") +
  labs(x = "Year"
       , y = "Rate per 1,000 Live Births"
       , title = "Local authorities with more than 1.5*IQR in difference to yearly fitted values") +
  scale_x_discrete(breaks = c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016)
                   , labels = c("2000", "2002", "2004", "2006", "2008"
                                , "2010", "2012", "2014", "2016")) +
  scale_y_continuous(limits = c(0, NA)
                     , expand = c(0, 1)) +
  scale_colour_manual("Local authority"
                      , values = (colorRampPalette(RColorBrewer::brewer.pal(12,"Paired"))(n_distinct(m2_variability_la_outliers_summary$utla17cd)))) +
  theme_bw() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")
        , panel.spacing = unit(1, "lines")
        , legend.position = "bottom"
  )
ggsave("output/descriptives_dependent/plots/Variability_outliers_m2_la_infant_mortality_utla.pdf")


# Distribution of dependent variable --------------------------------------

# Normal?

npages_dist <- ggforce::n_pages(ggplot(infant_mortality_outcome_long, aes(x = value)) +
                                  ggforce::facet_wrap_paginate(year_char ~ variable_new
                                                               , nrow=4, ncol=2, scales = "free_x"))

pdf("output/descriptives_dependent/plots/Normality_infant_mortality_utla.pdf")

for(i in 1:npages_dist){
  
  print(
    ggplot(infant_mortality_outcome_long %>%
             left_join(infant_mortality_desc %>%
                         dplyr::select(variable_new, year_char, mean, skew)
                       , by = c("variable_new", "year_char"))
           , aes(x = value)) +
      geom_density(fill = NT_colour("light purple 1"), colour = NT_colour("NT ink")) +
      geom_vline(aes(xintercept = mean), colour = NT_colour("NT ink"), linetype = "dotted", size = 0.5) +
      geom_text(infant_mortality_desc 
                , mapping = aes(x = Inf, y = Inf, label = paste("skewness: ",round(skew,2)))
                , hjust = "inward", vjust = "inward"
                , size = 3, colour = "black") +
      ggforce::facet_wrap_paginate(year_char ~ variable_new,  nrow = 4, ncol = 2, scales = "free_x", page = i) +
      labs(y = "Frequency"
           , x = "Rate per 1,000 Live Births"
           , title = "Density plots: infant mortality outcomes all years") +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      NT_style() +
      theme(panel.spacing = unit(1, "lines"))
  )
  
}

dev.off()

ggplot(infant_mortality_outcome_long %>%
         filter(variable_new == "u1mortality_nt_" & year_char == "2017") %>%
         left_join(infant_mortality_desc %>%
                     dplyr::select(variable_new, year_char, mean, skew)
                   , by = c("variable_new", "year_char"))
       , aes(x = value)) +
  geom_density(fill = NT_colour("light purple 1"), colour = NT_colour("NT ink")) +
  geom_vline(aes(xintercept = mean), colour = NT_colour("NT ink"), linetype = "dotted", size = 0.5) +
  geom_text(infant_mortality_desc %>%
              filter(variable_new == "u1mortality_nt_" & year_char == "2017")
            , mapping=aes(x=Inf, y=Inf, label = paste("skewness: ",round(skew,2)))
            , hjust = "inward", vjust = "inward", colour = "black") +
  labs(y = "Frequency"
       , x = "Rate per 1,000 Live Births"
       , title = "Density plot: NT Infant Mortality Rate 2017") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  NT_style()
ggsave("output/descriptives_dependent/plots/Normality_infant_mortality_utla_17.pdf", width = 5, height = 5)


# Map dependent variable --------------------------------------------------

im_shapefile_eng <- sf::st_read("data/County_UA_Dec17_EW_BFE/County_UA_Dec17_EW_BFE.shp", layer = "County_UA_Dec17_EW_BFE") %>%
  clean_names() %>% 
  filter(str_detect(ctyua17cd, "^E")) %>%
  mutate(ctyua17cd = case_when(ctyua17cd == "E06000053" ~ "E06000052"
                               , ctyua17cd == "E09000001" ~ "E09000012"
                               , TRUE ~ ctyua17cd)) %>%
  left_join(infant_mortality_outcome %>%
              dplyr::select(utla17cd, u1mortality_nt_17) %>%
              mutate(u1mortality_nt_17_round = round(u1mortality_nt_17,2)
                     , q0 = round(quantile(u1mortality_nt_17_round, probs = 0, na.rm = TRUE), 1)
                     , q2 = round(quantile(u1mortality_nt_17_round, probs = 0.2, na.rm = TRUE), 1)
                     , q4 = round(quantile(u1mortality_nt_17_round, probs = 0.4, na.rm = TRUE), 1)
                     , q6 = round(quantile(u1mortality_nt_17_round, probs = 0.6, na.rm = TRUE), 1)
                     , q8 = round(quantile(u1mortality_nt_17_round, probs = 0.8, na.rm = TRUE), 1)
                     , q10 = round(quantile(u1mortality_nt_17_round, probs = 1.0, na.rm = TRUE), 1)
                     , rate_bucket = case_when(u1mortality_nt_17_round < q2 ~ "1"
                                               , u1mortality_nt_17_round >= q2 & u1mortality_nt_17_round < q4 ~ "2"
                                               , u1mortality_nt_17_round >= q4 & u1mortality_nt_17_round < q6 ~ "3"
                                               , u1mortality_nt_17_round >= q6 & u1mortality_nt_17_round < q8 ~ "4"
                                               , u1mortality_nt_17_round >= q8 ~ "5"))
            , by = c("ctyua17cd" = "utla17cd"))

england <- ggplot(im_shapefile_eng) +
  geom_sf(aes(fill = rate_bucket), colour = NT_colour("NT ink")) +
  coord_sf(datum = NA) +
  scale_fill_NT("Infant deaths per\n1,000 live births"
                , palette = NT_palette("purple", reverse = TRUE)
                , labels = c("0.00-<2.6", "2.6-<3.2", "3.2-<3.9", "3.9-<5.1", "5.1-<=7.9")) +
  NT_style() +
  theme(
    axis.text = element_blank()
    , axis.title = element_blank()
    , legend.position = c(0.14, 0.5)
    , legend.text = element_text(colour = NT_colour("NT ink"), size = 8, family = "Circular")
    , legend.title = element_text(colour = NT_colour("NT ink"), size = 8, face = "bold", family = "Circular", hjust = 0)
    , panel.grid = element_blank()
    , plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit ="cm")
  )

london <- ggplot(im_shapefile_eng %>% filter(str_detect(ctyua17cd, "^E09"))) +
  geom_sf(aes(fill = rate_bucket), colour = NT_colour("NT ink")) +
  coord_sf(datum = NA) +
  labs(title = "London") +
  scale_fill_NT(palette = NT_palette("purple", reverse = TRUE)) +
  NT_style() +
  theme(
    axis.text = element_blank()
    , axis.title = element_blank()
    , legend.position = "none"
    , panel.grid = element_blank()
    , plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit ="cm")
    , plot.background = element_rect(fill = NT_colour("cool light grey"), colour = NT_colour("NT ink"))
    , panel.background = element_blank()
    , plot.title = element_text(margin = margin(t = 0.1, r = 0, b = 0, l = 0, unit ="cm")
                                , colour = NT_colour("NT ink"), size = 8, family = "Circular"
                                , hjust = 0.02)
  )


cowplot::ggdraw() +
  cowplot::draw_plot(england) +
  cowplot::draw_plot(london, x = 0.04, y = 0.65, width = 0.34, height = 0.34)

ggsave("output/descriptives_dependent/plots/map_infant_mortality_utla_17.png"
       , width = 13.7, units = "cm", dpi = 600)

