#-------------------------------------------------------------------------------
# Summarise dependent variable nationally both cross-sectional and longitudinal

# UK 4 countries trends over time plot
# Summary table of 4 countries data
# Summary table of changes over time
#-------------------------------------------------------------------------------


# Collect national data ---------------------------------------------------

# Calculate confidence limits - Byar's method

infant_mortality_uk <- read_csv("data/Infant_Mortality_UK.csv") %>%
  mutate(lcl = ((numerator * ((1 - (1 / (9 * numerator)) 
                               - (1.96/(3 * (sqrt(numerator))))) ^ 3))
                /denominator) * 1000
         , ucl = ((((numerator + 1) * ((1 - (1 /(9 * (numerator + 1))) 
                                        + (1.96 / (3 * (sqrt(numerator + 1))))) ^3 ))
                   /denominator) * 1000))


# UK 4 countries trends over time plot ------------------------------------

ggplot(infant_mortality_uk, aes(x = year_num, y = value, group = country, colour = country)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.1, size = 0.1) +
  labs(y = "Rate per 1,000 Live Births"
       , x = "Year"
       , title = "Infant Mortality rate in UK countries") +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018)
                     , labels = c("2008", "2010", "2012", "2014", "2016", "2018")) +
  scale_y_continuous(limits = c(0, NA)
                     , expand = c(0, 1)) +
  scale_colour_manual("Country"
                      , labels = c("E" = "England", "NI" = "Northern Ireland", "S" = "Scotland", "W" = "Wales")
                      , values = c("E" = NT_colour("bright purple"), "NI" = NT_colour("bright green"), "S" = NT_colour("bright blue"), "W" = NT_colour("bright red"))) +
  NT_style()
ggsave("output/descriptives_dependent/plots/Trend_infant_mortality_uk.pdf", width = 13.7, height = 10, units = "cm", dpi = 600)


# Save summary of UK 4 countries data -------------------------------------

write_csv(infant_mortality_uk, "output/descriptives_dependent/Summary_infant_mortality_uk.csv")


# Summary table of change over time ---------------------------------------

# Compare rates between earliest and latest year

trends_uk <- infant_mortality_uk %>%
  filter(!is.na(value)) %>%
  group_by(country) %>%
  filter(year_num == min(year_num)) %>%
  dplyr::select(country, year_num, year_label, value, numerator, denominator) %>%
  rename(start_year_num = year_num
         , start_year_label = year_label
         , start_value = value
         , start_numerator = numerator
         , start_denominator = denominator) %>%
  left_join(infant_mortality_uk %>%
              filter(!is.na(value)) %>%
              group_by(country) %>%
              filter(year_num == max(year_num)) %>%
              dplyr::select(country, year_num, year_label, value, numerator, denominator)
            , by = "country") %>%
  rename(end_year_num = year_num
         , end_year_label = year_label
         , end_value = value
         , end_numerator = numerator
         , end_denominator = denominator) %>%
  mutate(diff = end_value - start_value
         , perc_diff = (diff / start_value) * 100
         , p_value = prop.test(x=c(start_numerator, end_numerator)
                               , n=c(start_denominator, end_denominator)
                               , conf.level = 0.95
                               , correct = TRUE)$p.value
         , trend_char = case_when(diff > 0 & p_value < 0.05 ~ "Higher"
                                  , p_value >= 0.05 ~ "Stable"
                                  , diff < 0 & p_value < 0.05 ~ "Lower")) %>%
  dplyr::select(country, start_year_label, start_value, end_year_label, end_value, diff, perc_diff, p_value, trend_char)

write_csv(trends_uk, "output/descriptives_dependent/Trends_infant_mortality_uk.csv")

