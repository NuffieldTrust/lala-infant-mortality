#-------------------------------------------------------------------------------
# Check the relationship between dependent and independent variable

# Linear relationship: Scatter plots with loess curve
# Pearson's correlation coefficient
#-------------------------------------------------------------------------------


# Create long file of variables with chosen outcome -----------------------

# 2017 NT infant mortality rate

infant_mortality_outcome_vars_long <- infant_mortality_vars_long %>%
  left_join(infant_mortality_outcome_long %>%
              filter(variable_new == "u1mortality_nt_" & year_num == 2017) %>%
              select(utla17cd, value) %>%
              dplyr::rename(u1mortality_nt_17 = value)
            , by = "utla17cd")


# Linear relationship -----------------------------------------------------

# Scatter plots with loess curve

npages_corr <- ggforce::n_pages(ggplot(infant_mortality_outcome_vars_long, aes(y = u1mortality_nt_17, x = value)) +
                                  geom_point() +
                                  geom_smooth(method = "loess", formula = y ~ x) +
                                  ggforce::facet_wrap_paginate(~ variable, scales = "free", nrow = 3, ncol = 2))

pdf("output/correlation_dependent_independent/plots/Correlation_vars_infant_mortality_utla.pdf")

for(i in 1:npages_corr){
  
  print(ggplot(infant_mortality_outcome_vars_long, aes(y = u1mortality_nt_17, x = value)) +
          geom_point(colour = NT_colour("NT ink")) +
          geom_smooth(method = "loess", formula = y ~ x) +
          ggforce::facet_wrap_paginate(~ variable, scales = "free", nrow = 3, ncol =2, page = i) +
          labs(title = "Correlation plot with locally estimated smoothing line"
               , x = "Rate per 1,000 Live Birth") +
          NT_style() + 
          theme(panel.spacing = unit(1,"lines"))
        )
  
}

dev.off()

# Test log of mean rate in linear: Scatter plots with loess curve

npages_corr1 <- ggforce::n_pages(ggplot(infant_mortality_outcome_vars_long, aes(y = log(u1mortality_nt_17), x = value)) +
                                  geom_point() +
                                  geom_smooth(method = "loess", formula = y ~ x) +
                                  ggforce::facet_wrap_paginate(~ variable, scales = "free", nrow = 3, ncol = 2))

pdf("output/correlation_dependent_independent/plots/Correlation_vars_log_infant_mortality_utla.pdf")

for(i in 1:npages_corr1){
  
  print(ggplot(infant_mortality_outcome_vars_long, aes(y = log(u1mortality_nt_17), x = value)) +
          geom_point() +
          geom_smooth(method = "loess", formula = y ~ x) +
          ggforce::facet_wrap_paginate(~ variable, scales = "free", nrow = 3, ncol =2, page = i) +
          labs(title = "Correlation plot with locally estimated smoothing line"
               , x = "Rate per 1,000 Live Birth") +
          NT_style() + 
          theme(panel.spacing = unit(1,"lines"))
  )
  
}

dev.off()


# Pearson's correlation coefficient ---------------------------------------

pearson_correlation <- plyr::ddply(infant_mortality_outcome_vars_long, plyr::.(variable), summarise
                                   , "pearson_correlation" = cor.test(u1mortality_nt_17, value, method = "pearson", use = "na.or.complete")$estimate
                                   , "p_value" = cor.test(u1mortality_nt_17, value, method = "pearson", use = "na.or.complete")$p.value)

write_csv(pearson_correlation, "output/correlation_dependent_independent/Pearson_correlation_vars_infant_mortality_utla.csv")

