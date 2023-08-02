#-------------------------------------------------------------------------------
# Simple GLM regression

# Models of dependent variable with each independent variable individually

# Explore poisson model suitability
# Quasi poisson model fit
# Negative binomial model fit
# Coefficients and model fit file
# Negative binomial model plots
# Prediction and confidence interval plots
# Models with influential points excluded
#-------------------------------------------------------------------------------


# Create long file of variables with chosen outcome -----------------------

# 2017 NT infant mortality rate

infant_mortality_outcome_vars_long <- infant_mortality_vars_long %>%
  left_join(read_csv("data/Infant_Mortality_Counts_UTLA.csv") %>%
              filter(year_num == 2017) %>%
              dplyr::select(utla17cd, numerator, denominator) %>%
              dplyr::rename(u1mortality_nt_17 = numerator)
            , by = "utla17cd")


# Explore poisson model suitability ---------------------------------------

# Poisson model tests

# Dispersion test - tests the null hypothesis of equidispersion in Poisson GLMs against the alternative of overdispersion and/or underdispersion
# Dispersion test tells us whether negative binomial model is more appropriate and if mean=variance 
# Predictory variables must be independent (no multicollinniarity)

simple_models <- tibble(variable = unique(infant_mortality_outcome_vars_long$variable)) %>% 
  mutate(
    dataset  = map(variable, function(var) infant_mortality_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) glm(u1mortality_nt_17 ~ value + offset(log(denominator))
                                               , dset , family = poisson(link = "log")))
  )

poisson_model_tests <- simple_models %>%
  mutate(
    dispersion_pvalue      = map_chr(model, function(x) tidy(AER::dispersiontest(x, trafo = 1, alternative = c("two.sided")))$p.value)
    , dispersion_parameter = map_chr(model, function(x) tidy(AER::dispersiontest(x, trafo = 1, alternative = c("two.sided")))$estimate)
    , dispersion_check_pvalue = case_when(dispersion_pvalue >= 0.05 ~ 1, TRUE ~ 0)
    , dispersion_check_parameter = case_when((dispersion_parameter < 0.9 | dispersion_parameter > 1.1) ~ 1, TRUE ~ 0 )
    , checks_passed  = case_when((dispersion_check_pvalue == 1 | dispersion_check_parameter == 1 #& zeroinflation_check == 1#
    ) ~ 1, TRUE ~ 0)
    , checks_passed_excl_parameter = case_when((dispersion_check_pvalue == 1) ~ 1, TRUE ~ 0))%>%
  dplyr::select(-model, -dataset)

table(poisson_model_tests$dispersion_check_parameter)
# over double dispersed according to the parameter
table(poisson_model_tests$dispersion_check_pvalue)
# No over or under dispersion according to the p-value
table(poisson_model_tests$checks_passed)

write_csv(poisson_model_tests, "output/simple_glm_regression/Poisson_tests_infant_mortality_utla.csv")


# Quasi-poisson model fit -------------------------------------------------

simple_models_quasi <- tibble(variable = unique(infant_mortality_outcome_vars_long$variable)) %>% 
  mutate(
    dataset  = map(variable, function(var) infant_mortality_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) glm(u1mortality_nt_17 ~ value + offset(log(denominator))
                                               , dset , family = quasipoisson(link = "log")))
  )

simple_models_quasi_out <- simple_models_quasi %>% 
  mutate(
    observations       = map_int(model, nobs)
    , intercept_coeff  = map_chr(model, function(x) tidy(x)$estimate[1])
    , intercept_se     = map_chr(model, function(x) tidy(x)$std.error[1])
    , intercept_lcl    = map_chr(model, function(x) tidy(x, conf.int = TRUE, conf.level = 0.95)$conf.low[1])
    , intercept_ucl    = map_chr(model, function(x) tidy(x, conf.int = TRUE, conf.level = 0.95)$conf.high[1])
    , intercept_pvalue = map_chr(model, function(x) tidy(x)$p.value[1])
    , var_coeff        = map_chr(model, function(x) tidy(x)$estimate[2])
    , var_se           = map_chr(model, function(x) tidy(x)$std.error[2])
    , var_lcl          = map_chr(model, function(x) tidy(x, conf.int = TRUE, conf.level = 0.95)$conf.low[2])
    , var_ucl          = map_chr(model, function(x) tidy(x, conf.int = TRUE, conf.level = 0.95)$conf.high[2])
    , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
    , null_deviance    = map_chr(model, function(x) glance(x)$null.deviance)
    , deviance         = map_chr(model, function(x) glance(x)$deviance)
    , loglik           = map_chr(model, function(x) glance(x)$logLik)
    , aic              = map_chr(model, function(x) glance(x)$AIC)
    , bic              = map_chr(model, function(x) glance(x)$BIC)
    , pseudoR2_Efron   = map_chr(model, function(x) (DescTools::PseudoR2(x, c("Efron"))))) %>%
  dplyr::select(-model, -dataset)

write_csv(simple_models_quasi_out, "output/simple_glm_regression/QuasiPoisson_coeff_fit_infant_mortality_utla.csv")


# Negative binomial fit ---------------------------------------------------

simple_models_nb <- tibble(variable = unique(infant_mortality_outcome_vars_long$variable)) %>% 
  mutate(
    dataset  = map(variable, function(var) infant_mortality_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) MASS::glm.nb(u1mortality_nt_17 ~ value + offset(log(denominator))
                                                        , dset))
  )


# Coefficients and model fit file -----------------------------------------

simple_models_nb_out <- simple_models_nb %>% 
  mutate(
    observations       = map_int(model, nobs)
    , intercept_coeff  = map_chr(model, function(x) tidy(x)$estimate[1])
    , intercept_se     = map_chr(model, function(x) tidy(x)$std.error[1])
    , intercept_lcl    = map_chr(model, function(x) tidy(x, conf.int = TRUE, conf.level = 0.95)$conf.low[1])
    , intercept_ucl    = map_chr(model, function(x) tidy(x, conf.int = TRUE, conf.level = 0.95)$conf.high[1])
    , intercept_pvalue = map_chr(model, function(x) tidy(x)$p.value[1])
    , var_coeff        = map_chr(model, function(x) tidy(x)$estimate[2])
    , var_se           = map_chr(model, function(x) tidy(x)$std.error[2])
    , var_lcl          = map_chr(model, function(x) tidy(x, conf.int = TRUE, conf.level = 0.95)$conf.low[2])
    , var_ucl          = map_chr(model, function(x) tidy(x, conf.int = TRUE, conf.level = 0.95)$conf.high[2])
    , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
    , null_deviance    = map_chr(model, function(x) glance(x)$null.deviance)
    , deviance         = map_chr(model, function(x) glance(x)$deviance)
    , loglik           = map_chr(model, function(x) glance(x)$logLik)
    , aic              = map_chr(model, function(x) glance(x)$AIC)
    , bic              = map_chr(model, function(x) glance(x)$BIC)
    , standard_coeff   = map_chr(model, function(x) lm.beta::lm.beta(x)$standardized.coefficients[2])
    , rsq              = map_chr(model, function(x) rsq::rsq(x, adj = FALSE, type = "kl"))) %>%
  dplyr::select(-model, -dataset)

write_csv(simple_models_nb_out, "output/simple_glm_regression/Negativebinomial_coeff_fit_infant_mortality_utla.csv")


# Negative binomial model plots -------------------------------------------

# Response histogram
# Residuals vs fitted - want to be randomly distributed around centre of zero, line should be as straight as possible
# Quantile plot - want residuals to hug the line
# Cook's distance - don't want any data point to have too much influence
# Autocorrelation - points after lag 1 to be within confidence bounds
# Histogram - residuals normally distributed

nb_model_plots <- simple_models_nb %>%
  mutate(
    residuals = map(model, function(x) augment(x))
    ,  hist_response = map2(model, variable, function(dset, var_lab) ggplot(dset, aes(x = value, y = u1mortality_nt_17)) +
                              geom_histogram(aes(y = ..density..), bins = 10, fill = NT_colour("light purple 1")) +
                              labs(y = "Rate per 1,000 live births", x = "variable value", title = var_lab) +
                              NT_style())
    , res_v_fit  = map2(model, variable, function(dset, var_lab) ggplot(dset, aes(x = .fitted, y = .resid)) +
                          geom_point() + stat_smooth(method = "loess") + 
                          geom_hline(yintercept = 0, colour = NT_colour("bright red"), lty = "dashed") +
                          labs(title = var_lab, x = "Fitted values",  y = "Residuals") +
                          NT_style())
    , quantile   = map2(model, variable, function(dset, var_lab) ggplot(dset, aes(sample = .stdresid)) +
                          stat_qq() + 
                          stat_qq_line() + 
                          labs(x = "Theoretical quantiles", y = "Standardised residuals"
                               , title = var_lab) +
                          NT_style())
    , cooks_dist = map2(model, variable, function(dset, var_lab) ggplot(dset, aes(x = seq_along(.cooksd), y = .cooksd)) +
                          geom_bar(stat = "identity", position = "identity") + 
                          labs(x = "Observation number",y = "Cook's distance", title = var_lab) +
                          theme_bw() +
                          theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines")))
    , hist_resid = map2(residuals, variable, function(dset, var_lab) ggplot(dset, aes(x = .resid)) +
                          geom_histogram(aes(y = ..density..), bins = 10, fill = NT_colour("light purple 1")) +
                          stat_function(fun = dnorm, args = list(mean = mean(dset$.resid), sd = sd(dset$.resid))
                                        , colour = NT_colour("bright blue")) +
                          labs(y = "Frequency", x = "Residuals", title = var_lab) +
                          NT_style())
  ) %>%
  dplyr::select(-model, -residuals, -dataset)

hist_response <- ggpubr::ggarrange(plotlist = nb_model_plots$hist_response, nrow = 3, ncol = 2)
ggpubr::ggexport(hist_response, filename = "output/simple_glm_regression/plots/NB_hist_response_infant_mortality_utla.pdf")

res_v_fit <- ggpubr::ggarrange(plotlist = nb_model_plots$res_v_fit, nrow = 3, ncol = 2)
ggpubr::ggexport(res_v_fit, filename = "output/simple_glm_regression/plots/NB_res_v_fit_infant_mortality_utla.pdf")

normqq <- ggpubr::ggarrange(plotlist = nb_model_plots$quantile, nrow = 3, ncol = 2)
ggpubr::ggexport(normqq, filename = "output/simple_glm_regression/plots/NB_normqq_infant_mortality_utla.pdf")

cooks_dist <- ggpubr::ggarrange(plotlist = nb_model_plots$cooks_dist, nrow = 3, ncol = 2)
ggpubr::ggexport(cooks_dist, filename = "output/simple_glm_regression/plots/NB_cooks_dist_infant_mortality_utla.pdf")

hist_resid <- ggpubr::ggarrange(plotlist = nb_model_plots$hist_resid, nrow = 3, ncol = 2)
ggpubr::ggexport(hist_resid, filename = "output/simple_glm_regression/plots/NB_hist_resid_infant_mortality_utla.pdf")


# Prediction and confidence interval plots --------------------------------

simple_model_la_output <- infant_mortality_outcome_vars_long %>%
  left_join(simple_models_nb %>%
              dplyr::select(variable, model)
            , by = "variable") %>%
  group_by(variable) %>%
  do(ciTools::add_ci(., first(.$model), names = c("lcl", "ucl"))) %>%
  dplyr::select(-model)

npages_int <- ggforce::n_pages(ggplot(simple_model_la_output, aes(x=u1mortality_nt_17, y=value)) +
                                 geom_point(size=0.2) +
                                 ggforce::facet_wrap_paginate(~ variable, scales="free_y", ncol=2, nrow=3))

pdf("output/simple_glm_regression/plots/Negativebinomial_model_infant_mortality_utla.pdf")

for(i in 1:npages_int){
  
  print(
    ggplot(simple_model_la_output, aes(y=u1mortality_nt_17, x=value)) +
      geom_point(size = 0.2, colour = NT_colour("NT ink")) +
      ggforce::facet_wrap_paginate(~ variable, scales = "free_x", ncol = 2, nrow = 3, page = i) +
      geom_line(aes(y = pred), size = 0.2, colour = NT_colour("NT ink")) +
      geom_ribbon(aes(ymin = lcl, ymax = ucl), fill = NT_colour("bright blue"), alpha = 0.3) +
      labs( y = "Rate per 1,000 Live Births", x = "Variable value") +
      NT_style() +
      theme(panel.spacing = unit(1,"lines"))
  )
  
}

dev.off()


# Model with influential points excluded ----------------------------------

# options to use deviance for calculating residuals for detecting outliers
# BUT, can also use Cook's distance in same way as linear models

# Mean cook's distance for each model
# Points that are greater than 4 * mean cooks distance
# Exclude these and re run model
# Produce same outputs as above

outliers_models_nb <- simple_models_nb %>% 
  mutate(
    residuals   = map(model, function(x) augment(x) %>% 
                        mutate(mean_cook_threshold = 4*mean(.cooksd)
                               , observation = row_number()) %>%
                        filter(.cooksd < mean_cook_threshold))
    , new_data  = map2(dataset, residuals, function(dset, resids) dset %>% 
                         inner_join(resids %>% 
                                      dplyr::select(observation)
                                    , by = "observation"))
    , new_model  = map(new_data, function(dset) MASS::glm.nb(u1mortality_nt_17 ~ value + offset(log(denominator))
                                                             , dset))
  ) %>%
  dplyr::select(-dataset, -model, -residuals)

outliers_models_nb_out <- outliers_models_nb %>% 
  mutate(
    observations       = map_int(new_model, nobs)
    , intercept_coeff  = map_chr(new_model, function(x) tidy(x)$estimate[1])
    , intercept_se     = map_chr(new_model, function(x) tidy(x)$std.error[1])
    , intercept_lcl    = map_chr(new_model, function(x) tidy(x, conf.int = TRUE, conf.level = 0.95)$conf.low[1])
    , intercept_ucl    = map_chr(new_model, function(x) tidy(x, conf.int = TRUE, conf.level = 0.95)$conf.high[1])
    , intercept_pvalue = map_chr(new_model, function(x) tidy(x)$p.value[1])
    , var_coeff        = map_chr(new_model, function(x) tidy(x)$estimate[2])
    , var_se           = map_chr(new_model, function(x) tidy(x)$std.error[2])
    , var_lcl          = map_chr(new_model, function(x) tidy(x, conf.int = TRUE, conf.level = 0.95)$conf.low[2])
    , var_ucl          = map_chr(new_model, function(x) tidy(x, conf.int = TRUE, conf.level = 0.95)$conf.high[2])
    , var_pvalue       = map_chr(new_model, function(x) tidy(x)$p.value[2])
    , null_deviance    = map_chr(new_model, function(x) glance(x)$null.deviance)
    , deviance         = map_chr(new_model, function(x) glance(x)$deviance)
    , loglik           = map_chr(new_model, function(x) glance(x)$logLik)
    , aic              = map_chr(new_model, function(x) glance(x)$AIC)
    , bic              = map_chr(new_model, function(x) glance(x)$BIC)
    , standard_coeff   = map_chr(new_model, function(x) lm.beta::lm.beta(x)$standardized.coefficients[2])) %>%
  dplyr::select(-new_model, -new_data)

write_csv(outliers_models_nb_out, "output/simple_glm_regression/Linear_nb_coeff_fit_outlier_infant_mortality_utla.csv")

