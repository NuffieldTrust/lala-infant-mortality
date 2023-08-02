#-------------------------------------------------------------------------------
# Blocks multiple GLM regression

# Use literature themes to put variables in blocks
# Use stepwise regression, multicollinearity and knowledge of variables to refine block list
# Create a socioeconomic/demographic block which can be used for general adjustment
# Create a full multiple model with each theme and the socioeconomic/demographic block

# !!! Included socioeconomic/demographic variables under existing outcome themes if more relevant there
#-------------------------------------------------------------------------------


# Create long and wide file of variables with chosen outcome --------------

# 2017 NT infant mortality rate

# Wide

infant_mortality_outcome_vars <- infant_mortality_vars_unsupp %>%
  left_join(read_csv("data/Infant_Mortality_Counts_UTLA.csv") %>%
              filter(year_num == 2017) %>%
              dplyr::select(utla17cd, numerator, denominator) %>%
              dplyr::rename(u1mortality_nt_17 = numerator)
            , by = "utla17cd")

# Long

infant_mortality_outcome_vars_long <- infant_mortality_vars_long %>%
  left_join(read_csv("data/Infant_Mortality_Counts_UTLA.csv") %>%
              filter(year_num == 2017) %>%
              dplyr::select(utla17cd, numerator, denominator) %>%
              dplyr::rename(u1mortality_nt_17 = numerator)
            , by = "utla17cd")


# Import variable themes lookup -------------------------------------------

variable_themes <- read_csv("data/Variable_themes.csv")


# Individual maternal characteristics -------------------------------------

# Identify list of variables based on association to include in initial block
# Select from multiple time points

maternal <- infant_mortality_outcome_vars_long %>% 
  filter(variable_new %in% unique((variable_themes %>% 
                                     filter(theme == "individual maternal characteristics"))$variable)) %>%
  dplyr::select(variable, variable_new) %>%
  distinct(variable, variable_new)

maternal_simple_models <- maternal %>% 
  mutate(
    dataset  = map(variable, function(var) infant_mortality_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) MASS::glm.nb(u1mortality_nt_17 ~ value + offset(log(denominator))
                                                        , dset))
    , observations       = map_int(model, nobs)
    , var_coeff       = map_chr(model, function(x) tidy(x)$estimate[2])
    , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
    , null_deviance    = map_chr(model, function(x) glance(x)$null.deviance)
    , deviance         = map_chr(model, function(x) glance(x)$deviance)
    , loglik           = map_chr(model, function(x) glance(x)$logLik)
    , aic              = map_chr(model, function(x) glance(x)$AIC)
    , bic              = map_chr(model, function(x) glance(x)$BIC)
  ) %>%
  dplyr::select(-model, -dataset) %>%
  filter(observations == 150) %>%
  group_by(variable_new) %>%
  top_n(aic, n = -1)

#################################################################

# Create first multiple model and then use stepwise to refine
# Include one of each variables from the most associated list created in previous step
# Only keep one of any variables which you know to overlap e.g. prop_imd_15 and av_imd_15
# If the most associated list doesn't include an appropriate time point, include the most appropriate time point instead and drop the inappropriate

# Have removed the composite bme deliveries and black deliveries as I have included the split Asian ethnicities and African/Caribbean ethnicities

maternal_model <- MASS::glm.nb(u1mortality_nt_17 ~ . -utla17cd -denominator + offset(log(denominator)), data = infant_mortality_outcome_vars %>% dplyr::select(u1mortality_nt_17
                                                                                              , utla17cd
                                                                                              , denominator 
                                                                                              , asianidel_imp_16
                                                                                              , asianpdel_imp_16
                                                                                              , asianbdel_imp_16
                                                                                              , blackcdel_imp_16
                                                                                              , blackadel_imp_16
                                                                                              , smokedel_phe_imp_1617
                                                                                              , u20_del_ons_17 
                                                                                              , over35_del_ons_17
                                                                                              , sevobese_se_new_1617
                                                                                              , obese_se_new_1617
                                                                                              , uweight_se_new_1617))

stepwise_maternal_model <- MASS::stepAIC(maternal_model, trace = FALSE, direction="both")

# Check for multicollinearity with variance inflation factor and pearson correlation

vif_stepwise_maternal_model <- as_tibble(mctest::imcdiag(infant_mortality_outcome_vars %>% dplyr::select(attr(terms(stepwise_maternal_model), "term.labels"))
                                                            , infant_mortality_outcome_vars$u1mortality_nt_17)$idiags
                                            , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_stepwise_maternal_model <- as_tibble(cor(infant_mortality_outcome_vars %>% dplyr::select(attr(terms(stepwise_maternal_model), "term.labels")))
                                                , validate = NULL, .name_repair = NULL, rownames = "variables")

pearson_stepwise_maternal_model[pearson_stepwise_maternal_model > -0.8 & pearson_stepwise_maternal_model < 0.8] <- NA

summary(stepwise_maternal_model)

# Save block

maternal_block <- tidy(stepwise_maternal_model) %>%
  mutate(fdr_pvalue = p.adjust(p.value, method = "fdr")) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(stepwise_maternal_model)$standardized.coefficients))


# Individual infant characteristics ---------------------------------------

infant <- infant_mortality_outcome_vars_long %>% 
  filter(variable_new %in% unique((variable_themes %>% 
                                     filter(theme == "individual infant characteristics"))$variable)) %>%
  dplyr::select(variable, variable_new) %>%
  distinct(variable, variable_new)

infant_simple_models <- infant %>% 
  mutate(
    dataset  = map(variable, function(var) infant_mortality_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) MASS::glm.nb(u1mortality_nt_17 ~ value + offset(log(denominator))
                                                        , dset))
    , observations       = map_int(model, nobs)
    , var_coeff        = map_chr(model, function(x) tidy(x)$estimate[2])
    , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
    , null_deviance    = map_chr(model, function(x) glance(x)$null.deviance)
    , deviance         = map_chr(model, function(x) glance(x)$deviance)
    , loglik           = map_chr(model, function(x) glance(x)$logLik)
    , aic              = map_chr(model, function(x) glance(x)$AIC)
    , bic              = map_chr(model, function(x) glance(x)$BIC)
  ) %>%
  dplyr::select(-model, -dataset) %>%
  filter(observations == 150) %>%
  group_by(variable_new) %>%
  top_n(aic, n = -1)

infant_model <- MASS::glm.nb(u1mortality_nt_17 ~ . -utla17cd -denominator + offset(log(denominator))
                             , data = infant_mortality_outcome_vars %>% dplyr::select(u1mortality_nt_17
                                                                               , utla17cd
                                                                               , denominator
                                                                               , Prebirth_phe_1517
                                                                               , lowbirth_term_17))

stepwise_infant_model <- MASS::stepAIC(infant_model, trace = FALSE, direction = "both")

vif_stepwise_infant_model <- as_tibble(mctest::imcdiag(infant_mortality_outcome_vars %>% dplyr::select(attr(terms(stepwise_infant_model), "term.labels"))
                                                         , infant_mortality_outcome_vars$u1mortality_nt_17)$idiags
                                         , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_stepwise_infant_model <- as_tibble(cor(infant_mortality_outcome_vars %>% dplyr::select(attr(terms(stepwise_infant_model), "term.labels")))
                                             , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_stepwise_infant_model[pearson_stepwise_infant_model > -0.8 & pearson_stepwise_infant_model < 0.8] <- NA

summary(stepwise_infant_model)

infant_block <- tidy(stepwise_infant_model) %>%
  mutate(fdr_pvalue = p.adjust(p.value, method = "fdr")) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(stepwise_infant_model)$standardized.coefficients))


# Healthcare provision ----------------------------------------------------

healthprov <- infant_mortality_outcome_vars_long %>% 
  filter(variable_new %in% unique((variable_themes %>% 
                                     filter(theme == "healthcare provision"
                                            | (theme == "socioeconomic" 
                                               & suggested == "healthcare spending")))$variable)) %>%
  dplyr::select(variable, variable_new) %>%
  distinct(variable, variable_new)

healthprov_simple_models <- healthprov %>% 
  mutate(
    dataset  = map(variable, function(var) infant_mortality_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) MASS::glm.nb(u1mortality_nt_17 ~ value + offset(log(denominator))
                                                        , dset))
    , observations       = map_int(model, nobs)
    , var_coeff        = map_chr(model, function(x) tidy(x)$estimate[2])
    , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
    , null_deviance    = map_chr(model, function(x) glance(x)$null.deviance)
    , deviance         = map_chr(model, function(x) glance(x)$deviance)
    , loglik           = map_chr(model, function(x) glance(x)$logLik)
    , aic              = map_chr(model, function(x) glance(x)$AIC)
    , bic              = map_chr(model, function(x) glance(x)$BIC)
  ) %>%
  dplyr::select(-model, -dataset) %>%
  filter(observations == 150) %>%
  group_by(variable_new) %>%
  top_n(aic, n = -1) 

healthprov_model <- MASS::glm.nb(u1mortality_nt_17 ~ . -utla17cd -denominator + offset(log(denominator))
                                 , data = infant_mortality_outcome_vars %>% dplyr::select(u1mortality_nt_17, utla17cd
                                                                                   , denominator
                                                                                   , NBVs_phe_1718
                                                                                   , review_6_8week_mean_1718
                                                                                   , review_12mon_median_1719
                                                                                   , sp_ph_mhclg_pl_1516_1819 
                                                                                   #, sp_chnpres_mhclg_pool_1516_1819
                                                                                   #, sp_chpres_mhclg_pl_1516_1819
                                                                                   ))

stepwise_healthprov_model <- MASS::stepAIC(healthprov_model, trace = FALSE, direction = "both")


vif_stepwise_healthprov_model <- as_tibble(mctest::imcdiag(infant_mortality_outcome_vars %>% dplyr::select(attr(terms(stepwise_healthprov_model), "term.labels"))
                                                       , infant_mortality_outcome_vars$u1mortality_nt_17)$idiags
                                       , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_stepwise_healthprov_model <- as_tibble(cor(infant_mortality_outcome_vars %>% dplyr::select(attr(terms(stepwise_healthprov_model), "term.labels")))
                                           , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_stepwise_healthprov_model[pearson_stepwise_healthprov_model > -0.8 & pearson_stepwise_healthprov_model < 0.8] <- NA

summary(stepwise_healthprov_model)

healthprov_block <- tidy(stepwise_healthprov_model) %>%
  mutate(fdr_pvalue = p.adjust(p.value, method = "fdr")) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(stepwise_healthprov_model)$standardized.coefficients))


# Parental behaviours -----------------------------------------------------

parents <- infant_mortality_outcome_vars_long %>% 
  filter(variable_new %in% unique((variable_themes %>% 
                                     filter(theme == "parental behaviours"))$variable)) %>%
  dplyr::select(variable, variable_new) %>%
  distinct(variable, variable_new)

parents_simple_models <- parents %>% 
  mutate(
    dataset  = map(variable, function(var) infant_mortality_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) MASS::glm.nb(u1mortality_nt_17 ~ value + offset(log(denominator))
                                                        , dset))
    , observations       = map_int(model, nobs)
    , var_coeff        = map_chr(model, function(x) tidy(x)$estimate[2])
    , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
    , null_deviance    = map_chr(model, function(x) glance(x)$null.deviance)
    , deviance         = map_chr(model, function(x) glance(x)$deviance)
    , loglik           = map_chr(model, function(x) glance(x)$logLik)
    , aic              = map_chr(model, function(x) glance(x)$AIC)
    , bic              = map_chr(model, function(x) glance(x)$BIC)
  ) %>%
  dplyr::select(-model, -dataset) %>%
  filter(observations == 150) %>%
  group_by(variable_new) %>%
  top_n(aic, n = -1)

parents_model <- MASS::glm.nb(u1mortality_nt_17
                              ~ . -utla17cd -denominator + offset(log(denominator))
                              , data = infant_mortality_outcome_vars %>% dplyr::select(u1mortality_nt_17, utla17cd
                                                                                , denominator
                                                                                , bf_calc_1819
                                                                                , PCV_phe_1718
                                                                                , Combivacc_phe_1718
                                                                                , MenB_phe_imp_1718
                                                                                ))

stepwise_parents_model <- MASS::stepAIC(parents_model, trace = FALSE, direction = "both")

vif_stepwise_parents_model <- as_tibble(mctest::imcdiag(infant_mortality_outcome_vars %>% dplyr::select(attr(terms(stepwise_parents_model), "term.labels"))
                                                           , infant_mortality_outcome_vars$u1mortality_nt_17)$idiags
                                           , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_stepwise_parents_model <- as_tibble(cor(infant_mortality_outcome_vars %>% dplyr::select(attr(terms(stepwise_parents_model), "term.labels")))
                                               , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_stepwise_parents_model[pearson_stepwise_parents_model > -0.8 & pearson_stepwise_parents_model < 0.8] <- NA

summary(stepwise_parents_model)
summary(parents_model)

parents_block <- tidy(parents_model) %>%
  mutate(fdr_pvalue = p.adjust(p.value, method = "fdr")) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(parents_model)$standardized.coefficients))

# nothing left after stepwise 


# Immediate environmental factors -----------------------------------------

environment <- infant_mortality_outcome_vars_long %>% 
  filter(variable_new %in% unique((variable_themes %>% 
                                     filter(theme == "immediate environmental factors"))$variable)) %>%
  dplyr::select(variable, variable_new) %>%
  distinct(variable, variable_new)

environment_simple_models <- environment %>% 

    mutate(
      dataset  = map(variable, function(var) infant_mortality_outcome_vars_long %>% 
                       filter(variable == var) %>% 
                       mutate(observation = row_number()))
      , model  = map(dataset, function(dset) MASS::glm.nb(u1mortality_nt_17 ~ value + offset(log(denominator))
                                                          , dset))
      , observations       = map_int(model, nobs)
      , var_coeff        = map_chr(model, function(x) tidy(x)$estimate[2])
      , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
      , null_deviance    = map_chr(model, function(x) glance(x)$null.deviance)
      , deviance         = map_chr(model, function(x) glance(x)$deviance)
      , loglik           = map_chr(model, function(x) glance(x)$logLik)
      , aic              = map_chr(model, function(x) glance(x)$AIC)
      , bic              = map_chr(model, function(x) glance(x)$BIC)
    ) %>%
      dplyr::select(-model, -dataset) %>%
      filter(observations == 150) %>%
      group_by(variable_new) %>%
      top_n(aic, n = -1) 

environment_model <- MASS::glm.nb(u1mortality_nt_17 ~ . -utla17cd -denominator + offset(log(denominator)) 
                                  , data = infant_mortality_outcome_vars %>% select(u1mortality_nt_17, utla17cd, denominator
                                                                                         , prop_u5_hse_heat_imd_15
                                                                                         , prop_u5_hse_afford_imd_15
                                                                                         , prop_u5_hse_cond_imd_15
                                                                                         , prop_u5_hse_crowd_imd_15
                                                                                         , av_air_imd_15
                                                                                         , dwelling_pp_voa_17
                                                                                    ))

stepwise_environment_model <- MASS::stepAIC(environment_model, trace = FALSE, direction = "both")

vif_stepwise_environment_model <- as_tibble(mctest::imcdiag(infant_mortality_outcome_vars %>% dplyr::select(attr(terms(stepwise_environment_model), "term.labels"))
                                                       , infant_mortality_outcome_vars$u1mortality_nt_17)$idiags
                                       , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_stepwise_environment_model <- as_tibble(cor(infant_mortality_outcome_vars %>% dplyr::select(attr(terms(stepwise_environment_model), "term.labels")))
                                           , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_stepwise_environment_model[pearson_stepwise_environment_model > -0.8 & pearson_stepwise_environment_model < 0.8] <- NA

summary(stepwise_environment_model)

stepwise_environment_modela <- MASS::glm.nb(u1mortality_nt_17 ~ . -utla17cd -denominator + offset(log(denominator)) 
                                            , data = infant_mortality_outcome_vars %>% dplyr::select(u1mortality_nt_17, utla17cd, denominator
                                                                                              , attr(terms(stepwise_environment_model), "term.labels")
                                                                                              , -prop_u5_hse_afford_imd_15))

stepwise_environment_modelb <- MASS::glm.nb(u1mortality_nt_17 ~ . -utla17cd -denominator + offset(log(denominator)) 
                                            , data = infant_mortality_outcome_vars %>% dplyr::select(u1mortality_nt_17, utla17cd, denominator
                                                                                              , attr(terms(stepwise_environment_model), "term.labels")
                                                                                              , -prop_u5_hse_crowd_imd_15))

glance(stepwise_environment_modela)
glance(stepwise_environment_modelb)

summary(stepwise_environment_modela)
summary(stepwise_environment_modelb)

environment_block <- tidy(stepwise_environment_modelb) %>%
  mutate(fdr_pvalue = p.adjust(p.value, method = "fdr")) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(stepwise_environment_modelb)$standardized.coefficients))

# keep modelb because already have a crowding variable in dwelling


# Local authority factors -------------------------------------------------

la <- infant_mortality_outcome_vars_long %>% 
  filter(variable_new %in% unique((variable_themes %>% 
                                     filter(theme == "local authority factors"))$variable)) %>%
  dplyr::select(variable, variable_new) %>%
  distinct(variable, variable_new)

la_simple_models <- la %>% 
  mutate(
    dataset  = map(variable, function(var) infant_mortality_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) MASS::glm.nb(u1mortality_nt_17 ~ value + offset(log(denominator))
                                                        , dset))
    , observations       = map_int(model, nobs)
    , var_coeff        = map_chr(model, function(x) tidy(x)$estimate[2])
    , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
    , null_deviance    = map_chr(model, function(x) glance(x)$null.deviance)
    , deviance         = map_chr(model, function(x) glance(x)$deviance)
    , loglik           = map_chr(model, function(x) glance(x)$logLik)
    , aic              = map_chr(model, function(x) glance(x)$AIC)
    , bic              = map_chr(model, function(x) glance(x)$BIC)
  ) %>%
  dplyr::select(-model, -dataset) %>%
  filter(observations == 150) %>%
  group_by(variable_new) %>%
  top_n(aic, n = -1) 

la_model <- MASS::glm.nb(u1mortality_nt_17 ~
                           . -utla17cd -denominator
                         + offset(log(denominator)) , data = infant_mortality_outcome_vars %>% dplyr::select(u1mortality_nt_17, utla17cd,
                                                                                                      denominator
                                                                                                      , spu15_fam_mhclg_pool_1516_1819
                                                                                                      , sp_ey_esfa_1718
                                                                                                      , sp_subst_mhclg_pool_1617_1819 
                                                                                                      ))

stepwise_la_model <- MASS::stepAIC(la_model, trace = FALSE, direction = "both")

vif_stepwise_la_model <- as_tibble(mctest::imcdiag(infant_mortality_outcome_vars %>% dplyr::select(attr(terms(stepwise_la_model), "term.labels"))
                                                            , infant_mortality_outcome_vars$u1mortality_nt_17)$idiags
                                            , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_stepwise_la_model <- as_tibble(cor(infant_mortality_outcome_vars %>% dplyr::select(attr(terms(stepwise_la_model), "term.labels")))
                                                , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_stepwise_la_model[pearson_stepwise_la_model > -0.8 & pearson_stepwise_la_model < 0.8] <- NA

summary(stepwise_la_model)

la_block <- tidy(stepwise_la_model) %>%
  mutate(fdr_pvalue = p.adjust(p.value, method = "fdr")) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(stepwise_la_model)$standardized.coefficients))

# check what the spend variables are made of up - early years, sure start and family services are types of spending that make up total csc spending so have removed that


# Wider societal factors --------------------------------------------------

societal <- infant_mortality_outcome_vars_long %>% 
  filter(variable_new %in% unique((variable_themes %>% 
                                     filter(theme == "wider societal"))$variable)) %>%
  dplyr::select(variable, variable_new) %>%
  distinct(variable, variable_new)

societal_simple_models <- societal %>% 
  mutate(
    dataset  = map(variable, function(var) infant_mortality_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) MASS::glm.nb(u1mortality_nt_17 ~ value + offset(log(denominator))
                                                        , dset))
    , observations       = map_int(model, nobs)
    , var_coeff        = map_chr(model, function(x) tidy(x)$estimate[2])
    , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
    , null_deviance    = map_chr(model, function(x) glance(x)$null.deviance)
    , deviance         = map_chr(model, function(x) glance(x)$deviance)
    , loglik           = map_chr(model, function(x) glance(x)$logLik)
    , aic              = map_chr(model, function(x) glance(x)$AIC)
    , bic              = map_chr(model, function(x) glance(x)$BIC)
  ) %>%
  dplyr::select(-model, -dataset) %>%
  filter(observations == 150) %>%
  group_by(variable_new) %>%
  top_n(aic, n = -1) 

societal_model <- MASS::glm.nb(u1mortality_nt_17 ~
                           . -utla17cd -denominator
                         + offset(log(denominator)) , data = infant_mortality_outcome_vars %>% dplyr::select(u1mortality_nt_17, utla17cd, denominator 
                                                                                                      , prop_u5_inc_imd_15
                                                                                                      , prop_emp_imd_15
                                                                                                      ))

stepwise_societal_model <- MASS::stepAIC(societal_model, trace = FALSE, direction = "both")

vif_stepwise_societal_model <- as_tibble(mctest::imcdiag(infant_mortality_outcome_vars %>% dplyr::select(attr(terms(stepwise_societal_model), "term.labels"))
                                                   , infant_mortality_outcome_vars$u1mortality_nt_17)$idiags
                                   , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_stepwise_societal_model <- as_tibble(cor(infant_mortality_outcome_vars %>% dplyr::select(attr(terms(stepwise_societal_model), "term.labels")))
                                       , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_stepwise_societal_model[pearson_stepwise_societal_model > -0.8 & pearson_stepwise_societal_model < 0.8] <- NA

summary(stepwise_societal_model)

societal_block <- tidy(stepwise_societal_model) %>%
  mutate(fdr_pvalue = p.adjust(p.value, method = "fdr")) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(stepwise_societal_model)$standardized.coefficients))


# Socioeconomic / demographic ---------------------------------------------

# Exclude any of the socioeconomic/demographic variables that have now been absorbed into the existing outcome variable themes
# Create a block to adjust full models with
# Create a refined block using stepwise regression and checking for multicollinearity

socioeconomic <- infant_mortality_outcome_vars_long %>% 
  filter(variable_new %in% unique((variable_themes %>% 
                                     filter(theme == "socioeconomic"))$variable)) %>%
  dplyr::select(variable, variable_new) %>%
  distinct(variable, variable_new)

socioeconomic_simple_models <- socioeconomic %>% 
  mutate(
    dataset  = map(variable, function(var) infant_mortality_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) MASS::glm.nb(u1mortality_nt_17 ~ value + offset(log(denominator))
                                                        , dset))
    , observations       = map_int(model, nobs)
    , var_coeff       = map_chr(model, function(x) tidy(x)$estimate[2])
    , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
    , null_deviance    = map_chr(model, function(x) glance(x)$null.deviance)
    , deviance         = map_chr(model, function(x) glance(x)$deviance)
    , loglik           = map_chr(model, function(x) glance(x)$logLik)
    , aic              = map_chr(model, function(x) glance(x)$AIC)
    , bic              = map_chr(model, function(x) glance(x)$BIC)
  ) %>%
  dplyr::select(-model, -dataset) %>%
  filter(observations == 150) %>%
  group_by(variable_new) %>%
  top_n(aic, n = -1) 

#################################################################

# Create a multiple model
# Use the suggested variables attached to the themes to ensure only keeping one type of each type of socioeconomic/demographic variable
# If the year in the simple models output doesn't look appropriate then switch to appropriate year
# This block can be used to adjust other models although multicollinearity will need checking

socioeconomic_model <- MASS::glm.nb(u1mortality_nt_17 ~ . -utla17cd -denominator + offset(log(denominator)), 
                                    data = infant_mortality_outcome_vars %>% dplyr::select(u1mortality_nt_17, utla17cd, denominator
                                                                                    , prop_hea_imd_15
                                                                                    , av_cri_imd_15
                                                                                    , av_rd_store_imd_15
                                                                                    , prop_u5_accidents_imd_15
                                                                                    , prop_adult_skills_imd_15
                                                                                    , prop_edu_cyp_imd_15
                                                                                    ))

# Use stepwise regression to refine the list of variables

stepwise_socioeconomic_model <- MASS::stepAIC(socioeconomic_model, trace = FALSE, direction = "both")

# Check for multicollinearity (both VIF > 5 and pearson > 0.8)

vif_stepwise_socioeconomic <- as_tibble(mctest::imcdiag(infant_mortality_outcome_vars %>% dplyr::select(attr(terms(stepwise_socioeconomic_model), "term.labels"))
                                                        , infant_mortality_outcome_vars$u1mortality_nt_17)$idiags
                                        , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_stepwise_socioeconomic <- as_tibble(cor(infant_mortality_outcome_vars %>% dplyr::select(attr(terms(stepwise_socioeconomic_model), "term.labels")))
                                            , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_stepwise_socioeconomic[pearson_stepwise_socioeconomic > -0.8 & pearson_stepwise_socioeconomic < 0.8] <- NA

summary(stepwise_socioeconomic_model)

socioeconomic_block <- tidy(stepwise_socioeconomic_model) %>%
  mutate(fdr_pvalue = p.adjust(p.value, method = "fdr")) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(stepwise_socioeconomic_model)$standardized.coefficients))


# Block adjusted R-squared ------------------------------------------------

block_adj_rsq <- tibble(block = c("maternal", "infant", "healthprov", "environment", "la", "societal", "socioeconomic")
                  , adj_rsq = c(rsq::rsq(stepwise_maternal_model, adj = TRUE, type = "kl"),
                                rsq::rsq(stepwise_infant_model, adj = TRUE, type = "kl"),
                                rsq::rsq(stepwise_healthprov_model, adj = TRUE, type = "kl"),
                                rsq::rsq(stepwise_environment_modelb, adj = TRUE, type = "kl"),
                                rsq::rsq(stepwise_la_model, adj = TRUE, type = "kl"),
                                rsq::rsq(stepwise_societal_model, adj = TRUE, type = "kl"),
                                rsq::rsq(stepwise_socioeconomic_model, adj = TRUE, type = "kl")
                                ))


# Models with all themes included -----------------------------------------

# Adjust final blocks for socioeconomic/demographics using the refined socioeconomic block 
# Parents excluded as no variables left

model_all3 <- MASS::glm.nb(u1mortality_nt_17 ~ . -utla17cd -denominator + offset(log(denominator)), infant_mortality_outcome_vars %>% dplyr::select(u1mortality_nt_17, utla17cd, denominator
                                                                                , attr(terms(stepwise_maternal_model), "term.labels")
                                                                                , attr(terms(stepwise_infant_model), "term.labels")
                                                                                , attr(terms(stepwise_healthprov_model), "term.labels")
                                                                                , attr(terms(stepwise_environment_modelb), "term.labels")
                                                                                , attr(terms(stepwise_la_model), "term.labels")
                                                                                , attr(terms(stepwise_societal_model), "term.labels")
                                                                                , attr(terms(stepwise_socioeconomic_model), "term.labels")
                                                                                ))

stepwise_model_all3 <- MASS::stepAIC(model_all3, trace = FALSE, direction = "both")

vif_stepwise_model_all3 <- as_tibble(mctest::imcdiag(infant_mortality_outcome_vars %>% dplyr::select(attr(terms(stepwise_model_all3), "term.labels"))
                                                     , infant_mortality_outcome_vars$u1mortality_nt_17)$idiags
                                     , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_stepwise_model_all3 <- as_tibble(cor(infant_mortality_outcome_vars %>% dplyr::select(attr(terms(stepwise_model_all3), "term.labels")))
                                         , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_stepwise_model_all3[pearson_stepwise_model_all3 > -0.8 & pearson_stepwise_model_all3 < 0.8] <- NA

summary(stepwise_model_all3)

final_model_all3 <- tidy(stepwise_model_all3) %>%
  mutate(fdr_pvalue = p.adjust(p.value, method = "fdr")) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(stepwise_model_all3)$standardized.coefficients))

final_fit_all3 <- glance(stepwise_model_all3) %>%
  bind_cols(adj_rsq = rsq::rsq(stepwise_model_all3, adj = TRUE, type = "kl"))

############################

# Relative importance - Manual method

rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ asianpdel_imp_16 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl")
rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ prop_u5_hse_heat_imd_15 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl")
rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ sp_ph_mhclg_pl_1516_1819 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl")
rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ asianidel_imp_16 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl")
rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ dwelling_pp_voa_17 + offset(log(denominator)),  infant_mortality_outcome_vars), adj = FALSE, type = "kl")
rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ sevobese_se_new_1617 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl")
rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ prop_u5_hse_afford_imd_15 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl")
rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ sp_ey_esfa_1718 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl")

final_relimp_all3 <- tibble(asianpdel_imp_16 = rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ asianpdel_imp_16 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl")
                            , prop_u5_hse_heat_imd_15 = rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ asianpdel_imp_16 + prop_u5_hse_heat_imd_15 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl")
                                                      - rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ asianpdel_imp_16 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl")
                            , sp_ph_mhclg_pl_1516_1819 = rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ asianpdel_imp_16 + prop_u5_hse_heat_imd_15 + sp_ph_mhclg_pl_1516_1819 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl") 
                                                       - rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ asianpdel_imp_16 + prop_u5_hse_heat_imd_15 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl")
                            , asianidel_imp_16 = rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ asianpdel_imp_16 + prop_u5_hse_heat_imd_15 + sp_ph_mhclg_pl_1516_1819 + asianidel_imp_16 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl") 
                                               - rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ asianpdel_imp_16 + prop_u5_hse_heat_imd_15 + sp_ph_mhclg_pl_1516_1819 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl")
                            , dwelling_pp_voa_17 = rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ asianpdel_imp_16 + prop_u5_hse_heat_imd_15 + sp_ph_mhclg_pl_1516_1819 + asianidel_imp_16 + dwelling_pp_voa_17 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl") 
                                                 - rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ asianpdel_imp_16 + prop_u5_hse_heat_imd_15 + sp_ph_mhclg_pl_1516_1819 + asianidel_imp_16 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl")
                            , sevobese_se_new_1617 = rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ asianpdel_imp_16 + prop_u5_hse_heat_imd_15 + sp_ph_mhclg_pl_1516_1819 + asianidel_imp_16 + dwelling_pp_voa_17 + sevobese_se_new_1617 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl") 
                                                   - rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ asianpdel_imp_16 + prop_u5_hse_heat_imd_15 + sp_ph_mhclg_pl_1516_1819 + asianidel_imp_16 + dwelling_pp_voa_17 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl")
                            , prop_u5_hse_afford_imd_15 = rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ asianpdel_imp_16 + prop_u5_hse_heat_imd_15 + sp_ph_mhclg_pl_1516_1819 + asianidel_imp_16 + dwelling_pp_voa_17 + sevobese_se_new_1617 + prop_u5_hse_afford_imd_15 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl") 
                                                        - rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ asianpdel_imp_16 + prop_u5_hse_heat_imd_15 + sp_ph_mhclg_pl_1516_1819 + asianidel_imp_16 + dwelling_pp_voa_17 + sevobese_se_new_1617 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl")
                            , sp_ey_esfa_1718 = rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ asianpdel_imp_16 + prop_u5_hse_heat_imd_15 + sp_ph_mhclg_pl_1516_1819 + asianidel_imp_16 + dwelling_pp_voa_17 + sevobese_se_new_1617 + prop_u5_hse_afford_imd_15 + sp_ey_esfa_1718 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl") 
                                              - rsq::rsq(MASS::glm.nb(u1mortality_nt_17 ~ asianpdel_imp_16 + prop_u5_hse_heat_imd_15 + sp_ph_mhclg_pl_1516_1819 + asianidel_imp_16 + dwelling_pp_voa_17 + sevobese_se_new_1617 + prop_u5_hse_afford_imd_15 + offset(log(denominator)), infant_mortality_outcome_vars), adj = FALSE, type = "kl")
                            ) %>%
  pivot_longer(everything(), names_to = "model", values_to = "added_r_squared") %>%
  mutate(r_squared = sum(added_r_squared)
         , prop_r_squared = added_r_squared / r_squared)

############################

# Top 5 areas where actual value is better than predicted

final_all3_best <- augment(stepwise_model_all3) %>%
  top_n(-5, .resid)

#################################################################

# Check for influential points

stepwise_model_all3_influen <- MASS::glm.nb(u1mortality_nt_17 ~ . -utla17cd -denominator + offset(log(denominator)), data = infant_mortality_outcome_vars %>% 
                                              mutate(observation = row_number()) %>%
                                              filter(observation %in% unique(augment(stepwise_model_all3) %>%
                                                                               mutate(mean_cook_threshold = 4*mean(.cooksd)
                                                                                      , observation = row_number()) %>%
                                                                               filter(.cooksd < mean_cook_threshold))$observation) %>%
                                              dplyr::select(u1mortality_nt_17, utla17cd, denominator, attr(terms(stepwise_model_all3), "term.labels")))

vif_stepwise_model_all3_influen <- as_tibble(mctest::imcdiag(infant_mortality_outcome_vars %>% dplyr::select(attr(terms(stepwise_model_all3_influen), "term.labels"))
                                                       , infant_mortality_outcome_vars$u1mortality_nt_17)$idiags
                                       , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_stepwise_model_all3_influen <- as_tibble(cor(infant_mortality_outcome_vars %>% dplyr::select(attr(terms(stepwise_model_all3_influen), "term.labels")))
                                           , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_stepwise_model_all3_influen[pearson_stepwise_model_all3_influen > -0.8 & pearson_stepwise_model_all3_influen < 0.8] <- NA

summary(stepwise_model_all3_influen)

final_model_all3_influen <- tidy(stepwise_model_all3_influen) %>%
  mutate(fdr_pvalue = p.adjust(p.value, method = "fdr")) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(stepwise_model_all3_influen)$standardized.coefficients))

final_fit_all3_influen <- glance(stepwise_model_all3_influen)%>%
  bind_cols(adj_rsq = rsq::rsq(stepwise_model_all3_influen, adj = TRUE, type = "kl"))


# Save full model results -------------------------------------------------

write_xlsx(list(maternal_block = maternal_block, infant_block = infant_block
                , parents_block = parents_block, environment_block = environment_block
                , la_block = la_block, healthprov_block = healthprov_block
                , societal_block = societal_block, socioeconomic_block = socioeconomic_block
                , block_adj_rsq = block_adj_rsq
                , model3 = final_model_all3, model3_fit = final_fit_all3, model3_best = final_all3_best
                , model3_relimp = final_relimp_all3, model3_influen = final_model_all3_influen
                , model3_fit_influen = final_fit_all3_influen)
           ,"output/multiple_glm_regression/Blocks_full_models_im_UTLA.xlsx", col_names=TRUE)


# Summary information for final model variables ---------------------------

final_vars <- infant_mortality_outcome_vars_long %>%
  filter(variable %in% attr(terms(stepwise_model_all3), "term.labels")) %>%
  dplyr::select(utla17cd, variable, value) %>%
  bind_rows(infant_mortality_outcome %>%
              rename(value = u1mortality_nt_17) %>%
              mutate(variable = "u1mortality_nt_17") %>%
              dplyr::select(utla17cd, value, variable)) %>%
  group_by(variable) %>%
  mutate(q20 = quantile(value, c(.2))
         , q80 = quantile(value, c(.8))
         , quintile1 = case_when(value < q20 ~ value
                                 , TRUE ~ NA_real_)
         , quintile5 = case_when(value >= q80 ~ value
                                 , TRUE ~ NA_real_))

infant_mortality_vars_desc <- final_vars %>%
  group_by(variable) %>%
  summarise(min = min(value)
            , q25 = quantile(value, c(.25))
            , q40 = quantile(value, c(.40))
            , median = quantile(value, c(.50))
            , q60 = quantile(value, c(.60))
            , q75 = quantile(value, c(.75))
            , max = max(value)
            , mean = mean(value)
            , sd = sd(value)
            , mean_q1 = mean(quintile1, na.rm = TRUE)
            , mean_q5 = mean(quintile5, na.rm = TRUE))

write_csv(infant_mortality_vars_desc, "output/multiple_glm_regression/Blocks_final_vars_desc.csv")

