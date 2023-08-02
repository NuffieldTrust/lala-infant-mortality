#-------------------------------------------------------------------------------
# Load data for project and format

# Load csv files
# Create long data files
#-------------------------------------------------------------------------------


# Load csv files ----------------------------------------------------------

# Load wide dependent and independent variable csv files

# Join outcome specific variables and socioeconomic variables together

infant_mortality_outcome <- read_csv("data/Outcomes_Infant_Mortality_UTLA.csv") 

infant_mortality_vars <- read_csv("data/Infant_Mortality_UTLA.csv") %>%
  left_join(read_csv("data/Socioeconomic_UTLA.csv")
            , by = "utla17cd")

# With exception of local authority variable all should read as numeric in wide files

# Join on unsuppressed data
# This steps adds the HES sourced variables - deliveries by ethnic group
# This step has been redacted for GitHub as we used unsuppressed data

infant_mortality_vars_unsupp <- infant_mortality_vars


# Create long data files --------------------------------------------------

# For outcome and variables

# Create columns: variable, value, year_num, year_char, year_label, variable_new (without year)

infant_mortality_outcome_long <- infant_mortality_outcome %>%
  as.data.frame() %>%
  gather(key = "variable", value = "value", -c(utla17cd)) %>%
  mutate(year1 = str_sub(variable, -3, -1)
         , year2 = case_when(str_detect(str_sub(year1, 1, 1), "_") ~ 1
                             , TRUE ~ 0)
         , year_char = case_when(year2 == 0 ~ paste0(20, str_sub(variable, -4, -3))
                                 , TRUE ~ paste0(20, str_sub(variable, -2, -1)))
         , year_num = as.numeric(year_char)
         , year_label = case_when(year2 == 0 ~  paste0(year_char, "/"
                                                       , str_sub(variable, -2, -1))
                                  , TRUE ~ year_char)
         , variable_new = case_when(year2 == 0 ~ str_sub(variable, end= -5)
                                    , TRUE ~  str_sub(variable, end= -3))) %>%
  dplyr::select(-year1, -year2)


infant_mortality_vars_long <- infant_mortality_vars_unsupp %>%
  as.data.frame() %>%
  gather(key = "variable", value = "value", -c(utla17cd)) %>%
  mutate(year1 = str_sub(variable, -3, -1)
         , year2 = case_when(str_detect(str_sub(year1, 1, 1), "_") ~ 1
                             , TRUE ~ 0)
         , year_char = case_when(year2 == 0 ~ paste0(20, str_sub(variable, -4, -3))
                                 , TRUE ~ paste0(20, str_sub(variable, -2, -1)))
         , year_num = as.numeric(year_char)
         , year_label = case_when(year2 == 0 ~  paste0(year_char, "/", str_sub(variable, -2, -1))
                                  , TRUE ~ year_char)
         , variable_new = case_when(year2 == 0 ~ str_sub(variable, end= -5)
                                    , TRUE ~  str_sub(variable,end= -3))
  ) %>%
  dplyr::select(-year1, -year2)

# Check that 0% NA in all new variables

