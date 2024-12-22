library(dplyr)
library(tidyr)

data0 <- new_data

data_named <- data0
colnames(data_named) <- varlabels


data_named[1:10, c(9, 10, 140, 141,159, 165) ]
#colnames(data_named) # keep 10, 140, 159, get rid of 9, 141, 165
data_named <- data_named[,-c(9, 141, 165)]
data_named <- data_named %>% 
  janitor::clean_names()

# Step 1: Pivot to a long format, separating the variable name from the year
df_long <- data_named %>%
  pivot_longer(
    cols = -pubid_yth_id_code_1997,  # Keep subject_id as a unique identifier
    names_to = c("variable", "year"),
    names_pattern = "^(.*)_(\\d{4})$",
    values_to = "value"
  ) %>% 
  mutate(id = pubid_yth_id_code_1997) %>% 
  select(-pubid_yth_id_code_1997) 


df_wide <- df_long %>%
  # Step 1: Remove rows with NA in critical columns
  filter(!is.na(value) & !is.na(year) & !is.na(variable) & !is.na(id)) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) 

# Select variables of interest and fix, recode health and emotions variables
df_select <- df_wide %>% 
  select(id, year, how_rs_genl_health, how_rs_general_health, cv_age_int_date, 
         key_sex_symbol, key_bdate_m_y_symbol, key_race_ethnicity_symbol,
         #  sex, race, cohort, 
         cv_urban_rural,
         hrs_wk_r_uses_a_computer, hrs_wk_r_watches_television, number_days_wk_exercise_30_minutes,
         how_r_rates_his_current_life, how_exciting_is_rs_life,
         how_oft_r_been_happy_person, how_oft_r_been_nervous_person, how_oft_r_calm_peaceful_past_mo, how_oft_r_felt_down_or_blue, how_oft_r_depressed_last_month) %>% 
  mutate(health = if_else(year == 1997, how_rs_genl_health, how_rs_general_health)) %>% 
  mutate(health = 6 - health) %>% 
  select(-c("how_rs_genl_health", "how_rs_general_health")) %>% 
  mutate(age = cv_age_int_date) %>% 
  mutate(felt_happy = 5 - how_oft_r_been_happy_person) %>% 
  mutate(felt_nervous = 5 - how_oft_r_been_nervous_person) %>% 
  mutate(felt_calm = 5 - how_oft_r_calm_peaceful_past_mo) %>% 
  mutate(felt_down = 5 - how_oft_r_felt_down_or_blue) %>% 
  mutate(depressed = 5 - how_oft_r_depressed_last_month) %>% 
  mutate(watched_tv = hrs_wk_r_watches_television) %>% 
  mutate(used_computer = hrs_wk_r_uses_a_computer) %>% 
  mutate(exercised = number_days_wk_exercise_30_minutes) %>% 
  mutate(rate_life = how_r_rates_his_current_life) %>% 
  select(c(id, year, age, key_sex_symbol, key_bdate_m_y_symbol, key_race_ethnicity_symbol,
           health, felt_happy, felt_nervous, felt_calm, felt_down, depressed, watched_tv, used_computer, exercised, rate_life))

# Put static variables from 1997 in every year
df_select <- df_select %>% 
  group_by(id) %>%
  mutate(sex = first(key_sex_symbol[year == 1997])) %>%
  mutate(cohort = first(key_bdate_m_y_symbol[year == 1997])) %>%
  mutate(race = first(key_race_ethnicity_symbol[year == 1997])) %>%
  ungroup() %>% 
  select(- c(key_sex_symbol, key_bdate_m_y_symbol, key_race_ethnicity_symbol)) %>% 
  select(id, year, health, sex, cohort, race, everything())

# Select only variables of interest and waves of interest
df_select <- df_select %>% 
  select(c(id, year, health, sex, cohort, race, age, health, 
           felt_happy, felt_nervous, felt_calm, felt_down, depressed, 
           watched_tv, used_computer
           )) %>% 
  na.omit()

# Check attrition
df_select %>%
  group_by(year) %>%
  drop_na(health) %>% 
  summarize(count = n_distinct(id))

last_wave <- df_select %>%
  group_by(id) %>%
  summarize(last_wave_present = max(year))

dropouts <- last_wave %>%
  filter(last_wave_present < max(last_wave_present))

# Fixed-effects model with a  time 
library(plm)
fe_model_trend <- plm(health ~ depressed + felt_happy + as.factor(year), 
                      index = c("id", "year"),
                      data = df_select, 
                      model = "within")

summary(fe_model_trend)

# felt_happy + felt_down + felt_nervous + felt_calm + depressed + age + as.factor(year)

# Compute VIFs to check for multicolinearity
# Pooled OLS regression
pooled_lm <- lm(health ~ watched_tv + felt_happy + as.factor(year), data = df_select)
# Compute VIFs
library(car)
vif_values <- vif(pooled_lm)
print(vif_values)

