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

# Step 2: Pivot to a wide format, creating columns for each variable-year combination
df_wide <- df_long %>%
  filter(!is.na(variable) & !is.na(year)) %>%
  pivot_wider(
    names_from = c(variable),
    values_from = value
  )

duplicates <- df_long %>%
  group_by(year, id, variable) %>%
  filter(n() > 1)
View(duplicates)
df_long_clean <- df_long %>%
  filter(!is.na(variable) & !is.na(year))
duplicates_clean <- df_long_clean %>%
  group_by(year, id, variable) %>%
  filter(n() > 1)


df_long_clean <- df_long %>%
  filter(!is.na(value) & !is.na(year) & !is.na(variable))


# Pivot to wide format
# df_wide <- df_long_clean %>%
#   pivot_wider(
#     names_from = variable,
#     values_from = value
#   )

duplicates <- df_long_clean %>%
  group_by(year, id, variable) %>%
  filter(n() > 1)
View(duplicates)

df_long_clean <- df_long %>%
  filter(!is.na(value) & !is.na(year) & !is.na(variable) & !is.na(id)) %>%
  filter(year != "" & variable != "" & id != "")
df_wide <- df_long_clean %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  )

df_wide <- df_long_clean %>%
  pivot_wider(
    names_from = variable,
    values_from = value,
    values_fn = list(value = ~ .[1])  # Forces each cell to be a single element
  )


# Flatten any list columns if they exist
df_wide <- df_wide %>%
  unnest(cols = everything())
str(df_wide)

# Put static variables from 1997 in every year
df_wide <- df_wide %>% 
  group_by(id) %>%
  mutate(sex = first(key_sex_symbol[year == 1997])) %>%
  mutate(cohort = first(key_bdate_m_y_symbol[year == 1997])) %>%
  mutate(race = first(key_race_ethnicity_symbol[year == 1997])) %>%
  ungroup()

# Select variables of interest and fix, recode health variable
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
  select(-c("how_rs_genl_health", "how_rs_general_health")) 

# Put static variables from 1997 in every year
df_select <- df_select %>% 
  group_by(id) %>%
  mutate(sex = first(key_sex_symbol[year == 1997])) %>%
  mutate(cohort = first(key_bdate_m_y_symbol[year == 1997])) %>%
  mutate(race = first(key_race_ethnicity_symbol[year == 1997])) %>%
  ungroup() %>% 
  select(- c(key_sex_symbol, key_bdate_m_y_symbol, key_race_ethnicity_symbol)) %>% 
  select(id, year, health, sex, cohort, race, everything())

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



