

library(dplyr)
library(purrr)
library(survey)
library(broom)

weighted_lm_by_cohort <- gss_svy %>%
  filter(cohort >= 1900 & cohort <= 1996) %>% 
  mutate(cohort_cut = cut(cohort, breaks = 25)) %>% 
  group_by(cohort_cut) %>%
  group_map_dfr(~ {
    # Create a survey design for the current group
    survey_design <- .x %>%
      as_survey_design(weights = wtsscomp)
    
    # Fit the weighted regression model
    model <- svyglm(health ~ age, design = survey_design)
    
    # Extract model results
    broom::tidy(model, conf.int = TRUE) %>%
      mutate(cohort_cut = unique(.x$cohort_cut)) # Add cohort_cut for identification
  }) %>%
  filter(term == "age") %>%
  select(cohort_cut, estimate, std.error, conf.low, conf.high, statistic, p.value) %>% 
  mutate(cohort = map_dbl(cohort_cut, ~ mean(as.numeric(str_extract_all(.x, "\\d+")[[1]]))))




knitr::kable(weighted_lm_by_cohort)

summary(weighted_lm_by_cohort)

# Plot the coefficients with error bars
weighted_lm_by_cohort %>% 
ggplot(aes(x = cohort, y = estimate)) +
  # geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(0.05)) +
  # geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(
    title = "Coefficient of Age for Different Cohorts",
    subtitle = "GSS Dataset",
    x = "Cohort",
    y = "Coefficient of Age"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Regress the age coefficients on year
coef_model <- lm(estimate ~ cohort, data = weighted_lm_by_cohort)
summary(coef_model)

# Plot the regression
ggplot(weighted_lm_by_cohort, aes(x = cohort, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(0.05)) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.3) +
  labs(
    title = "Regression of 'Age' Coefficient Over Years",
    subtitle = "GSS Dataset",
    x = "Year",
    y = "Coefficient of Age"
  ) +
  theme_minimal()

summary(weighted_lm_by_year)

# Perform linear regression of 'coef' (age coefficient) vs 'year'
lm_coef_vs_year_cohort <- lm(estimate ~ cohort, data = weighted_lm_by_cohort)

# View the summary of the regression
summary(lm_coef_vs_year_cohort)

```


gss_svy_15 %>% 
  mutate(cohort = cohort_15_yr) %>% 
  filter(cohort != "NA") %>% 
  #  mutate(cohort = cut(cohort, breaks = 7)) %>% # Create cohorts with 6 breaks
  group_by(age, cohort) %>% 
  summarize(mean_health = survey_mean(health, na.rm = TRUE)) %>% 
  ggplot(aes(x = age, y = mean_health, color = cohort)) +
  labs(title = "Age Profiles by Cohort") +
  geom_line()
