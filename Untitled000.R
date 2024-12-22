# Incorporating covariates without survcey weights

formulas <- list(
  formula1 = health ~ age,
  formula2 = health ~ age + happy,
  formula2 = health ~ age + satfin + educ,
  formula3 = health ~ age + satfin + happy,
  formula4 = health ~ age + happy + satfin + educ
)

# Get a vector of unique years
years <- sort(unique(data_gss$year))

# Create a data frame of all combinations
formula_df <- expand.grid(formula_name = names(formulas), year = years, stringsAsFactors = FALSE)

# Run regressions and extract results
results <- formula_df %>%
  mutate(
    formula = formulas[formula_name],
    data = map(year, ~ filter(data_gss, year == .x)),
    model = map2(formula, data, ~ lm(.x, data = .y)),
    tidy = map(model, tidy),
    glance = map(model, glance)
  )

# Extract coefficients
coefficients_df <- results %>%
  unnest(tidy)

# Extract model summary statistics
model_stats_df <- results %>%
  unnest(glance)

# Focus on age coefficient
age_coefficients <- coefficients_df %>%
  filter(term == "age") %>%
  select(formula_name, year, estimate, std.error, statistic, p.value)

# Regress Age Coefficient Over Time
#For each formula, regress the age coefficient over time to analyze the trend:

age_coeff_trend <- age_coefficients %>%
  group_by(formula_name) %>%
  nest() %>%
  mutate(
    trend_model = map(data, ~ lm(estimate ~ year, data = .x)),
    trend_tidy = map(trend_model, tidy),
    trend_glance = map(trend_model, glance)
  )

# Extract trend summary statistics
trend_summary <- age_coeff_trend %>%
  unnest(trend_glance) %>%
  select(formula_name, r.squared, adj.r.squared, p.value, statistic)

# Merge age coefficients with trend summary
final_results <- age_coefficients %>%
  left_join(trend_summary, by = "formula_name") %>%
  arrange(formula_name, year)

# Summary table
# Assuming 'age_coeff_trend' is from the previous analysis

# Extract trend statistics for each formula
trend_stats <- age_coeff_trend %>%
  mutate(
    trend_summary = map(trend_model, ~ {
      model <- .x
      summary <- summary(model)
      slope <- coef(summary)["year", "Estimate"]
      std_error <- coef(summary)["year", "Std. Error"]
      p_value <- coef(summary)["year", "Pr(>|t|)"]
      conf_int <- confint(model)["year", ]
      r_squared <- summary$r.squared
      
      tibble(
        slope = slope,
        std_error = std_error,
        ci_lower = conf_int[1],
        ci_upper = conf_int[2],
        p_value = p_value,
        r_squared = r_squared
      )
    })
  ) %>%
  unnest(trend_summary) %>%
  select(formula_name, slope, std_error, ci_lower, ci_upper, p_value, r_squared)

trend_stats

ggplot(age_coefficients, aes(x = year, y = estimate, color = formula_name)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Trend of Age Coefficient Over Time",
    x = "Year",
    y = "Age Coefficient",
    color = "Formula"
  ) +
  theme_minimal()

