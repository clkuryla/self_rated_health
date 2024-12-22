# Create cohort groups (e.g., decades)
data_gss <- data_gss %>%
  mutate(cohort_group = cut(cohort, breaks = seq(1900, 2010, by = 10), right = FALSE,
                            labels = paste(seq(1900, 2000, by = 10), seq(1909, 2009, by = 10), sep = "-")))

# Update survey design object
gss_svy <- data_gss %>%
  as_survey_design(ids = 1, weights = wtsscomp)

# Regression including age and cohort_group
weighted_lm_cohort <- gss_svy %>%
  svyglm(health ~ age + cohort_group, design = .)

# Summarize the model
summary(weighted_lm_cohort)


# Prepare data for the plot
coef_data <- broom::tidy(weighted_lm_cohort) %>%
  filter(term != "(Intercept)") %>%
  mutate(significant = ifelse(p.value < 0.05, "Significant", "Not Significant"))

# Plot
library(ggplot2)
ggplot(coef_data, aes(x = term, y = estimate, fill = significant)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  labs(
    title = "Cohort Effects on Self-Rated Health (SRH)",
    x = "Cohort Group",
    y = "Coefficient Estimate",
    fill = "Significance"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# "Positive coefficients indicate better SRH compared to the 1900-1909 cohort (reference group), adjusting for age."

# "SRH improved significantly for cohorts born between 1920-1979, peaking in 1940-1949."

#  "Recent cohorts (1980-2009) show no significant change."
