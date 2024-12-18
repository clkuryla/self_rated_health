GSS Self-Rated Health as predicted by Age of Respondent
================
Christine Lucille Kuryla
2024-10-02

- [Fetch, load, clean, and recode
  data](#fetch-load-clean-and-recode-data)
  - [Fetch GSS data](#fetch-gss-data)
  - [Load and clean data](#load-and-clean-data)
- [Self rated health as predicted by age over the
  years](#self-rated-health-as-predicted-by-age-over-the-years)
  - [SRH vs year of survey for different
    ages](#srh-vs-year-of-survey-for-different-ages)
  - [Relationship of self-rated health to age, separated out by
    years](#relationship-of-self-rated-health-to-age-separated-out-by-years)
  - [Regress self-rated health on age, for each
    year](#regress-self-rated-health-on-age-for-each-year)
  - [Regress the srh vs age coefficients from each year on the year of
    the
    survey](#regress-the-srh-vs-age-coefficients-from-each-year-on-the-year-of-the-survey)
- [ANOVA](#anova)
- [Cohort Effects](#cohort-effects)
  - [Visualize age profiles by
    cohort](#visualize-age-profiles-by-cohort)
  - [Model srh vs age for each cohort and plot the betas and predicted
    age
    values](#model-srh-vs-age-for-each-cohort-and-plot-the-betas-and-predicted-age-values)
- [Other](#other)
- [More ANOVA – Tukey, QQ-plot, etc](#more-anova--tukey-qq-plot-etc)
- [SD over time](#sd-over-time)
- [APC Analysis](#apc-analysis)

Here’s a summary of the interesting findings from my analysis of
self-rated health in the GSS dataset so far.

See <https://github.com/clkuryla/self_rated_health/blob/main/gss_eda.md>
for more EDA and details.

- `health` The first variable of interest is “health”, which will be the
  main subject of our analysis:
  <https://gssdataexplorer.norc.org/variables/437/vshow>

Question on survey: “Would you say your own health, in general, is
excellent, good, fair, or poor?”

Coded in this analysis (it was recoded from the raw data) as: 4 -
Excellent 3 - Good 2 - Fair 1 - Poor

Other variables used are:

- `age`
  - Age of respondent at time of survey
- `year`
  - Year of survey
- `cohort`
  - Birth year of respondent

Additional covariates and analyses to come.

# Fetch, load, clean, and recode data

## Fetch GSS data

``` r
# Feel free to modify to play with more covariates and variables.

#install.packages('gssr', repos =  c('https://kjhealy.r-universe.dev', 'https://cloud.r-project.org'))
# install.packages('gssrdoc', repos = c('https://kjhealy.r-universe.dev', 'https://cloud.r-project.org'))

library(gssr)
library(gssrdoc)

data("gss_all"). # this file is big! 

# It's a bit excessive to download the entire GSS dataset every time we knit, so lets just save some variables of interest and write it out for future use.

data_gss <- as.data.frame(gss_all) %>% 
  select("year",      # year of survey
         "cohort",    # birthyear
         "age",       # age at time of survey
         "health",    # self-rated health
         "sex",       # sex
         "happy",     # self-rated happiness
         "life",      # is life exciting or dull
         "educ",      # years of education
         "polviews",  # 1 extremely liberal, 4 moderate, 7 extremely conservative
         "class",    # 1 lower, 2 middle, 3 working, 4 upper, 5 no class
         "satfin"    # 1 pretty well satisfied, 2 more or less satisfied, 3 not satisfied at all
         )

write_csv(data_gss, "data/extracted_gss_variables.csv")
```

## Load and clean data

Here we’ll load our data, clean some unwanted values, and recode the
unintuitive variables.

``` r
data_gss <- read_csv("data/extracted_gss_variables.csv") %>% 
  filter(cohort != 9999) %>% 
  na.omit() %>% 
  mutate(health = 5 - health)  %>%  # reverse the coding so it's more intuitive (higher number for excellent, lower number for poor)
  mutate(happy = 4 - happy) %>% # same
  mutate(life = 4 - life) %>% # reverse again, these variables tend to be unintuitively ordered!!!
  mutate(satfin = 4 - satfin) # same again!
```

    ## Rows: 72390 Columns: 17
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (17): year, cohort, age, health, sex, happy, life, educ, polviews, class...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# Self rated health as predicted by age over the years

Let’s explore the effect of different cohorts on SRH at certain ages.

## SRH vs year of survey for different ages

In the following figure, I cut the age of participants into 6 groups and
plotted the mean of the group’s self-rated health for each year (that’s
what each dot is). As you can qualitatively see, the spread seems to
narrow.

``` r
data_gss %>% 
  mutate(age = cut(age, breaks = 6)) %>% # Create cohorts with 6 breaks
  group_by(age, year) %>% 
  summarize(mean_health = mean(health)) %>% 
  ggplot(aes(x = year, y = mean_health, color = age)) +
  geom_line() +
  geom_point() +
  labs(title = "Average SRH Per Year for Each Age Group",
       subtitle = "GSS Dataset",
       x = "Year", 
       y = "Average SRH") 
```

    ## `summarise()` has grouped output by 'age'. You can override using the `.groups`
    ## argument.

![](srh_gss_files/figure-gfm/multiple_cohort_breaks_health-1.png)<!-- -->

This qualitative result is robust to the size of the categorical
variables I split “age” into.

``` r
par(mfrow = c(2, 2))

p1 <- data_gss %>% 
  mutate(age = cut(age, breaks = 10)) %>% # Create cohorts with 6 breaks
  group_by(age, year) %>% 
  summarize(mean_health = mean(health)) %>% 
  ggplot(aes(x = year, y = mean_health, color = age)) +
  labs(title = "SRH for Different Ages over the Years") +
  geom_line()
```

    ## `summarise()` has grouped output by 'age'. You can override using the `.groups`
    ## argument.

``` r
p2 <- data_gss %>% 
  mutate(age = cut(age, breaks = 7)) %>% # Create cohorts with 6 breaks
  group_by(age, year) %>% 
  summarize(mean_health = mean(health)) %>% 
  ggplot(aes(x = year, y = mean_health, color = age)) +
  labs(title = "SRH for Different Ages over the Years") +
  geom_line()
```

    ## `summarise()` has grouped output by 'age'. You can override using the `.groups`
    ## argument.

``` r
p3 <- data_gss %>% 
  mutate(age = cut(age, breaks = 3)) %>% # Create cohorts with 6 breaks
  group_by(age, year) %>% 
  summarize(mean_health = mean(health)) %>% 
  ggplot(aes(x = year, y = mean_health, color = age)) +
  labs(title = "SRH for Different Ages over the Years") +
  geom_line()
```

    ## `summarise()` has grouped output by 'age'. You can override using the `.groups`
    ## argument.

``` r
p4 <- data_gss %>% 
  mutate(age = cut(age, breaks = 4)) %>% # Create cohorts with 6 breaks
  group_by(age, year) %>% 
  summarize(mean_health = mean(health)) %>% 
  ggplot(aes(x = year, y = mean_health, color = age)) +
  labs(title = "SRH for Different Ages over the Years") +
  geom_line()
```

    ## `summarise()` has grouped output by 'age'. You can override using the `.groups`
    ## argument.

``` r
gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2)
```

![](srh_gss_files/figure-gfm/multiple_cohort_breaks_health_2-1.png)<!-- -->

## Relationship of self-rated health to age, separated out by years

Well, it seems like the spread of self-rated health among ages decreases
as time goes on (later years). Let’s look at that by faceting mean
self-rated health vs age by year.

Note that intuitively, we’d expect it to be a negative slope because
older people intuitively should have worse health.

Notice *the slopes seem to flatten over time.*

``` r
# health vs age per year
data_gss %>% 
  group_by(age, year) %>% 
  summarize(mean_health = mean(health)) %>% 
  ggplot(aes(x = age, y = mean_health)) +
  geom_line(color = "cornflowerblue") +
  facet_wrap(~ year) +
  labs(title = "Self-Rated Health By Age (Per Year)",
       subtitle = "GSS Dataset",
       x = "Age of Respondent", 
       y = "Average SRH",
       )
```

    ## `summarise()` has grouped output by 'age'. You can override using the `.groups`
    ## argument.

![](srh_gss_files/figure-gfm/health_v_age_per_year-1.png)<!-- -->

## Regress self-rated health on age, for each year

Let’s do a simple linear regression on each self-rated-health vs age,
subsetted for each year (the plots on the faceted figure), look at the
significance, and plot the coefficients for age with 95% CIs:

``` r
library(broom)

# Aggregate slopes

# years_of_gss <- c(data_gss %>% select(year) %>% unique() )
# lm_health_v_age_0 <- data_gss %>%
#   group_by(year) %>%
#   summarize(coef = coef(lm(health ~ age, data = cur_data()))["age"])

# Perform linear regression for each year and extract the coefficient of 'age' with confidence intervals, se, t stat, p val
lm_health_v_age_0 <- data_gss %>%
  group_by(year) %>%
  do(tidy(lm(health ~ age, data = .), conf.int = TRUE)) %>%  # Add conf.int = TRUE for CIs
  filter(term == "age") %>%
  select(year, coef = estimate, conf.low, conf.high, se = std.error, t_statistic = statistic,  p_value = p.value)

# View the results with confidence intervals, se, t statistic, and p value
# print(lm_health_v_age_0)
knitr::kable(lm_health_v_age_0, 
             caption = "GSS Dataset")
```

| year |       coef |   conf.low |  conf.high |        se | t_statistic |   p_value |
|-----:|-----------:|-----------:|-----------:|----------:|------------:|----------:|
| 1974 | -0.0158654 | -0.0184460 | -0.0132848 | 0.0013155 |  -12.060660 | 0.0000000 |
| 1976 | -0.0152871 | -0.0177023 | -0.0128720 | 0.0012311 |  -12.417048 | 0.0000000 |
| 1977 | -0.0173802 | -0.0199845 | -0.0147759 | 0.0013276 |  -13.091284 | 0.0000000 |
| 1980 | -0.0135810 | -0.0160869 | -0.0110752 | 0.0012774 |  -10.631625 | 0.0000000 |
| 1982 | -0.0141285 | -0.0164127 | -0.0118443 | 0.0011646 |  -12.131803 | 0.0000000 |
| 1984 | -0.0116424 | -0.0139974 | -0.0092873 | 0.0012005 |   -9.697885 | 0.0000000 |
| 1985 | -0.0140029 | -0.0164432 | -0.0115626 | 0.0012440 |  -11.256192 | 0.0000000 |
| 1987 | -0.0146647 | -0.0169564 | -0.0123729 | 0.0011684 |  -12.551162 | 0.0000000 |
| 1988 | -0.0148085 | -0.0177554 | -0.0118617 | 0.0015015 |   -9.862543 | 0.0000000 |
| 1989 | -0.0120553 | -0.0148491 | -0.0092615 | 0.0014236 |   -8.468330 | 0.0000000 |
| 1990 | -0.0119579 | -0.0149275 | -0.0089883 | 0.0015129 |   -7.903794 | 0.0000000 |
| 1991 | -0.0123329 | -0.0151446 | -0.0095212 | 0.0014327 |   -8.608409 | 0.0000000 |
| 1993 | -0.0114162 | -0.0143419 | -0.0084904 | 0.0014909 |   -7.657145 | 0.0000000 |
| 1994 | -0.0110152 | -0.0131700 | -0.0088605 | 0.0010987 |  -10.025841 | 0.0000000 |
| 1996 | -0.0093719 | -0.0115458 | -0.0071980 | 0.0011084 |   -8.455377 | 0.0000000 |
| 1998 | -0.0121266 | -0.0143559 | -0.0098972 | 0.0011367 |  -10.668675 | 0.0000000 |
| 2000 | -0.0110311 | -0.0132073 | -0.0088550 | 0.0011095 |   -9.942676 | 0.0000000 |
| 2002 | -0.0093179 | -0.0124336 | -0.0062022 | 0.0015875 |   -5.869724 | 0.0000000 |
| 2004 | -0.0068007 | -0.0098177 | -0.0037836 | 0.0015372 |   -4.424084 | 0.0000109 |
| 2006 | -0.0095170 | -0.0116356 | -0.0073983 | 0.0010803 |   -8.809931 | 0.0000000 |
| 2008 | -0.0088418 | -0.0113572 | -0.0063265 | 0.0012821 |   -6.896222 | 0.0000000 |
| 2010 | -0.0084057 | -0.0111318 | -0.0056796 | 0.0013895 |   -6.049419 | 0.0000000 |
| 2012 | -0.0089991 | -0.0117175 | -0.0062807 | 0.0013856 |   -6.494753 | 0.0000000 |
| 2014 | -0.0068691 | -0.0092502 | -0.0044880 | 0.0012140 |   -5.658428 | 0.0000000 |
| 2016 | -0.0036989 | -0.0058975 | -0.0015004 | 0.0011210 |   -3.299775 | 0.0009869 |
| 2018 | -0.0032140 | -0.0054825 | -0.0009455 | 0.0011565 |   -2.779134 | 0.0055200 |

GSS Dataset

Note that every single beta is statistically significant. Now let’s
visualize it.

``` r
# Plot coefficients
ggplot(lm_health_v_age_0, aes(x = year, y = coef)) +
  geom_point() +
  labs(
    title = "Change in 'Age' Coefficient Over Years",
    subtitle = "GSS Dataset",
    x = "Year",
    y = "Coefficient of Age"
  ) +
  theme_minimal()
```

![](srh_gss_files/figure-gfm/regress_age_coeff_plot-1.png)<!-- -->

``` r
# Plot coefficients with CI
ggplot(lm_health_v_age_0, aes(x = year, y = coef)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +  # Add shaded area for confidence intervals
  labs(
    title = "Change in 'Age' Coefficient Over Years with Confidence Intervals",
    subtitle = "GSS Dataset",
    x = "Year",
    y = "Coefficient of Age"
  ) +
  theme_minimal()
```

![](srh_gss_files/figure-gfm/regress_age_coeff_plot-2.png)<!-- -->

## Regress the srh vs age coefficients from each year on the year of the survey

The relationship looks surprisingly strong and linear, so let’s do
another regression of the coefficients on year. It is super
statistically significant (which I’m not sure totally how to interpret
since it’s on coefficients):

``` r
# Visualize
ggplot(lm_health_v_age_0, aes(x = year, y = coef)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  # Adds the regression line with standard error shading
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +  # Confidence intervals for the coefficients
  labs(
    title = "Regression of 'Age' Coefficient Over Years",
    subtitle = "GSS Dataset",
    x = "Year",
    y = "Coefficient of Age"
  ) +
  theme_minimal()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](srh_gss_files/figure-gfm/regress_age_coeff_on_year-1.png)<!-- -->

``` r
# Perform linear regression of 'coef' (age coefficient) vs 'year'
lm_coef_vs_year <- lm(coef ~ year, data = lm_health_v_age_0)

# View the summary of the regression
summary(lm_coef_vs_year)
```

    ## 
    ## Call:
    ## lm(formula = coef ~ year, data = lm_health_v_age_0)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0021432 -0.0011266  0.0001330  0.0006075  0.0022505 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -5.125e-01  4.235e-02  -12.10 1.05e-11 ***
    ## year         2.513e-04  2.123e-05   11.84 1.65e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.001381 on 24 degrees of freedom
    ## Multiple R-squared:  0.8538, Adjusted R-squared:  0.8477 
    ## F-statistic: 140.2 on 1 and 24 DF,  p-value: 1.649e-11

So basically this shows that as years pass, the predictive power of
someone’s age on their self-rated health decreases. This relationship is
(super!!!) strong and statitically significant. As seen in the output
above, the t-stat/p-values and f-stat/p-values are overwhelmingly
statistically significant. Also look at the RSE and the
Rsquared/adjusted Rsquared. WILD.

One sort of interesting thing to note is that the change seems super
gradual and weirdly smooth. It doesn’t really look like something
specific happened as the years passed that caused an abrupt shift in how
people of different ages rated their health. It’s (honestly quite
surprisingly) gradual and very linear looking.

Let’s look at the residuals.

``` r
# Let's look at the residuals
par(mfrow = c(1,1))
plot(fitted(lm_coef_vs_year), lm_coef_vs_year$residuals)
abline(0,0)
```

![](srh_gss_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
qqplot(fitted(lm_coef_vs_year), resid(lm_coef_vs_year))
```

![](srh_gss_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
qqnorm(resid(lm_coef_vs_year))
qqline(resid(lm_coef_vs_year))
```

![](srh_gss_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

``` r
plot(density(resid(lm_coef_vs_year)))
```

![](srh_gss_files/figure-gfm/unnamed-chunk-1-4.png)<!-- -->

They seem pretty normal.

# ANOVA

``` r
# Run an anova

anova_model <- aov(health ~ age * year * cohort, data = data_gss)
summary(anova_model)

# Tukey HSD
#TukeyHSD(anova_model)


# Cut up age/period/cohort to make an anova possible

# Manually create readable labels for cohort
data_gss_cut <- data_gss %>% 
  mutate(age_10 = as.factor(cut(age, breaks = 10))) %>%
  mutate(cohort_10 = as.factor(cut(cohort, breaks = 10, 
                                   labels = paste0("(", formatC(head(seq(min(cohort), max(cohort), length.out = 11), -1), format = "f", digits = 0), 
                                                   ",", 
                                                   formatC(tail(seq(min(cohort), max(cohort), length.out = 11), -1), format = "f", digits = 0), 
                                                   "]")))) %>%
  mutate(year_10 = as.factor(cut(year, breaks = 10)))

# Check the output
data_gss_cut %>% select(cohort_10) %>% ggplot(aes(x = cohort_10)) + geom_bar()
data_gss_cut %>% select(year_10) %>% ggplot(aes(x = year_10)) + geom_bar()
data_gss_cut %>% select(age_10) %>% ggplot(aes(x = age_10)) + geom_bar()

# Run an anova

anova_model <- aov(health ~ age_10 * year_10 * cohort_10, data = data_gss_cut)
summary(anova_model)

# See more ANOVA at the end of the rmd 
```

# Cohort Effects

## Visualize age profiles by cohort

First let’s visualize potential cohort effects.

``` r
data_gss %>% 
  mutate(cohort = cut(cohort, breaks = 7)) %>% # Create cohorts with 6 breaks
  group_by(age, cohort) %>% 
  summarize(mean_health = mean(health)) %>% 
  ggplot(aes(x = age, y = mean_health, color = cohort)) +
  labs(title = "Age Profiles by Cohort") +
  geom_line()
```

    ## `summarise()` has grouped output by 'age'. You can override using the `.groups`
    ## argument.

![](srh_gss_files/figure-gfm/cohort_effects-1.png)<!-- -->

``` r
data_gss %>% 
  mutate(cohort = cut(cohort, breaks = 4)) %>% # Create cohorts with 6 breaks
  group_by(age, cohort) %>% 
  summarize(mean_health = mean(health)) %>% 
  ggplot(aes(x = age, y = mean_health, color = cohort)) +
  labs(title = "Age Profiles by Cohort") +
  geom_line()
```

    ## `summarise()` has grouped output by 'age'. You can override using the `.groups`
    ## argument.

![](srh_gss_files/figure-gfm/cohort_effects-2.png)<!-- -->

``` r
data_gss %>% 
  mutate(cohort = cut(cohort, breaks = 3)) %>% # Create cohorts with 6 breaks
  group_by(age, cohort) %>% 
  summarize(mean_health = mean(health)) %>% 
  ggplot(aes(x = age, y = mean_health, color = cohort)) +
  labs(title = "Age Profiles by Cohort") +
  geom_line()
```

    ## `summarise()` has grouped output by 'age'. You can override using the `.groups`
    ## argument.

![](srh_gss_files/figure-gfm/cohort_effects-3.png)<!-- -->

## Model srh vs age for each cohort and plot the betas and predicted age values

Let’s try to see why the oldest cohort line sort of looks like it lines
up with the youngest by modeling the trend of srh vs age (seems linear)
and comparing the expected mean health rating at 20, 40, and 60 years of
age for the different cohorts, as well as the coefficient.

``` r
data_gss %>% filter(cohort > 1914 & cohort < 1995) %>% select(cohort) %>% unique() %>% summarise(n())
```

    ## # A tibble: 1 × 1
    ##   `n()`
    ##   <int>
    ## 1    80

``` r
# 80

lm_health_v_age_cohorts <- data_gss %>%
  filter(cohort > 1914 & cohort < 1995) %>% 
 # filter(sex == 2) %>% 
  mutate(cohort_cut = cut(cohort, breaks = 40)) %>%   # Create cohort categories
  group_by(cohort_cut) %>%
  nest() %>%   # Nest the data within each cohort
  mutate(
    model = map(data, ~ lm(health ~ age, data = .x)),   # Fit a model for each cohort
    predict_20 = map2_dbl(model, data, ~ predict(.x, newdata = tibble(age = 20))),  # Predict for age = 20
    predict_25 = map2_dbl(model, data, ~ predict(.x, newdata = tibble(age = 25))),
    predict_30 = map2_dbl(model, data, ~ predict(.x, newdata = tibble(age = 30))),
    predict_35 = map2_dbl(model, data, ~ predict(.x, newdata = tibble(age = 35))),
    predict_40 = map2_dbl(model, data, ~ predict(.x, newdata = tibble(age = 40))),
    predict_45 = map2_dbl(model, data, ~ predict(.x, newdata = tibble(age = 45))),
    predict_50 = map2_dbl(model, data, ~ predict(.x, newdata = tibble(age = 50))),
    predict_55 = map2_dbl(model, data, ~ predict(.x, newdata = tibble(age = 55))),
    predict_60 = map2_dbl(model, data, ~ predict(.x, newdata = tibble(age = 60))),
    predict_65 = map2_dbl(model, data, ~ predict(.x, newdata = tibble(age = 65))),
    beta = map2_dbl(model, data, ~ coef(.x)[2]),
    se = map2_dbl(model, data, ~ as.numeric(tidy(.x)[2, "std.error"]))
  ) %>% 
#  select(cohort_cut, predict_x, slope) %>%  # Select relevant columns %>% 
  mutate(cohort_mean = mean(as.numeric(unlist(regmatches(cohort_cut, gregexpr("[0-9]+", cohort_cut))))))

#tidy(lm(health~age, data_gss))[2,"std.error"]

# View the results
print(lm_health_v_age_cohorts)
```

    ## # A tibble: 40 × 16
    ## # Groups:   cohort_cut [40]
    ##    cohort_cut  data     model  predict_20 predict_25 predict_30 predict_35
    ##    <fct>       <list>   <list>      <dbl>      <dbl>      <dbl>      <dbl>
    ##  1 (1953,1954] <tibble> <lm>         3.34       3.28       3.22       3.17
    ##  2 (1933,1935] <tibble> <lm>         3.38       3.33       3.28       3.22
    ##  3 (1915,1917] <tibble> <lm>         2.96       2.92       2.89       2.85
    ##  4 (1943,1945] <tibble> <lm>         3.38       3.33       3.28       3.23
    ##  5 (1925,1927] <tibble> <lm>         3.14       3.10       3.06       3.03
    ##  6 (1919,1921] <tibble> <lm>         2.91       2.88       2.86       2.83
    ##  7 (1947,1949] <tibble> <lm>         3.44       3.37       3.31       3.24
    ##  8 (1951,1953] <tibble> <lm>         3.35       3.30       3.24       3.18
    ##  9 (1949,1951] <tibble> <lm>         3.45       3.38       3.31       3.24
    ## 10 (1939,1941] <tibble> <lm>         3.55       3.46       3.38       3.30
    ## # ℹ 30 more rows
    ## # ℹ 9 more variables: predict_40 <dbl>, predict_45 <dbl>, predict_50 <dbl>,
    ## #   predict_55 <dbl>, predict_60 <dbl>, predict_65 <dbl>, beta <dbl>, se <dbl>,
    ## #   cohort_mean <dbl>

``` r
# Now let's plot it

lm_health_v_age_cohorts %>% 
  ggplot(aes(x = cohort_mean, y = beta)) +
  labs(title = "Estimate for Coefficient for SRH vs Age for each Cohort",
       x = "Birth Year",
       y = "Coefficient") +
#  geom_errorbar(ymin = beta - se, ymax = beta + se) +
  geom_point()
```

![](srh_gss_files/figure-gfm/cohort_effects_continued-1.png)<!-- -->

``` r
lm_health_v_age_cohorts %>% 
  ggplot(aes(x = cohort_mean, y = predict_20)) +
  labs(title = "Predicted SRH at age 20 for each Cohort") +
  geom_point()
```

![](srh_gss_files/figure-gfm/cohort_effects_continued-2.png)<!-- -->

``` r
 ggplot(lm_health_v_age_cohorts, aes(x = cohort_mean, y = predict_30)) +
  labs(title = "Predicted SRH at age 30 for each Cohort") +
  geom_point()
```

![](srh_gss_files/figure-gfm/cohort_effects_continued-3.png)<!-- -->

``` r
  ggplot(lm_health_v_age_cohorts, aes(x = cohort_mean, y = predict_65)) +
  labs(title = "Predicted SRH at age 40 for each Cohort") +
  geom_point()
```

![](srh_gss_files/figure-gfm/cohort_effects_continued-4.png)<!-- -->

``` r
# Plot many ages
  
# Reshape the data: gather all the prediction columns into one long format
lm_health_long <- lm_health_v_age_cohorts %>%
  pivot_longer(cols = starts_with("predict"), 
               names_to = "prediction_age", 
               values_to = "predicted_srh")

# Create the plot using the long data format
p <- ggplot(lm_health_long, aes(x = cohort_mean, y = predicted_srh)) +
  geom_point() +
  facet_wrap(~prediction_age, ncol = 4) +  # Facet by the different predictions (predict_20, predict_25, etc.)
  labs(
    title = "Predicted Self-Rated Health at Various Ages for Each Cohort",
    x = "Cohort Mean",
    y = "Predicted Self-Rated Health"
  )

# Display the combined plot
print(p)
```

![](srh_gss_files/figure-gfm/cohort_effects_continued-5.png)<!-- -->

# Other

``` r
data_gss %>% 
  mutate(cohort = cut(cohort, breaks = 8)) %>% # Create cohorts with 6 breaks
  group_by(year, cohort) %>% 
  summarize(mean_health = mean(health)) %>% 
  ggplot(aes(x = year, y = mean_health, color = cohort)) +
  labs(title = "Year Profiles by Cohort") +
  geom_line()
```

    ## `summarise()` has grouped output by 'year'. You can override using the
    ## `.groups` argument.

![](srh_gss_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

# More ANOVA – Tukey, QQ-plot, etc

``` r
# Model Diagnostics

# Plot Q-Q plot to check for normality of residuals
qqnorm(residuals(anova_model))
qqline(residuals(anova_model), col = "hotpink")

# Shapiro-Wilk normality test
#shapiro.test(residuals(anova_model))

# Residuals vs Fitted plot
plot(fitted(anova_model), residuals(anova_model))
abline(h = 0, col = "red")

# Breusch-Pagan test for homoscedasticity
#library(lmtest)
#bptest(anova_model)


# Tukey HSD

#TukeyHSD(anova_model)
```

# SD over time

``` r
data_gss %>% 
  group_by(year) %>% 
  summarize(sd = sd(health)) %>% 
  ggplot(aes(x = year, y = sd)) +
  geom_point()
```

![](srh_gss_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
data_gss %>% 
  group_by(year) %>% 
  summarize(mean = mean(health)) %>% 
  ggplot(aes(x = year, y = mean)) +
  geom_point()
```

![](srh_gss_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
data_gss %>% 
  group_by(year) %>% 
  summarize(sd_over_mean = sd(health)/mean(health)) %>% 
  ggplot(aes(x = year, y = sd_over_mean)) +
  geom_point()
```

![](srh_gss_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

``` r
data_gss %>% 
  group_by(year) %>% 
  summarize(median = median(health)) %>% 
  ggplot(aes(x = year, y = median)) +
  geom_point()
```

![](srh_gss_files/figure-gfm/unnamed-chunk-3-4.png)<!-- -->

# APC Analysis

``` r
# Load necessary libraries
library(gss)        # Contains GSS data
library(dplyr)      # For data manipulation
library(ggplot2)    # For data visualization

# 1. Defining Cohorts and Periods
# -------------------------------
# 
# # Load the GSS dataset
# data("gss_all")  # Load GSS data from the gss package
# 
# # Inspect the data
# str(gss_all)
# 
# # Select relevant variables: SRH, age, and survey year
# gss_data <- gss_all %>%
#   select(year, age, health)

# # Remove missing values
# gss_data <- gss_data %>%
#   filter(!is.na(age), !is.na(health))

gss_data <- data_gss

# Create age groups (e.g., in 10-year intervals)
gss_data <- gss_data %>%
  mutate(age_group = cut(age, breaks = seq(18, 89, by = 10), right = FALSE))

# Calculate birth cohort
gss_data <- gss_data %>%
  mutate(cohort = year - age)

# Create cohort groups (e.g., in 10-year intervals)
gss_data <- gss_data %>%
  mutate(cohort_group = cut(cohort, breaks = seq(1900, 2000, by = 10), right = FALSE))

# 2. Constructing Synthetic Cohort Time Series
# --------------------------------------------

# Aggregate SRH by age group and survey year (period)
age_period_data <- gss_data %>%
  group_by(year, age_group) %>%
  summarize(avg_srh = mean(health, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'year'. You can override using the
    ## `.groups` argument.

``` r
# Aggregate SRH by cohort group and period
cohort_period_data <- gss_data %>%
  group_by(year, cohort_group) %>%
  summarize(avg_srh = mean(health, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'year'. You can override using the
    ## `.groups` argument.

``` r
# 3. Analyzing APC Effects
# ------------------------

# Analyze how SRH varies across age groups over time
age_trends <- age_period_data %>%
  group_by(age_group) %>%
  summarize(trend = lm(avg_srh ~ year, data = .)$coefficients[2])

age_trends
```

    ## # A tibble: 8 × 2
    ##   age_group    trend
    ##   <fct>        <dbl>
    ## 1 [18,28)   0.000481
    ## 2 [28,38)   0.000481
    ## 3 [38,48)   0.000481
    ## 4 [48,58)   0.000481
    ## 5 [58,68)   0.000481
    ## 6 [68,78)   0.000481
    ## 7 [78,88)   0.000481
    ## 8 <NA>      0.000481

``` r
# Calculate trends using group_modify()
age_trends <- age_period_data %>%
  group_by(age_group) %>%
  group_modify(~ {
    model <- lm(avg_srh ~ year, data = .x)
    data.frame(trend = coef(model)["year"])
  })

# View the results
print(age_trends)
```

    ## # A tibble: 8 × 2
    ## # Groups:   age_group [8]
    ##   age_group     trend
    ##   <fct>         <dbl>
    ## 1 [18,28)   -0.00590 
    ## 2 [28,38)   -0.00542 
    ## 3 [38,48)   -0.00563 
    ## 4 [48,58)   -0.000247
    ## 5 [58,68)    0.00403 
    ## 6 [68,78)    0.00769 
    ## 7 [78,88)    0.00616 
    ## 8 <NA>       0.00316

``` r
# Analyze how SRH varies across cohorts over time
# Calculate trends using group_modify()
cohort_trends <- cohort_period_data %>%
  group_by(cohort_group) %>%
  group_modify(~ {
    model <- lm(avg_srh ~ year, data = .x)
    data.frame(trend = coef(model)["year"])
  })

# View the results
print(cohort_trends)
```

    ## # A tibble: 11 × 2
    ## # Groups:   cohort_group [11]
    ##    cohort_group            trend
    ##    <fct>                   <dbl>
    ##  1 [1.9e+03,1.91e+03)  -0.000643
    ##  2 [1.91e+03,1.92e+03) -0.00335 
    ##  3 [1.92e+03,1.93e+03) -0.00818 
    ##  4 [1.93e+03,1.94e+03) -0.00830 
    ##  5 [1.94e+03,1.95e+03) -0.0122  
    ##  6 [1.95e+03,1.96e+03) -0.0123  
    ##  7 [1.96e+03,1.97e+03) -0.0113  
    ##  8 [1.97e+03,1.98e+03) -0.0126  
    ##  9 [1.98e+03,1.99e+03) -0.00519 
    ## 10 [1.99e+03,2e+03)    -0.0430  
    ## 11 <NA>                 0.0198

``` r
# 4. Limitations and Considerations
# ---------------------------------

# Note: Since this is pooled cross-sectional data, we're observing population averages.
# Individual-level changes are not captured.

# 5. Visualization and Interpretation
# -----------------------------------

# Plot average SRH over time for each age group
ggplot(age_period_data, aes(x = year, y = avg_srh, color = age_group)) +
  geom_line() +
  labs(title = "Average SRH Over Time by Age Group",
       x = "Year",
       y = "Average Self-Rated Health",
       color = "Age Group") +
  theme_minimal()
```

![](srh_gss_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# Plot average SRH over time for each cohort group
ggplot(cohort_period_data, aes(x = year, y = avg_srh, color = cohort_group)) +
  geom_line() +
  labs(title = "Average SRH Over Time by Cohort Group",
       x = "Year",
       y = "Average Self-Rated Health",
       color = "Cohort Group") +
  theme_minimal()
```

![](srh_gss_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
# Interpretation:
# - The first plot shows how average SRH changes over time within each age group.
# - The second plot shows how average SRH changes over time within each birth cohort.
# - Trends can indicate age effects, period effects, and cohort effects.

# Additional Analysis:

# Examine period effects by averaging SRH across all respondents each year
period_data <- gss_data %>%
  group_by(year) %>%
  summarize(avg_srh = mean(health, na.rm = TRUE))

# Plot average SRH over time
ggplot(period_data, aes(x = year, y = avg_srh)) +
  geom_line() +
  labs(title = "Average SRH Over Time (Period Effect)",
       x = "Year",
       y = "Average Self-Rated Health") +
  theme_minimal()
```

![](srh_gss_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
# Examine age effects by averaging SRH across all years for each age group
age_data <- gss_data %>%
  group_by(age_group) %>%
  summarize(avg_srh = mean(health, na.rm = TRUE))

# Plot average SRH by age group
ggplot(age_data, aes(x = age_group, y = avg_srh)) +
  geom_bar(stat = "identity") +
  labs(title = "Average SRH by Age Group (Age Effect)",
       x = "Age Group",
       y = "Average Self-Rated Health") +
  theme_minimal()
```

![](srh_gss_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

``` r
# Examine cohort effects by averaging SRH across all years for each cohort group
cohort_data <- gss_data %>%
  group_by(cohort_group) %>%
  summarize(avg_srh = mean(health, na.rm = TRUE))

# Plot average SRH by cohort group
ggplot(cohort_data, aes(x = cohort_group, y = avg_srh)) +
  geom_bar(stat = "identity") +
  labs(title = "Average SRH by Cohort Group (Cohort Effect)",
       x = "Cohort Group",
       y = "Average Self-Rated Health") +
  theme_minimal()
```

![](srh_gss_files/figure-gfm/unnamed-chunk-4-5.png)<!-- -->

``` r
# Conclusion:
# - These analyses help disentangle age, period, and cohort effects on SRH.
# - Observing these trends can inform public health strategies and policies.
```
