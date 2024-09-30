GSS EDA - health and covariates
================
Christine Lucille Kuryla
2024-09-27

- [Fetch GSS data](#fetch-gss-data)
- [Variables of interest](#variables-of-interest)
  - [“health”](#health)
  - [“year”](#year)
  - [“cohort”](#cohort)
  - [“happy”](#happy)
  - [“sex”](#sex)
  - [“educ”](#educ)
  - [“life”](#life)
  - [“satfin”](#satfin)
  - [“polviews”](#polviews)
- [Load and clean data](#load-and-clean-data)
  - [Peek at GSS df](#peek-at-gss-df)
  - [Life expectancy data](#life-expectancy-data)
- [Histograms](#histograms)
- [Correlation Heatmap](#correlation-heatmap)
- [Mean health over
  Age/Period/Cohort](#mean-health-over-ageperiodcohort)
- [Cohorts](#cohorts)
  - [Happiness and health](#happiness-and-health)

Take a look at GSS dataset, what’s available, some trends, etc.

The first variable of interest is “health”, which will be the main
subject of our analysis.

<https://gssdataexplorer.norc.org/variables/437/vshow>

Question on survey: “Would you say your own health, in general, is
excellent, good, fair, or poor?”

# Fetch GSS data

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

# Variables of interest

<details>
<summary>
**Click to expand and explore the variables of interest, including the
coding and the way the question is phrased.**
</summary>

### “health”

<https://gssdataexplorer.norc.org/variables/437/vshow?back=variableList>

Variable: Health Module: Core Gss Tags: Health

“Would you say your own health, in general, is excellent, good, fair, or
poor?”

-100 .i: Inapplicable  
-99 .n: No answer  
-98 .d: Do not Know/Cannot Choose  
-97 .s: Skipped on Web  
1 Excellent  
2 Good  
3 Fair  
4 Poor

*Note* we will recode this data to reverse it to be more intuitive, such
that for our analyses, higher numbers will indicate better self-rated
health:

4 Excellent  
3 Good  
2 Fair  
1 Poor

### “year”

<https://gssdataexplorer.norc.org/variables/1/vshow?back=variableList>

Year of survey

Variable: Year

GSS year for this respondent

### “cohort”

<https://gssdataexplorer.norc.org/variables/5507/vshow?back=variableList>

Year of birth

Birth cohort of respondent.

### “happy”

general happiness

“Taken all together, how would you say things are these days–would you
say that you are very happy, pretty happy, or not too happy?”

-100 .i: Inapplicable  
-99 .n: No answer  
-98 .d: Do not Know/Cannot Choose  
-97 .s: Skipped on Web  
1 Very happy  
2 Pretty happy  
3 Not too happy

*Note* we will recode this data to reverse it to be more intuitive, such
that for our analyses, higher numbers will indicate higher happiness:

3 Very happy  
2 Pretty happy  
1 Not too happy

### “sex”

<https://gssdataexplorer.norc.org/variables/81/vshow?back=variableList>

respondent’s sex

-100 .i: Inapplicable 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 19 0 -99 .n: No answer 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 71 9 -98 .d: Do not Know/Cannot Choose 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8 -97 .s:
Skipped on Web 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 2 3 1 MALE  
2 FEMALE

### “educ”

<https://gssdataexplorer.norc.org/variables/55/vshow>

highest year of school completed

Questions associated with this variable: ASK ALL PARTS OF QUESTION ABOUT
RESPONDENT BEFORE GOING ON TO ASK ABOUT R’S FATHER; AND THEN R’S MOTHER;
THEN R’S SPOUSE, IF R IS CURRENTLY MARRIED. A. What is the highest grade
in elementary school or high school that (you/your father/ your
mother/your \[husband/wife\]) finished and got credit for? CODE EXACT
GRADE. B. IF FINISHED 9th-12th GRADE OR DK\*: Did (you/he/she) ever get
a high school diploma or a GED certificate? \[SEE D BELOW.\] \[See
REMARKS\] C. Did (you/he/she) complete one or more years of college for
credit–not including schooling such as business college, technical or
vocational school? IF YES: How many years did (you/he/she) complete? Do
you (Does \[he/she\]) have any college degrees? (IF YES: What degree or
degrees?) CODE HIGHEST DEGREE EARNED.

-99 .n: No answer 5 5 3 2 4 3 3 1 4 0 3 0 1 6 0 5 0 7 0 4 1 4 9 7 2 9 2
4 1 0 8 1 48 20 -98 .d: Do not Know/Cannot Choose 0 0 0 1 2 7 3 4 4 2 0
0 0 4 3 2 2 0 4 3 8 8 0 5 0 2 3 1 1 1 1 2 18 0 0 No formal schooling 14
7 6 7 4 4 6 1 7 5 3 3 6 9 1 3 4 2 3 4 4 2 1 5 4 22 6 5 3 8 2 4 9 3 1 1st
grade 4 4 1 2 0 0 4 1 2 1 3 2 0 2 1 2 1 0 0 1 0 0 0 2 1 4 0 1 2 1 3 2 1
1 2 2nd grade 13 7 1 3 4 1 1 … 19 7 or more years of college 20 8 or
nore years of college

### “life”

is life exciting or dull

<https://gssdataexplorer.norc.org/variables/438/vshow>

“In general, do you find life exciting, pretty routine, or dull?”

100 .i: Inapplicable  
-99 .n: No answer  
-98 .d: Do not Know/Cannot Choose  
-97 .s: Skipped on Web  
1 Exciting  
2 Routine 3 Dull

*Note* we will recode this data to reverse it to be more intuitive, such
that for our analyses, higher numbers will indicate a more exciting
life:

3 Exciting  
2 Routine 1 Dull

### “satfin”

satisfaction with financial situation

<https://gssdataexplorer.norc.org/variables/572/vshow>

“We are interested in how people are getting along financially these
days. So far as you and your family are concerned, would you say that
you are pretty well satisfied with your present financial situation,
more or less satisfied, or not satisfied at all?”

-100 .i: Inapplicable 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1393
1472 1518 0 0 0 0 0 0 0 0 -99 .n: No answer 4 2 4 7 3 6 1 2 8 2 6 4 1 6
4 1 2 6 3 11 4 3 5 1 2 8 2 5 5 3 6 3 1 4 -98 .d: Do not Know/Cannot
Choose 1 1 2 4 4 3 2 4 5 5 3 5 3 2 3 4 3 5 7 5 4 5 9 2 3 4 5 1 2 3 5 7 0
7 -97 .s: Skipped on Web 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 15 7 1 Pretty well satisfied  
2 More or less satisfied  
3 Not satisfied at all

*Note* we will recode this data to reverse it to be more intuitive, such
that for our analyses, higher numbers will indicate more financial
satisfaction:

3 Pretty well satisfied  
2 More or less satisfied  
1 Not satisfied at all

### “polviews”

think of self as liberal or conservative

<https://gssdataexplorer.norc.org/variables/178/vshow>

“We hear a lot of talk these days about liberals and conservatives. I’m
going to show you a seven-point scale on which the political views that
people might hold are arranged from extremely liberal–point 1–to
extremely conservative–point 7. Where would you place yourself on this
scale?”

-100 .i: Inapplicable 0 0 0 0 0 0 0 795 0 0 0 0 0 0 0 0 0 0 0 0 0 1393
1472 0 0 0 0 0 0 0 15 1 -99 .n: No answer 4 12 5 6 27 17 17 3 11 9 2 42
9 7 4 4 9 12 6 8 24 5 5 23 13 10 13 24 30 22 3 27 -98 .d: Do not
Know/Cannot Choose 70 81 93 71 70 22 104 31 52 63 67 98 56 88 53 54 49
101 155 133 149 36 26 154 77 61 87 65 81 79 10 73 -97 .s: Skipped on Web
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 40 17 1
Extremely liberal  
2 Liberal 3 Slightly liberal  
4 Moderate, middle of the road  
5 Slightly conservative  
6 Conservative  
7 Extremely conservative

</details>

# Load and clean data

Here we’ll load our data, clean some unwanted values, and recode the
unintuitive variables.

``` r
data_gss <- read_csv("data/extracted_gss_variables.csv") %>% 
  filter(cohort != 9999) %>% 
  na.omit() %>% 
  mutate(health = 5 - health)  %>%  # reverse the coding so it's more intuitive (higher number for excellent, lower number for poor)
  mutate(happy = 4 - happy) %>% # same
  mutate(life = 4 - life) %>% # reverse again, these variables tend to be unintuitively ordered!!!
  mutate(satfin = 4 - satfin) 
```

    ## Rows: 72390 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (11): year, cohort, age, health, sex, happy, life, educ, polviews, class...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Peek at GSS df

``` r
head(data_gss, n = 10)
```

    ## # A tibble: 10 × 11
    ##     year cohort   age health   sex happy  life  educ polviews class satfin
    ##    <dbl>  <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl> <dbl>  <dbl>
    ##  1  1974   1953    21      4     1     3     3    14        4     2      3
    ##  2  1974   1933    41      3     1     3     3    16        5     3      2
    ##  3  1974   1891    83      3     2     3     2    10        6     2      3
    ##  4  1974   1905    69      3     2     2     2    10        6     3      3
    ##  5  1974   1916    58      2     2     2     2    12        6     2      2
    ##  6  1974   1944    30      3     1     2     3    16        5     3      2
    ##  7  1974   1926    48      3     1     3     3    17        5     3      3
    ##  8  1974   1907    67      4     1     3     3    10        5     2      3
    ##  9  1974   1920    54      4     2     3     3    11        6     2      1
    ## 10  1974   1885    89      3     1     2     2     6        4     4      3

``` r
glimpse(data_gss)
```

    ## Rows: 39,965
    ## Columns: 11
    ## $ year     <dbl> 1974, 1974, 1974, 1974, 1974, 1974, 1974, 1974, 1974, 1974, 1…
    ## $ cohort   <dbl> 1953, 1933, 1891, 1905, 1916, 1944, 1926, 1907, 1920, 1885, 1…
    ## $ age      <dbl> 21, 41, 83, 69, 58, 30, 48, 67, 54, 89, 71, 27, 30, 23, 63, 7…
    ## $ health   <dbl> 4, 3, 3, 3, 2, 3, 3, 4, 4, 3, 2, 2, 2, 4, 1, 1, 1, 2, 1, 1, 4…
    ## $ sex      <dbl> 1, 1, 2, 2, 2, 1, 1, 1, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2…
    ## $ happy    <dbl> 3, 3, 3, 2, 2, 2, 3, 3, 3, 2, 3, 2, 2, 3, 3, 2, 3, 1, 2, 1, 3…
    ## $ life     <dbl> 3, 3, 2, 2, 2, 3, 3, 3, 3, 2, 2, 2, 2, 3, 1, 1, 3, 1, 2, 1, 3…
    ## $ educ     <dbl> 14, 16, 10, 10, 12, 16, 17, 10, 11, 6, 5, 11, 12, 12, 8, 8, 1…
    ## $ polviews <dbl> 4, 5, 6, 6, 6, 5, 5, 5, 6, 4, 2, 4, 4, 4, 4, 7, 2, 4, 6, 6, 3…
    ## $ class    <dbl> 2, 3, 2, 3, 2, 3, 3, 2, 2, 4, 3, 3, 3, 2, 1, 4, 3, 4, 3, 4, 1…
    ## $ satfin   <dbl> 3, 2, 3, 3, 2, 2, 3, 3, 1, 3, 3, 1, 1, 3, 1, 3, 1, 3, 3, 3, 1…

## Life expectancy data

``` r
# https://population.un.org/wpp/Download/Standard/CSV/
data_le <- read_csv("big_data/WPP2024_Demographic_Indicators_Medium.csv")
data_le <- data_le %>% 
  filter(Location == "United States of America")

# Just get the years of interest for merging with the gss data
year_range <- seq(min(data_gss$year), max(data_gss$year))
data_le <- data_le %>% 
  filter(Time %in% year_range)

write_csv(data_le, "data/un_life_expectancy_etc.csv")
```

# Histograms

Let’s make a few quick histograms to get a sense of the survey response
distributions.

``` r
# Tidyverse and flexible number of histograms

library(patchwork)

# Create list to store ggplot objects
plot_list <- list()

# Loop through column names and create plots
for (var in colnames(data_gss)) {
  if (is.numeric(data_gss[[var]])) {  # Only create histograms for numeric columns
    p <- ggplot(data_gss, aes(x = .data[[var]])) +  # Use .data[[var]] for tidy evaluation
      geom_histogram(bins = 20, fill = "pink", color = "hotpink") +
      theme_minimal()
    
    plot_list[[var]] <- p
  }
}

# Combine all plots using patchwork and add a title
combined_plot <- wrap_plots(plot_list, ncol = 3) +
  plot_annotation(title = "Histograms of Survey Responses")

# Display the combined plot
print(combined_plot)
```

![](gss_eda_files/figure-gfm/histograms-1.png)<!-- -->

# Correlation Heatmap

Just to get some idea and familiarize ourselves with the data.

``` r
correlation_auto <- cor(data_gss)
knitr::kable(correlation_auto)
```

|  | year | cohort | age | health | sex | happy | life | educ | polviews | class | satfin |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| year | 1.0000000 | 0.5895682 | 0.1156949 | -0.0550720 | -0.0057159 | -0.0856281 | -0.0001011 | 0.2471477 | -0.0136163 | -0.0263427 | -0.0223087 |
| cohort | 0.5895682 | 1.0000000 | -0.7340846 | 0.1406949 | -0.0252270 | -0.0745466 | 0.0528437 | 0.2747073 | -0.0984603 | -0.1212801 | -0.1544326 |
| age | 0.1156949 | -0.7340846 | 1.0000000 | -0.2193179 | 0.0262172 | 0.0196846 | -0.0650690 | -0.1300390 | 0.1096333 | 0.1269966 | 0.1711570 |
| health | -0.0550720 | 0.1406949 | -0.2193179 | 1.0000000 | -0.0264873 | 0.2722478 | 0.2645358 | 0.2565862 | -0.0060991 | 0.1656160 | 0.1780896 |
| sex | -0.0057159 | -0.0252270 | 0.0262172 | -0.0264873 | 1.0000000 | 0.0070138 | -0.0556529 | -0.0303791 | -0.0322852 | -0.0154809 | -0.0314176 |
| happy | -0.0856281 | -0.0745466 | 0.0196846 | 0.2722478 | 0.0070138 | 1.0000000 | 0.3374248 | 0.0761941 | 0.0562054 | 0.1745948 | 0.2957047 |
| life | -0.0001011 | 0.0528437 | -0.0650690 | 0.2645358 | -0.0556529 | 0.3374248 | 1.0000000 | 0.2047851 | -0.0148122 | 0.1641290 | 0.1743384 |
| educ | 0.2471477 | 0.2747073 | -0.1300390 | 0.2565862 | -0.0303791 | 0.0761941 | 0.2047851 | 1.0000000 | -0.0752421 | 0.2765887 | 0.1106097 |
| polviews | -0.0136163 | -0.0984603 | 0.1096333 | -0.0060991 | -0.0322852 | 0.0562054 | -0.0148122 | -0.0752421 | 1.0000000 | 0.0118224 | 0.0586271 |
| class | -0.0263427 | -0.1212801 | 0.1269966 | 0.1656160 | -0.0154809 | 0.1745948 | 0.1641290 | 0.2765887 | 0.0118224 | 1.0000000 | 0.3343757 |
| satfin | -0.0223087 | -0.1544326 | 0.1711570 | 0.1780896 | -0.0314176 | 0.2957047 | 0.1743384 | 0.1106097 | 0.0586271 | 0.3343757 | 1.0000000 |

``` r
# Create a heatmap

#upper_tri <- matrixcalc::upper.triangle(correlation_auto)
#melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)

melted_cormat <- reshape2::melt(cor(data_gss), na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
 geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
   name="Pearson\nCorrelation") +
  theme_minimal()+ 
 theme(axis.text.x = element_text(angle = 45, vjust = 1, 
     hjust = 1))+
 coord_fixed() +
  geom_text(aes(Var2, Var1, label = if_else(value != 0, as.character(round(value, digits = 2)), " ")))
```

![](gss_eda_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

# Mean health over Age/Period/Cohort

Now let’s take a look at the trend of people’s self-rated health over
time, as well as by age, and by birthyear.

``` r
# Mean health over time, age, and birth year

data_gss %>% 
  group_by(year) %>% 
  summarize(mean_health = mean(health)) %>% 
  ggplot(aes(x = year, y = mean_health)) +
  geom_line(color = "cornflowerblue") +
  labs(title = "Period: Self-Rated Health Each Year",
       subtitle = "(all ages together)" )
```

![](gss_eda_files/figure-gfm/over_apc-1.png)<!-- -->

``` r
data_gss %>% 
  group_by(age) %>% 
  summarize(mean_health = mean(health)) %>% 
  ggplot(aes(x = age, y = mean_health)) +
  geom_line(color = "cornflowerblue") +
  labs(title = "Age: Self-Rated Health By Age" ,
       subtitle = "(all years together)" )
```

![](gss_eda_files/figure-gfm/over_apc-2.png)<!-- -->

``` r
data_gss %>% 
  group_by(cohort) %>% 
  summarize(mean_health = mean(health)) %>% 
  ggplot(aes(x = cohort, y = mean_health)) +
  geom_line(color = "cornflowerblue") +
  labs(title = "Cohort: Self-Rated Health by Birth Year",
       subtitle = "(all years together)" )
```

![](gss_eda_files/figure-gfm/over_apc-3.png)<!-- -->

# Cohorts

Let’s explore how each cohort tends to rate their health when at the
same age.

``` r
data_gss %>% 
  mutate(cohort = cut(cohort, breaks = 6)) %>% # Create cohorts with 6 breaks
  group_by(age, cohort) %>% 
  summarize(mean_health = mean(health)) %>% 
  ggplot(aes(x = age, y = mean_health, color = cohort)) +
  geom_line()
```

    ## `summarise()` has grouped output by 'age'. You can override using the `.groups`
    ## argument.

![](gss_eda_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
data_gss %>% 
  mutate(cohort = cut(cohort, breaks = 6)) %>% 
  group_by(year, cohort) %>% 
  summarize(mean_health = mean(health)) %>% 
  ggplot(aes(x = year, y = mean_health, color = cohort)) +
  geom_line()
```

    ## `summarise()` has grouped output by 'year'. You can override using the
    ## `.groups` argument.

![](gss_eda_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

## Happiness and health

``` r
data_gss %>% 
  ggplot(aes(x = health, y = happy)) +
  geom_smooth(method = "lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](gss_eda_files/figure-gfm/happiness_and_health-1.png)<!-- -->

``` r
data_gss %>% 
  mutate(year = cut(year, breaks = 6)) %>% 
  mutate(age = cut(age, breaks = 6)) %>% 
  ggplot(aes(x = health, y = happy, color = age)) +
  facet_wrap(~ year) +
  geom_smooth(method = "lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](gss_eda_files/figure-gfm/happiness_and_health-2.png)<!-- -->

``` r
data_gss %>% 
  mutate(cohort = cut(cohort, breaks = 6)) %>% 
  mutate(year = cut(year, breaks = 6)) %>% 
  ggplot(aes(x = health, y = happy, color = cohort)) +
  facet_wrap(~ year) +
  geom_smooth(method = "lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](gss_eda_files/figure-gfm/happiness_and_health-3.png)<!-- -->

``` r
######data_gss %>% 
  # group_by(year) %>% 
  # summarize(mean_health = mean(health)) %>% 
  # ggplot(aes(x = cohort, y = mean_health)) +
  # geom_line()

data_gss %>% 
  mutate(cohort = cut(cohort, breaks = 6)) %>% 
#  mutate(year = cut(year, breaks = 6)) %>% 
#  mutate(age = cut(year, breaks = 6)) %>% 
  ggplot(aes(x = age, y = health, color = cohort)) +
#  facet_wrap(~ year) +
  geom_smooth(method = "lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](gss_eda_files/figure-gfm/happiness_and_health-4.png)<!-- -->
