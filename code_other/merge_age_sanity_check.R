


# Comparing the IDs in the different datasets

data_nhanes_clk <- read_csv(here("data/nhanes4_selected_apcsrh.csv")) %>% 
  mutate(age_group = as.factor( 
    cut(
      age,
      breaks = c(17, 29, 39, 49, 59, 69, Inf),
      labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"),
      levels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"),
      right = TRUE
    )),
    health_cat = factor(srh, 
                        levels = 1:5,
                        labels = c("Poor", "Fair", "Good", "Very Good", "Excellent"))) %>% 
  mutate(age_clk = age) %>% 
  mutate(id = SEQN) %>% 
  mutate(year = floor(year))

data_nh_kamaryn <- read_csv(here("code_examples/kamaryn_nhanes/nhanes_1999-2018_2023-11-29.csv")) %>% 
  mutate(age_kamaryn = age_visit)

# nh_dates <- read_csv(here("code_examples/kamaryn_nhanes/nhanes_1999-2018_2023-11-29.csv"), c)
data_nhanes_gloria <- read.csv(here("big_data/NHANES/Gloria_preprocessed/Data-3/Core_Dataset_Aim2.csv")) %>% 
 # mutate(SEQN == ifelse("a_" == str_extract(SEQN, "a_"), NA, SEQN)) %>% 
  mutate(id = as.numeric(SEQN)) %>% 
  mutate(age_gloria = age) 

# N per dataset
nrow(data_nh_kamaryn)
nrow(data_nhanes_gloria)
nrow(data_nhanes_clk)

# Overlap in each dataset
length(intersect(data_nh_kamaryn$id, data_nhanes_gloria$id))
length(intersect(data_nh_kamaryn$id, data_nhanes_clk$id))
length(intersect(data_nhanes_clk$id, data_nhanes_gloria$id))

select_kamaryn <- data_nh_kamaryn %>% 
  filter(visit == 1) %>% 
  select(id, age_kamaryn) 
nrow(select_kamaryn)

select_gloria <- data_nhanes_gloria %>% 
  select(id, age_gloria, year)

select_clk <- data_nhanes_clk %>% 
  select(id, age_clk, year)

df_age_id_check <- inner_join(select_kamaryn, 
                              select_clk, by = "id")
nrow(df_age_id_check)
df_age_id_check <- inner_join(df_age_id_check, select_gloria, by = "id")
nrow(df_age_id_check)

nrow(df_age_id_check)

all(df_age_id_check$age_gloria == df_age_id_check$age_kamaryn)


df_age_gloria_clk <- inner_join(select_clk %>% mutate(age = age_clk),
                                select_gloria %>% mutate(age = age_gloria),
                                by = c("id", "age", "year"))
nrow(df_age_gloria_clk)

df_age_kamaryn_clk <- inner_join(select_clk %>% mutate(age = age_clk),
                                select_kamaryn %>% mutate(age = age_kamaryn),
                                by = c("id", "age"))
nrow(df_age_kamaryn_clk)

