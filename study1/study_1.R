# Load necessary libraries
library(tidyverse)
library(emmeans)
library(effectsize)

#'Do means matter morally?' Study 1

# --- Load and Clean Data ---

# Load dataset
filename = "study1/study1_anon.csv"
headers = read_csv(filename, col_names = FALSE, n_max = 1)
mm1raw = read_csv(filename, skip = 3, col_names = FALSE)
colnames(mm1raw) = headers

# Filter to complete responses
mm1demog = mm1raw %>%
  filter(Progress == 100, !is.na(anon_id)) %>%
  mutate(
    ResponseId,
    Endorsement = case_when(choice == 4 ~ "Difference",
                            choice == 5 ~ "Equivalence"),
    endorsement_num = choice - 4,
    Bipolar = rating_1,
    Conservatism = (politics_1 + politics_2) / 2,
    Religiosity = religiosity_1,
    Social = politics_2,
    Fiscal = politics_1, 
    Age = age, 
    Gender = gender,
    Order = if_else(str_sub(FL_21_DO, 1, 5) == 'ABSTR', 'Abstract1st', 'Scenarios1st')
  ) %>%
  select(ResponseId, Order, Methods, Age, Gender, Endorsement, endorsement_num, Bipolar, Conservatism, Social, Fiscal, Religiosity)

# --- Compare Conventional vs Technological DV (4 pairs) ---
mm1resp = mm1raw %>%
  select(., contains(c("C1","C2", "C3", "C4", "T1", "T2", "T3", "T4")), ResponseId) %>%
  gather(-ResponseId, key = 'key', value = 'resp', na.rm = TRUE) %>%
  mutate(scen = str_sub(key, 1, 3),
         condition = str_sub(key, 4, 4), 
         item = str_sub(key, 5, 5)) %>%
  select(-key) %>%
  group_by(ResponseId, condition, item) %>%
  summarise(response = mean(resp)) %>%
  pivot_wider(id_cols = c('ResponseId', 'condition'), names_from = 'item', values_from = 'response') %>%
  mutate(response = (`1` + `2` + `3` + `4`)/4)

mm1 = full_join(mm1demog, mm1resp) %>%
  filter(!is.na(Methods))

# --- Demographics ---

# Mean & SD for Age
mm1demog %>%
  summarise(
    mean_age = mean(Age, na.rm = TRUE),
    sd_age   = sd(Age, na.rm = TRUE),
    n        = n()
  )

# Gender
mm1demog %>%
  count(Gender) %>%
  mutate(prop = n / sum(n))

# Religiosity
mm1demog %>%
  count(Religiosity) %>%
  mutate(prop = n / sum(n))

# Political
mm1demog %>%
  count(Conservatism) %>%
  mutate(prop = n / sum(n))

mm1demog %>%
  summarise(
    n_below5 = sum(Conservatism < 5, na.rm = TRUE),
    prop_below5 = mean(Conservatism < 5, na.rm = TRUE),
    n_total = sum(!is.na(Conservatism)))




# t-test and Cohen d: Are Conventional vs Technological interventions the same?
mm1 %>%
  group_by(Methods) %>%
  summarise(
    mean_response = mean(response, na.rm = TRUE),
    sd_response   = sd(response, na.rm = TRUE),
    n             = n())
t.test(response ~ Methods, mm1)
effectsize::cohens_d(response ~ Methods, data = mm1)


# t-test and Cohen d: Are each Conventional vs Technological intervention the same?
mm1scen = mm1raw %>%
  select(., contains(c("C1","C2", "C3", "C4", "T1", "T2", "T3", "T4")), ResponseId) %>%
  gather(-ResponseId, key = 'key', value = 'resp', na.rm = TRUE) %>%
  mutate(scen = str_sub(key, 1, 3),
         condition = str_sub(key, 4, 4), 
         item = str_sub(key, 5, 5)) %>%
  select(-key) %>%
  group_by(ResponseId, condition, scen) %>%
  summarise(response = mean(resp))

for (i in levels(as.factor(mm1scen$scen))) {
  temp = mm1scen %>% filter(scen == i)
  print(paste("Scenario = ", i))
  print(effectsize::cohens_d(temp$response, temp$condition))
}


# Are Difference and Equivalence endorsed equally?
mm1 %>%
  count(Endorsement) %>%
  mutate(prop = n / sum(n))


# t-test: Dichotomous measure
prop.test(xtabs(~ Endorsement, mm1), p = .5)

# Bipolar compassion 

# Chi-square test: Are Difference and Equivalence endorsed equally?
t.test(mm1$Bipolar, mu = 50)


# Does abstract endorsement predict people's preferences?
mod1 = lm(response ~ Endorsement*Methods, mm1) 
anova(mod1)

emmeans(mod1, pairwise ~ Endorsement | Methods)
cohens_d(subset(mm1, Endorsement == 'Equivalence')$response,subset(mm1, Endorsement == 'Equivalence')$condition)

mod2 = lm(response ~ Methods*Bipolar, mm1) 
anova(mod2)
slopes <- emtrends(mod2, ~ Methods, var = "Bipolar")
summary(slopes, infer = TRUE)