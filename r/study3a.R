
# Study 3a: Pre-registered factorial experiment

# Load packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(jtools)
library(emmeans)

# Load raw data
source("r/functions.R")
study3a <- read_qualtrics('data/study3a/study3a_anon.csv')

#Basic demographic summary 
demo <- study3a %>%
  distinct(anon_id, .keep_all = TRUE) %>%   
  mutate(
    gender_label = case_when(
      gender == 21 ~ "Male",
      gender == 22 ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  summarise(
    n_participants = n(),
    median_age = median(age, na.rm = TRUE),
    prop_female = mean(gender_label == "Female", na.rm = TRUE)
  )

print(demo)

mj <- study3a %>%
  select(matches("^(COG|COS|MEM|MOOD|PHY)_MJ[1-3]_[0-9]$"), anon_id) %>%
  mutate(across(-anon_id, as.numeric)) %>%
  pivot_longer(-anon_id, names_to = "key", values_to = "resp", values_drop_na = TRUE) %>%
  mutate(
    scenario = str_sub(key, 1, 3),
    item = str_sub(key, -5, -3)) %>%
  group_by(anon_id, scenario, item) %>%
  summarise(resp = mean(resp, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = item, values_from = resp, id_cols = c(anon_id, scenario)) %>%
  mutate(resp = (MJ1 + MJ2 + (100 - MJ3)) / 3,
         scenario = as.factor(scenario))

mc <- study3a %>%
  select(matches("^(COG|COS|MEM|MOOD|PHY)_(BE|NOR|EFF|REW)_[0-9]$"), anon_id) %>%
  mutate(across(-anon_id, as.numeric)) %>%
  pivot_longer(-anon_id, names_to = "key", values_to = "med", values_drop_na = TRUE) %>%
  mutate(
    scenario = str_sub(key, 1, 3),
    item = str_extract(key, "BE|NOR|EFF|REW")) %>%
  group_by(anon_id, scenario, item) %>%
  summarise(med = mean(med, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = item, values_from = med, id_cols = c(anon_id, scenario)) %>%
  rename(normality = NOR, effort = EFF, reward = REW, body = BE) %>%
  mutate(scenario = as.factor(scenario))

ivFactors <- study3a %>%
  select(matches("Internal|Effort|Reward|Normality", ignore.case = FALSE), anon_id) %>%
  pivot_longer(-anon_id, names_to = "key", values_to = "factorName", values_drop_na = TRUE) %>%
  mutate(
    scenario = str_to_upper(str_sub(key, 1, 3)),
    item = str_sub(key, 4, -1)) %>%
  filter(!is.na(anon_id), anon_id != "64441db380a4f44c2bfec904") %>%   # one excluded participant
  pivot_wider(names_from = item, values_from = factorName, id_cols = c(anon_id, scenario)) %>%
  unnest() %>%
  mutate(scenario = as.factor(scenario))

dtP5 <- full_join(mj, mc, by = c("anon_id", "scenario"))
dtP5 <- left_join(dtP5, ivFactors, by = c("anon_id", "scenario"))

dtP5 <- dtP5 %>%
  mutate(
    Internal = relevel(factor(Internal),  ref = "External"),
    Reward = relevel(factor(Reward),    ref = "Low"),
    Effort = relevel(factor(Effort),    ref = "High"),
    Normality = relevel(factor(Normality), ref = "High")
  )


m_body <- lmer(scale(body) ~ Internal + (1 | scenario), data = dtP5)
m_eff <- lmer(scale(effort) ~ Effort + (1 | scenario), data = dtP5)
m_norm <- lmer(scale(normality) ~ Normality + (1 | scenario), data = dtP5)
m_rew <- lmer(scale(reward) ~ Reward + (1 | scenario), data = dtP5)

mc_table <- bind_rows(
  summ(m_body, digits = 3)$coeftable %>% as.data.frame() %>%
    filter(row.names(.) != "(Intercept)") %>% mutate(check = "Body envelope"),
  summ(m_eff, digits = 3)$coeftable %>% as.data.frame() %>%
    filter(row.names(.) != "(Intercept)") %>% mutate(check = "Effort"),
  summ(m_norm, digits = 3)$coeftable %>% as.data.frame() %>%
    filter(row.names(.) != "(Intercept)") %>% mutate(check = "Normality"),
  summ(m_rew, digits = 3)$coeftable %>% as.data.frame() %>%
    filter(row.names(.) != "(Intercept)") %>% mutate(check = "Reward")
) 

mc_table


add_mod <- lmer(scale(resp) ~ Internal + Normality + Effort + Reward +
                  (1 | scenario), data = dtP5)

summ(add_mod, digits = 3, confint = TRUE)

cont_mod <- lmer(scale(resp) ~ body + normality + effort + reward +
                   (1 | scenario), data = dtP5)

summ(cont_mod, digits = 3, confint = TRUE)


cont_mod_std <- lmer(scale(resp) ~ scale(body) + scale(normality) + 
                       scale(effort) + scale(reward) + (1 | scenario), 
                     data = dtP5)

summ(cont_mod_std, digits = 3, confint = TRUE)


## Exploratory: Internal --> perceived effort / reward
eff_internal <- lmer(scale(effort) ~ Internal + Normality + Effort + Reward +
                       (1 | scenario), data = dtP5)
rew_internal <- lmer(scale(reward) ~ Internal + Normality + Effort + Reward +
                       (1 | scenario), data = dtP5)

summ(eff_internal, digits = 3)$coeftable %>%
  as.data.frame() %>% filter(row.names(.) == "InternalInternal")

summ(rew_internal, digits = 3)$coeftable %>%
  as.data.frame() %>% filter(row.names(.) == "InternalInternal")

# Save the additive model for later use in Study 3b
saveRDS(add_mod, "data/study3a/add_mod_study3a.rds")
saveRDS(dtP5, "data/study3a/dtP5_study3a.rds")