
# Study 3b: Pre‑registered replication with strict comprehension criteria

# Load packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(jtools)
library(emmeans)
library(plyr)    

source("r/functions.R")  

# Load Study 3a objects needed for the combined figure ----
add_mod_study3a <- readRDS("data/study3a/add_mod_study3a.rds")
dtP5_study3a <- readRDS("data/study3a/dtP5_study3a.rds")

study3b <- read_qualtrics("data/study3b/study3b_anon.csv")


# Demographic summary
demo <- study3b %>%
  distinct(anon_id, .keep_all = TRUE) %>%
  mutate(
    gender_label = case_when(
      gender == 21 ~ "Male",
      gender == 22 ~ "Female",
      TRUE         ~ NA_character_
    )
  ) %>%
  dplyr::summarise(
    n_participants = n(),
    median_age     = median(age, na.rm = TRUE),
    prop_female    = mean(gender_label == "Female", na.rm = TRUE)
  )

demo


#Data preparation

mj_b <- study3b %>%
  select(matches("^(COG|COS|MEM|MOOD|PHY)_MJ[1-3]_[0-9]$"), anon_id) %>%
  mutate(across(-anon_id, as.numeric)) %>%
  pivot_longer(-anon_id, names_to = "key", values_to = "resp", values_drop_na = TRUE) %>%
  mutate(
    scenario = str_sub(key, 1, 3),
    item     = str_sub(key, -5, -3)
  ) %>%
  group_by(anon_id, scenario, item) %>%
  dplyr::summarise(resp = mean(resp, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = item, values_from = resp, id_cols = c(anon_id, scenario)) %>%
  mutate(resp = (MJ1 + MJ2 + (100 - MJ3)) / 3,
         scenario = as.factor(scenario))

mc_b <- study3b %>%
  select(matches("^(COG|COS|MEM|MOOD|PHY)_(BE|NOR|EFF|REW)_[0-9]$"), anon_id) %>%
  mutate(across(-anon_id, as.numeric)) %>%
  pivot_longer(-anon_id, names_to = "key", values_to = "med", values_drop_na = TRUE) %>%
  mutate(
    scenario = str_sub(key, 1, 3),
    item     = str_extract(key, "BE|NOR|EFF|REW")
  ) %>%
  group_by(anon_id, scenario, item) %>%
  dplyr::summarise(med = mean(med, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = item, values_from = med, id_cols = c(anon_id, scenario)) %>%
  dplyr::rename(normality = NOR, effort = EFF, reward = REW, body = BE) %>%
  mutate(scenario = as.factor(scenario))

ivFactors_b <- study3b %>%
  select(matches("Internal|Effort|Reward|Normality", ignore.case = FALSE), anon_id) %>%
  pivot_longer(-anon_id, names_to = "key", values_to = "factorName", values_drop_na = TRUE) %>%
  mutate(
    scenario = str_to_upper(str_sub(key, 1, 3)),
    item = str_sub(key, 4, -1)
  ) %>%
  filter(!is.na(anon_id)) %>%
  group_by(anon_id, scenario, item) %>%
  dplyr::summarise(factorName = first(factorName), .groups = "drop") %>%
  pivot_wider(names_from = item, values_from = factorName, id_cols = c(anon_id, scenario)) %>%
  mutate(scenario = as.factor(scenario))

cheat_b <- study3b %>%
  select(matches("^(COG|COS|MEM|MOOD|PHY)_(FAIR|CHEAT)_[0-9]$"), anon_id) %>%
  mutate(across(-anon_id, as.numeric)) %>%
  pivot_longer(-anon_id, names_to = "key", values_to = "resp", values_drop_na = TRUE) %>%
  mutate(
    scenario = str_sub(key, 1, 3),
    item = str_extract(key, "FAIR|CHEAT")
  ) %>%
  group_by(anon_id, scenario, item) %>%
  dplyr::summarise(resp = mean(resp, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = item, values_from = resp, id_cols = c(anon_id, scenario)) %>%
  select(anon_id, scenario, fair = FAIR, cheat = CHEAT)

safety_b <- study3b %>%
  select(matches("^(COG|COS|MEM|MOOD|PHY)_(SAFE|NAT)_[0-9]$"), anon_id) %>%
  mutate(across(-anon_id, as.numeric)) %>%
  pivot_longer(-anon_id, names_to = "key", values_to = "resp", values_drop_na = TRUE) %>%
  mutate(
    scenario = str_sub(key, 1, 3),
    item = str_extract(key, "SAFE|NAT")
  ) %>%
  group_by(anon_id, scenario, item) %>%
  dplyr::summarise(resp = mean(resp, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = item, values_from = resp, id_cols = c(anon_id, scenario)) %>%
  select(anon_id, scenario, safe = SAFE, natural = NAT)

dtP5b <- full_join(mj_b, mc_b, by = c("anon_id", "scenario"))
dtP5b <- left_join(dtP5b, ivFactors_b, by = c("anon_id", "scenario"))
dtP5b <- left_join(dtP5b, cheat_b, by = c("anon_id", "scenario"))
dtP5b <- left_join(dtP5b, safety_b, by = c("anon_id", "scenario"))

dtP5b <- dtP5b %>%
  mutate(
    Internal = relevel(factor(Internal),  ref = "External"),
    Reward = relevel(factor(Reward),    ref = "Low"),
    Effort = relevel(factor(Effort),    ref = "High"),
    Normality = relevel(factor(Normality), ref = "High")
  )

# Analyses (reported in manuscript)

mcheck_body_b <- lmer(scale(body) ~ Internal + (1 | scenario), data = dtP5b)
mcheck_eff_b <- lmer(scale(effort) ~ Effort + (1 | scenario), data = dtP5b)
mcheck_norm_b <- lmer(scale(normality) ~ Normality + (1 | scenario), data = dtP5b)
mcheck_rew_b <- lmer(scale(reward) ~ Reward + (1 | scenario), data = dtP5b)

mc_table_b <- bind_rows(
  summ(mcheck_body_b, digits = 3)$coeftable %>% as.data.frame() %>%
    filter(row.names(.) != "(Intercept)") %>% mutate(check = "Body envelope"),
  summ(mcheck_eff_b, digits = 3)$coeftable %>% as.data.frame() %>%
    filter(row.names(.) != "(Intercept)") %>% mutate(check = "Effort"),
  summ(mcheck_norm_b, digits = 3)$coeftable %>% as.data.frame() %>%
    filter(row.names(.) != "(Intercept)") %>% mutate(check = "Normality"),
  summ(mcheck_rew_b, digits = 3)$coeftable %>% as.data.frame() %>%
    filter(row.names(.) != "(Intercept)") %>% mutate(check = "Reward")
) 

mc_table_b

add_mod_b <- lmer(scale(resp) ~ Internal + Normality + Effort + Reward +
                    (1 | scenario), data = dtP5b)

summ(add_mod_b, digits = 3, confint = TRUE)

cont_mod_std_b <- lmer(scale(resp) ~ scale(body) + scale(normality) +
                         scale(effort) + scale(reward) + (1 | scenario),
                       data = dtP5b)

summ(cont_mod_std_b, digits = 3, confint = TRUE)

eff_int_b <- lmer(scale(effort) ~ Internal + Normality + Effort + Reward + (1 | scenario), data = dtP5b)
rew_int_b <- lmer(scale(reward) ~ Internal + Normality + Effort + Reward + (1 | scenario), data = dtP5b)
cheat_int_b <- lmer(scale(cheat)  ~ Internal + Normality + Effort + Reward + (1 | scenario), data = dtP5b)
safe_int_b <- lmer(scale(safe)   ~ Internal + Normality + Effort + Reward + (1 | scenario), data = dtP5b)

summ(eff_int_b, digits = 3)$coeftable %>% as.data.frame() %>% filter(row.names(.) == "InternalInternal")

summ(rew_int_b, digits = 3)$coeftable %>% as.data.frame() %>% filter(row.names(.) == "InternalInternal")

summ(cheat_int_b, digits = 3)$coeftable %>% as.data.frame() %>% filter(row.names(.) == "InternalInternal")

summ(safe_int_b, digits = 3)$coeftable %>% as.data.frame() %>% filter(row.names(.) == "InternalInternal")


# Figure 6

mod1a <- lm(scale(resp) ~ Internal * Normality * Effort * Reward * scenario, data = dtP5_study3a)

temp_a <- bind_rows(
  emmeans(mod1a, pairwise ~ Internal  | Normality * Effort * Reward * scenario)$contrasts %>% data.frame() %>% mutate(effect = "Internal"),
  emmeans(mod1a, pairwise ~ Normality | Internal * Effort * Reward * scenario)$contrasts %>% data.frame() %>% mutate(effect = "Normality"),
  emmeans(mod1a, pairwise ~ Effort    | Normality * Internal * Reward * scenario)$contrasts %>% data.frame() %>% mutate(effect = "Effort"),
  emmeans(mod1a, pairwise ~ Reward    | Normality * Effort * Internal * scenario)$contrasts %>% data.frame() %>% mutate(effect = "Reward")
) %>%
  mutate(experiment = "Study 3a")

mod1b <- lm(scale(resp) ~ Internal * Normality * Effort * Reward * scenario, data = dtP5b)

temp_b <- bind_rows(
  emmeans(mod1b, pairwise ~ Internal  | Normality * Effort * Reward * scenario)$contrasts %>% data.frame() %>% mutate(effect = "Internal"),
  emmeans(mod1b, pairwise ~ Normality | Internal * Effort * Reward * scenario)$contrasts %>% data.frame() %>% mutate(effect = "Normality"),
  emmeans(mod1b, pairwise ~ Effort    | Normality * Internal * Reward * scenario)$contrasts %>% data.frame() %>% mutate(effect = "Effort"),
  emmeans(mod1b, pairwise ~ Reward    | Normality * Effort * Internal * scenario)$contrasts %>% data.frame() %>% mutate(effect = "Reward")
) %>%
  mutate(experiment = "Study 3b")

grand_effs_a <- summ(add_mod_study3a, confint = TRUE, digits = 3)$coeftable %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  filter(term != "(Intercept)") %>%
  dplyr::rename(estimate = Est., low_ci = `2.5%`, hi_ci = `97.5%`) %>%
  mutate(
    effect = case_when(
      term == "InternalInternal" ~ "Internal",
      term == "NormalityLow"     ~ "Normality",
      term == "EffortLow"        ~ "Effort",
      term == "RewardHigh"       ~ "Reward"
    ),
    experiment = "Study 3a"
  )

grand_effs_b <- summ(add_mod_b, confint = TRUE, digits = 3)$coeftable %>%
  as.data.frame() %>%
  rownames_to_column("term") %>%
  filter(term != "(Intercept)") %>%
  dplyr::rename(estimate = Est., low_ci = `2.5%`, hi_ci = `97.5%`) %>%
  mutate(
    effect = case_when(
      term == "InternalInternal" ~ "Internal",
      term == "NormalityLow"     ~ "Normality",
      term == "EffortLow"        ~ "Effort",
      term == "RewardHigh"       ~ "Reward"
    ),
    experiment = "Study 3b"
  )

grand_all <- bind_rows(grand_effs_a, grand_effs_b)

plot_data <- bind_rows(temp_a, temp_b) %>%
  mutate(
    estimate = if_else(effect == "Reward", estimate, -estimate),
    experiment = factor(experiment, levels = c("Study 3a", "Study 3b"))
  )

grand_all <- grand_all %>%
  mutate(
    estimate = if_else(effect == "Reward", estimate, -estimate),
    low_ci   = if_else(effect == "Reward", low_ci, -low_ci),
    hi_ci    = if_else(effect == "Reward", hi_ci, -hi_ci)
  )

plot_data <- plot_data %>%
  mutate(effect = factor(effect, levels = c("Internal", "Normality", "Effort", "Reward")))

grand_all <- grand_all %>%
  mutate(effect = factor(effect, levels = c("Internal", "Normality", "Effort", "Reward")))

fig6 <- ggplot(plot_data, aes(x = effect, y = estimate, fill = experiment)) +
  geom_split_violin(
    trim = FALSE, draw_quantiles = c(0.25, 0.75),
    width = 0.8, linewidth = 0.3, scale = "width"
  ) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_jitter(
    aes(color = experiment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.3),
    size = 0.7, alpha = 0.6, shape = 21, fill = "white"
  ) +
  geom_linerange(
    data = grand_all,
    aes(ymin = low_ci, ymax = hi_ci),
    position = position_dodge(width = 0.3)
  ) +
  geom_point(
    data = grand_all,
    aes(y = estimate, group = experiment),
    fill = "white", shape = 21, size = 3,
    position = position_dodge(width = 0.3)
  ) +
  scale_fill_manual(values = c("Study 3a" = "skyblue", "Study 3b" = "brown")) +
  scale_color_manual(values = c("Study 3a" = "skyblue", "Study 3b" = "brown")) +
  scale_x_discrete(
    labels = c(
      "Internal"  = "Body Envelope\n(ref.: External)",
      "Effort"    = "Effort\n(ref.: High)",
      "Normality" = "Normality\n(ref.: High)",
      "Reward"    = "Reward\n(ref.: Small)"
    )) +
  labs(
    y = "Semi‑standardized regression coefficients",
    x = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = c(0.5, 0.95),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    axis.text = element_text(size = 12),
    text = element_text(size = 12),
    axis.ticks = element_blank()
  )

print(fig6)

ggsave("Figures/Figure6.png", fig6, width = 16, height = 9, units = "cm", dpi = 300)