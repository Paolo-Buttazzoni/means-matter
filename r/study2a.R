
#Study 2a: open-ended justifications


# Load necessary libraries
library(tidyverse)
library(psych)
library(emmeans)
library(ggrepel)
library(lme4)
library(dplyr)
library(epitools)


# Load necessary functions
source("r/functions.R")

mm2a_raw <- read_qualtrics('data/study2a/study2a_anon.csv')
mm2a = mm2a_raw %>%
  filter(Progress == 100, !is.na(anon_id)) %>%
  mutate(
    Endorsement = if_else(choice == 4, 'Difference', 'Equivalence'),
    Conservatism = (politics_1 + politics_2 + politics_3) / 3,
    Religiosity = religiosity_1,
    Social = politics_3,
    Fiscal = politics_2,
    strata = factor(STUDY_ID, labels = c('Left_UK', 'Left_UK', 'Right_UK', 'Right_UK', 
                                         'Right_US', 'Right_US', 'Left_US', 'Left_US')),
    #left/right -> L/R
    politics = str_sub(strata, 1, 1),
    #extract UK or US
    country = str_sub(strata, -2))

glimpse(mm2a)
# Mean & SD for Age
mm2a %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age   = sd(age, na.rm = TRUE),
    n        = n()
  )

# Gender
mm2a %>%
  count(gender) %>%
  mutate(prop = n / sum(n))

# Religiosity
mm2a %>%
  count(Religiosity) %>%
  mutate(prop = n / sum(n))

# Political
mm2a %>%
  count(Conservatism) %>%
  mutate(prop = n / sum(n))


mm2a %>%
  summarise(
    n_below5 = sum(Conservatism < 5, na.rm = TRUE),
    prop_below5 = mean(Conservatism < 5, na.rm = TRUE),
    n_total = sum(!is.na(Conservatism)))

#endorsement

mm2a <- mm2a %>%
  mutate(length_char = if_else(!is.na(reasonsDiff), 
                               nchar(reasonsDiff), nchar(reasonsEquiv)), 
         spaces = if_else(!is.na(reasonsDiff), 
                          str_count(reasonsDiff, pattern = " "), 
                          str_count(reasonsEquiv, pattern = " ")))

xtabs(~ Endorsement, mm2a)
prop.test(xtabs(~ Endorsement, mm2a), p = .5)


codesI <- process_coder_data('data/study2a/coder1_anon.csv') %>% 
  pivot_longer(cols = -c(justID, anon_id), names_to = 'category', values_to = 'c1resp')

codesII <- process_coder_data('data/study2a/coder2_anon.csv') %>% 
  pivot_longer(cols = -c(justID, anon_id), names_to = 'category', values_to = 'c2resp')

codes <- full_join(codesI, codesII, by = c('justID', 'category', 'anon_id')) %>%
  mutate(status = if_else(c1resp == c2resp, 'Agreement', 'Disagreement'),
         present = case_when(
           c1resp + c2resp == 2 ~ 'Present',
           c1resp + c2resp == 0 ~ 'Absent'
         ))

# Inter-rater reliability
cohen_kappa <- psych::cohen.kappa(x = cbind(codes$c1resp, codes$c2resp))
cohen_kappa


tiebreak_raw <- read_csv('data/study2a/code_tiebreaker_anon.csv')

tiebreak_lookup <- tiebreak_raw %>%
  select(justID, category, anon_id, resolve)

codes_tb <- codes %>%
  left_join(tiebreak_lookup, by = c('justID', 'category', 'anon_id')) %>%
  mutate(
    Present = if_else(status == 'Disagreement' & resolve == 1, 'Present', present)
  )

codes_tb %>% count(Present)


mm2a_codes <- codes_tb %>%
  mutate(just_value = Present == 'Present') %>%
  full_join(mm2a, by = 'anon_id')

mod_just <- glmer(just_value ~ category * Endorsement + (1 | anon_id),
                  mm2a_codes, family = 'binomial')

emm_results <- emmeans(mod_just, pairwise ~ Endorsement | category, type = 'response')
emm_probs <- as.data.frame(emm_results$emmeans)

rr_table <- emm_probs %>%
  select(category, Endorsement, prob) %>%
  pivot_wider(names_from = Endorsement, values_from = prob) %>%
  mutate(
    RR_Equivalence_vs_Difference = round(Equivalence / Difference, 4),
    RR_Difference_vs_Equivalence = round(Difference / Equivalence, 4)
  )

rr_table %>% filter(category %in% c("UTILITY", "NOHARM", "AUTONOMY", "EFFORT", "NATURAL", "UNSAFE"))

# Contrasts (ratio, SE, z-ratio, p-value directly from emmeans)
emm_results$contrasts %>% 
  as.data.frame() %>% 
  filter(category %in% c("UTILITY", "NOHARM", "AUTONOMY", "EFFORT", "NATURAL", "UNSAFE"))



emm_only <- emmeans(mod_just, ~ Endorsement | category, type = 'response')

emm_contrasts_flipped <- pairs(emm_only, reverse = TRUE)

emm_contrasts_flipped %>% 
  as.data.frame() %>% 
  filter(category %in% c("UTILITY", "NOHARM", "AUTONOMY", "EFFORT", "NATURAL", "UNSAFE"))


#Figure 3 ####
tempdt <- codes %>%
  filter(present == 'Present') %>%
  left_join(mm2a, by = 'anon_id') %>%
  mutate(difference = Endorsement == 'Difference')

fig3 <- tempdt %>%
  group_by(category) %>%
  summarise(mean_diff = mean(difference, na.rm = TRUE) - .5, 
            freq = n()) %>%
  ggplot(aes(x = mean_diff, y = freq)) + 
  annotate(geom = 'rect', xmin = -.5, xmax = 0, ymin = 0, ymax = Inf, fill = 'grey', 
           alpha = .3) +
  annotate(geom = 'rect', xmin = 0, xmax = .5, ymin = 0, ymax = Inf, fill = 'firebrick', 
           alpha = .3) +
  geom_vline(xintercept = 0, linetype = 2, color = 'white', linewidth = 1) +
  theme_classic() +
  geom_col(width = .005, position = 'identity') + 
  geom_point(size = 1, fill = 'white', shape = 21, stroke = 1, color = 'black') +
  ggrepel::geom_label_repel(aes(label = category), xlim = c(-.5, .5), 
                            fontface = 'bold') + 
  scale_y_continuous(name = 'Frequency', limits = c(0, 30), expand = c(0, 0)) + 
  scale_x_continuous(name = 'Mean endorsement', limits = c(-.5, .53), 
                     labels = c('Equivalence', 'Both\nequally', 'Difference'), 
                     breaks = c(-.4, 0, .42),
                     expand = c(0, 0)) + 
  theme(axis.ticks = element_blank(),
        axis.title = element_text(size = 12, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position = 'bottom',
        plot.margin = margin(0.3,1,0.3, 1, "cm"),
        legend.title = element_text(size = 10))

ggsave('figures/figure3.png', dpi = 300, width = 17, height = 13, units = 'cm')