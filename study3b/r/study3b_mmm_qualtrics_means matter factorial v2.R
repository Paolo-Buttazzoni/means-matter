library(tidyverse)
library(readxl)

filename = "study3b/study3b.csv"
headers = read_csv(filename, col_names = FALSE, n_max = 1)
p5 = read_csv(filename, skip = 3, col_names = FALSE)
colnames(p5)= headers

glimpse(p5)

mj = p5 %>%
  select(contains('MJ'), PROLIFIC_PID) %>%
  gather(-PROLIFIC_PID, key = 'key', value = 'resp', na.rm = TRUE) %>%
  mutate(scenario = str_sub(key, 1, 3), 
         item = str_sub(key, -5, -3)) %>%
  filter(!is.na(PROLIFIC_PID)) %>%
  pivot_wider(names_from = 'item', values_from = 'resp', id_cols = c('PROLIFIC_PID', 'scenario')) %>%
  unnest() %>%
  mutate(resp = (MJ1 + MJ2 + (100 - MJ3))/3)

mj$scenario = as.factor(mj$scenario)
levels(mj$scenario)

cheat = p5 %>%
  select(contains('_FAIR'), contains('_CHEAT'), PROLIFIC_PID) %>%
  gather(-PROLIFIC_PID, key = 'key', value = 'resp', na.rm = TRUE) %>%
  mutate(scenario = str_sub(key, 1, 3), 
         item = str_sub(key, 5, 8)) %>%
  filter(!is.na(PROLIFIC_PID)) %>%
  pivot_wider(names_from = 'item', values_from = 'resp', id_cols = c('PROLIFIC_PID', 'scenario')) %>% 
  mutate(fair = if_else(is.na(FAIR), `_FAI`, FAIR),
         cheat = if_else(is.na(CHEA), `_CHE`, CHEA)) %>%
  select(PROLIFIC_PID, scenario, fair, cheat) %>% unnest()

glimpse(mj)

mj %>%
  group_by(scenario) %>%
  summarise(mean(resp, na.rm = TRUE))

mc = p5 %>%
  select(matches("Com_", ignore.case = FALSE), PROLIFIC_PID) %>%
  gather(-PROLIFIC_PID, key = 'key', value = 'comp', na.rm = TRUE) %>%
  mutate(scenario = str_sub(key, -3, -1), 
         item = as.factor(str_sub(key, 5, 6)), 
         scenario = if_else(scenario == 'cog', 'Cog', scenario)) %>%
  filter(!is.na(PROLIFIC_PID)) %>%
  pivot_wider(names_from = 'item', values_from = 'comp', id_cols = c('PROLIFIC_PID', 'scenario')) %>%
  unnest() 

mc$scenario = as.factor(mc$scenario)
levels(mc$scenario) = c('COG', 'COS', 'MEM', 'MOO', 'PHY')

dtP5 = full_join(mj, mc, by = c('PROLIFIC_PID', 'scenario'))


dtP5b = full_join(dtP5, cheat, by = c('PROLIFIC_PID', 'scenario'))

sec_ratings= p5 %>%
  select(matches("_BE_1|_NOR_1|_EFF_1|_REW_1|_SAFE_1", ignore.case = FALSE), PROLIFIC_PID) %>%
  gather(-PROLIFIC_PID, key = 'key', value = 'med', na.rm = TRUE) %>%
  mutate(scenario = str_sub(key, 1, 3), 
         item = as.factor(str_sub(key, -5, -3))) %>%
  filter(!is.na(PROLIFIC_PID)) %>%
  pivot_wider(names_from = 'item', values_from = 'med', id_cols = c('PROLIFIC_PID', 'scenario')) %>%
  rename(BE = `_BE`) %>%
  rename('normality' = NOR, 'effort' = EFF, 'reward' = REW, 'body' = BE, 'safe' = AFE)

dtP5b = full_join(dtP5b, sec_ratings, by = c('PROLIFIC_PID', 'scenario'))

ivFactors = p5 %>%
  select(matches("Internal|Effort|Reward|Normality", ignore.case = FALSE), PROLIFIC_PID) %>%
  gather(-PROLIFIC_PID, key = 'key', value = 'factorName', na.rm = TRUE) %>%
  mutate(scenario = str_to_upper(str_sub(key, 1, 3)), 
         item = str_sub(key, 4, -1)) %>%
  filter(!is.na(PROLIFIC_PID), PROLIFIC_PID != '64441db380a4f44c2bfec904') %>%
  pivot_wider(names_from = 'item', values_from = 'factorName', id_cols = c('PROLIFIC_PID', 'scenario')) %>%
  unnest()

glimpse(ivFactors)

ivFactors$scenario = as.factor(ivFactors$scenario)
levels(ivFactors$scenario)

dtP5 = left_join(dtP5, ivFactors, by = c('PROLIFIC_PID', 'scenario'))

glimpse(dtP5)

sc = p5 %>%
  select(matches("_NAT|_SAFE|_FAIR|_CHEAT|_VALUE|_UTIL", ignore.case = FALSE), PROLIFIC_PID) %>%
  gather(-PROLIFIC_PID, key = 'key', value = 'med', na.rm = TRUE) %>%
  mutate(scenario = str_sub(key, 1, 3), 
         item = as.factor(str_sub(key, -5, -3))) %>%
  filter(!is.na(PROLIFIC_PID)) %>%
  pivot_wider(names_from = 'item', values_from = 'med', id_cols = c('PROLIFIC_PID', 'scenario')) %>%
  mutate(AIR = if_else(is.na(AIR), `IR `, AIR)) %>%
  select(-`IR `) %>%
  rename('safe' = AFE, 'fair' = AIR, 'cheat' = EAT, 'natural' = NAT) %>%
  unnest()

dtP5 = left_join(dtP5, sc, by = c('PROLIFIC_PID', 'scenario'))

sumdt5 = dtP5 %>%
  group_by(Effort, Reward, Normality, Internal, scenario) %>%
  summarise(moral = mean(resp, na.rm =TRUE), 
            effort = mean(effort, na.rm = TRUE),
            reward = mean(reward, na.rm = TRUE),
            normal = mean(normality, na.rm = TRUE),
            internal = mean(body, na.rm = TRUE))


xtabs(~ Ef + Effort, dtP5)
glimpse(dtP5)


dtP5 = dtP5 %>%
  mutate(pass_n = case_when(Normality == 'High' & No == 1 ~ 'pass',
                            Normality == 'Low' & No == 4 & scenario == 'COG' ~ 'pass',
                            Normality == 'Low' & No == 2 & scenario == 'COS' ~ 'pass',
                            Normality == 'Low' & No == 4 & scenario == 'MEM' ~ 'pass',
                            Normality == 'Low' & No == 2 & scenario == 'MOO' ~ 'pass',
                            Normality == 'Low' & No == 2 & scenario == 'PHY' ~ 'pass'),
         pass_e = case_when(Effort == 'High' & Ef == 1 & scenario %in% c('COG', 'MOO') ~ 'pass',
                            Effort == 'High' & Ef == 2 & scenario %in% c('PHY') ~ 'pass',
                            Effort == 'High' & Ef == 4 & scenario %in% c('COS', 'MEM') ~ 'pass',
                            Effort == 'Low' & Ef == 4 & scenario %in% c('COG') ~ 'pass',
                            Effort == 'Low' & Ef == 2 & scenario %in% c('MOO') ~ 'pass',
                            Effort == 'Low' & Ef == 1 & scenario %in% c('COS', 'MEM', 'PHY') ~ 'pass'),
         pass_r = case_when(Reward == 'High' & Re == 4 & scenario %in% c('COG', 'COS', 'MEM') ~ 'pass',
                            Reward == 'High' & Re == 2 & scenario %in% c('MOO', 'PHY') ~ 'pass',
                            Reward == 'Low' & Re == 1  ~ 'pass'),
         pass_i = case_when(Internal == 'External' & BE == 4 & scenario %in% c('COG', 'MEM') ~ 'pass',
                            Internal == 'External' & BE == 1 & scenario %in% c('COS', 'MOO', 'PHY') ~ 'pass',
                            Internal == 'Internal' & BE == 5 & scenario %in% c('COG') ~ 'pass',
                            Internal == 'Internal' & BE == 4 & scenario %in% c('COS') ~ 'pass',
                            Internal == 'Internal' & BE == 1 & scenario %in% c('MEM') ~ 'pass', 
                            Internal == 'Internal' & BE == 2 & scenario %in% c('MOO', 'PHY') ~ 'pass'), 
         total_pass = (pass_n == 'pass') + (pass_e == 'pass') + (pass_r == 'pass') + (pass_i == 'pass'))



dtP5 %>%
  group_by(scenario, Normality, No) %>%
  tally() %>%
  ggplot(aes(y = n, x = Normality, fill = as.factor(No))) + 
  geom_col(position = 'dodge') + facet_wrap(~ scenario)

dtP5 %>%
  group_by(scenario, Effort, Ef) %>%
  tally() %>%
  ggplot(aes(y = n, x = Effort, fill = as.factor(Ef))) + 
  geom_col(position = 'dodge') + facet_wrap(~ scenario)

dtP5 %>%
  group_by(scenario, Reward, Re) %>%
  tally() %>%
  ggplot(aes(y = n, x = Reward, fill = as.factor(Re))) + 
  geom_col(position = 'dodge') + facet_wrap(~ scenario)


dtP5 %>%
  group_by(scenario, Internal, BE) %>%
  tally() %>%
  ggplot(aes(y = n, x = Internal, fill = as.factor(BE))) + 
  geom_col(position = 'dodge') + facet_wrap(~ scenario)

  
mod1 = lmer(scale(body) ~ Internal + (1 | scenario), dtP5)
emmeans(mod1, pairwise ~ Internal)
jtools::summ(mod1, digits = 3)

mod1 = lmer(scale(normality) ~ Normality + (1 | scenario), dtP5)
emmeans(mod1, pairwise ~ Normality)
jtools::summ(mod1, digits = 3)

mod1 = lmer(scale(effort) ~ Effort + (1 | scenario), dtP5)
emmeans(mod1, pairwise ~ Effort)
jtools::summ(mod1, digits = 3)

mod1 = lmer(scale(reward) ~ Reward + (1 | scenario), dtP5)
emmeans(mod1, pairwise ~ Reward)

mod1 = lmer(body ~ Internal + (1 | scenario), dtP5)
emmeans(mod1, pairwise ~ Internal)


t.test(body ~ Internal, dtP5)
effectsize::cohens_d(dtP5$body ~ dtP5$Internal)

dtP5 %>%
  group_by(Internal) %>%
  summarise(mean(body, na.rm = TRUE))

dtP5 %>%
  group_by(Normality) %>%
  summarise(mean(normality, na.rm = TRUE))

t.test(normality ~ Normality, dtP5)
effectsize::cohens_d(dtP5$normality ~ dtP5$Normality)

scenTab = dtP5 %>%
  group_by(Internal, Effort, Reward, Normality) %>%
  tally()

View(scenTab)

dtP5 %>%
  group_by(Effort) %>%
  summarise(mean(effort, na.rm = TRUE))

t.test(effort ~ Effort, dtP5)
effectsize::cohens_d(dtP5$effort ~ dtP5$Effort)

dtP5 %>%
  group_by(Reward) %>%
  summarise(mean(reward, na.rm = TRUE))


mod1 = lmer(resp ~ Internal * Normality * Effort * Reward + (1 | scenario), dtP5)
car::Anova(mod1, test.statistic = 'F')

mod1 = lmer(resp ~ Internal * Normality * Effort * Reward + (1 | scenario), 
            subset(dtP5, total_pass == 4))
car::Anova(mod1, test.statistic = 'F')

mod1add = lmer(scale(resp) ~ Internal + Normality + Effort + Reward + (1 | scenario), 
               subset(dtP5, total_pass == 4))
jtools::summ(mod1add, digits = 3, confint = TRUE)

mod1add = lmer(scale(cheat) ~ Internal + Normality + Effort + Reward + (1 | scenario), 
               subset(dtP5b, total_pass == 4))
car::Anova(mod1add)

mod1add = lmer(scale(safe) ~ Internal + Normality + Effort + Reward + (1 | scenario), 
               subset(dtP5b, total_pass == 4))
car::Anova(mod1add)
jtools::summ(mod1add, digits = 3, confint = TRUE)

grand_effs_b = jtools::summ(mod1add, confint = TRUE)$coeftable %>%
  data.frame() %>%
  rename(estimate = Est., low_ci = X2.5., hi_ci = X97.5.) %>%
  mutate(effect = c(NA, 'Internal', 'Normality', 'Effort', 'Reward')) %>%
  filter(!is.na(effect)) %>%
  mutate(experiment = "Study 3b")

mod1b = lm(scale(resp) ~ Internal * Normality * Effort * Reward * scenario, dtP5)

temp_b = 
  bind_rows(
    emmeans(mod1b, pairwise ~ Internal | Normality * Effort * Reward * scenario)$contrasts %>%
      data.frame() %>% mutate(effect = 'Internal'),
    emmeans(mod1b, pairwise ~ Normality | Internal * Effort * Reward* scenario)$contrasts %>%
      data.frame() %>% mutate(effect = 'Normality'),
    emmeans(mod1b, pairwise ~ Effort | Normality * Internal * Reward* scenario)$contrasts %>%
      data.frame() %>% mutate(effect = 'Effort'),
    emmeans(mod1b, pairwise ~ Reward | Normality * Effort * Internal* scenario)$contrasts %>%
      data.frame() %>% mutate(effect = 'Reward')) %>%
  mutate(experiment = "Study 3b")

ggplot(temp, aes(x = reorder(effect, estimate), y = estimate)) + 
  geom_violin(trim = FALSE, draw_quantiles = c(.25, .75), fill = 'firebrick', alpha = .4
  ) + 
  geom_hline(yintercept = 0, linetype = 2) +
  geom_jitter(width = .05, size = .7, alpha = .6, shape = 21, fill = 'white') + 
  geom_linerange(data = grand_effs, aes(ymin = -hi_ci, ymax = -low_ci)) +
  geom_point(data = grand_effs, aes(y = -estimate), 
             fill = 'white', shape = 21, size = 3) +
  coord_flip() + 
  theme_classic() + 
  scale_y_continuous(name = 'Semi-standardized\nregression coefficients', breaks = seq(-1, 2, 1), 
                     limits = c(-1.4, 2.4)) + 
  scale_x_discrete(name = NULL) + 
  theme(legend.position = 'top',
        axis.text = element_text(size= 12),
        text = element_text(size= 12),
        legend.title = element_blank(),
        axis.ticks = element_blank())

ggsave('Figures/Figure5b.jpg', dpi = 300, width = 12, height = 8, units = 'cm')


mod1 = lmer(scale(cheat) ~ Internal * Normality * Effort * Reward + (1 | scenario), dtP5b)
car::Anova(mod1, test.statistic = 'F')
emmeans(mod1, pairwise ~ Internal)
emmeans(mod1, pairwise ~ Normality)

mod1 = lmer(scale(fair) ~ Internal * Normality * Effort * Reward + (1 | scenario), dtP5b)
car::Anova(mod1, test.statistic = 'F')
emmeans(mod1, pairwise ~ Internal)
emmeans(mod1, pairwise ~ Effort)
