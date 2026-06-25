library(tidyverse)
library(readxl)

filename = "study3a/study3a.csv"
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
  select(PROLIFIC_PID, scenario, fair, cheat)

glimpse(mj)

mj %>%
  group_by(scenario) %>%
  summarise(mean(resp, na.rm = TRUE))

mc = p5 %>%
  select(matches("_BE|_NOR|_EFF|_REW", ignore.case = FALSE), PROLIFIC_PID) %>%
  gather(-PROLIFIC_PID, key = 'key', value = 'med', na.rm = TRUE) %>%
  mutate(scenario = str_sub(key, 1, 3), 
         item = as.factor(str_sub(key, -5, -3))) %>%
  filter(!is.na(PROLIFIC_PID)) %>%
  pivot_wider(names_from = 'item', values_from = 'med', id_cols = c('PROLIFIC_PID', 'scenario')) %>%
  mutate(BE = if_else(is.na(`_BE`), `BE `, `_BE`)) %>%
  select(-`BE `, -`_BE`) %>%
  rename('normality' = NOR, 'effort' = EFF, 'reward' = REW, 'body' = 'BE')

mc$scenario = as.factor(mc$scenario)
levels(mc$scenario)

dtP5 = full_join(mj, mc, by = c('PROLIFIC_PID', 'scenario'))


dtP5b = full_join(dtP5, cheat, by = c('PROLIFIC_PID', 'scenario'))

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
  rename('safe' = AFE, 'fair' = AIR, 'cheat' = EAT, 'natural' = NAT, 'value' = LUE, 'conseq' = TIL)

dtP5 = left_join(dtP5, sc, by = c('PROLIFIC_PID', 'scenario'))

sumdt5 = dtP5 %>%
  group_by(Effort, Reward, Normality, Internal, scenario) %>%
  summarise(moral = mean(resp, na.rm =TRUE), 
            effort = mean(effort, na.rm = TRUE),
            reward = mean(reward, na.rm = TRUE),
            normal = mean(normality, na.rm = TRUE),
            internal = mean(body, na.rm = TRUE))




ggplot(sumdt5, aes(x = (effort - reward), y = moral, color = Internal)) + 
  geom_point(aes(color = Internal)) + geom_smooth(method = 'lm')

cor.test(~ internal + moral, sumdt5)

ggplot(sumdt5, aes(x = effort, y = moral)) + 
  geom_point(aes(color = Effort)) + geom_smooth(method = 'lm', aes(fill = Reward))

cor.test(~ effort + moral, sumdt5)

ggplot(sumdt5, aes(x = reward, y = moral)) + 
  geom_point(aes(color = Reward)) + geom_smooth(method = 'lm')

cor.test(~ reward + moral, sumdt5)

ggplot(sumdt5, aes(x = normal, y = moral)) + 
  geom_point(aes(color = Normality)) + geom_smooth(method = 'lm')

cor.test(~ normal + moral, sumdt5)

sumdt5 = sumdt5 %>%
  mutate(zInternal = internal - 50, 
         zReward = reward - 50, 
         zNormal = normal - 50, 
         zEffort = effort - 50)

mod4 = lm(moral ~ internal + effort + normal + reward, sumdt5)
car::Anova(mod4)
jtools::summ(mod4, digits = 3)

mod4 = lm(moral ~ zNormal + zReward + zInternal * zEffort, sumdt5)
car::Anova(mod4)
jtools::summ(mod4, digits = 3)

xtabs(~ Effort + Reward , dtP5)

ggplot(sumdt5, aes(x = effort, y = moral, color = Reward)) + 
  geom_point(aes(color = Reward)) + geom_smooth(method = 'lm')

cor.test(~ effort + moral, subset(sumdt5, Reward == 'Low'))
cor.test(~ effort + moral, subset(sumdt5, Reward == 'High'))


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

cor.test(~ resp + body, dtP5)
cor.test(~ resp + normality, dtP5)
cor.test(~ resp + effort, dtP5)
cor.test(~ resp + reward, dtP5)

mod1 = lmer(resp ~ Internal * Normality * Effort * Reward + (1 | scenario), dtP5)
car::Anova(mod1, test.statistic = 'F')

dtP5$Internal = relevel(factor(dtP5$Internal), ref = 'External')
dtP5$Reward = relevel(factor(dtP5$Reward), ref = 'Low')
dtP5$Effort = relevel(factor(dtP5$Effort), ref = 'High')
dtP5$Normality = relevel(factor(dtP5$Normality), ref = 'High')

mod1add = lmer(scale(resp) ~ Internal + Normality + Effort + Reward + (1 | scenario), dtP5)

grand_effs_a = jtools::summ(mod1add, confint = TRUE)$coeftable %>%
  data.frame() %>%
  rename(estimate = Est., low_ci = X2.5., hi_ci = X97.5.) %>%
  mutate(effect = c(NA, 'Internal', 'Normality', 'Effort', 'Reward')) %>%
  filter(!is.na(effect))  %>%
  mutate(experiment = "Study 3a")

mod1a = lm(scale(resp) ~ Internal * Normality * Effort * Reward * scenario, dtP5)

temp_a = 
  bind_rows(
    emmeans(mod1a, pairwise ~ Internal | Normality * Effort * Reward * scenario)$contrasts %>%
      data.frame() %>% mutate(effect = 'Internal'),
    emmeans(mod1a, pairwise ~ Normality | Internal * Effort * Reward* scenario)$contrasts %>%
      data.frame() %>% mutate(effect = 'Normality'),
    emmeans(mod1a, pairwise ~ Effort | Normality * Internal * Reward* scenario)$contrasts %>%
      data.frame() %>% mutate(effect = 'Effort'),
    emmeans(mod1a, pairwise ~ Reward | Normality * Effort * Internal* scenario)$contrasts %>%
      data.frame() %>% mutate(effect = 'Reward'))  %>%
  mutate(experiment = "Study 3a")

ggplot(bind_rows(temp_a, temp_b), aes(x = reorder(effect, -estimate), y = estimate)) + 
  geom_violin(trim = FALSE, draw_quantiles = c(.25, .75), fill = 'skyblue',
              scale = "width") + 
  geom_hline(yintercept = 0, linetype = 2) +
  geom_jitter(width = .05, size = .7, alpha = .6, shape = 21, fill = 'white') + 
  geom_linerange(data = grand_effs, aes(ymin = -hi_ci, ymax = -low_ci)) +
  geom_point(data = bind_rows(grand_effs_a, grand_effs_b), aes(y = -estimate), 
             fill = 'white', shape = 21, size = 3) +
  coord_flip() + 
  facet_grid(. ~ experiment) +
  theme_classic() + 
  scale_y_continuous(name = 'Semi-standardized\nregression coefficients', breaks = seq(-1, 2, 1), 
                     limits = c(-1.1, 2)) + 
  scale_x_discrete(name = NULL) + 
  theme(legend.position = 'top',
        axis.text = element_text(size= 12),
        text = element_text(size= 12),
        legend.title = element_blank(),
        axis.ticks = element_blank())

ggsave('Figures/Figure5.jpg', dpi = 300, width = 12, height = 8, units = 'cm')

grand_effs = bind_rows(grand_effs_a, 
                       grand_effs_b) %>%
  mutate(estimate = if_else(estimate < 0, -estimate, estimate),
         low_ci = if_else(abs(low_ci) > abs(hi_ci), -hi_ci, low_ci),
         hi_ci = if_else(abs(low_ci) > abs(hi_ci), -low_ci, hi_ci))

ggplot(bind_rows(temp_a, temp_b) %>%
         mutate(estimate = if_else(effect == "Reward", estimate, -estimate)), 
       aes(x = reorder(effect, estimate), y = estimate, 
           fill = experiment)) + 
  geom_split_violin(trim = FALSE, draw_quantiles = c(.25, .75),
                    width = .8, linewidth = .3, scale = "width") + 
  geom_hline(yintercept = 0, linetype = 2) +
  geom_jitter(aes(color = experiment), 
              position = position_jitterdodge(jitter.width = .2, 
                                              dodge.width = .3),
              size = .7, alpha = .6, shape = 21, fill = 'white') + 
 geom_linerange(data = bind_rows(grand_effs_a, grand_effs_b), 
                aes(ymin = hi_ci, ymax = low_ci), 
                position = position_dodge(width = .3)) +
  geom_point(data = bind_rows(grand_effs_a, grand_effs_b), 
             aes(y = estimate, group = experiment), 
             fill = 'white', shape = 21, size = 3, 
             position = position_dodge(width = .3)) +
  theme_classic() + 
  scale_y_continuous(name = 'Semi-standardized regression coefficients', breaks = seq(-2, 2, 1), 
                     limits = c(-2.2, 1.6)) + 
  scale_x_discrete(name = NULL,   labels = c(
    "Internal" = "Body Envelope\n(ref.: External)",
    "Reward" = "Reward\n(ref.: Small)",
    "Effort" = "Effort\n(ref.: High)",
    "Normality" = "Normality\n(ref.: High)"
  )) + 
  scale_fill_manual(values = c('skyblue', 'brown')) +
  scale_color_manual(values = c('skyblue', 'brown')) +
  theme(legend.position = c(.5, .95),
        legend.direction = "horizontal",
        axis.text = element_text(size= 12),
        text = element_text(size= 12),
        legend.title = element_blank(),
        axis.ticks = element_blank())

ggsave('Figures/Figure6MM.png', dpi = 300, width = 16, height = 9, units = 'cm')

mod1 = lmer(scale(cheat) ~ Internal * Normality * Effort * Reward + (1 | scenario), dtP5b)
car::Anova(mod1, test.statistic = 'F')
emmeans(mod1, pairwise ~ Internal)
emmeans(mod1, pairwise ~ Normality)

mod1 = lmer(scale(fair) ~ Internal * Normality * Effort * Reward + (1 | scenario), dtP5b)
car::Anova(mod1, test.statistic = 'F')
emmeans(mod1, pairwise ~ Internal)
emmeans(mod1, pairwise ~ Effort)

glimpse(dtP5)

emmeans(mod1, pairwise ~ Internal)
emmeans(mod1, pairwise ~ Normality)
emmeans(mod1, pairwise ~ Effort)
emmeans(mod1, pairwise ~ Reward | Effort)

jtools::summ(mod1, digits = 3, confint = TRUE)

mod1i = lmer(resp ~ Normality * Effort * Reward + (1 | scenario), subset(dtP5, !is.na(Reward)))
anova(mod1, mod1i)
mod1n = lmer(resp ~ Internal * Effort * Reward + (1 | scenario), dtP5)
anova(mod1, mod1n)
mod1e = lmer(resp ~ Internal * Normality * Reward + (1 | scenario), dtP5)
anova(mod1, mod1e)
  mod1r = lmer(resp ~ Internal * Normality * Effort + (1 | scenario), dtP5)
  anova(mod1, mod1r)

emmeans(mod1, pairwise ~ Internal)
emmeans(mod1, pairwise ~ Normality)
emmeans(mod1, pairwise ~ Effort)
emmeans(mod1, pairwise ~ Reward)
emmeans(mod1, pairwise ~ Reward | Effort)
emmeans(mod1, pairwise ~ Effort * Reward | Normality)
emmeans(mod1, pairwise ~ Effort | Normality)

mod2 = lmer(resp ~ body + normality + effort + reward + (1 | scenario), dtP5)
car::Anova(mod2, test.statistic = 'F')
jtools::summ(mod2, digits = 3, confint = TRUE, scale = TRUE, transform.response = TRUE)

t.test(reward ~ Reward, dtP5)
effectsize::cohens_d(dtP5$reward ~ dtP5$Reward)

ge4 = dtP5 %>%
  group_by(Normality, Reward, Effort, Internal, scenario) %>%
  summarise(resp = mean(resp, na.rm = TRUE))

ggplot(subset(dtP5, !is.na(Internal)), aes(x = resp, group = Internal, color = Internal))+
  geom_histogram(alpha = .5, aes(y = ..density.., fill = Internal), position = 'identity') + 
  geom_density(linewidth = 2) + 
  theme_minimal() + 
  scale_color_brewer(palette = 'Dark2') + 
  scale_fill_brewer(palette = 'Dark2') + 
  scale_y_continuous(name = NULL, breaks = NULL) + 
  scale_x_continuous(name = 'Moral judgment', 
                     breaks = c(0, 50, 100), 
                     labels = c('0\nMaximum\ndisapproval', '50', 
                                '100\nMaximum\napproval')) + 
  theme(legend.position = 'top', 
        plot.margin = margin(0.3,1,0.3, 1, "cm"))

ggsave('figExp4external.jpg', width = 7, height = 4, dpi = 400)


ggplot(subset(dtP5, !is.na(Normality)), aes(x = resp, group = Normality, color = Normality))+
  geom_histogram(alpha = .5, aes(y = ..density.., fill = Normality), position = 'identity') + 
  geom_density(linewidth = 2) + 
  theme_minimal() + 
  scale_color_brewer(palette = 'Dark2') + 
  scale_fill_brewer(palette = 'Dark2') + 
  scale_y_continuous(name = NULL, breaks = NULL) + 
  scale_x_continuous(name = 'Moral judgment', 
                     breaks = c(0, 50, 100), 
                     labels = c('0\nMaximum\ndisapproval', '50', 
                                '100\nMaximum\napproval')) + 
  theme(legend.position = 'top', 
        plot.margin = margin(0.3,1,0.3, 1, "cm"))

t.test(resp ~ Internal, dtP5)
effectsize::cohens_d(dtP5$resp ~ dtP5$Internal) # 0.17

dtP5 %>%
  group_by(scenario, Normality) %>%
  summarise(mean(resp, na.rm = TRUE), n())

t.test(resp ~ Normality, dtP5) 
effectsize::cohens_d(dtP5$resp ~ dtP5$Normality) # 0.07

condMeans = dtP5 %>%
  group_by(Normality, Internal, Effort, Reward, scenario) %>%
  summarise(m = mean(resp, na.rm = TRUE), 
            n = mean(normality, na.rm = TRUE), 
            e = mean(effort, na.rm = TRUE), 
            r = mean(reward, na.rm = TRUE), 
            b = mean(body, na.rm = TRUE)) %>%
  gather(m:b, key = 'depvar', value = 'resp')

ggplot(condMeans, aes(x = paste(Normality, Internal, Effort, Reward), 
                      y = resp, color = Reward)) + 
  geom_hline(yintercept = 50, linetype = 2) + 
  geom_jitter() + coord_flip() + facet_grid(scenario ~ depvar)

condMeans$EvR = condMeans$e - condMeans$r

cor.test(~ b + m, condMeans)

ggplot(condMeans, aes(x = b, y = m)) + 
  geom_point() + geom_smooth()

t.test(m ~ Internal, condMeans)
t.test(m ~ Reward, condMeans)
t.test(m ~ Effort, condMeans)

dtP5 %>%
  group_by(Effort, Reward, Normality) %>%
  summarise(mean(resp, na.rm = TRUE))

t.test(resp ~ Effort, dtP5)
effectsize::cohens_d(dtP5$resp ~ dtP5$Effort)

EvR = dtP5 %>%
  group_by(scenario, Reward, Effort) %>%
  summarise(moral = mean(resp, na.rm = TRUE))

ggplot(EvR, aes(x = scenario, y = moral)) + 
  geom_hline(yintercept = 50) + 
  facet_grid(. ~ Effort + Reward) + geom_point(aes(color = paste(Effort, Reward))) + coord_flip()

t.test(resp ~ Reward, dtP5)
effectsize::cohens_d(dtP5$resp ~ dtP5$Reward)

mod1 = lm(resp ~ Internal * Normality * Effort * Reward, dtP5)
car::Anova(mod1)
sjstats::anova_stats(mod1)

mod1 = lm(resp ~ scale(body) + scale(normality) + scale(effort) * scale(reward), dtP5)
car::Anova(mod1)
sjstats::anova_stats(mod1)

figIscen = dtP5 %>%
  group_by(scenario, Internal, Normality, Effort, Reward) %>%
  summarise(resp = mean(resp, na.rm = TRUE)) %>%
  filter(!is.na(Internal)) %>%
  mutate(unacceptable = as.numeric(Internal == 'Internal') + 
           as.numeric(Normality == 'Low') + as.numeric(Effort == 'Low') + as.numeric(Reward == 'High'), 
         unDich = unacceptable == 4, 
         acceptable = as.numeric(Internal == 'External') + 
           as.numeric(Normality == 'High') + as.numeric(Effort == 'High'), 
         acDich = unacceptable == 3, 
         class = case_when(
           unacceptable == 4 ~ 'Worst',
           acceptable == 3 ~ 'Best',
           unacceptable != 4 & acceptable != 3 ~ 'Control'
         ))

ggplot(figIscen, aes(x = reorder(paste(Internal, Normality, Effort, Reward), resp), y = resp, 
                     fill = class)) + 
  geom_line(aes(group = scenario), linetype = 2) +
  geom_point(shape = 21, aes(size = class))  + coord_flip() + 
  scale_fill_manual(values = c('white', 'grey', 'black')) + 
  scale_size_manual(values = c(2, 1, 2))

mod1 = lmer(zResp ~ iFactor + rFactor + nFactor + eFactor + rXe + eXn + rXn + (1 | scenario), dtP5)
car::Anova(mod1)

dtP5= dtP5 %>%
  mutate(iFactor = as.numeric(Internal == 'Internal'), 
         rFactor = as.numeric(Reward == 'High'), 
         nFactor = as.numeric(Normality == 'Low'), 
         eFactor = as.numeric(Effort == 'Low'), 
         rXe = rFactor * eFactor, 
         eXn = eFactor * nFactor, 
         rXn = rFactor * nFactor, 
         rXeXn = rFactor * nFactor * eFactor, 
         zResp = scale(resp))

mod2 = lmer(zResp ~ iFactor + nFactor + eFactor + rFactor + rXe + eXn + rXn + 
              body + normality + effort + reward + 
              (1 | scenario), dtP5)
car::Anova(mod2)
jtools::summ(mod2, digits = 3)
sjstats::anova_stats(mod2)

emmeans(mod2, pairwise ~ nFactor | eFactor * rFactor)

jtools::summ(mod2, digits = 3)

emtrends(mod2, pairwise ~ iFactor, var = 'body')
emtrends(mod2, pairwise ~ nFactor, var = 'normality')
emtrends(mod2, pairwise ~ rFactor, var = 'reward')
emtrends(mod2, pairwise ~ eFactor, var = 'effort')
emtrends(mod2, pairwise ~ eFactor, var = 'effort')


sjstats::anova_stats(mod2)




med2e = lme4::lmer(effort ~  iFactor + rFactor + nFactor + eFactor + rXe + (1 | scenario), dtP5)
jtools::summ(med2e, digits = 3)
car::Anova(med2e)

med2r = lme4::lmer(reward ~  iFactor + rFactor + nFactor + eFactor + rXe + (1 | scenario), dtP5)
jtools::summ(med2r, digits = 3)
car::Anova(med2r)

med2n = lme4::lmer(normality ~  iFactor + rFactor + nFactor + eFactor + rXe +  (1 | scenario), dtP5)
jtools::summ(med2n, digits = 3)
car::Anova(med2n)

med2b = lme4::lmer(body ~  iFactor + rFactor + nFactor + eFactor  + rXe +  (1 | scenario), dtP5)
jtools::summ(med2b, digits = 3)
car::Anova(med2b)

med2j = lme4::lmer(cheat ~  iFactor + rFactor + nFactor + eFactor  + rXe +  (1 | scenario), dtP5)
jtools::summ(med2j, digits = 3)
car::Anova(med2j)

library(mediation)

med.out1 <- summary(mediate(med2b, mod2, treat = "iFactor", mediator = "body", sims = 5000, dropobs = TRUE))
row1 = data.frame(ACME = med.out1$d0, Low_ACME = med.out1$d0.ci[1], Hi_ACME = med.out1$d0.ci[2], 
           ADE = med.out1$z0, Low_ADE = med.out1$z0.ci[1], Hi_ADE = med.out1$z0.ci[2], 
           predictor = 'Body envelope violation', mediator = 'Body envelope')

med.out2 <- summary(mediate(med2n, mod2, treat = "nFactor", mediator = "normality", sims = 5000, dropobs = TRUE))
row2 = data.frame(ACME = med.out2$d0, Low_ACME = med.out2$d0.ci[1], Hi_ACME = med.out2$d0.ci[2], 
                  ADE = med.out2$z0, Low_ADE = med.out2$z0.ci[1], Hi_ADE = med.out2$z0.ci[2], 
                  predictor = 'Abnormality', mediator = 'Normality')


med.out3 <- summary(mediate(med2e, mod2, treat = "eFactor", mediator = "effort", sims = 5000, dropobs = TRUE))
row3 = data.frame(ACME = med.out3$d0, Low_ACME = med.out3$d0.ci[1], Hi_ACME = med.out3$d0.ci[2], 
                  ADE = med.out3$z0, Low_ADE = med.out3$z0.ci[1], Hi_ADE = med.out3$z0.ci[2], 
                  predictor = 'Low effort', mediator = 'Effort')

med.out4 <- summary(mediate(med2e, mod2, treat = "iFactor", mediator = "effort", sims = 5000, dropobs = TRUE))
row4 = data.frame(ACME = med.out4$d0, Low_ACME = med.out4$d0.ci[1], Hi_ACME = med.out4$d0.ci[2], 
                  ADE = med.out4$z0, Low_ADE = med.out4$z0.ci[1], Hi_ADE = med.out4$z0.ci[2], 
                  predictor = 'Body envelope violation', mediator = 'Effort')

med.out5 <- summary(mediate(med2e, mod2, treat = "rXe", mediator = "effort", sims = 5000, dropobs = TRUE))
row5 = data.frame(ACME = med.out5$d0, Low_ACME = med.out5$d0.ci[1], Hi_ACME = med.out5$d0.ci[2], 
                  ADE = med.out5$z0, Low_ADE = med.out5$z0.ci[1], Hi_ADE = med.out5$z0.ci[2], 
                  predictor = 'Low effort + Large reward', mediator = 'Effort')

med.out6 <- summary(mediate(med2r, mod2, treat = "rFactor", mediator = "reward", sims = 5000, dropobs = TRUE))
row6 = data.frame(ACME = med.out6$d0, Low_ACME = med.out6$d0.ci[1], Hi_ACME = med.out6$d0.ci[2], 
                  ADE = med.out6$z0, Low_ADE = med.out6$z0.ci[1], Hi_ADE = med.out6$z0.ci[2], 
                  predictor = 'Large reward', mediator = 'Reward')

med.out7 <- summary(mediate(med2r, mod2, treat = "iFactor", mediator = "reward", sims = 5000, dropobs = TRUE))
row7 = data.frame(ACME = med.out7$d0, Low_ACME = med.out7$d0.ci[1], Hi_ACME = med.out7$d0.ci[2], 
                  ADE = med.out7$z0, Low_ADE = med.out7$z0.ci[1], Hi_ADE = med.out7$z0.ci[2], 
                  predictor = 'Body envelope violation', mediator = 'Reward')

figMeds = bind_rows(row1, row2, row3,  row5, row6) %>%
  gather(-predictor, -mediator, key = 'key', value = 'resp') %>%
  mutate(effect = if_else(str_detect(key, 'ADE'), 'Direct', 'Indirect'), 
         type = case_when(key %in% c('ADE', 'ACME') ~ 'point', 
                          str_detect(key, 'Low') ~ 'low', 
                          str_detect(key, 'Hi') ~ 'high')) %>%
  pivot_wider(id_cols = c('predictor', 'mediator', 'effect'), 
              names_from = 'type', values_from = 'resp')

ggplot(figMeds, aes(x = predictor, y = point, color = effect)) + 
  geom_linerange(aes(ymin = low, ymax = high)) + 
  geom_hline(linetype = 2, yintercept = 0)+ 
  geom_point() + facet_grid(mediator ~ effect, scales = 'free_y', space = 'free') +coord_flip()

figDirect = subset(figMeds, effect == 'Direct')
figIndirect = subset(figMeds, effect == 'Indirect') 

fig4a = ggplot(figDirect, aes(x = predictor, y = point, color = predictor)) + 
  geom_linerange(aes(ymin = low, ymax = high)) + 
  geom_hline(linetype = 2, yintercept = 0)+ 
  geom_point(aes(size = abs(point))) + facet_grid(predictor ~ ., 
                             scales = 'free_y', space = 'free') +
  scale_alpha_manual(values = c(.5, .9)) + 
  theme_minimal() + 
  scale_color_brewer(palette = 'Dark2') + 
  scale_fill_brewer(palette = 'Dark2') + 
  theme(legend.position = 'none', 
        strip.text = element_blank(),
        axis.text.y = element_blank())+coord_flip() +
  scale_y_continuous(name = 'Direct Effect') +
  scale_x_discrete(name = NULL)


fig4b = ggplot(figIndirect, aes(x = mediator, y = point, color = predictor)) + 
  geom_linerange(aes(ymin = low, ymax = high)) + 
  geom_hline(linetype = 2, yintercept = 0)+ 
  geom_point(aes(size = abs(point))) + facet_grid(predictor ~ ., 
                             scales = 'free_y', space = 'fixed') +
  scale_y_continuous(name = 'Indirect Effects') + 
  theme_minimal() + 
  scale_color_brewer(palette = 'Dark2') + 
  scale_fill_brewer(palette = 'Dark2') + 
  theme(legend.position = 'none')+coord_flip()  +
  scale_x_discrete(name = 'Mediator')

ggarrange(fig4a, fig4b)


ggplot(figMeds, aes(x = predictor, y = point, color = predictor, shape = effect)) + 
  geom_linerange(aes(ymin = low, ymax = high)) + 
  geom_hline(linetype = 2, yintercept = 0)+ 
  geom_point(aes(size = abs(point)), fill = 'white') + facet_grid(predictor ~ effect, 
                                                  scales = 'free_y', space = 'fixed') +
  scale_y_continuous(name = NULL) + 
  theme_minimal() + 
  scale_color_brewer(palette = 'Dark2') + 
  scale_fill_brewer(palette = 'Dark2') + 
  theme(legend.position = 'none',
        axis.text.y = element_blank(), 
        panel.grid.major.y = element_blank(),
        strip.text.y = element_text(angle = 0, face = 'bold'))+coord_flip()  +
  scale_x_discrete(name = NULL) + scale_shape_manual(values = c(16, 23))

ggsave('figExp5.jpg', width = 7, height = 4, dpi = 400)


dtP5 %>% group_by(Effort, Reward) %>% summarise(mean(resp, na.rm = TRUE), mean(fair, na.rm = TRUE))


figI = dtP5 %>%
  group_by(Internal) %>%
  summarise(resp = mean(resp, na.rm = TRUE), 
            body = mean(body, na.rm = TRUE))

fI = ggplot(dtP5, aes(x = body, y = resp)) + 
  geom_jitter(aes(color = Internal), alpha = .3) + 
  geom_smooth(method = 'lm', color = 'black', size = 2) + 
  geom_smooth(linetype = 3, aes(color = Internal, fill = Internal), method = 'lm', alpha = .2) + 
  geom_point(data = figI, size = 3, aes(color = Internal, fill = Internal), shape = 21)  + 
  coord_cartesian(ylim=c(30, 70))


figN = dtP5 %>%
  group_by(Normality) %>%
  summarise(resp = mean(resp, na.rm = TRUE), 
            normality = mean(normality, na.rm = TRUE))

fN = ggplot(dtP5, aes(x = normality, y = resp)) + 
  # geom_jitter(aes(color = Internal), alpha = .3) + 
  geom_smooth(method = 'lm', color = 'black', size = 2) + 
  geom_smooth(linetype = 3, aes(color = Normality, fill = Normality), method = 'lm', alpha = .2)  + 
  geom_point(data = figN, size = 3, aes(color = Normality, fill = Normality), shape = 21) + 
  coord_cartesian(ylim=c(30, 70))


figE = dtP5 %>%
  group_by(Effort) %>%
  summarise(resp = mean(resp, na.rm = TRUE), 
            effort = mean(effort, na.rm = TRUE))

fE = ggplot(dtP5, aes(x = effort, y = resp)) + 
  # geom_jitter(aes(color = Internal), alpha = .3) + 
  geom_smooth(method = 'lm', color = 'black', size = 2) + 
  geom_smooth(linetype = 3, aes(color = Effort, fill = Effort), method = 'lm', alpha = .2) + 
  geom_point(data = figE, size = 2, aes(fill = Effort, color = Effort), shape = 21)  + 
  coord_cartesian(ylim=c(30, 70))

figR = dtP5 %>%
  group_by(Reward) %>%
  summarise(resp = mean(resp, na.rm = TRUE), 
            reward = mean(reward, na.rm = TRUE))

fR = ggplot(dtP5, aes(x = reward, y = resp)) + 
  # geom_jitter(aes(color = Internal), alpha = .3) + 
  geom_smooth(method = 'lm', color = 'black', size = 2) + 
  geom_smooth(linetype = 3, aes(color = Reward, fill = Reward), method = 'lm', alpha = .2) + 
  geom_point(data = figR, size = 2, aes(fill = Reward, color = Reward), shape = 21)  + 
  coord_cartesian(ylim=c(30, 70))


ggarrange(fI, fN, fE, fR)

ggplot(dtP5, aes(x = effort, y = resp)) + 
 # geom_jitter(aes(color = Reward), alpha = .3) + geom_smooth(method = 'lm', color = 'black') + 
  geom_smooth(linetype = 2, aes(color = Reward, fill = Reward), method = 'lm')




sjstats::anova_stats(mod2)

mod2 = lmer(resp ~ Internal * Reward * Normality * Effort + (1 | scenario), dtP5)
car::Anova(mod2)
sjstats::anova_stats(mod2)

mod2 = lmer(resp ~ Reward * Normality * Effort + (1 | scenario), subset(dtP5, Internal == 'Internal'))
car::Anova(mod2)

mod2 = lmer(resp ~ Reward * Normality * Effort + (1 | scenario), subset(dtP5, Internal == 'External'))
car::Anova(mod2)
emmeans(mod2, pairwise ~ Effort | Reward)

emmeans(mod2, pairwise ~ Internal | Normality)


AIC(mod2)
jtools::summ(mod2)
sjstats::anova_stats(mod2)
emmeans(mod2, pairwise ~ Reward | Effort)
emmeans(mod2, pairwise ~ Internal | Normality)

dtP5 = dtP5 %>%
  mutate(just = (fair + cheat)/2, 
         eval = (MJ3 + fair + cheat + conseq + (100 - MJ1) + (100 - MJ2) + (100 - value))/7)

mod2 = lmer(cheat ~ Internal * Effort * Reward * Normality + (1 | scenario), dtP5)
car::Anova(mod2)
jtools::summ(mod2)
AIC(mod2)
emmeans(mod2, pairwise ~ Effort | Reward)
emmeans(mod2, pairwise ~ Internal)
emmeans(mod2, pairwise ~ Normality)

means5 = dtP5 %>%
  group_by(Internal, Effort, Reward, Normality) %>%
  summarise(resp = mean(resp, na.rm = TRUE))

ggplot(means5, aes(y = resp, x = reorder(paste(Effort, Reward, Normality), resp), color = Internal)) + 
  geom_point() + coord_flip() 

glimpse(dtP5)

cor.test(~ resp + fair, dtP5)
cor.test(~ resp + cheat, dtP5)
cor.test(~ resp + natural, dtP5)
cor.test(~ resp + safe, dtP5)

dtP5 = dtP5 %>%
  mutate(eMr = effort - reward, 
         ePr = effort + reward)

ggplot(subset(dtP5, reward > 50), aes(x = effort, y = reward, fill = resp, color = resp)) + geom_jitter(shape = 21, size = 2) + geom_smooth() + 
  scale_fill_gradient2(midpoint = 50, 
                       low = "red",
                       mid = "white",
                       high = "blue") + 
  scale_color_gradient2(midpoint = 50, 
                       low = "red",
                       mid = "white",
                       high = "blue") + theme_minimal()


mod5 = lm(resp ~ eMr + ePr, dtP5)
car::Anova(mod5)
jtools::summ(mod5, digits = 3)

dtP5 %>%
  select(MJ1:MJ3, fair:conseq) %>%
  filter(complete.cases(.)) %>%
  stats::factanal(., factors = 2)

dtP5 %>%
  select(MJ1:MJ3, fair:conseq) %>%
  psych::alpha(check.keys = TRUE)

emmeans(mod2, pairwise ~ Internal * Normality)
tMod = car::Anova(lmer(resp ~ Internal * Effort * Reward * Normality + (1 | scenario), dtP5))
rownames(tMod)
nrow(tMod)


### Set target N and resampling N
sampleSizeL = 50
sampleSizeH = 200
numSamples = 50
iterat = sampleSizeH - sampleSizeL + 1

## 1st power analysis
pValues1 = matrix(ncol= nrow(tMod) + 1, nrow=numSamples*iterat)
colnames(pValues1) = c("N", rownames(tMod))

pValues3 = matrix(ncol=4, nrow=numSamples*iterat)
colnames(pValues3) = c("N", "MC - NC", "MC - SC", "NC - SC")

for (i in 1:iterat) {
  for (j in 1:numSamples) {
    obs = (sampleSizeL + i - 1)*10
    Resampled = sample_n(dtP5, obs, replace=TRUE)
    rowNum = (i*numSamples + j) - numSamples
    
    bootModel = suppressMessages(lmer(resp ~ Internal * Effort * Reward * Normality + (1 | scenario), Resampled))
    output = car::Anova(bootModel, type = 2)
    pValues1[rowNum, ] = c(obs, output$`Pr(>Chisq)`)
  } 
  message('Iter ', i, ' of ', iterat)
}

pValues1 = as.data.frame(pValues1)

plotPwr1 = pValues1 %>%
  gather(-N, key = "term", value = "result") %>%
  mutate(sig = case_when(result <=.05  ~ "Significant",
                         result >.05  ~ "Not Significant"), 
         sig_num = case_when(result <=.05  ~ 1,
                             result >.05  ~ 0))

summaryPwr1 = plotPwr1 %>%
  group_by(term, N) %>% 
  summarize(power = mean(sig_num, na.rm = T))

ggplot(summaryPwr1, aes(x = N, y = power)) + 
  geom_hline(yintercept = .9, linetype = 2)+
  geom_point( alpha = .5) + geom_smooth()+
  xlab('Sample size')+theme_bw()+ facet_wrap(~ term) + 
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") 


