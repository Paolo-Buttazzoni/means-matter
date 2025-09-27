# Load necessary libraries
library(tidyverse)
library(emmeans)
library(effectsize)
library(psych)


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
    Order = if_else(str_sub(FL_21_DO, 1, 5) == 'ABSTR', 'Abstract1st', 'Scenarios1st'))  %>%
  select(ResponseId, Order, Methods, Age, Gender, Endorsement, endorsement_num, Bipolar, Conservatism, Social, Fiscal, Religiosity)

# --- Compare Conventional vs Technological DV (4 pairs) ---
mm1resp = mm1raw %>%
  select(., contains(c("C1","C2","C3","C4","T1","T2","T3","T4")),
         ResponseId, safe_1, conseq_1, AIchoice,
         AIscale_1:AIscale_18, OUS_1:OUS_9) %>%
  gather(-ResponseId, -safe_1, -conseq_1, -AIchoice, 
         -(AIscale_1:AIscale_18), -(OUS_1:OUS_9),
         key = 'key', value = 'resp', na.rm = TRUE) %>%
  mutate(scen = str_sub(key, 1, 3),
         condition = str_sub(key, 4, 4), 
         item = str_sub(key, 5, 5)) %>%
  select(-key) %>%
  group_by(ResponseId, condition, item, safe_1, conseq_1, AIchoice,
           across(AIscale_1:AIscale_18), across(OUS_1:OUS_9)) %>%
  summarise(response = mean(resp), .groups = "drop") %>%
  pivot_wider(
    id_cols = c(ResponseId, condition, safe_1, conseq_1, AIchoice,
                starts_with("AIscale_"), starts_with("OUS_")),
    names_from = 'item', values_from = 'response'
  ) %>%
  mutate(
    response = (`1` + `2` + `3` + `4`)/4,
    Safe = safe_1,
    Conseq = conseq_1,
    ActImp = case_when(AIchoice == 2 ~ "Impact", 
                       AIchoice == 1 ~ "Act"),
    actscore = rowMeans(select(., AIscale_1:AIscale_9), na.rm = TRUE),
    impscore = rowMeans(select(., AIscale_10:AIscale_18), na.rm = TRUE),
    ibscore  = rowMeans(select(., OUS_1:OUS_5), na.rm = TRUE) - 25,
    ihscore  = rowMeans(select(., OUS_6:OUS_9), na.rm = TRUE) - 25
  ) %>% 
  select(-safe_1, -conseq_1)



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

#Figure 1

fig1a = ggplot(mm1, aes(x = response, group = Methods, color = Methods))+
  geom_histogram(alpha = .5, aes(y = ..density..), fill = 'transparent') + 
  geom_density(linewidth = 1, aes(fill = Methods), alpha = .3) + 
  theme_minimal() + 
  scale_color_brewer(palette = 'Dark2') + 
  scale_fill_brewer(palette = 'Dark2') + 
  scale_y_continuous(name = NULL, breaks = NULL) + 
  scale_x_continuous(name = 'Moral judgment', 
                     breaks = c(0, 50, 100), 
                     labels = c('0\nMaximum\ndisapproval', '50', 
                                '100\nMaximum\napproval')) + 
  theme(legend.position = c(.2, .8), 
        plot.margin = margin(0.3,1,0.3, 1, "cm"))

fig1c = ggplot(data = mm1, aes(x = response))+
  geom_rect(data = endorse, aes(fill = Endorsement), alpha = .2, xmin = 0, xmax = 100, ymin = 0, ymax = 0.75) +
  scale_fill_manual(values = c("firebrick", "darkgrey")) +
  new_scale_fill() +   # reset fill scale
  geom_histogram(data = mm1, aes(x = response, group = Methods, color = Methods, y = ..density..), alpha = .5, fill = 'transparent') + 
  geom_density(data = mm1, aes(x = response, group = Methods, color = Methods, fill = Methods), linewidth = 1, alpha = .3) + 
  theme_minimal() + facet_grid(Endorsement ~ .) +
  scale_color_brewer(palette = 'Dark2') + 
  scale_fill_brewer(palette = 'Dark2') + 
  scale_y_continuous(name = NULL, breaks = NULL) + 
  scale_x_continuous(name = 'Moral judgment', 
                     breaks = c(0, 50, 100), 
                     labels = c('0\nMaximum\ndisapproval', '50', 
                                '100\nMaximum\napproval')) + 
  theme(legend.position = 'none', 
        plot.margin = margin(0.3,1,0.3, 1, "cm"))

endorse = data.frame(Endorsement = c('Equivalence', 'Difference'),
                     Proportion = c(140/334, 194/334),
                     response = 50)

fig1b = ggplot(endorse, aes(fill = Endorsement, y = Proportion, x = 0)) + 
  geom_hline(yintercept = .5, linetype = 1) +
  geom_col(position = 'stack', alpha = .5) + coord_flip() +
  annotate("rect", ymin = 0.3660039, ymax = 0.4742095, xmin = -Inf, xmax = Inf,
           alpha = .15, fill = "orange") +
  geom_hline(yintercept = 0.4191617, linetype = 2, color = "orange", linewidth = 1) +
  theme_minimal() + 
  scale_color_manual(values = c( 'firebrick', 'grey')) + 
  scale_fill_manual(values = c('firebrick', 'grey')) + 
  scale_x_continuous(name = NULL, breaks = NULL, limits = c(-.45, .45)) +
  scale_y_continuous(name = 'Proportion (Equivalence)', 
                     breaks = seq(0, 1, .2), 
                     labels = c('0%', '20%', '40%', '60%', '80%', '100%'), 
                     sec.axis = dup_axis(name = 'Proportion (Difference)', 
                                         breaks = seq(0, 1, .2), 
                                         labels = c('100%', '80%', '60%', '40%', '20%', '0%'))) + 
  theme(legend.position = 'none',
        plot.margin = margin(0.3,1,0.3, 1, "cm"))

ggarrange(fig1a, ggarrange(fig1b, fig1c, nrow = 2, heights = c(1, 2), 
                           labels = c('B', 'C')), 
          labels = c('A', NULL))

ggsave('Figures/Figure1.jpg', dpi = 300, width = 16, height = 12, units = 'cm')


# Supplementary Analysis 1. Do perceptions of safety predict a preference for conventional enhancement?

# Compute group stats
mm1 %>%
  group_by(Methods) %>%
  summarise(
    mean_Conseq = mean(Conseq, na.rm = TRUE),
    sd_Conseq   = sd(Conseq, na.rm = TRUE),
    mean_safe = mean(Safe, na.rm = TRUE),
    sd_safe   = sd(Safe, na.rm = TRUE),
    n = n()
  )

cor.test(mm1$Safe, mm1$Conseq, use = "pairwise.complete.obs")

alpha(mm1[, c("Safe", "Conseq")])

mm1$futureharm = (mm1$Conseq + mm1$Safe)/2 
meanConvFH = mean(subset(mm1, Methods == 'Conventional')$futureharm, na.rm = TRUE)
mm1$futureharm = mm1$futureharm - meanConvFH

mod3 = lm(response ~ futureharm*Methods, mm1)
anova(mod3)

emmeans(mod3, pairwise ~ Methods)

#Supplementary Figure 1

mm1 %>% group_by(Methods) %>% summarise(min(futureharm, na.rm = TRUE),
                                        max(futureharm, na.rm = TRUE))

ggplot(mm1, aes(x = futureharm, y = response, color = Methods)) +
  geom_vline(xintercept = -3.11, linetype = 2) +
  geom_vline(xintercept = 32.4, linetype = 2) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.7) +  
  geom_smooth(method = "lm") + 
  theme_classic() +
  ylim(0, 100) +
  scale_color_brewer(palette = "Set2") +
  labs(
    x = "Future Harm",
    y = "Moral Judgment",
    color = "Methods"
  ) +
  theme(
    legend.position = c(0.15, 0.15)
  )


ggsave('Figures/Sup_Figure1.jpg', dpi = 300, width = 16, height = 12, units = 'cm')


# Supplementary Analysis 2. Does endorsing impartial beneficence and instrumental harm predict 
# preferences for conventional and technological enhancements? 

mod4 = lm(response ~ (ibscore + ihscore)*Methods, mm1)
anova(mod4)
emtrends(mod4, pairwise ~ Methods, var = 'ibscore')
emtrends(mod4, pairwise ~ Methods, var = 'ihscore')

#Supplementary Figure 2

ggplot(mm1, aes(x = ihscore, y = response, color = Methods)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.7) +  
  geom_smooth(method = "lm", se = TRUE) + 
  theme_classic() +
  ylim(0, 100) +
  scale_color_brewer(palette = "Set2") +
  labs(
    x = "Instrumental Harm (IH score)",
    y = "Moral Judgment",
    color = "Methods"
  ) +
  theme(
    legend.position = c(0.15, 0.15)
  )

ggsave('Figures/Sup_Figure2.jpg', dpi = 300, width = 16, height = 12, units = 'cm')


# Supplementary Analysis 3. Act-Impact

xtabs(~ ActImp, mm1)
xtabs(~ ActImp + Endorsement, mm1)

t.test(ibscore ~ ActImp, mm1)
t.test(ihscore ~ ActImp, mm1)




# Supplementary Analysis 4. Mega

alpha(mm1[, c("Religiosity", "Social", "Fiscal")])
mm1$pol = (mm1$Religiosity + mm1$Social + mm1$Fiscal)/3




emtrends(mod4, pairwise ~ Methods, var = 'impscore')


mod3 = lm(resp ~ Religiosity+Social+(actscore + impscore)+(ihscore + ibscore), ge2, 
          subset = Methods == 'Technological')
jtools::summ(mod3, digits = 3, confint = TRUE)
anova(mod3)

mod3 = lm(resp ~ (actscore + impscore)*Methods, ge2)

interactions::interact_plot(mod3, pred = 'actscore', modx = 'Methods', interval = TRUE, 
                            plot.points = TRUE)

mod3 = lm(resp ~ (actrate + imprate)*Methods, ge2)
anova(mod3)

glimpse(ge2)


mod2 = lm(response ~ ActImp*Methods*Vignettes, mm1)
anova(mod2)

glimpse(mm1)

ge2 %>%
  select(AIscale_1:AIscale_18) %>%
  stats::factanal(., factor = 2)

t.test(actscore ~ ActImp, ge2)
t.test(impscore ~ ActImp, ge2)


effectsize::cohens_d(response ~ Methods, data = mm1)

mod4 = lm(response ~ (actscore + impscore)*Methods, mm1)
anova(mod4)


# common support
mod5 = lm(resp ~ Methods*(actscore), ge2, subset = futureharm > -3.1 & futureharm < 32.4)
anova(mod5)



ggplot(subset(ge2, impscore > 2), aes(x = actscore, y = resp, color = Methods))+
  geom_jitter()+geom_smooth(method = 'lm')+
  theme_minimal()+ylim(0,100)+
  scale_color_brewer(palette = 'Set2')

names(ge2)