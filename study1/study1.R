# Load necessary libraries
library(tidyverse)
library(emmeans)
library(effectsize)
library(interactions)
library(psych)
library(ggnewscale)
library(ggpubr)

#'Do means matter morally?' study 1
# --- Step 1: Load and Clean Data ---

# Load dataset
filename = "study1/data/study1.csv"
headers = read_csv(filename, col_names = FALSE, n_max = 1)
mm1raw = read_csv(filename, skip = 3, col_names = FALSE)
colnames(mm1raw) = headers

# Filter to complete responses
mm1demog = mm1raw %>%
  filter(Progress == 100, !is.na(PROLIFIC_PID)) %>%
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

t.test(response ~ Methods, mm1)
mm1 %>%
  group_by(condition) %>%
  summarise(mean(response), 
            sd(response))

t.test(`1` ~ condition, mm1)
t.test(`2` ~ condition, mm1)
t.test(`3` ~ condition, mm1)
t.test(`4` ~ condition, mm1)

effectsize::cohens_d(mm1$response,mm1$condition)

anova(lm(response ~ Order*Methods, mm1))

endorse_resp = lm(response ~ Methods*Endorsement, mm1)
car::Anova(endorse_resp)
emmeans(endorse_resp, pairwise ~ Methods | Endorsement)
emmeans(endorse_resp, pairwise ~ Endorsement | Methods)

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

# Chi-square test: Are Difference and Equivalence endorsed equally?
xtabs(~ Endorsement, mm1)
prop.test(xtabs(~ Endorsement, mm1), p = .5)

endorsement_order = glm(endorsement_num ~ Order*Methods, mm1, family = 'binomial') 
emmeans(endorsement_order, pairwise ~ Order, type = 'response')

# Chi-square test: Are Difference and Equivalence endorsed equally?
t.test(mm1$Bipolar, mu = 50)

bipolar_order = lm(Bipolar ~ Order*Methods, mm1) 
anova(bipolar_order)
emmeans(bipolar_order, pairwise ~ Order, type = 'response')

effectsize::cohens_d(subset(mm1, Endorsement == 'Equivalence')$response,subset(mm1, Endorsement == 'Equivalence')$condition)


fig1a = ggplot(mm1, aes(x = response, group = Methods, color = Methods))+
  geom_histogram(alpha = .5, aes(y = ..density..), fill = 'transparent') + 
  geom_density(linewidth = 1, aes(fill = Methods), alpha = .3) + 
  theme_minimal() + 
  scale_color_brewer(palette = 'Dark2', 
                     labels = c('Familiar', 'Technologically novel')
                     ) + 
  scale_fill_brewer(palette = 'Dark2',
                    labels = c('Familiar', 'Technologically novel')
  ) + 
  scale_y_continuous(name = NULL, breaks = NULL) + 
  scale_x_continuous(name = 'Moral judgment', 
                     breaks = c(0, 50, 100), 
                     labels = c('0\nMaximum\ndisapproval', '50', 
                                '100\nMaximum\napproval')) + 
  theme(legend.position = c(.3, .8), 
        text = element_text(family = "Helvetica Neue"),
        plot.margin = margin(0.3,1,0.3, 1, "cm"))

fig1c = ggplot(data = mm1, aes(x = response))+
  geom_rect(data = endorse, aes(fill = Endorsement), alpha = .2, xmin = 0, xmax = 100, ymin = 0, ymax = Inf) +
  scale_fill_manual(values = c("firebrick", "darkgrey")) +
  new_scale_fill() +   # reset fill scale
  geom_histogram(data = mm1, 
                 aes(x = response, group = Methods, color = Methods, 
                     y = ..density..), alpha = .5, fill = 'transparent') + 
  geom_density(data = mm1, 
               aes(x = response, group = Methods, color = Methods, 
                   fill = Methods), linewidth = 1, alpha = .3) + 
  theme_minimal() + facet_grid(Endorsement ~ .) +
  scale_color_brewer(palette = 'Dark2') + 
  scale_fill_brewer(palette = 'Dark2') + 
  scale_y_continuous(name = NULL, breaks = NULL) + 
  scale_x_continuous(name = 'Moral judgment', 
                     breaks = c(0, 50, 100), 
                     labels = c('0\nMaximum\ndisapproval', '50', 
                                '100\nMaximum\napproval')) + 
  theme(legend.position = 'none', 
         text = element_text(family = "Helvetica Neue"),
        plot.margin = margin(0.3,1,0.3, 1, "cm"))


xtabs(~ Endorsement, mm1)

endorse = mm1 %>%
  mutate(total = n()) %>%
  group_by(Endorsement, total) %>%
  summarise(freq = n()) %>%
  reframe(Proportion = DescTools::BinomCI(freq, total, method = 'wilson')[1],
          lower = DescTools::BinomCI(freq, total, method = 'wilson')[2],
          upper = DescTools::BinomCI(freq, total, method = 'wilson')[3]) %>%
  mutate(response = 50)
            
fig1b = ggplot(endorse, aes(fill = Endorsement, y = Proportion, x = 0)) + 
  geom_hline(yintercept = .5, linetype = 1) +
  geom_col(position = 'stack', alpha = .5) + coord_flip() +
  annotate("rect", ymin = 0.402, ymax = 0.526, xmin = -Inf, xmax = Inf,
           alpha = .15, fill = "orange") +
  geom_hline(yintercept = 0.463, linetype = 2, color = "orange", linewidth = 1) +
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
        text = element_text(family = "Helvetica Neue"),
        plot.margin = margin(0.3,1,0.3, 1, "cm"))

ggarrange(fig1a, ggarrange(fig1b, fig1c, nrow = 2, heights = c(1, 2), 
                                    labels = c('B', 'C')), 
                  labels = c('A', NULL))

ggsave('Figures/Figure1MM.png', dpi = 300, width = 20, height = 12, units = 'cm')
