
#Study 1a: Matched vignette pairs across enhancement domains 


# Load necessary libraries
library(tidyverse)
library(emmeans)
library(effectsize)
library(interactions)
library(psych)
library(ggnewscale)
library(ggpubr)

# Load raw data
source("r/functions.R")
mm1a_raw <- read_qualtrics('data/study1a/study1a_anon.csv')



# Filter to complete responses
mm1demog = mm1raw %>%
  filter(Progress == 100, !is.na(ResponseId)) %>%
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
  select(ResponseId, Order, Methods, Age, Gender, Endorsement, endorsement_num, 
         Bipolar, Conservatism, Social, Fiscal, Religiosity)

mm1resp = mm1rAgemm1resp = mm1raw %>%
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

# Overall alpha (across conditions)
mm1resp %>%
  ungroup() %>%
  group_by(condition) %>%
  group_map(~ tibble(raw_alpha = alpha(select(.x, `1`, `2`, `3`, `4`))$total$raw_alpha)) %>%
  bind_rows(.id = "condition")

mm1 = full_join(mm1demog, mm1resp) %>%
  filter(!is.na(Methods))

#Demographic info ####
mm1demog %>%
  summarise(
    N = n(),
    Mean_Age = mean(Age, na.rm = TRUE),
    SD_Age = sd(Age, na.rm = TRUE))

mm1demog %>%
  count(Gender) %>%
  mutate(Proportion = n / sum(n))

mm1demog %>%
  count(Religiosity) %>%
  mutate(Proportion = n / sum(n))

mm1demog %>%
  count(Conservatism) %>%
  mutate(Proportion = n / sum(n))

#Results ####
mm1 %>%
  group_by(condition) %>%
  summarise(mean(response), 
            sd(response))

t.test(response ~ Methods, mm1)


effectsize::cohens_d(mm1$response,mm1$condition)

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


xtabs(~ Endorsement, mm1)
prop.test(xtabs(~ Endorsement, mm1), p = .5)

t.test(mm1$Bipolar, mu = 50)


endorse_resp = lm(response ~ Methods*Endorsement, mm1)
car::Anova(endorse_resp)
emmeans(endorse_resp, pairwise ~ Endorsement | Methods)
effectsize::cohens_d(subset(mm1, Endorsement == 'Equivalence')$response,subset(mm1, Endorsement == 'Equivalence')$condition)




# Figure 1 ####
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

ggsave('figures/figure1.png', dpi = 300, width = 20, height = 12, units = 'cm')