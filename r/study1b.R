
#Study 1b: Nationally representative survey data

# DATA SOURCE
# This study uses data from the Pew Research Center's American Trends
# Panel, Wave 99: "AI and Human Enhancement" (fielded Nov. 1-7, 2021),
# report published March 17, 2022:
# https://www.pewresearch.org/internet/2022/03/17/ai-and-human-enhancement-americans-openness-is-tempered-by-a-range-of-concerns/
#
# Survey materials (questionnaire/topline) are reproduced on p. 134
# of the report PDF.
#
# Both the dataset and materials require a free Pew Research Center
# account to access. Create an account / log in here, then download
# American Trends Panel Wave 99:
# https://www.pewresearch.org/dataset/american-trends-panel-wave-99/

library(haven)
library(tidyverse)
library(lmerTest)
library(emmeans)


pew <- read_sav("private/study1b/data/ATP_W99_study1b.sav")
glimpse(pew)

demog_pew = pew %>%
  select(QKEY, religiosity = F_RELCOM3CAT, 
         age_cat = F_AGECAT, gender = F_GENDER, 
         education = F_EDUCCAT2, politics = F_IDEO) %>%
  mutate(religiosity = if_else(religiosity == 99, NA, 5 - religiosity),
         politics = if_else(politics == 99, NA, 6 - politics),
         age_cat = if_else(age_cat == 99, NA, age_cat),
         gender = if_else(gender > 2, NA, 3 - gender),
         education = if_else(education == 99, NA, education))

views_pew = pew %>%
  select(QKEY, EXOV5_W99, GENEV5_W99, BCHIP5_W99) %>%
  gather(-QKEY, key = 'item', value = 'view', na.rm = TRUE) %>%
  mutate(domain = str_sub(item, 1, 4)) %>%
  filter(view <= 3) %>%
  select(-item) %>%
  mutate(view_label = case_when(view == 1 ~ 'Equivalence',
                                view == 2 ~ 'Difference'),
         equivalence = 2 - view) %>%
  full_join(., demog_pew, by = 'QKEY')

xtabs(~ view + domain, views_pew)

views_pew_long = views_pew %>%
  gather(religiosity:politics, key = 'predictor', value = 'value', na.rm = TRUE) %>%
  mutate(pred_label = 
           case_when(predictor == 'age_cat' ~ 'Age',
                     predictor == 'education' ~ 'Educational attainment',
                     predictor == 'gender' ~ 'Gender',
                     predictor == 'politics' ~ 'Political orientation',
                     predictor == 'religiosity' ~ 'Religiosity'), 
         domain_label = 
           case_when(
             domain == 'BCHI' ~ 'Brain-Computer Interface',
             domain == 'EXOV' ~ 'Robotic Exoskeleton',
             domain == 'GENE' ~ 'Embryonic Gene Editing'
           ))

views_pew_long %>%
  filter(!is.na(domain)) %>%
  group_by(domain_label) %>%
  summarise(equivalence = mean(equivalence, na.rm = TRUE))


# 1. Demographic predictors of Equivalence vs Difference view
demo_model <- glmer(equivalence ~ religiosity + age_cat + gender + education + politics + 
                      (1 | QKEY) + (1 | domain), 
                    data = views_pew, family = "binomial")

summary(demo_model)
exp(fixef(demo_model))  # ORs (toward Equivalence)

# religiosity and politics reported toward the Difference view in the paragraph:
1 / exp(fixef(demo_model)[c("religiosity", "politics")])


# 2 & 3. Attitudes toward enhancement AND therapeutic uses
attitudes_pew <- pew %>%
  select(QKEY, EXOV12_a_W99:EXOV12_c_W99, GENEV13_a_W99:GENEV13_b_W99, 
         BCHIP14_a_W99:BCHIP14_d_W99) %>% 
  gather(-QKEY, key = 'item', value = 'response', na.rm = TRUE) %>%
  mutate(domain = str_sub(item, 1, 4), 
         resp_ord = case_when(response == 1 ~ 1, 
                              response == 2 ~ 3, 
                              response == 3 ~ 2), 
         use = case_when(
           item %in% c('BCHIP14_a_W99', 'BCHIP14_b_W99', 'GENEV13_b_W99',
                       'EXOV12_a_W99') ~ 'Treatment',
           item %in% c('BCHIP14_c_W99', 'BCHIP14_d_W99', 'GENEV13_a_W99',
                       'EXOV12_b_W99') ~ 'Enhancement'
         )) %>%
  filter(response <= 3)

# Join view/view_label/equivalence in ONCE.
attitudes_pew <- full_join(attitudes_pew, views_pew, by = c('QKEY', 'domain'))

attitudes_pew <- attitudes_pew %>%
  mutate(
    approval   = as.integer(resp_ord == 1),
    ambivalent = as.integer(resp_ord == 2),
    opposition = as.integer(resp_ord == 3)
  )

# One model per outcome, fit on the full data, use * view_label interaction —
approval_model <- glmer(
  approval ~ use * view_label + (1 | domain) + (1 | QKEY),
  data   = attitudes_pew,
  family = binomial
)
emmeans(approval_model,   revpairwise ~ view_label | use, type = 'response')

ambivalent_model <- glmer(
  ambivalent ~ use * view_label + (1 | domain) + (1 | QKEY),
  data   = attitudes_pew,
  family = binomial
)
emmeans(ambivalent_model, revpairwise ~ view_label | use, type = 'response')

opposition_model <- glmer(
  opposition ~ use * view_label + (1 | domain) + (1 | QKEY),
  data   = attitudes_pew,
  family = binomial
)
emmeans(opposition_model, revpairwise ~ view_label | use, type = 'response')


#Figure 2

# Figure 2a: demographic predictors of Equivalence, by domain
mean_views = views_pew_long %>%
  filter(!is.na(domain)) %>%
  group_by(pred_label, domain_label, value) %>%
  summarise(equivalence = mean(equivalence, na.rm = TRUE))

fig2a = ggplot(views_pew_long, aes(x = value, y = equivalence)) + 
  geom_hline(yintercept = .5, linetype = 2, linewidth = .3) +
  stat_smooth(
    method = "glm",
    method.args = list(family = binomial),
    aes(alpha = pred_label == 'Gender', linetype = domain_label,
        linewidth = pred_label == 'Gender'),
    fullrange = TRUE,
    color = '#686868',
    fill = '#686868', 
    linewidth = .4
  ) +
  scale_alpha_manual(values = c(.4, .02), guide = "none") +
  geom_point(data = mean_views, 
             shape = 21, size = 2, fill = 'white', stroke = .5, 
             show.legend = FALSE) +
  facet_wrap(~ pred_label, nrow = 1, scales = 'free_x') +
  scale_y_continuous(name = 'Endorsement', breaks = c(0, .5, 1),
                     labels = NULL,
                     limits = c(0, 1), expand = c(0, 0),
                     sec.axis = dup_axis(name = NULL,
                                         labels = c('Diff.\n', '', '\nEquiv.'))) + 
  scale_x_continuous(name = NULL) + 
  scale_linetype_manual(values = c(3, 2, 1)) +
  theme_classic() + 
  theme(legend.position = c(.5, 1.15), 
        legend.direction = 'horizontal',
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = "transparent"),
        axis.text.x = element_blank(),
        axis.line = element_blank(),
        plot.margin = margin(t = 25, r = 5, b = 2, l = 5),
        panel.border = element_rect(colour = 'black', linewidth = .5),
        axis.ticks = element_blank(),
        strip.background = element_blank())

fig2a

# Figure 2b: approval/ambivalence/opposition by view and use
attitudes_pew_fig = attitudes_pew %>% ungroup() %>%
  filter(!is.na(view_label), !is.na(use)) %>%
  group_by(domain, use, item, view_label) %>%
  summarise(resp0 = -mean(resp_ord == 3, na.rm = TRUE),
            resp1 = mean(resp_ord == 1, na.rm = TRUE),
            resp0_h = -DescTools::BinomCI(sum(resp_ord == 3, na.rm = TRUE),
                                          sum(!is.na(resp_ord)))[2],
            resp0_l = -DescTools::BinomCI(sum(resp_ord == 3, na.rm = TRUE),
                                          sum(!is.na(resp_ord)))[3],
            resp1_l = DescTools::BinomCI(sum(resp_ord == 1, na.rm = TRUE),
                                         sum(!is.na(resp_ord)))[2],
            resp1_h = DescTools::BinomCI(sum(resp_ord == 1, na.rm = TRUE),
                                         sum(!is.na(resp_ord)))[3],
            diff = resp1 + resp0) %>%
  mutate(item_label = case_when(
    item == "EXOV12_a_W99" ~ "To improve the quality of life for people with physical limitations",
    item == "EXOV12_b_W99" ~  "To give people greater strength for recreational activities",
    item == "GENEV13_a_W99" ~ "To change a baby's physical characteristics to make them more attractive",
    item == "GENEV13_b_W99" ~ "To treat serious diseases or health conditions that a person currently has,",
    item == "BCHIP14_a_W99" ~  "To treat age-related decline in mental abilities",
    item == "BCHIP14_b_W99"  ~  "To allow increased movement for people who are paralyzed",
    item == "BCHIP14_c_W99" ~ "To make it possible for thoughts in the brain to search content on the internet without typing",
    item == "BCHIP14_d_W99" ~ "To translate thoughts in the brain, without speaking, into text on a screen"   
  ),
  domain_label = 
    case_when(
      domain == 'BCHI' ~ 'Brain-Computer Interface',
      domain == 'EXOV' ~ 'Robotic Exoskeleton',
      domain == 'GENE' ~ 'Embryonic Gene Editing'
    ))

fig2b = ggplot(attitudes_pew_fig, aes(x = item)) +
  geom_col(aes(y = resp1, group = view_label, alpha = view_label), width = .35, 
           fill = 'darkgreen', position = position_dodge(width = .5)) +
  geom_col(aes(y = resp0, group = view_label, alpha = view_label), width = .35, 
           fill = 'firebrick', position = position_dodge(width = .5)) +
  geom_linerange(aes(ymin = resp1_l, ymax = resp1_h, group = view_label, alpha = view_label),
                 color = 'darkgreen', position = position_dodge(width = .5)) +
  geom_linerange(aes(ymin = resp0_l, ymax = resp0_h,  group = view_label, alpha = view_label),
                 color = 'firebrick', position = position_dodge(width = .5)) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_y_continuous(name = "Proportion", labels = c("100%", "\nOppose", "", 
                                                     "\nFavor", "100%"),
                     expand = c(0, 0),
                     limits = c(-1, 1), breaks = seq(-1, 1, .5)) +
  scale_x_discrete(name = NULL, breaks = NULL, expand = c(0.12, 0.12)) +
  scale_alpha_manual(values = c(.9, .4), guide = 'none') +
  facet_wrap(domain_label ~ reorder(item_label, diff),  ncol = 2, scales = 'free') +
  coord_flip() + theme_classic() + 
  theme(panel.spacing.y = unit(0, "pt"), 
        panel.spacing.x = unit(30, "pt"), 
        axis.line.y = element_blank(),
        axis.text.x = element_text(size = 9),
        strip.text = element_text(size = 8),
        plot.margin = margin(t = 5, r = 15, b = 5, l = 15),
        strip.background = element_blank())

fig2b

ggpubr::ggarrange(fig2a, fig2b, nrow = 2, labels = c('A', 'B'), 
                  heights = c(.6, 1))

ggsave('figures/figure2.png', dpi = 300, width = 25, height = 20, units = 'cm')