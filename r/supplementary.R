
##Libraries 

# Load raw data
source("r/functions.R")
mm1a_raw <- read_qualtrics('data/study1a/study1a_anon.csv')

glimpse(mm1)

mm1 = left_join(mm1, 
          mm1raw %>%
            select(ResponseId, safe = safe_1, conseq = conseq_1), 
          by = "ResponseId")

mm1 %>%
  group_by(Methods) %>%
  summarise(
    safe_mean = mean(safe, na.rm = TRUE),
    safe_sd = sd(safe, na.rm = TRUE),
    conseq_mean = mean(conseq, na.rm = TRUE),
    conseq_sd = sd(conseq, na.rm = TRUE)
  )

cor.test(~ safe + conseq, data = mm1, method = "pearson")

mm1 %>%
  select(safe, conseq) %>%
  psych::alpha(check.keys = TRUE)

mm1$risk = (mm1$safe + mm1$conseq)/2

endorse_resp = lm(response ~ Methods + risk, mm1)
jtools::summ(endorse_resp, digits = 3, confint = TRUE)
emmeans(endorse_resp, pairwise ~ Endorsement | Methods)
effectsize::cohens_d(subset(mm1, Endorsement == 'Equivalence')$response,subset(mm1, Endorsement == 'Equivalence')$condition)

common_support = mm1 %>%
  group_by(Methods) %>%
  summarise(
    risk_min = min(risk, na.rm = TRUE),
    risk_max = max(risk, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(min_value = max(risk_min), max_value = min(risk_max)) %>%
  distinct(min_value, max_value)

ggplot(mm1, aes(x = risk, y = response, color = Methods)) +
  geom_point() +
  geom_vline(xintercept = common_support$min_value, linetype = "dashed", color = "black") +
  geom_vline(xintercept = common_support$max_value, linetype = "dashed", color = "black") +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25), 
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25),
                     expand = c(0, 0)) +
  labs(x = "Risk Perception", y = "Moral attitudes", color = "Methods") +
  theme_classic() + 
  theme(legend.position = c(.8, .85), 
        axis.title = element_text(size = 13, face = 'bold'), 
        legend.text = element_text(size = 11),
        plot.margin = margin(0.3, 1, 0.3, 1, "cm"),
        axis.text= element_text(size = 12)) + 
  scale_color_brewer(palette = 'Dark2', 
                     labels = c('Familiar', 'Technologically novel')) + 
 guides(fill = "none", color = guide_legend(override.aes = list(fill = NA)))


ggsave(ggplot2::last_plot(), filename = "figures/figureS1.png", width = 8, height = 5, dpi = 300)

endorse_resp2 = lm(response ~ Methods + risk, 
                  mm1 %>%
                    filter(risk >= common_support$min_value, risk <= common_support$max_value))
car::Anova(endorse_resp2)
jtools::summ(endorse_resp2, digits = 3, confint = TRUE)

lm(response ~ Methods, 
   mm1) %>% 
jtools::summ(digits = 3, confint = TRUE)
