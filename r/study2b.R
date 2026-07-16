
#Study 2b: sixteen-property measurement

library(lme4)
library(lmerTest)
library(jtools)
library(car)
library(MuMIn)
library(tidyverse)
library(ggplot2)
library(ggtext)     
library(ggrepel) 
library(glmnet)
library(ggpubr)

# Load raw data
source("r/functions.R")
mm2b_raw <- read_qualtrics('data/study2b/study2b_anon.csv')

# Demographics / condition / scenario info
# (Progress == 100 filter applied here already)
mm2b_demog <- mm2b_raw %>%
  select(Progress, Duration = `Duration (in seconds)`, RecordedDate,
         anon_id, Condition, Scenario,
         age:politics_2, Comp1 = Q544, Comp2 = Q545, choice, rating = rating_1) %>%
  filter(!is.na(anon_id), Progress == 100, !is.na(choice)) %>%
  mutate(Conservatism = (politics_1 + politics_2) / 2)


# Moral outcome (3-item composite: M1 approve, M2 encourage, M3 moral eval)
m1pos <- (1:22) * 3 - 2
m2pos <- (1:22) * 3 - 1
m3pos <- (1:22) * 3

mm2b_resps <- mm2b_raw %>%
  select(Q167_1:Q730_4, anon_id) %>%
  select(ends_with("_4"), anon_id) %>%
  mutate(
    moral1 = rowSums(.[m1pos], na.rm = TRUE),
    moral2 = rowSums(.[m2pos], na.rm = TRUE),
    moral3 = rowSums(.[m3pos], na.rm = TRUE)
  ) %>%
  select(anon_id:moral3)


mm2b_clean <- full_join(mm2b_resps, mm2b_demog, by = 'anon_id') %>%
  mutate(moral = (moral1 + moral2 + moral3) / 3) %>%
  filter(!is.na(anon_id), !is.na(Condition), !is.na(Scenario)) %>%
  arrange(anon_id, desc(Duration)) %>%
  distinct(anon_id, Condition, .keep_all = TRUE)


# Sixteen properties
properties <- c("effort", "natural", "useful", "greatergood", "autonomy",
                "enhancement", "irreversible", "unfair", "cheating", "external",
                "normal", "unsafe", "futurerisk", "dehumanize", "changeperson", "playgod")

mm2b_trials <- mm2b_raw %>%
  filter(Progress == 100) %>%
  select(Q167_1:Q730_4, anon_id, Condition) %>%
  select(ends_with("_1"), anon_id, Condition)

item_cols <- mm2b_trials %>% select(-anon_id, -Condition)

for (i in seq_along(properties)) {
  pos <- (1:22) * 16 + (i - 16)
  mm2b_trials[[properties[i]]] <- rowSums(item_cols[pos], na.rm = TRUE)
}

mm2b_trials <- mm2b_trials %>%
  select(anon_id, Condition, all_of(properties)) %>%
  distinct(anon_id, Condition, .keep_all = TRUE)


mm2b_clean <- full_join(mm2b_clean, mm2b_trials, by = c('anon_id', 'Condition')) %>%
  filter(!is.na(Scenario))

# Comprehension checks (attention/manipulation checks)
mm2b_clean <- mm2b_clean %>%
  mutate(
    check2 = case_when(
      Scenario == 'Anxiety'    & Comp2 == 18 ~ 'Keep',
      Scenario == 'Bullying'   & Comp2 == 16 ~ 'Keep',
      Scenario == 'Cognitive1' & Comp2 == 19 ~ 'Keep',
      Scenario == 'Cognitive2' & Comp2 == 21 ~ 'Keep',
      Scenario == 'Doping'     & Comp2 == 14 ~ 'Keep',
      Scenario == 'Height'     & Comp2 == 12 ~ 'Keep',
      Scenario == 'HIV'        & Comp2 == 13 ~ 'Keep',
      Scenario == 'Muscle'     & Comp2 == 1  ~ 'Keep',
      Scenario == 'Musical'    & Comp2 == 20 ~ 'Keep',
      Scenario == 'Prison'     & Comp2 == 17 ~ 'Keep',
      Scenario == 'School'     & Comp2 == 15 ~ 'Keep'
    ),
    check1 = case_when(
      Condition == 'Conventional'     & Comp1 < 12  ~ 'Keep',
      Condition == 'Biotechnological' & Comp1 > 11  ~ 'Keep'
    )
  ) %>%
  filter(check2 == 'Keep', check1 == 'Keep')

# TABLE 1: Mixed-effects linear regression, moral attitudes as DV
mm2b_z <- mm2b_clean %>%
  mutate(across(all_of(properties), ~ as.numeric(scale(.))),
         moral_z = as.numeric(scale(moral)))

mod2z <- lmerTest::lmer(
  moral_z ~ effort + natural + useful + greatergood + autonomy +
    enhancement + irreversible + unfair + cheating + external +
    normal + unsafe + futurerisk + dehumanize + changeperson + playgod +
    (1 | Scenario),
  data = mm2b_z
)

summary(mod2z)
confint(mod2z, method = "Wald")
AIC(mod2z)
MuMIn::r.squaredGLMM(mod2z)
car::vif(mod2z)

# TABLE 2: Logistic regression, enhancement type (biotech) as DV

mm2b_clean <- mm2b_clean %>%
  mutate(biotech = Condition == 'Biotechnological')

mm2b_z <- mm2b_z %>%
  mutate(biotech = mm2b_clean$biotech)

mod3z <- glmer(
  biotech ~ effort + natural + useful + greatergood + autonomy +
    enhancement + irreversible + unfair + cheating + external +
    normal + unsafe + futurerisk + dehumanize + changeperson + playgod +
    (1 | Scenario),
  data = mm2b_z, family = 'binomial'
)

summary(mod3z)
confint(mod3z, method = "Wald")
car::vif(mod3z)

mod3z_null <- glmer(biotech ~ 1 + (1 | Scenario), data = mm2b_z, family = 'binomial')
McFadden_R2 <- 1 - (logLik(mod3z) / logLik(mod3z_null))
McFadden_R2
AIC(mod3z)



# Figure 4
byscen3 <- mm2b_clean %>%
  group_by(Condition, Scenario) %>%
  summarise_at(vars(moral, all_of(properties)), mean, na.rm = TRUE) %>%
  ungroup()

figmm3 <- byscen3 %>%
  pivot_longer(all_of(properties), names_to = "item", values_to = "resp")

mm3means <- figmm3 %>%
  group_by(item, Condition) %>%
  summarise(resp = mean(resp, na.rm = TRUE),
            moral = mean(moral, na.rm = TRUE),
            .groups = "drop")

mm3means$Condition <- factor(mm3means$Condition,
                             levels = c('Biotechnological', 'Conventional'))
levels(mm3means$Condition) <- c('Technological', 'Conventional')

figmm3$Condition <- factor(figmm3$Condition,
                           levels = c('Biotechnological', 'Conventional'))
levels(figmm3$Condition) <- c('Technological', 'Conventional')

mmDistance <- mm3means %>%
  pivot_wider(names_from = 'Condition', values_from = c('resp', 'moral')) %>%
  mutate(
    xDiff = resp_Conventional - resp_Technological,
    yDiff = moral_Conventional - moral_Technological,
    diagDiff = sqrt(abs(xDiff * yDiff))
  ) %>%
  select(item, diagDiff)

figmm3 <- left_join(figmm3, mmDistance, by = 'item')
mm3means <- left_join(mm3means, mmDistance, by = 'item')

item_labels <- c(
  effort       = "To ACHIEVE-Y, X had to put in effort and hard work.",
  natural      = "ACT is (0 = natural, 100 = artificial).",
  useful       = "ACT was a useful and efficient way of ACHIEVING-Y.",
  greatergood  = "X's actions were (0 = bad, 100 = good) for society.",
  autonomy     = "X's actions violated someone's autonomy.",
  enhancement  = "X's actions were more like (0) treatment or (100) enhancement.",
  irreversible = "The changes ACT brought about in X are irreversible.",
  unfair       = "What X did was unfair toward others.",
  cheating     = "X cheated, or took the easy way out, to ACHIEVE-Y.",
  external     = "X ACHIEVED-Y by (0 = intervening on their insides; 100 = modifying environment).",
  normal       = "X ACHIEVED-Y through (0 = new/unusual; 100 = established/usual) means.",
  unsafe       = "The methods X used to ACHIEVE-Y were (0 = safe, 100 = unsafe).",
  futurerisk   = "X's actions will have (0 = good, 100 = bad) long-term consequences.",
  dehumanize   = "ACT made X less human.",
  changeperson = "ACT changed who X is as a person.",
  playgod      = "ACT to ACHIEVE-Y is like playing God."
)

figmm3$itemLong  <- item_labels[figmm3$item]
mm3means$itemLong <- item_labels[mm3means$item]

figmm3   <- figmm3   %>% mutate(item = if_else(item == "external", "body-envelope", item))
mm3means <- mm3means %>% mutate(item = if_else(item == "external", "body-envelope", item))

figmm3$wrapped_itemLong   <- label_wrap_gen(40)(figmm3$itemLong)
mm3means$wrapped_itemLong <- label_wrap_gen(40)(mm3means$itemLong)

figmm3$wrapped_itemLong_html   <- gsub("\n", "<br/>", figmm3$wrapped_itemLong)
mm3means$wrapped_itemLong_html <- gsub("\n", "<br/>", mm3means$wrapped_itemLong)

figmm3$facet_label_html <- paste0(
  "<span style='font-size:13px;'><b>", figmm3$item, "</b></span><br/><i>",
  figmm3$wrapped_itemLong_html, "</i>"
)
mm3means$facet_label_html <- paste0(
  "<span style='font-size:13px;'><b>", mm3means$item, "</b></span><br/><i>",
  mm3means$wrapped_itemLong_html, "</i>"
)

figmm3$facet_label_html   <- with(figmm3, reorder(facet_label_html, -abs(diagDiff)))
mm3means$facet_label_html <- with(mm3means, reorder(facet_label_html, -abs(diagDiff)))

ggplot(figmm3, aes(x = resp, y = moral)) +
  geom_smooth(fill = 'grey', method = 'lm', formula = y ~ x,
              alpha = .4, color = NA, fullrange = TRUE) +
  geom_point(aes(color = Condition), alpha = .5, shape = 21, size = .8) +
  geom_line(data = mm3means, aes(x = resp, y = moral, group = facet_label_html),
            linetype = 1, color = 'black', linewidth = 1) +
  geom_point(data = mm3means, aes(x = resp, y = moral, color = Condition),
             size = 3, shape = 21, fill = 'white', stroke = 1.5) +
  facet_wrap(~ facet_label_html) +
  theme_minimal() +
  scale_color_brewer(palette = 'Dark2', name = "Methods",
                     labels = c('Familiar', 'Technologically novel')) +
  scale_fill_brewer(palette = 'Dark2', name = "Methods",
                    labels = c('Familiar', 'Technologically novel')) +
  scale_x_continuous(name = NULL, expand = c(0, 0), breaks = c(0, 50, 100)) +
  scale_y_continuous(name = 'Moral judgment', expand = c(0, 0), breaks = c(0, 50, 100)) +
  theme(legend.position = 'bottom', panel.grid.minor = element_blank(),
        strip.text = element_markdown(size = 6.5, hjust = 0),
        panel.spacing.x = grid::unit(1, "lines"),
        plot.margin = margin(0.3, 1, 0.3, 1, "cm")) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))

ggsave('figures/figure4.png', width = 24, height = 22, dpi = 400, units = 'cm')


#Figure 5
properties <- c("effort", "natural", "useful", "greatergood", "autonomy",
                "enhancement", "irreversible", "unfair", "cheating", "external",
                "normal", "unsafe", "futurerisk", "dehumanize", "changeperson", "playgod")

X <- mm2b_clean %>% select(all_of(properties))
y_moral   <- mm2b_clean$moral
y_biotech <- as.numeric(mm2b_clean$biotech)   # convert logical to 0/1 for glmnet

set.seed(123)
alphas <- seq(0, 1, by = 0.02)   

# DV model (moral)
cv_errors <- numeric(length(alphas))
results <- vector("list", length(alphas))
for (i in seq_along(alphas)) {
  cv_fit <- cv.glmnet(x = as.matrix(X), y = y_moral,
                      alpha = alphas[i], family = "gaussian", nfolds = 10)
  results[[i]] <- cv_fit
  cv_errors[i] <- min(cv_fit$cvm)
}
best_idx <- which.min(cv_errors)
best_alpha <- alphas[best_idx]
best_fit <- results[[best_idx]]
cat("DV model (moral) -- best alpha:", best_alpha,
    " best lambda:", best_fit$lambda.min, "\n")

# IV model (biotech)
cv_errors2 <- numeric(length(alphas))
results2 <- vector("list", length(alphas))
for (i in seq_along(alphas)) {
  cv_fit <- cv.glmnet(x = as.matrix(X), y = y_biotech,
                      alpha = alphas[i], family = "binomial", nfolds = 10)
  results2[[i]] <- cv_fit
  cv_errors2[i] <- min(cv_fit$cvm)
}
best_idx2 <- which.min(cv_errors2)
best_alpha2 <- alphas[best_idx2]
best_fit2 <- results2[[best_idx2]]
cat("IV model (biotech) -- best alpha:", best_alpha2,
    " best lambda:", best_fit2$lambda.min, "\n")

coef_iv_df <- as.data.frame(as.matrix(coef(best_fit2, s = "lambda.min"))) %>%
  rename(coef_iv = 1) %>%
  mutate(predictor = rownames(.))

coef_dv_df <- as.data.frame(as.matrix(coef(best_fit, s = "lambda.min"))) %>%
  rename(coef_dv = 1) %>%
  mutate(predictor = rownames(.))

mm_fig_data <- full_join(coef_iv_df, coef_dv_df, by = "predictor") %>%
  filter(predictor != "(Intercept)") %>%
  mutate(
    product = abs(coef_iv * coef_dv),
    abs_x   = abs(coef_iv) + 0.01,
    abs_y   = abs(coef_dv) + 0.01,
    retain  = if_else(abs(product) > 0.0005, "y", "n"),
    face_label = if_else(retain == "y", "bold", "plain"),
    predictor = if_else(predictor == "external", "body-envelope", predictor)
  )


pA <- ggplot(mm_fig_data, aes(x = abs_x, y = abs_y)) +
  geom_point(shape = 21, size = 0.5, aes(color = retain)) +
  scale_color_manual(values = c("black", "#DAA520")) +
  geom_text_repel(aes(label = predictor, color = retain, fontface = face_label),
                  force_pull = 2, segment.size = 0.3,
                  min.segment.length = 1, force = 0.5) +
  scale_y_log10(name = "Absolute coefficient of moral judgment model") +
  scale_x_log10(name = "Absolute coefficient of condition (biotech) model") +
  theme_classic() +
  theme(
    text = element_text(size = 11),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    axis.line = element_blank(),
    panel.background = element_rect(fill = "#efefef", color = NA),
    legend.position = "none",
    plot.margin = margin(10, 3, 5, 3, "pt")
  )

pB <- ggplot(mm_fig_data, aes(x = reorder(predictor, abs(product)), y = abs(product))) +
  geom_vline(xintercept = 11.5, linetype = 2) +
  geom_col(width = 0.1, aes(color = retain, fill = retain)) +
  geom_point(shape = 21, fill = "white", stroke = 1.3, size = 2.5,
             aes(color = retain)) +
  scale_y_continuous(expand = c(0, 0), name = "Product of coefficients") +
  scale_x_discrete(name = NULL) +
  scale_color_manual(values = c("darkgrey", "#DAA520")) +
  scale_fill_manual(values = c("darkgrey", "#DAA520")) +
  coord_flip() +
  theme_classic() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.position = "none",
    plot.margin = margin(10, 3, 5, 3, "pt")
  )

ggarrange(pA, pB, labels = c("A", "B"), widths = c(1.4, 1), nrow = 1)
ggsave("figures/figure5.png", dpi = 300, width = 18, height = 12, units = "cm")
