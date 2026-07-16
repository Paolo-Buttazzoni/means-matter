
#Study 2b: sixteen-property measurement
rm(list = ls())  

# Load necessary libraries
library(lme4)
library(lmerTest) 
library(jtools)
library(car)
library(MuMIn)

# Load necessary functions
source("r/functions.R")


mm2b_raw <- read_qualtrics('data/study2b/study2b_anon.csv')

mm2b_demog = mm2b_raw %>%
  select(Progress, Duration = `Duration (in seconds)`, RecordedDate, anon_id, Condition, Scenario, 
         age:politics_2, Comp1 = Q544, Comp2 = Q545, choice, rating = rating_1) %>%
  filter(!is.na(anon_id), Progress == 100, !is.na(choice)) %>% 
  mutate(Conservatism = (politics_1 + politics_2) / 2)

m1pos = (1:22)*3 - 2; m2pos = (1:22)*3 - 1; m3pos = (1:22)*3

mm2b_resps = mm2b_raw %>%
  select(Q167_1:Q730_4, anon_id) %>%
  select(ends_with(c("_4")), anon_id) %>%
  mutate(moral1 = rowSums(.[m1pos], na.rm = TRUE),
         moral2 = rowSums(.[m2pos], na.rm = TRUE),
         moral3 = rowSums(.[m3pos], na.rm = TRUE)) %>%
  select(anon_id:moral3)

mm2b_clean = full_join(mm2b_resps, mm2b_demog, by = 'anon_id') %>%
  mutate(moral = (moral1 + moral2 + moral3)/3) %>%
  filter(!is.na(anon_id), !is.na(Condition), !is.na(Scenario)) %>%
  distinct(anon_id, Condition, .keep_all = TRUE)   # dedupe BEFORE merging properties

properties <- c("effort","natural","useful","greatergood","autonomy","enhancement",
                "irreversible","unfair","cheating","external","normal","unsafe",
                "futurerisk","dehumanize","changeperson","playgod")

mm2b_trials <- mm2b_raw %>%
  filter(Progress == 100) %>%              # <-- add this
  select(Q167_1:Q730_4, anon_id, Condition) %>%
  select(ends_with("_1"), anon_id, Condition)

item_cols <- mm2b_trials %>% select(-anon_id, -Condition)

for (i in seq_along(properties)) {
  pos <- (1:22) * 16 + (i - 16)
  mm2b_trials[[properties[i]]] <- rowSums(item_cols[pos], na.rm = TRUE)
}

mm2b_trials <- mm2b_trials %>%
  select(anon_id, Condition, all_of(properties))

mm2b_clean = full_join(mm2b_clean, mm2b_trials, by = c('anon_id', 'Condition')) %>%
  filter(!is.na(Scenario))

# comprehension checks — this is what should get you close to N = 559
mm2b_clean <- mm2b_clean %>%
  mutate(
    check2 = case_when(
      Scenario == 'Anxiety' & Comp2 == 18 ~ 'Keep', 
      Scenario == 'Bullying' & Comp2 == 16 ~ 'Keep', 
      Scenario == 'Cognitive1' & Comp2 == 19 ~ 'Keep', 
      Scenario == 'Cognitive2' & Comp2 == 21 ~ 'Keep', 
      Scenario == 'Doping' & Comp2 == 14 ~ 'Keep', 
      Scenario == 'Height' & Comp2 == 12 ~ 'Keep', 
      Scenario == 'HIV' & Comp2 == 13 ~ 'Keep', 
      Scenario == 'Muscle' & Comp2 == 1 ~ 'Keep', 
      Scenario == 'Musical' & Comp2 == 20 ~ 'Keep', 
      Scenario == 'Prison' & Comp2 == 17 ~ 'Keep', 
      Scenario == 'School' & Comp2 == 15 ~ 'Keep'
    ),
    check1 = case_when(
      Condition == 'Conventional' & Comp1 < 12 ~ 'Keep', 
      Condition == 'Biotechnological' & Comp1 > 11 ~ 'Keep'
    )
  ) %>%
  filter(check2 == 'Keep', check1 == 'Keep')

nrow(mm2b_clean)   # check this against 559 before fitting anything

vars16 <- c("effort","natural","useful","greatergood","autonomy","enhancement",
            "irreversible","unfair","cheating","external","normal","unsafe",
            "futurerisk","dehumanize","changeperson","playgod")

mm2b_z <- mm2b_clean %>%
  mutate(across(all_of(vars16), ~ as.numeric(scale(.))),
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


# how many rows before check1/check2 filtering?
mm2b_pre_check <- full_join(
  full_join(mm2b_resps, mm2b_demog, by = 'anon_id') %>%
    mutate(moral = (moral1 + moral2 + moral3)/3) %>%
    filter(!is.na(anon_id), !is.na(Condition), !is.na(Scenario)) %>%
    distinct(anon_id, Condition, .keep_all = TRUE),
  mm2b_trials, by = c('anon_id','Condition')
) %>% filter(!is.na(Scenario))

nrow(mm2b_pre_check)  # N before comprehension checks

# how many pass check1 alone, check2 alone, both?
mm2b_pre_check %>%
  mutate(
    check1 = case_when(
      Condition == 'Conventional' & Comp1 < 12 ~ 'Keep', 
      Condition == 'Biotechnological' & Comp1 > 11 ~ 'Keep'
    ),
    check2 = case_when(
      Scenario == 'Anxiety' & Comp2 == 18 ~ 'Keep', 
      Scenario == 'Bullying' & Comp2 == 16 ~ 'Keep', 
      Scenario == 'Cognitive1' & Comp2 == 19 ~ 'Keep', 
      Scenario == 'Cognitive2' & Comp2 == 21 ~ 'Keep', 
      Scenario == 'Doping' & Comp2 == 14 ~ 'Keep', 
      Scenario == 'Height' & Comp2 == 12 ~ 'Keep', 
      Scenario == 'HIV' & Comp2 == 13 ~ 'Keep', 
      Scenario == 'Muscle' & Comp2 == 1 ~ 'Keep', 
      Scenario == 'Musical' & Comp2 == 20 ~ 'Keep', 
      Scenario == 'Prison' & Comp2 == 17 ~ 'Keep', 
      Scenario == 'School' & Comp2 == 15 ~ 'Keep'
    )
  ) %>%
  summarise(
    n_total = n(),
    n_check1_keep = sum(check1 == 'Keep', na.rm = TRUE),
    n_check1_na   = sum(is.na(check1)),
    n_check2_keep = sum(check2 == 'Keep', na.rm = TRUE),
    n_check2_na   = sum(is.na(check2)),
    n_both = sum(check1 == 'Keep' & check2 == 'Keep', na.rm = TRUE)
  )



mm2b_pre_check %>%
  mutate(
    check2 = case_when(
      Scenario == 'Anxiety' & Comp2 == 18 ~ 'Keep', 
      Scenario == 'Bullying' & Comp2 == 16 ~ 'Keep', 
      Scenario == 'Cognitive1' & Comp2 == 19 ~ 'Keep', 
      Scenario == 'Cognitive2' & Comp2 == 21 ~ 'Keep', 
      Scenario == 'Doping' & Comp2 == 14 ~ 'Keep', 
      Scenario == 'Height' & Comp2 == 12 ~ 'Keep', 
      Scenario == 'HIV' & Comp2 == 13 ~ 'Keep', 
      Scenario == 'Muscle' & Comp2 == 1 ~ 'Keep', 
      Scenario == 'Musical' & Comp2 == 20 ~ 'Keep', 
      Scenario == 'Prison' & Comp2 == 17 ~ 'Keep', 
      Scenario == 'School' & Comp2 == 15 ~ 'Keep'
    )
  ) %>%
  filter(is.na(check2)) %>%
  count(Scenario, Comp2) %>%
  arrange(Scenario, Comp2)


mm2b_pre_check %>%
  mutate(
    check1 = case_when(
      Condition == 'Conventional' & Comp1 < 12 ~ 'Keep', 
      Condition == 'Biotechnological' & Comp1 > 11 ~ 'Keep'
    )
  ) %>%
  filter(is.na(check1)) %>%
  count(Condition, Comp1) %>%
  arrange(Condition, Comp1)

mm2b_pre_check %>% count(Scenario)

mm2b_raw %>% filter(anon_id %in% c("anon_208","anon_398","anon_443","anon_479","anon_556","anon_582")) %>%
  select(anon_id, Progress, Duration = `Duration (in seconds)`, Condition, Scenario)


# Create figure data
fig1 = mm2b_clean %>%
  select(Condition, moral, starts_with("playgod")) %>%
  gather(-moral, -Condition, key = 'property', value = 'resp')

mod2 = lmer(moral ~ effort + natural + useful + greatergood + 
              autonomy +  enhancement + irreversible + unfair + 
              cheating + external + normal + unsafe + futurerisk + 
              dehumanize + changeperson + playgod +
              (1 | Scenario), mm2b_clean)
jtools::summ(mod2, digits = 3, scale = TRUE, confint = TRUE, transform.response = TRUE)
car::vif(mod2)


mm2b_clean = mm2b_clean %>%
  mutate(biotech = Condition == 'Biotechnological')

mod3 = glm(biotech ~ effort + natural + useful + greatergood + 
             autonomy +  enhancement + irreversible + unfair + 
             cheating + external + normal + unsafe + futurerisk + 
             dehumanize + changeperson + playgod, mm2b_clean, family = 'binomial')
jtools::summ(mod3, digits = 3,  confint = TRUE)
car::vif(mod3)

library(glmnet)

# X = predictor matrix (standardized)
# y = outcome vector (numeric or factor for classification)

# if y is continuous (linear regression):
family <- "gaussian"

# if y is binary (logistic regression):
# family <- "binomial"

alphas <- seq(0, 1, by = 0.1)   # sweep alpha from 0 to 1
results <- list()
cv_errors <- numeric(length(alphas))

set.seed(123)  # reproducibility

X = mm2b_clean %>%
  select(effort:playgod)

y = mm2b_clean$moral

for (i in seq_along(alphas)) {
  cv_fit <- cv.glmnet(
    x = as.matrix(X),
    y = y,
    alpha = alphas[i],     # mix between ridge (0) and lasso (1)
    family = family,
    nfolds = 10
  )
  
  results[[i]] <- cv_fit
  cv_errors[i] <- min(cv_fit$cvm)  # record CV error
}

# pick alpha with lowest CV error
best_idx <- which.min(cv_errors)
best_alpha <- alphas[best_idx]
best_fit <- results[[best_idx]]

cat("Best alpha:", best_alpha, "\n")
cat("Best lambda:", best_fit$lambda.min, "\n")

# extract coefficients at best alpha and lambda
coef(best_fit, s = "lambda.min") %>%
  round(3)

y = mm2b_clean$biotech
family <- "binomial"

for (i in seq_along(alphas)) {
  cv_fit <- cv.glmnet(
    x = as.matrix(X),
    y = y,
    alpha = alphas[i],     # mix between ridge (0) and lasso (1)
    family = family,
    nfolds = 10
  )
  
  results[[i]] <- cv_fit
  cv_errors[i] <- min(cv_fit$cvm)  # record CV error
}

# pick alpha with lowest CV error
best_idx2 <- which.min(cv_errors)
best_alpha2 <- alphas[best_idx2]
best_fit2 <- results[[best_idx2]]

cat("Best alpha:", best_alpha2, "\n")
cat("Best lambda:", best_fit2$lambda.min, "\n")

# extract coefficients at best alpha and lambda
coef(best_fit2, s = "lambda.min") %>%
  round(3)


full_join(
  coef(best_fit2, s = "lambda.min") %>% as.matrix() %>%
    as.data.frame() %>%
    mutate(predictor = rownames(.)),
  coef(best_fit, s = "lambda.min") %>% as.matrix() %>%
    as.data.frame() %>%
    mutate(predictor = rownames(.)),
  by = 'predictor') %>%
  mutate(product = lambda.min.x * lambda.min.y) %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = reorder(predictor, -abs(product)), y = abs(product))) + 
  geom_col(width = .1, color = 'black') +
  geom_point(shape = 21, fill = 'white', stroke = 1, size = 2.5) + 
  scale_y_continuous(limits = c(0, .0021), expand = c(0.0045, 0), 
                     name = 'Product of coefficients') +
  scale_x_discrete(name = 'Predictor') +
  coord_flip() + 
  theme_classic() + 
  theme(axis.ticks.y = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.position = 'bottom',
        plot.margin = margin(0.3,1,0.3, 1, "cm"),
        legend.title = element_text(size = 10))

full_join(
  coef(best_fit2, s = "lambda.min") %>% as.matrix() %>%
    as.data.frame() %>%
    mutate(predictor = rownames(.)),
  coef(best_fit, s = "lambda.min") %>% as.matrix() %>%
    as.data.frame() %>%
    mutate(predictor = rownames(.)),
  by = 'predictor') %>%
  mutate(product = lambda.min.x * lambda.min.y) %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = abs(lambda.min.x), y = abs(lambda.min.y))) + 
  geom_point(shape = 21, size = .5) +
  ggrepel::geom_text_repel(aes(label = predictor)) + 
  scale_y_continuous(name = 'Absolute coefficient of moral judgment model', 
                     limits = c(0, .3)) +
  scale_x_continuous(name = 'Absolute coefficient of condition model',
                     limits = c(0, .04)) +
  theme_classic() + 
  theme(axis.ticks.y = element_blank(),
        text = element_text(size = 11),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        legend.position = 'bottom',
        plot.margin = margin(0.3,1,0.3, 1, "cm"),
        legend.title = element_text(size = 10))

ggsave('Figures/Figure4a.jpg', dpi = 300, width = 15, height = 15, units = 'cm')

install.packages('glmmPen')
library(glmmPen)

# Fit a penalized GLMM
model <- glmmPen(
  formula = moral ~ effort + natural + useful + greatergood + 
    autonomy +  enhancement + irreversible + unfair + 
    cheating + external + normal + unsafe + futurerisk + 
    dehumanize + changeperson + playgod +
    (1 | Scenario),
  data = mm2b_clean,
  family = "gaussian",  # or "binomial", "poisson"
  penalty = "lasso",   # or "SCAD", "MCP"
  alpha = 1,           # 1 for LASSO, 0 for ridge
)



full_join(jtools::summ(mod3, digits = 3,  confint = TRUE)$coeftable %>%
            data.frame() %>% rename(coef_b = `Est.`) %>%
            mutate(predictor = row.names(.)), 
          jtools::summ(mod2, digits = 3,  confint = TRUE)$coeftable %>%
            data.frame() %>% rename(coef_a = `Est.`) %>%
            mutate(predictor = row.names(.)), 
          by = 'predictor') %>%
  mutate(product = round(`z.val.` * `t.val.`, 2), 
         product_coef = round(coef_a * coef_b, 6)*100) %>%
  select(predictor, product, product_coef) %>%
  filter(predictor != '(Intercept)') %>%
  ggplot(aes(x = abs(product), y = abs(product_coef), label = predictor)) + 
  geom_point() + geom_label()

mod2 = glmer(biotech ~ effort * external * normal + 
               (1 | Scenario), mm3c, family = 'binomial')
car::Anova(mod2)

mod2 = lmer(moral ~ effort * normal + external +
              (1 | Scenario), mm3)
car::Anova(mod2)
interactions::interact_plot(mod2, pred = 'effort', modx = 'normal', 
                            mod2 = 'useful')


interactions::sim_slopes(mod2, pred = 'effort', modx = 'normal', mod2 = 'external')
interactions::interact_plot(mod2, pred = 'normal', modx = 'effort')

mod2 = lmer(moral ~ effort * normal * useful + 
              natural + greatergood + 
              autonomy +  enhancement + unfair + 
              cheating + unsafe + futurerisk + Condition +
              (1 | Scenario), mm3c)
car::Anova(mod2)
jtools::summ(mod2, digits = 3, scale = TRUE, transform.response = TRUE, part.corr = TRUE)

cor.test(~ unsafe + futurerisk, mm3c)

mod2 = lmer(moral ~ effort * useful * normal + greatergood + 
              unfair + cheating + futurerisk + 
              playgod +
              (1 | Condition/Scenario), mm3)


car::Anova(mod2)

jtools::summ(mod2, digits = 3, scale = TRUE)

mm2b_clean = mm2b_clean %>%
  mutate(biotech = Condition == 'Biotechnological')

mod3 = glmer(biotech ~ effort + natural + useful + greatergood + 
               autonomy +  enhancement + irreversible + unfair + 
               cheating + external + normal + unsafe + futurerisk + 
               dehumanize + changeperson + playgod +
               (1 | Scenario), mm2b_clean, family = 'binomial')
jtools::summ(mod3, digits = 3)

glimpse(mm3)
mm3 %>%
  select(effort:playgod) %>%
  stats::factanal(., factors = 5)

# normal 5.354 3.93
sqrt(5.354*3.93)
# cheating 2.081 4.778
sqrt(4.778*2.081)
# effort 1.868 4.016
sqrt(1.868*4.016)

numScen$scen

byscen2 = numScen %>%
  mutate(
    Condition = if_else(condition == 'tech', 
                        'Biotechnological', 
                        'Conventional'),
    Scenario = case_when(
      scen == 'Anx' ~ 'Anxiety',
      scen == 'Bul' ~ 'Bullying',
      scen == 'Cog' ~ 'Cognitive1',
      scen == 'Dop' ~ 'Doping',
      scen == 'Hei' ~ 'Height',
      scen == 'Hiv' ~ 'HIV',
      scen == 'Mus' ~ 'Muscle',
      scen == 'Pri' ~ 'Prison',
      scen == 'Sch' ~ 'School',
      scen == 'Vio' ~ 'Musical',
      scen == 'Wor' ~ 'Cognitive2',
    )
  )

byscen3 = mm2b_clean %>%
  group_by(Condition, Scenario) %>%
  summarise_at(vars(moral:playgod), mean, na.rm = TRUE)

t.test(moral ~ Condition, byscen3)
t.test(effort ~ Condition, byscen3)
t.test(useful ~ Condition, byscen3)

mm3byscen = full_join(byscen3, byscen2, by = c('Condition', 'Scenario')) %>%
  select(-scen, -condition)

cor.test(~ moral + attitude, mm3byscen)

ggplot(mm3, aes(x = sqrt(effort), y = sqrt(useful), color = cheating)) + 
  geom_jitter(aes(color = cheating)) 

mm3 %>%
  group_by(effort > 50, greatergood > 50) %>%
  summarise(mean(cheating, na.rm = TRUE),
            mean(unfair, na.rm = TRUE),
            mean(moral, na.rm = TRUE))

ggplot(mm3, aes(x = sqrt(effort) - sqrt(useful), y = cheating)) + 
  geom_jitter(aes(color = cheating)) + geom_smooth(method = 'lm')

ggplot(mm3byscen, aes(x = attitude, y = moral, label = Scenario)) + 
  geom_point()+geom_smooth()+geom_label_repel()

figmm3 = byscen3 %>%
  gather(effort:playgod, key = 'item', value = 'resp')

mm3means = byscen3 %>%
  gather(effort:playgod, key = 'item', value = 'resp') %>%
  group_by(item, Condition) %>%
  summarise(resp = mean(resp, na.rm = TRUE), 
            moral = mean(moral, na.rm = TRUE))

mm3means$Condition = as.factor(mm3means$Condition)
levels(mm3means$Condition) = c('Technological', 'Conventional')

mmDistance = mm3means %>%
  pivot_wider(names_from = 'Condition', values_from = c('resp', 'moral')) %>%
  mutate(xDiff = resp_Conventional - resp_Biotechnological, 
         yDiff = moral_Conventional - moral_Biotechnological, 
         diagDiff = sqrt(abs(xDiff * yDiff))) %>%
  select(item, diagDiff)

mm3means$itemLong = as.factor(mm3means$item)
levels(mm3means$itemLong) = c(
  'X´s actions violated someone´s autonomy.', 
  'ACT changed who X is as a person.',
  'X cheated, or took the easy way out, to ACHIEVE-Y.',
  'ACT made X less human.',
  'To ACHIEVE-Y, X had to put in effort and hard work.',
  'X´s actions were more like (0) treatment or (100) enhancement.', 
  'X ACHIEVED-Y by (0 = intervening on their insides; 100 = modifying environment).',
  'X´s actions will have (0 = good, 100 = bad) long-term consequences.',
  'X´s actions were (0 = bad, 100 = good) for society.',
  'Th changes ACT brought about in X are irreversible.', 
  'ACT is (0 = natural, 100 = artificial).',
  'X ACHIEVED-Y through (0 = new/unusual; 100 = established/usual) means.',
  'ACT to ACHIEVE-Y is like playing God.',
  'What X did was unfair toward others.', 
  'The methods X used to ACHIEVE-Y were (0 = safe, 100 = unsafe)', 
  'ACT was a useful and efficient way of ACHIEVING-Y')


figmm3$Condition = as.factor(figmm3$Condition)
levels(figmm3$Condition) = c('Biotechnological', 'Conventional')

figmm3$itemLong = as.factor(figmm3$item)
levels(figmm3$itemLong) = c(
  'X´s actions violated someone´s autonomy.', 
  'ACT changed who X is as a person.',
  'X cheated, or took the easy way out, to ACHIEVE-Y.',
  'ACT made X less human.',
  'To ACHIEVE-Y, X had to put in effort and hard work.',
  'X´s actions were more like: (0) treatment or (100) enhancement.', 
  'X ACHIEVED-Y by (0 = intervening on their insides; 100 = modifying environment).',
  'X´s actions will have (0 = good, 100 = bad) long-term consequences.',
  'X´s actions were (0 = bad, 100 = good) for society.',
  'Th changes ACT brought about in X are irreversible.', 
  'ACT is (0 = natural, 100 = artificial).',
  'X ACHIEVED-Y through (0 = new/unusual; 100 = established/usual) means.',
  'ACT to ACHIEVE-Y is like playing God.',
  'What X did was unfair toward others.', 
  'The methods X used to ACHIEVE-Y were (0 = safe, 100 = unsafe)', 
  'ACT was a useful and efficient way of ACHIEVING-Y'
)


figmm3 = left_join(figmm3, mmDistance, by = 'item')

mm3means = left_join(mm3means, mmDistance, by = 'item')

figmm3 = arrange(figmm3, desc(abs(diagDiff)))

ggplot(arrange(figmm3, desc(abs(diagDiff))), aes(x = resp, y = moral)) + 
  geom_smooth(fill = 'grey', method = 'lm', formula = y ~ x + x^2, 
              alpha = .4, color = NA) +
  facet_wrap(~ itemLong, labeller = labeller(itemLong = label_wrap_gen(40)))
geom_point(aes(color = Condition), alpha = .5, shape = 4, size = .8) +
  geom_line(data = mm3means, aes(x = resp, y = moral, group = itemLong), linetype = 1, 
            color = 'black', linewidth = 1.2) +
  geom_point(data = mm3means, aes(x = resp, y = moral, color = Condition),
             size = 3, shape = 21, alpha = .8, fill = 'white', stroke = 1.5) +
  facet_wrap(~ itemLong, labeller = labeller(itemLong = label_wrap_gen(40))) + 
  theme_minimal() + theme(legend.position = 'top') + 
  scale_color_brewer(palette = 'Dark2') + 
  scale_fill_brewer(palette = 'Dark2') + 
  scale_x_continuous(name = NULL, 
                     breaks = c(0, 50, 100)) +
  scale_y_continuous(name = 'Moral judgment', 
                     breaks = c(0, 50, 100)) + 
  theme(legend.position = 'top', panel.grid.minor = element_blank(),
        legend.title = element_blank(), strip.text = element_text(size = 8),
        plot.margin = margin(0.3,1,0.3, 1, "cm")) + 
  coord_fixed(ratio = 1, ylim = c(0, 100), xlim = c(0, 100))



modItem = lmer(attitude ~ resp*item + Condition + (1 | Scenario), figmm3)
anova(modItem)

library(ggplot2)
library(ggtext)           # for element_markdown()

# 1) wrap itemLong to 40 characters (this inserts \n)
figmm3$wrapped_itemLong <- ggplot2::label_wrap_gen(40)(figmm3$itemLong)
mm3means$wrapped_itemLong <- ggplot2::label_wrap_gen(40)(mm3means$itemLong)

# 2) convert those line breaks to HTML <br/> so they display inside <i>...</i>
figmm3$wrapped_itemLong_html <- gsub("\n", "<br/>", figmm3$wrapped_itemLong)
mm3means$wrapped_itemLong_html <- gsub("\n", "<br/>", mm3means$wrapped_itemLong)

# 3) create the HTML label: bold item, line break, italic itemLong
figmm3$facet_label_html <- paste0("<span style='font-size:12px;'><b>", figmm3$item, "</b></span><br/><i>", figmm3$wrapped_itemLong_html, "</i>")
mm3means$facet_label_html <- paste0("<span style='font-size:12px;'><b>", mm3means$item, "</b></span><br/><i>", mm3means$wrapped_itemLong_html, "</i>")

# 4) reorder the HTML labels by -abs(diagDiff)
#    reorder() will preserve the label strings (including the HTML)
figmm3$facet_label_html <- with(figmm3, reorder(facet_label_html, -abs(diagDiff)))
mm3means$facet_label_html <- with(mm3means, reorder(facet_label_html, -abs(diagDiff)))


ggplot(figmm3, aes(x = resp, y = moral)) + 
  geom_smooth(fill = 'grey', method = 'lm', formula = y ~ x + x^2, 
              alpha = .4, color = NA)+
  geom_point(aes(color = Condition), alpha = .5, shape = 21, size = .8) +
  geom_line(data = mm3means, aes(x = resp, y = moral, group = facet_label_html), linetype = 1, 
            color = 'black', linewidth = 1) +
  geom_point(data = mm3means, aes(x = resp, y = moral, color = Condition),
             size = 3, shape = 21, fill = 'white', stroke = 1.5) +
  facet_wrap(~ facet_label_html) + 
  theme_minimal() + theme(legend.position = 'top') + 
  scale_color_brewer(palette = 'Dark2') + 
  scale_fill_brewer(palette = 'Dark2') + 
  scale_x_continuous(name = "Predictor", expand = c(0, 0),limits = c(0, 100),
                     breaks = c(0, 50, 100)) +
  scale_y_continuous(name = 'Moral judgment', expand = c(0, 0), limits = c(0, 100),
                     breaks = c(0, 50, 100)) + 
  theme(legend.position = 'bottom', panel.grid.minor = element_blank(),
        legend.title = element_blank(), strip.text = element_markdown(size = 6),
        panel.spacing.x = grid::unit(1, "lines"),
        plot.margin = margin(0.3,1,0.3, 1, "cm")) + 
  coord_fixed(ratio = 1, ylim = c(0, 100), xlim = c(0, 100))

ggsave('Figure2b_exp.jpg', width = 22, height = 25, dpi = 400, units = 'cm')

emtrends(modItem, pairwise ~ item, var = 'resp', adjust = 'none')

figmm3 = figmm3 %>%
  mutate(biotech = Condition == 'Biotechnological')

modItem2 = glmer(biotech ~ scale(resp)*item + (1 | Scenario), figmm3, 
                 family = 'binomial')
emtrends(modItem2, pairwise ~ item, var = 'resp', adjust = 'none')

cor.test(~ effort + attitude, mm3byscen)
cor.test(~ natural + attitude, mm3byscen)

mod5 = lm(unfair ~ effort * normal + external, mm3byscen)
car::Anova(mod5)
jtools::summ(mod5, digits = 3)

cor.test(~ effort + attitude, mm3byscen)
cor.test(~ natural + attitude, mm3byscen)
cor.test(~ external + attitude, mm3byscen)
cor.test(~ normal + attitude, mm3byscen)

library(rpart)
library(rpart.plot)
install.packages('rpart.plot')

# grow tree
fit <- rpart(moral~ .,
             method="anova", data=train)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(fit) # visualize cross-validation results  

# plot tree
plot(fit, uniform=TRUE,
     main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# moral attitude
# cheating unsafe futurerisk dehumanize changeperson playgod unfair greatergood
# effort natural useful autonomy enhancement irreversible external normal

mm3 %>%
  select(effort, natural, useful, autonomy, enhancement, irreversible, external, normal) %>%
  stats::factanal(., factor = 4)

# cheating unsafe futurerisk dehumanize changeperson playgod unfair greatergood

mm3 %>%
  select(cheating, unfair, unsafe, futurerisk, greatergood, 
         dehumanize, changeperson, playgod) %>%
  stats::factanal(., factor = 4)

mod5 = lm(attitude ~ unsafe + cheating, mm3byscen)
car::Anova(mod5)
jtools::summ(mod5, digits = 3)

mm3byscen %>%
  select(attitude, cheating, unfair, unsafe, futurerisk, greatergood, 
         dehumanize, changeperson, playgod) %>%
  apaTables::apa.cor.table(., 'evalTable2.doc')

mm3byscen %>%
  select(moral, effort, natural, useful, autonomy, 
         enhancement, irreversible, external, normal) %>%
  apaTables::apa.cor.table(., 'evalTable3.doc')

glimpse(mm3)
mm3$choice = 1 - mm3$choice

mod6 = lm(moral ~ choice*Condition, mm3)
emmeans(mod6, pairwise ~ Condition | choice)
jtools::summ(mod6, digits = 3)

mod6 = lmer(moral ~ choice*(effort + natural + useful + autonomy +  
                              external + normal) + (1 |  Scenario), mm3c)
car::Anova(mod6)

mod6 = lmer(moral ~ choice*(effort + natural + useful + autonomy +  
                              external + normal) + (1 |  Scenario), mm3)
car::Anova(mod6)

emtrends(mod6, pairwise ~ choice, var = 'effort')
emtrends(mod6, pairwise ~ choice, var = 'normal')
emtrends(mod6, pairwise ~ choice, var = 'autonomy')

## evaluations: cheating, dehumanizing, risk, greatergood, playgod, unfair
## descriptions: autonomy, changeperson, effort, enhancement, external, irreversible, natural
## normal, unsafe, useful

mm3 %>%
  select(moral, cheating, dehumanize, futurerisk, greatergood, playgod, unfair) %>%
  stats::factanal(., factors = 3)

mm3 %>%
  select(moral, autonomy, changeperson, effort, enhancement, external, irreversible, 
         natural, normal, unsafe, useful) %>%
  stats::factanal(., factors = 3)


install.packages('glmnet')
library(glmnet)
library(caret)

# Set training control
train_cont <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = "random",
                           verboseIter = TRUE)


# Train the model
glimpse(mm3)
train = mm3 %>%
  select(moral:playgod) %>%
  sample_frac(.5)

train = mm3 %>%
  select(effort:biotech) %>%
  mutate(biotech= as.factor(biotech)) 

train = mm3 %>%
  select(effort:playgod) 



# Make predictions on training set

predictions_train <- predict(elastic_reg, x)
eval_results(y_train, predictions_train, train) 

# Make predictions on test set

predictions_test <- predict(elastic_reg, x_test)
eval_results(y_test, predictions_test, test)

# attitude, cheating, unfair, unsafe, futurerisk, 
# greatergood, dehumanize, changeperson, playgod

## effort*normal --> cheating/unfair --> attitude

mod6 = lm(playgod ~ effort * normal + external, mm3byscen)
car::Anova(mod6)

## external --> unsafe/risk --> attitude
mod6 = lm(unsafe ~ natural + external + effort * normal, mm3byscen)
car::Anova(mod6)

mod6 = lm(moral ~ choice*(natural + external) + effort * normal, mm3c)
car::Anova(mod6)

emtrends(mod6, pairwise ~ choice, var = 'natural')

## useful --> greatergood --> attitude



# cheating unsafe futurerisk dehumanize changeperson playgod unfair greatergood
# effort natural useful autonomy enhancement irreversible external normal

mod6 = lm(greatergood ~ useful + external + effort * normal, mm3byscen)
car::Anova(mod6)
jtools::summ(mod6, digits = 3)

mod7 = lmer(moral ~ choice*(effort * normal * useful + natural + external) + (1 | Scenario), mm3c)
car::Anova(mod7)

mod7 = glm(biotech ~ effort + normal + external, 
           mm3byscen, family = 'binomial')
car::Anova(mod7)

interactions::interact_plot(mod7, pred = 'normal', modx = 'useful', mod2 = 'effort', 
                            interval = TRUE)

ggplot(mm3, aes(x = effort, y = moral))+
  geom_hline(yintercept = 50, linetype = 2)+
  geom_jitter(aes(color = unfair))+geom_smooth(method = 'lm') +
  facet_grid(normal > 43 ~ useful > 71)


glimpse(IDsMM3)
IDsMM3 = mm3c %>%
  filter(!is.na(moral1)) %>%
  select(PROLIFIC_PID) %>%
  distinct()

write.csv(IDsMM3, 'ids3pay.csv', row.names = FALSE)

#### 

glimpse(mm3c)

xtabs(~ choice, mm3c)
help(prop.test)
prop.test(309, 309+237, p = .5)
hist(mm3c$rating)
t.test(mm3c$rating, mu = 50)
t.test(rating ~ choice, mm3c)

xtabs(~ gender + choice, mm3c)
cor.test(~ age + rating, mm3c)
cor.test(~ religiosity_1 + rating, mm3c)
cor.test(~ politics_1 + rating, mm3c)
cor.test(~ politics_2 + rating, mm3c)
hist(mm3c$politics_1)
hist(mm3c$politics_2)
mean(mm3c$politics_1, na.rm = TRUE)
mean(mm3c$politics_2, na.rm = TRUE)




