library(tidyverse)
library(readr)
library(ggtext)
library(glmnet)

#This one is 'Do means matter morally?' study 3
filename = "study2b/study2b.csv"
headers = read_csv(filename, col_names = FALSE, n_max = 1)
mm2b = read_csv(filename, skip = 3, col_names = FALSE)
colnames(mm2b)= headers

glimpse(mm2b)

mm2b_demog = mm2b %>%
  select(Progress, Duration = `Duration (in seconds)`, RecordedDate, PROLIFIC_PID, Condition, Scenario, 
         age:politics_2, Comp1 = Q544, Comp2 = Q545, choice, rating = rating_1) %>%
  filter(!is.na(PROLIFIC_PID), Progress == 100, !is.na(choice))

glimpse(mm2b_demog)

mm2b_demog %>% 
  select(PROLIFIC_PID) %>%
  distinct()

#previous code to load, filter and format data
#make a historgram of how long people took for survey
#hist(pdata3$Duration)

#demographic traits
#t.test(religiosity_1 ~ choice, pdata3)
#t.test(politics_1 ~ choice, pdata3)
#t.test(politics_2 ~ choice, pdata3)
#t.test(age ~ choice, pdata3)
#xtabs(~ gender + choice, pdata3)

#M1 approve or disapprove of action
#M2 should other people be encouraged or discouraged?
#M3 from a moral pov, what Jesse did is XX
m1pos = (1:22)*3 - 2
m2pos = (1:22)*3 - 1
m3pos = (1:22)*3 

mm2b_resps = mm2b %>%
  select(Q167_1:Q730_4, PROLIFIC_PID) %>%
  select(ends_with(c("_4")), PROLIFIC_PID) %>%
  mutate(moral1 = rowSums(.[m1pos], na.rm = TRUE),
         moral2 = rowSums(.[m2pos], na.rm = TRUE),
         moral3 = rowSums(.[m3pos], na.rm = TRUE)) %>%
  select(PROLIFIC_PID:moral3)

glimpse(mm2b_resps)

mm2b_clean = full_join(mm2b_resps, mm2b_demog, by = 'PROLIFIC_PID') %>%
  mutate(moral = (moral1 + moral2 + moral3)/3) %>%
  filter(!is.na(PROLIFIC_PID), !is.na(Condition), !is.na(Scenario))

glimpse(mm2b_clean)

t.test(moral ~ Condition, mm2b_clean)

mod1b = lm(moral ~ Condition*Scenario, mm2b_clean)
anova(mod1b)

effectsize::cohens_d(mm2b_clean$moral, mm2b_clean$Condition)

#to check with Ivar, why are some conditions NA?
ggplot(mm2b_clean %>% filter(!is.na(Condition)), aes(x = moral, group = Condition, color = Condition)) + 
  geom_density()

mm2b_clean %>%
  select(moral1:moral3) %>%
  psych::alpha(., check.keys = TRUE)

# Define column positions (make sure these are correct)
effP = (1:22)*16 - 15
natP = (1:22)*16 - 14
useP = (1:22)*16 - 13
socP = (1:22)*16 - 12
autP = (1:22)*16 - 11
tedP = (1:22)*16 - 10
revP = (1:22)*16 - 9
unfP = (1:22)*16 - 8
cheP = (1:22)*16 - 7
extP = (1:22)*16 - 6
norP = (1:22)*16 - 5
safP = (1:22)*16 - 4
futP = (1:22)*16 - 3
dehP = (1:22)*16 - 2
perP = (1:22)*16 - 1
godP = (1:22)*16  

# Verify column names in the dataset before selecting
print(colnames(mm2b))

# Create tdata3: Extract relevant columns & compute moral dimensions
mm2b_trials = mm2b %>%
  select(Q167_1:Q730_4, PROLIFIC_PID, Condition) %>%
  select(ends_with("_1"), PROLIFIC_PID, Condition) %>%
  mutate(
    effort = rowSums(.[effP], na.rm = TRUE),
    natural = rowSums(.[natP], na.rm = TRUE),
    useful = rowSums(.[useP], na.rm = TRUE), 
    greatergood = rowSums(.[socP], na.rm = TRUE),
    autonomy = rowSums(.[autP], na.rm = TRUE),
    enhancement = rowSums(.[tedP], na.rm = TRUE), 
    irreversible = rowSums(.[revP], na.rm = TRUE),
    unfair = rowSums(.[unfP], na.rm = TRUE),
    cheating = rowSums(.[cheP], na.rm = TRUE), 
    external = rowSums(.[extP], na.rm = TRUE),
    normal = rowSums(.[norP], na.rm = TRUE),
    unsafe = rowSums(.[safP], na.rm = TRUE), 
    futurerisk = rowSums(.[futP], na.rm = TRUE),
    dehumanize = rowSums(.[dehP], na.rm = TRUE),
    changeperson = rowSums(.[perP], na.rm = TRUE), 
    playgod = rowSums(.[godP], na.rm = TRUE), 
  ) %>%
  select(PROLIFIC_PID, Condition, effort:playgod)

glimpse(mm2b_trials)

# Merge processed moral dimensions with the main dataset
mm2b_clean = full_join(mm2b_clean %>%
                         filter(!is.na(PROLIFIC_PID), !is.na(Condition)), 
                       mm2b_trials %>%
                         filter(!is.na(PROLIFIC_PID), !is.na(Condition)), 
                       by =  c('PROLIFIC_PID', 'Condition')) %>%  # Ensure valid participant IDs
  filter(!is.na(Scenario))

glimpse(mm2b_clean)

# Cross-tabulation of Scenario and Condition
xtabs(~ Scenario + Condition, mm2b_clean)

xtabs(~ PROLIFIC_PID, mm2b_clean)

mm2b_clean %>%
  select(PROLIFIC_PID) %>%
  group_by(PROLIFIC_PID) %>%
  tally() %>%
  filter(n > 1) %>%
  left_join(., mm2b_clean, by = 'PROLIFIC_PID') %>%
  View()

# Apply comprehension checks to filter valid responses
mm2b_clean = mm2b_clean %>%
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
    )) %>%
  filter(check2 == 'Keep', check1 == 'Keep')

# Verify that 'playgod' exists before selecting it
if (!"playgod" %in% colnames(mm2b_clean)) {
  warning("Column 'playgod' not found in mm3c. It may not have been included in the dataset.")
}

# Create figure data
fig1 = mm2b_clean %>%
  select(Condition, moral, starts_with("playgod")) %>%
  gather(-moral, -Condition, key = 'property', value = 'resp')

install.packages("ggpubr")  # If not installed
library(ggpubr)
install.packages("lme4")  # Install if not already installed
library(lme4)  # Load the package

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


# X = predictor matrix (standardized)
# y = outcome vector (numeric or factor for classification)

# if y is continuous (linear regression):
family <- "gaussian"

# if y is binary (logistic regression):
# family <- "binomial"

alphas <- seq(0, 1, by = 0.001)   # sweep alpha from 0 to 1
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


mm_fig2b_data = full_join(
coef(best_fit2, s = "lambda.min") %>% as.matrix() %>%
  as.data.frame() %>%
  mutate(predictor = rownames(.)),
coef(best_fit, s = "lambda.min") %>% as.matrix() %>%
  as.data.frame() %>%
  mutate(predictor = rownames(.)),
by = 'predictor') %>%
  mutate(product = abs(lambda.min.x * lambda.min.y), 
         abs_x = abs(lambda.min.x) + .01,
         abs_y = abs(lambda.min.y) + .01,
         retain = if_else(abs(product) > .0005, 'y', 'n'), 
         face_label = if_else(retain == "y", 'bold', 'plain')) %>%
  filter(predictor != "(Intercept)") 



fig2b_b = ggplot(mm_fig2b_data, aes(x = reorder(predictor, abs(product)), y = abs(product))) + 
  geom_vline(xintercept = 11.5, linetype = 2) +
  geom_col(width = .1, aes(color = retain, fill = retain)) +
  geom_point(shape = 21, fill = 'white', stroke = 1.3, size = 2.5, 
             aes(color = retain)) + 
  scale_y_continuous(limits = c(0, .0021), expand = c(0, 0), 
                     labels = c("0", "0.5", "1", "1.5", "2"),
                     name = 'Product of coefficients') +
  scale_x_discrete(name = 'Predictor') +
  scale_color_manual(values = c('darkgrey', '#DAA520')) +
  scale_fill_manual(values = c('darkgrey', '#DAA520')) +
  coord_flip() + 
  theme_classic() + 
  theme(axis.ticks.y = element_blank(),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12),
      legend.position = 'none',
      plot.margin = margin(10,3,5,3, "pt"),
      legend.title = element_text(size = 10))

fig2a_a = ggplot(mm_fig2b_data, aes(x = abs_x, y = abs_y)) + 
  geom_point(shape = 21, size = .5, aes(color = retain)) +
  scale_color_manual(values = c('black', '#DAA520')) +
  scale_fill_manual(values = c('darkgrey', '#DAA520')) +
  ggrepel::geom_text_repel(aes(label = predictor, color = retain, 
                               fontface = face_label), force_pull = 2, 
                           segment.size = 0.3,
                           min.segment.length = 1, force = .5) + 
  scale_y_continuous(name = 'Absolute coefficient of moral judgment model', 
                     #breaks = c(.001, .01, .1, 1, 10), limits = c(.001, 100),
                     breaks = c(.01, .02, .04, .08, .16, .32),
                     transform = "log"
                     ) +
  scale_x_continuous(name = 'Absolute coefficient of condition model',
                    # breaks = seq(0, 0.05, 0.02), limits = c(.005, .07),
                     #breaks = c(.001, .01, .1, 1, 10), limits = c(.001, 100),
                     # limits = c(0, .04), 
                     breaks = c(.01, .02, .04),
                     transform = "log"
                     ) +
  theme_classic() + 
  theme(axis.ticks.y = element_blank(),
        text = element_text(size = 11),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.position = 'none',
        plot.margin = margin(10,3,5,3, "pt"),
        legend.title = element_text(size = 10))


ggpubr::ggarrange(fig2a_a, fig2b_b, labels = c('A', 'B'))
ggsave('Figures/Figure4a_coefs.jpg', dpi = 300, width = 18, height = 12, units = 'cm')


library(glmnet)

set.seed(123)

B <- 1000 # number of bootstrap resamples
n <- nrow(X)
boot_results <- matrix(NA, nrow = B, ncol = ncol(X))
colnames(boot_results) <- colnames(X)

alphas <- seq(0, 1, by = 0.1) # sweep alphas as before

for (b in 1:B) {
  # sample with replacement
  idx <- sample(1:n, size = n, replace = TRUE)
  Xb <- X[idx, ]
  yb <- y[idx]
  ivb <- iv[idx] # your "condition" IV
  
  # ----- Step 1: DV model -----
  cv_errors <- numeric(length(alphas))
  results <- list()
  
  for (i in seq_along(alphas)) {
    cv_fit <- cv.glmnet(
      x = as.matrix(Xb),
      y = yb,
      alpha = alphas[i],
      family = "gaussian", # or "binomial" if DV is binary
      nfolds = 10
    )
    results[[i]] <- cv_fit
    cv_errors[i] <- min(cv_fit$cvm)
  }
  
  best_idx <- which.min(cv_errors)
  best_fit <- results[[best_idx]]
  coef_dv <- as.vector(coef(best_fit, s = "lambda.min"))
  
  # ----- Step 2: IV model (flipped) -----
  results <- list()
  cv_errors <- numeric(length(alphas))
  
  for (i in seq_along(alphas)) {
    cv_fit <- cv.glmnet(
      x = as.matrix(Xb),
      y = ivb,
      alpha = alphas[i],
      family = "binomial", # if IV is categorical
      nfolds = 10
    )
    results[[i]] <- cv_fit
    cv_errors[i] <- min(cv_fit$cvm)
  }
  
  best_idx <- which.min(cv_errors)
  best_fit <- results[[best_idx]]
  coef_iv <- as.vector(coef(best_fit, s = "lambda.min"))
  
  # ----- Product-of-coefficients -----
  # (exclude intercept, assume predictors in same order)
  boot_results[b, ] <- coef_dv[-1] * coef_iv[-1]
}

# summarize bootstrap distribution
boot_means <- apply(boot_results, 2, mean, na.rm = TRUE)
boot_cis <- apply(boot_results, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)

# inspect results
head(boot_means)
head(boot_cis)

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




