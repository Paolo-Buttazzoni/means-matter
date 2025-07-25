library(tidyverse)
library(readr)

#This one is 'Do means matter morally?' study 3
filename = "mmm_study3.csv"
headers = read_csv(filename, col_names = FALSE, n_max = 1)
ge3raw = read_csv(filename, skip = 3, col_names = FALSE)
colnames(ge3raw)= headers

glimpse(ge3raw)

pdata3 = ge3raw %>%
  select(Progress, Duration = `Duration (in seconds)`, RecordedDate, PROLIFIC_PID, Condition, Scenario, 
         age:politics_2, Comp1 = Q544, Comp2 = Q545, choice, rating = rating_1) %>%
  filter(!is.na(PROLIFIC_PID), Progress == 100, !is.na(choice))
glimpse(pdata3)
View(pdata3)
pdata3 %>% 
  select(PROLIFIC_PID) %>%
  distinct()

#previous code to load, filter and format data
#make a historgram of how long people took for survey
hist(pdata3$Duration)

#demographic traits
t.test(religiosity_1 ~ choice, pdata3)
t.test(politics_1 ~ choice, pdata3)
t.test(politics_2 ~ choice, pdata3)
t.test(age ~ choice, pdata3)
xtabs(~ gender + choice, pdata3)

#M1 approve or disapprove of action
#M2 should other people be encouraged or discouraged?
#M3 from a moral pov, what Jesse did is XX
m1pos = (1:22)*3 - 2
m2pos = (1:22)*3 - 1
m3pos = (1:22)*3 

mdata3 = ge3raw %>%
  select(Q167_1:Q730_4, PROLIFIC_PID) %>%
  select(ends_with(c("_4")), PROLIFIC_PID) %>%
  mutate(moral1 = rowSums(.[m1pos], na.rm = TRUE),
         moral2 = rowSums(.[m2pos], na.rm = TRUE),
         moral3 = rowSums(.[m3pos], na.rm = TRUE)) %>%
  select(PROLIFIC_PID:moral3)

glimpse(mdata3)

mm3 = full_join(pdata3, mdata3, by = 'PROLIFIC_PID') %>%
  mutate(moral = (moral1 + moral2 + moral3)/3)

glimpse(mm3)

t.test(moral ~ Condition, mm3)

mod1b = lm(moral ~ Condition*Scenario, mm3)
anova(mod1b)

effectsize::cohens_d(mm3$moral, mm3$Condition)

#to check with Ivar, why are some conditions NA?
ggplot(mm3 %>% filter(!is.na(Condition)), aes(x = moral, group = Condition, color = Condition)) + 
  geom_density()

mm3 %>%
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
print(colnames(ge3raw))

# Create tdata3: Extract relevant columns & compute moral dimensions
tdata3 = ge3raw %>%
  select(Q167_1:Q730_4, PROLIFIC_PID) %>%
  select(ends_with("_1"), PROLIFIC_PID) %>%
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
    playgod = if ("playgod" %in% colnames(ge3raw)) rowSums(.[godP], na.rm = TRUE) else NA
  ) %>%
  select(PROLIFIC_PID:playgod)

glimpse(tdata3)

# Merge processed moral dimensions with the main dataset
mm3c = full_join(mm3, tdata3, by = 'PROLIFIC_PID') %>%
  filter(!is.na(PROLIFIC_PID))  # Ensure valid participant IDs

glimpse(mm3c)

# Cross-tabulation of Scenario and Condition
xtabs(~ Scenario + Condition, mm3c)

# Apply comprehension checks to filter valid responses
mm3c = mm3c %>%
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
if (!"playgod" %in% colnames(mm3c)) {
  warning("Column 'playgod' not found in mm3c. It may not have been included in the dataset.")
}

# Create figure data
fig1 = mm3c %>%
  select(Condition, moral, starts_with("playgod")) %>%
  gather(-moral, -Condition, key = 'property', value = 'resp')

install.packages("ggpubr")  # If not installed
library(ggpubr)
install.packages("lme4")  # Install if not already installed
library(lme4)  # Load the package


ggplot(fig1, aes(x = resp, y = moral))+
  geom_jitter(alpha = .5, aes(color = Condition), size = .5, shape = 21) + geom_smooth(color = 'grey') +
  stat_conf_ellipse(aes(color = Condition, fill = Condition), geom = 'polygon', alpha = .5)+
  facet_wrap(~ property) + theme_minimal() + theme(legend.position = 'top')

ggsave('figExp3.jpg', width = 8, height = 8, dpi = 400)

mm3c %>%
  group_by(Scenario, Condition) %>%

mod2 = lmer(moral ~ effort + natural + useful + greatergood + 
              autonomy +  enhancement + irreversible + unfair + 
              cheating + external + normal + unsafe + futurerisk + 
              dehumanize + changeperson + playgod +
              (1 | Scenario), mm3c)
jtools::summ(mod2, digits = 3)

mod2 = glmer(biotech ~ effort + natural + useful + greatergood + 
              autonomy +  enhancement + irreversible + unfair + 
              cheating + external + normal + unsafe + futurerisk + 
              dehumanize + changeperson + playgod +
              (1 | Scenario), mm3c, family = 'binomial')
jtools::summ(mod2, digits = 3)

mod2 = lmer(moral ~ effort + natural + useful + greatergood + 
              autonomy +  enhancement + irreversible + unfair + 
              cheating + external + normal + unsafe + futurerisk + 
              dehumanize + changeperson + playgod +
              (1 | Scenario), mm3c)
jtools::summ(mod2, digits = 3)

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

mm3 = mm3 %>%
  mutate(biotech = Condition == 'Biotechnological')

mod3 = glmer(biotech ~ effort + natural + useful + greatergood + 
              autonomy +  enhancement + irreversible + unfair + 
              cheating + external + normal + unsafe + futurerisk + 
              dehumanize + changeperson + playgod +
              (1 | Scenario), mm3, family = 'binomial')
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

byscen3 = mm3c %>%
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

figmm3 = mm3byscen %>%
  gather(effort:playgod, key = 'item', value = 'resp')

mm3means = mm3 %>%
  gather(effort:playgod, key = 'item', value = 'resp') %>%
  group_by(item, Condition) %>%
  summarise(resp = mean(resp, na.rm = TRUE), 
         moral = mean(moral, na.rm = TRUE))

mm3means$Condition = as.factor(mm3means$Condition)
levels(mm3means$Condition) = c('Technological', 'Conventional')

mmDistance = mm3means %>%
  pivot_wider(names_from = 'Condition', values_from = c('resp', 'moral')) %>%
  mutate(xDiff = resp_Conventional - resp_Technological, 
         yDiff = moral_Conventional - moral_Technological, 
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
levels(figmm3$Condition) = c('Technological', 'Conventional')

figmm3$itemLong = as.factor(figmm3$item)
levels(figmm3$itemLong) = c(
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
  'ACT was a useful and efficient way of ACHIEVING-Y'
)


figmm3 = left_join(figmm3, mmDistance, by = 'item')

mm3means = left_join(mm3means, mmDistance, by = 'item')

figmm3 = arrange(figmm3, desc(abs(diagDiff)))

ggplot(arrange(figmm3, desc(abs(diagDiff))), aes(x = resp, y = moral)) + 
  geom_smooth(fill = 'grey', method = 'lm', formula = y ~ x + x^2, 
              alpha = .4, color = NA)+
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

ggsave('figExp4.jpg', width = 12, height = 12, dpi = 400)

modItem = lmer(attitude ~ resp*item + Condition + (1 | Scenario), figmm3)
anova(modItem)

ggplot(figmm3, aes(x = resp, y = moral)) + 
  geom_smooth(fill = 'grey', method = 'lm', formula = y ~ x + x^2, 
              alpha = .4, color = NA)+
  geom_point(aes(color = Condition), alpha = .5, shape = 4, size = .8) +
  geom_line(data = mm3means, aes(x = resp, y = moral, group = reorder(item, abs(diagDiff))), linetype = 1, 
            color = 'black', linewidth = 1.2) +
  geom_point(data = mm3means, aes(x = resp, y = moral, color = Condition),
             size = 3, shape = 21, alpha = .8, fill = 'white', stroke = 1.5) +
  facet_wrap(~ reorder(item, -abs(diagDiff))) + 
  theme_minimal() + theme(legend.position = 'top') + 
  scale_color_brewer(palette = 'Dark2') + 
  scale_fill_brewer(palette = 'Dark2') + 
  scale_x_continuous(name = NULL, 
                     breaks = c(0, 50, 100)) +
  scale_y_continuous(name = 'Moral judgment', 
                     breaks = c(0, 50, 100)) + 
  theme(legend.position = 'top', panel.grid.minor = element_blank(),
        legend.title = element_blank(), strip.text = element_text(size = 10),
        plot.margin = margin(0.3,1,0.3, 1, "cm")) + 
  coord_fixed(ratio = 1, ylim = c(0, 100), xlim = c(0, 100))

ggsave('figExp4.jpg', width = 9, height = 9, dpi = 400)

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




