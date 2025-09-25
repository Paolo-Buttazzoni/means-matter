library(tidyverse)

#Cleaning data

#This is "Do means matter morally Study 2"
filename = "mmm_studyA.csv"
headers = read_csv(filename, col_names = FALSE, n_max = 1)
ge2raw = read_csv(filename, skip = 3, col_names = FALSE)
colnames(ge2raw)= headers

glimpse(ge2raw)

ge2 = ge2raw %>%
  filter(Progress == 100, !is.na(PROLIFIC_PID)) %>%
  mutate(Endorsement = case_when(choice == 4 ~ 'Difference', 
                                 choice == 5 ~ 'Equivalence'), 
         Conservatism = (politics_1 + politics_2 )/2) %>%
  rename(
    Religiosity = religiosity_1,
    Social = politics_2,
    Fiscal = politics_1
  )

#Chi Square test to determine endorsement
chisq.test(xtabs(~ Endorsement, ge2))

xtabs(~ Endorsement, ge2)
#prop test to determine endorsement 
prop.test(131, 131+113, p = .5)
endorse = data.frame(View = c('Equivalence', 'Difference'),
                                  Proportion = c(140/334, 194/334))

#Non significant demographic tests to determine endorsement
t.test(age ~ Endorsement, ge2)
effectsize::cohens_d(age ~ Endorsement, data = ge2)
fisher.test(xtabs(~ gender + Endorsement, ge2))
t.test(Religiosity ~ Endorsement, ge2)
effectsize::cohens_d(Religiosity ~ Endorsement, data = ge2)
t.test(Fiscal ~ Endorsement, ge2)
effectsize::cohens_d(Fiscal ~ Endorsement, data = ge2)
t.test(Social ~ Endorsement, ge2)
effectsize::cohens_d(Social ~ Endorsement, data = ge2)

#Abstract views depends by order
t.test(rating_1 ~ Vignettes, ge2)
ggplot(ge2, aes(x = rating_1, group = Endorsement, color = Endorsement))+
  geom_density()
wilcox.test(subset(ge2, Endorsement == 'Equivalence')$rating_1, mu = 50)
endorsement_counts <- xtabs(~ Endorsement + Vignettes, data = ge2)
print(endorsement_counts)

#testing if it varies by age (n.s.)
cor.test(~ age + rating_1, ge2)
ggplot(ge2, aes(x = age, y = rating_1))+
  geom_jitter()+geom_smooth()

#testing other correlations (n.s. i think)
modG = lm(rating_1 ~ as.factor(gender)*Vignettes, ge2)
car::Anova(modG)
library(emmeans)
emmeans(modG, pairwise~ gender | Vignettes, adjust = 'none')

cor.test(~ Religiosity + rating_1, ge2)
cor.test(~ Social + rating_1, ge2)
cor.test(~ Fiscal + rating_1, ge2)

effectsize::cohens_d(age ~ Endorsement, data = ge2)
fisher.test(xtabs(~ gender + Endorsement, ge2))
t.test(Religiosity ~ Endorsement, ge2)
effectsize::cohens_d(Religiosity ~ Endorsement, data = ge2)
t.test(Fiscal ~ Endorsement, ge2)
effectsize::cohens_d(Fiscal ~ Endorsement, data = ge2)
t.test(Social ~ Endorsement, ge2)
effectsize::cohens_d(Social ~ Endorsement, data = ge2)


#looking at the variables
convDVs = ge2 %>%
  select(., contains(c("C1","C2", "C3", "C4"))) 

techDVs = ge2 %>%
  select(., contains(c("T1","T2", "T3", "T4")))

#cleaning each of the variables (C1-C4 are the DVs of interest; same for T1-T4)
convL =  ge2 %>%
  select(., contains(c("C1","C2", "C3", "C4")), PROLIFIC_PID) %>%
  gather(-PROLIFIC_PID, key = 'key', value = 'resp', na.rm = TRUE) %>%
  mutate(scen = str_sub(key, 1, 3),
         var = str_sub(key, 4, 5)) %>%
  pivot_wider(., id_cols = c('PROLIFIC_PID', 'scen'), names_from = 'var', values_from = 'resp') %>%
  mutate(resp = (C1 + C2 + C3 + C4)/4)

techL =  ge2 %>%
  select(., contains(c("T1","T2", "T3", "T4")), PROLIFIC_PID) %>%
  gather(-PROLIFIC_PID, key = 'key', value = 'resp', na.rm = TRUE) %>%
  mutate(scen = str_sub(key, 1, 3),
         var = str_sub(key, 4, 5)) %>%
  pivot_wider(., id_cols = c('PROLIFIC_PID', 'scen'), names_from = 'var', values_from = 'resp') %>%
  mutate(resp = (T1 + T2 + T3 + T4)/4)

#T-test across C/T 1:4
t.test(convL$C1, techL$T1)
t.test(convL$C2, techL$T2)
t.test(convL$C3, techL$T3)
t.test(convL$C4, techL$T4)

tdata = bind_rows(convL, techL) %>%
  mutate(Condition = if_else(is.na(C1), 'Technological', 'Conventional'))

#significant overall and across all pairs of scenarios p < 0.01
mod4 = lm(resp ~ scen*Condition, tdata)
emmeans(mod4, pairwise ~ Condition | scen)

#checking cronbach alpha
techL %>%
  select(T1:T4) %>%
  psych::alpha(., check.keys = TRUE)

convL %>%
  select(C1:C4) %>%
  psych::alpha(., check.keys = TRUE)

#overall aggregate approval + big cohens d
ge2 = ge2 %>%
  mutate(conv = rowMeans(convDVs, na.rm = TRUE), 
         tech = rowMeans(techDVs, na.rm = TRUE), 
         resp = if_else(is.na(conv), tech, conv))

t.test(resp ~ Methods, ge2)
effectsize::cohens_d(resp ~ Methods, data = ge2)

# T-test and effect size for those who endorse "Difference"
t.test(resp ~ Methods, data = subset(ge2, Endorsement == "Difference"))
effectsize::cohens_d(resp ~ Methods, data = subset(ge2, Endorsement == "Difference"))

# T-test and effect size for those who endorse "Equivalence"
t.test(resp ~ Methods, data = subset(ge2, Endorsement == "Equivalence"))
effectsize::cohens_d(resp ~ Methods, data = subset(ge2, Endorsement == "Equivalence"))



ggplot(ge2, aes(x = resp, group = Methods, color = Methods))+
  geom_histogram(alpha = .5, aes(y = ..density.., fill = Methods)) + 
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

ggsave('Figure1.jpg', dpi = 300, width = 7, height = 4)

#endorsement percentage (not used)
ggplot(endorse, aes(fill = View, y = Proportion, x = 0)) + 
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
  theme(legend.position = 'top', 
        plot.margin = margin(0.3,1,0.3, 1, "cm"))

ggsave('Figure2.jpg', dpi = 300, width = 7, height = 2)

ggplot(ge2, aes(x = resp, group = Methods, color = Methods))+
  geom_histogram(alpha = .5, aes(y = ..density.., fill = Methods)) + 
  geom_density(linewidth = 2) + facet_grid(Endorsement ~ .) + 
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

ggsave('Figure3.jpg', dpi = 300, width = 7, height = 4)



# more analysis, this feels like it should be separated into a new study?
justif = data.frame(Code = c('Autonomy', 'Utility', 'No Harm', 'Consent', 
                             'TED', 'Natural', 'Unsafe', 'Fairness', 'Effort', 'Intrinsic'),
                     Frequency = c(15, 18, 15, 19, 8, 27, 23, 14, 31, 8)) %>%
  mutate(Valence = if_else(Code %in% c('Autonomy', 'Utility', 'No Harm'), 
                           'In Favor', 'Against'), 
         Frequency = if_else(Valence == 'Against', -Frequency, Frequency))

ggplot(justif, aes(x = reorder(Code, abs(Frequency)), y = Frequency, color = Valence, fill = Valence)) + 
  geom_col(alpha = .5) + coord_flip() + facet_grid(Valence ~ ., scales = 'free_y', space = 'free') + 
  theme_minimal() + 
  scale_color_manual(values = c( 'firebrick', 'grey')) + 
  scale_fill_manual(values = c('firebrick', 'grey')) + 
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = 'Frequency', 
                     breaks = seq(-30, 30, 10), 
                     labels = c('30', '20', '10', '0', '10', '20', '30'),
                     sec.axis = dup_axis(name = NULL)) + 
  theme(legend.position = 'none', panel.grid.major.y = element_blank(),
        plot.margin = margin(0.3,1,0.3, 1, "cm"))

ggsave('Figure4.jpg', dpi = 300, width = 9, height = 4)

ggplot(ge2, aes(x = resp, group = Methods, color = Methods))+
  geom_density()+facet_grid(. ~ Endorsement)

ggplot(ge2, aes(x = resp, group = Endorsement, color = Endorsement))+
  geom_density()+facet_grid(. ~ Methods)



mod1 = lm(resp ~ Endorsement*Methods*Vignettes, ge2)
anova(mod1)
emmeans(mod1, pairwise  ~ Endorsement | Methods)
emmeans(mod1, pairwise  ~ Endorsement | Methods * Vignettes)

effectsize::cohens_d(resp ~ Methods, data = subset(ge2, Endorsement == 'Difference'))
effectsize::cohens_d(resp ~ Methods, data = subset(ge2, Endorsement == 'Equivalence'))

mod1b = lm(resp ~ rating_1*Methods*Vignettes, ge2)
anova(mod1b)
emtrends(mod1b, pairwise  ~ Methods, var = 'rating_1')
emtrends(mod1b, pairwise  ~ Methods | Vignettes, var = 'rating_1')
emtrends(mod1b, pairwise  ~ Vignettes | Methods, var = 'rating_1')

interactions::sim_slopes(mod1b, pred = 'rating_1', modx = 'Methods', digits = 3, confint = TRUE)

ge2$gender = as.factor(ge2$gender)
levels(ge2$gender) = c('Male', 'Female', 'NB')
mod1c = lm(resp ~ age*Methods*Vignettes, ge2)
anova(mod1c)
emtrends(mod1c, pairwise  ~ Methods, var = 'age')

mod1c = lm(resp ~ gender*Methods*Vignettes, ge2)
anova(mod1c)
emmeans(mod1c, pairwise  ~ gender | Methods, adjust = 'none')

mod1c = lm(resp ~ Social*Methods*Vignettes, ge2)
anova(mod1c)
emtrends(mod1c, pairwise  ~ Methods, var = 'Social')

mod1c = lm(resp ~ Fiscal*Methods*Vignettes, ge2)
anova(mod1c)
emtrends(mod1c, pairwise  ~ Methods, var = 'Fiscal')

psych::describe(ge2$Fiscal)
psych::describe(ge2$Social)

ge2 %>%
  filter(Methods == 'Technological') %>%
  select(age, Social, Fiscal, Religiosity, rating_1, resp) %>%
  apaTables::apa.cor.table(., 'tech1.doc')

ge2 %>%
  filter(Methods == 'Conventional') %>%
  select(age, Social, Fiscal, Religiosity, rating_1, resp) %>%
  apaTables::apa.cor.table(., 'conv1.doc')

ge2$pol = (ge2$Religiosity + ge2$Social + ge2$Fiscal)/3

## Significant effect here
ge2 %>%
  select(Religiosity, Fiscal, Social) %>%
  psych::alpha(., check.keys = TRUE)

cor.test(~ pol + resp, subset(ge2, Methods == 'Technological'))
cor.test(~ pol + resp, subset(ge2, Methods == 'Conventional'))

modFig = lm(resp ~ (gender + pol)*Methods, ge2)
car::Anova(modFig)
interactions::interact_plot(modFig, pred = 'pol', modx = 'Methods', interval = TRUE)

# main effect of gender driven by NB
ggplot(ge2, aes(x = gender, y = resp, fill = gender)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Effect of Gender on Response", x = "Gender", y = "Response")

# Visualization of the main effect of Methods
ggplot(ge2, aes(x = Methods, y = resp, fill = Methods)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Effect of Methods on Response", x = "Methods", y = "Response")


### act impact

## im not sure how to interpret the ACT impact 
cor.test(~ pol + actscore, ge2)
cor.test(~ pol + impscore, ge2)

figScen = tdata %>%
  group_by(scen) %>%
  summarise(c1 = mean(C1, na.rm = TRUE),
            t1 = mean(T1, na.rm = TRUE),
            c2 = mean(C2, na.rm = TRUE),
            t2 = mean(T2, na.rm = TRUE),
            c3 = mean(C3, na.rm = TRUE),
            t3 = mean(T3, na.rm = TRUE),
            c4 = mean(C4, na.rm = TRUE),
            t4 = mean(T4, na.rm = TRUE)) %>%
  gather(-scen, key = 'key', value = 'value') %>%
  mutate(cond = str_sub(key, 1, 1), 
         depvar = str_sub(key, 2, 2)) %>%
  pivot_wider(., id_cols = c(scen, depvar), names_from ='cond', values_from = 'value')


library(ggrepel)

figScen = tdata %>%
group_by(scen) %>%
summarise(c1 = mean(C1, na.rm = TRUE),
          t1 = mean(T1, na.rm = TRUE),
          c2 = mean(C2, na.rm = TRUE),
          t2 = mean(T2, na.rm = TRUE),
          c3 = mean(C3, na.rm = TRUE),
          t3 = mean(T3, na.rm = TRUE),
          c4 = mean(C4, na.rm = TRUE),
          t4 = mean(T4, na.rm = TRUE)) %>%
  gather(-scen, key = 'key', value = 'value') %>%
  mutate(cond = str_sub(key, 1, 1),
         depvar = str_sub(key, 2, 2)) %>%
  pivot_wider(., id_cols = c(scen, depvar), names_from ='cond', values_from = 'value')


numScen = tdata %>%
  group_by(scen) %>%
  summarise(c1 = mean(C1, na.rm = TRUE),
            t1 = mean(T1, na.rm = TRUE),
            c2 = mean(C2, na.rm = TRUE),
            t2 = mean(T2, na.rm = TRUE),
            c3 = mean(C3, na.rm = TRUE),
            t3 = mean(T3, na.rm = TRUE),
            c4 = mean(C4, na.rm = TRUE),
            t4 = mean(T4, na.rm = TRUE), 
            conv = (c1 + c2 + c3 + c4)/4,
            tech = (t1 + t2 + t3 + t4)/4) %>%
  select(scen, tech, conv) %>% 
  gather(-scen, key = 'condition', value = 'attitude')



ggplot(figScen, aes(x = c, y = t, label = scen, color = depvar))+
  geom_hline(yintercept = 50, linetype = 3)+
  geom_vline(xintercept = 50, linetype = 3)+
  geom_point()+geom_label_repel()+
  facet_wrap(~ depvar)

glimpse(ge2)

t.test(imprate ~ AIchoice, ge2)
t.test(imprate ~ ActImp, ge2)

ge2 = ge2 %>%
  mutate(ActImp = case_when(AIchoice == 2 ~ 'Impact', 
                            AIchoice == 1 ~ 'Act'))

act = ge2 %>%
  select(AIscale_1:AIscale_9) 

impact = ge2 %>%
  select(AIscale_10:AIscale_18)

ib = ge2 %>%
  select(OUS_1:OUS_5)

ih = ge2 %>%
  select(OUS_6:OUS_9)

impact = ge2 %>%
  select(AIscale_10:AIscale_18)


ge2 = ge2 %>%
  mutate(actscore = rowMeans(act, na.rm = TRUE), 
         impscore = rowMeans(impact, na.rm = TRUE), 
         ibscore = rowMeans(ib, na.rm = TRUE)-25, 
         ihscore = rowMeans(ih, na.rm = TRUE)-25)
glimpse(ge2 )

xtabs(~ ActImp + Endorsement, ge2)
chisq.test(xtabs(~ ActImp + Endorsement, ge2))

mod2 = lm(resp ~ ActImp*Methods*Vignettes, ge2)
anova(mod2)

glimpse(ge2)

ge2 %>%
  select(AIscale_1:AIscale_18) %>%
  stats::factanal(., factor = 2)

t.test(actscore ~ ActImp, ge2)
t.test(impscore ~ ActImp, ge2)
t.test(ibscore ~ ActImp, ge2)
t.test(ihscore ~ ActImp, ge2)

effectsize::cohens_d(resp ~ Methods, data = ge2)

mod3 = lm(resp ~ (actscore + impscore)*Methods, ge2)
anova(mod3)

mod4 = lm(resp ~ (ibscore + ihscore + pol)*Methods, ge2)
anova(mod4)

interactions::interact_plot(mod4, pred = 'ibscore', modx = 'Methods')

cor.test(~ ihscore + resp, ge2, subset = Methods == 'Technological')
cor.test(~ ihscore + resp, ge2, subset = Methods == 'Conventional')

emtrends(mod3, pairwise ~ Methods, var = 'impscore')
emtrends(mod4, pairwise ~ Methods, var = 'ihscore')

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

cor.test(~safe_1 + conseq_1, ge2)
t.test(safe_1 ~ Methods, ge2)
t.test(conseq_1 ~ Methods, ge2)
cor.test(~safe_1 + resp, ge2)
cor.test(~conseq_1 + resp, ge2)


ge2$futureharm = (ge2$conseq_1 + ge2$safe_1)/2 
meanConvFH = mean(subset(ge2, Methods == 'Conventional')$futureharm, na.rm = TRUE)
ge2$futureharm = ge2$futureharm - meanConvFH

mod4 = lm(resp ~ (safe_1)*Methods, ge2)
interactions::interact_plot(mod4, pred = 'safe_1', modx = 'Methods', interval = TRUE, 
                            plot.points = TRUE)
mod4 = lm(resp ~ (conseq_1)*Methods, ge2)
interactions::interact_plot(mod4, pred = 'conseq_1', modx = 'Methods', interval = TRUE, 
                            plot.points = TRUE)

mod4 = lm(resp ~ futureharm*Methods, ge2)
anova(mod4)
interactions::interact_plot(mod4, pred = 'futureharm', modx = 'Methods', interval = TRUE, 
                            plot.points = TRUE)

ge2 %>% group_by(Methods) %>% summarise(min(futureharm, na.rm = TRUE),
                                        max(futureharm, na.rm = TRUE))
# common support
mod5 = lm(resp ~ Methods*(actscore), ge2, subset = futureharm > -3.1 & futureharm < 32.4)
anova(mod5)

ggplot(ge2, aes(x = futureharm, y = resp, 
                color = Methods))+
  geom_vline(xintercept = -3.11, linetype = 2)+
  geom_vline(xintercept = 32.4, linetype = 2)+
  geom_jitter()+geom_smooth(method = 'lm')+
  theme_minimal()+ylim(0,100)+
  scale_color_brewer(palette = 'Set2')


ggplot(subset(ge2, impscore > 2), aes(x = actscore, y = resp, color = Methods))+
  geom_jitter()+geom_smooth(method = 'lm')+
  theme_minimal()+ylim(0,100)+
  scale_color_brewer(palette = 'Set2')

names(ge2)


