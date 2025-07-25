tiebreaker = read.csv('Do Means Matter Open Ended Responses - tiebreaker.csv') %>%
  select(justID, resolve, category)

glimpse(codesIJ)

codes2 = full_join(codesIJ, tiebreaker, by = c('justID', 'category')) %>%
  mutate(Present = if_else(status == 'Disagreement' & resolve == 1, 'Present', present)) %>%
  select(justID, category, Present, PROLIFIC_PID, response) 

mod

codesFig = codes2 %>%
  left_join(., ge1, by = 'PROLIFIC_PID') %>%
  mutate(
    conservatism = (Religiosity + Social)/2
  )
  
glimpse(codesFig)

codesFig$Difference = 5 - codesFig$choice 

modP = glm(Difference ~ Present*category, codesFig, family = 'binomial')
emmeans(modP, pairwise ~ Present | category, type = 'response')


codesFig %>%
  select(Religiosity:Social) %>%
  psych::alpha(., check.keys = TRUE)

fig1 = codesFig %>%
  mutate(z_cons = scale(Religiosity + politics_1 + Fiscal + Social)) %>%
  filter(Present == 'Present') %>%
  group_by(category) %>%
  summarise(
    conservatism = mean(z_cons, na.rm = TRUE),
    difference = mean(Difference, na.rm = TRUE),
    nObs = n()
  ) %>%
  mutate(
    color = (conservatism*2 + difference)/2
  )
  

ggplot(fig1, aes(x = conservatism, y = difference, label = category, 
                 color = color)) + 
  geom_point(aes(size = nObs),shape = 21, fill = 'white')+geom_text_repel(fontface= 'bold', size = 3.5) +
  geom_hline(yintercept = .5, linetype = 2, color = 'grey')+
  geom_vline(xintercept = 0, linetype = 2, color = 'grey')+
  scale_color_gradient2(low = 'navyblue', mid = 'grey', high = 'darkred', midpoint = 0.2)+
  scale_y_continuous(name = 'Endorsement', 
                     breaks = seq(.25, .75, .25), limits = c(0, 1),
                     labels = c('Equivalence', '', 'Difference'), 
                     guide = guide_axis(angle = 90))+
  scale_x_continuous(name = 'Religious conservatism', 
                     breaks = seq(-.5, .5, .5), limits = c(-.6, .6),
                     labels = c('Secular\nliberal', 'Moderate', 'Religious\nconservative'))+
  theme_minimal()+theme(legend.position = 'none', 
                        panel.grid = element_blank())

ggsave('Figure1GE.jpg', width = 14, height = 10, units = 'cm', dpi = 300)


ggplot(ge1, aes(x = 5 - choice, y = relpref))+geom_jitter()

codes2wide = codes2 %>%
  mutate(resp = case_when(
    Present == 'Present' ~ 1,
    Present == 'Absent' ~ 0,
  )) %>%
  pivot_wider(., id_cols = c('justID', 'PROLIFIC_PID', 'response'), 
              names_from = 'category', values_from = 'resp') %>%
  select(-UNFAIR)
  
glimpse(codes2wide)

xtabs(~ category + Present, codes2)

ge1add = full_join(ge1, codes2wide, by = 'PROLIFIC_PID')
glimpse(ge1add)


ge1add$Difference = 5 - ge1add$choice 

glimpse(ge1add)

regData = ge1add %>%
  select(Difference, NATURAL:TED) %>%
  select(-FAIRNESS, -TED, -INTRINSIC)

mod1 = glm(Difference ~ ., regData, family = 'binomial')
jtools::summ(mod1, confint = TRUE, digits = 3, exp = TRUE)

drop1.test = drop1(mod1, test = 'Chisq')
drop1.test

mod2 = glm(Difference ~ 0 + ., regData, family = 'binomial')
jtools::summ(mod2, confint = TRUE, digits = 3, exp = TRUE)

drop2.test = drop1(mod2, test = 'Chisq')
drop2.test

