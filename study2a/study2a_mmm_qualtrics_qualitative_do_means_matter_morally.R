# Load Required Libraries
library(tidyverse)
library(effectsize)
library(psych)
library(jtools)
library(emmeans)
library(ggrepel)
library(viridis) # For color-blind-friendly palettes

# Function to Read and Prepare Data
read_qualtrics <- function(filename) {
  headers <- read_csv(filename, col_names = FALSE, n_max = 1) %>% as.character()
  data <- read_csv(filename, skip = 3, col_names = FALSE)
  colnames(data) <- headers
  return(data)
}

#Qualitative_Do_Means_Matter_Morally!!

# Load Data
mm2a <- read_qualtrics('study2a/study2a.csv')

mm2a = mm2a %>%
  filter(Progress == 100, !is.na(PROLIFIC_PID)) %>%
  mutate(
    Endorsement = if_else(choice == 4, 'Difference', 'Equivalence'),
    Conservatism = (politics_1 + politics_2 + politics_3) / 3,
    Religiosity = religiosity_1,
    Social = politics_3,
    Fiscal = politics_2,
    strata = factor(STUDY_ID, labels = c('Left_UK', 'Left_UK', 'Right_UK', 'Right_UK', 
                                         'Right_US', 'Right_US', 'Left_US', 'Left_US')),
    #left/right -> L/R
    politics = str_sub(strata, 1, 1),
    #extract UK or US
    country = str_sub(strata, -2))

xtabs(~ Endorsement, mm2a)
prop.test(xtabs(~ Endorsement, mm2a), p = .5)

mm2a = mm2a %>%
  mutate(length_char = if_else(!is.na(reasonsDiff), 
                               nchar(reasonsDiff), nchar(reasonsEquiv)), 
         spaces = if_else(!is.na(reasonsDiff), 
                          str_count(reasonsDiff, pattern = " "), str_count(reasonsEquiv, pattern = " ")))

ggplot(mm2a, aes(x= length_char, fill = Endorsement)) + 
  geom_density(alpha = .5) + 
  theme_bw() + 
  scale_x_log10(name = "Justification\n(length in characters)", 
                     limits = c(20, 3000), 
                breaks = c(30, 100, 300, 1000)) + 
  theme(legend.position = c(.7, .7))
  
wilcox.test(length_char ~ Endorsement, mm2a)

DescTools::MedianCI(mm2a$length_char)
quantile(mm2a$length_char)
quantile(mm2a$spaces)


# Descriptive Statistics
mm2a %>%
  group_by(Endorsement) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    mean_religiosity = mean(Religiosity, na.rm = TRUE),
    sd_religiosity = sd(Religiosity, na.rm = TRUE),
    mean_fiscal = mean(Fiscal, na.rm = TRUE),
    sd_fiscal = sd(Fiscal, na.rm = TRUE),
    mean_social = mean(Social, na.rm = TRUE),
    sd_social = sd(Social, na.rm = TRUE)
  ) %>%
  View(.)



# Relpref for Calculation and Plotting (Diff - Equiv)

mm2a <- mm2a %>% mutate(relpref = rating_1 - rating_2)
lm(relpref ~ Endorsement, mm2a) %>%
  jtools::summ(.)


#Other stuff
# Perform Statistical Tests
perform_tests <- function(var, data) {
  list(
    t_test = t.test(reformulate('Endorsement', var), data),
    cohens_d = effectsize::cohens_d(reformulate('Endorsement', var), data = data)
  )
}

test_results <- list(
  age = perform_tests('age', ge1),
  Religiosity = perform_tests('Religiosity', ge1),
  Fiscal = perform_tests('Fiscal', ge1),
  Social = perform_tests('Social', ge1)
)

# Fisher's Exact Test for Gender and Endorsement
fisher_test_result <- fisher.test(xtabs(~ gender + Endorsement, ge1))

# Correlation Tests
cor_tests <- list(
  religiosity_social = cor.test(~ Religiosity + Social, ge1),
  religiosity_fiscal = cor.test(~ Religiosity + Fiscal, ge1),
  rating2_religiosity = cor.test(~ rating_2 + Religiosity, ge1),
  rating1_religiosity = cor.test(~ rating_1 + Religiosity, ge1)
)

# Alpha Reliability Analysis for Religiosity and Social
alpha_result <- ge1 %>%
  select(Religiosity, Social) %>%
  psych::alpha(., check.keys = TRUE)


set.seed(123)  # Set seed for reproducible jitter
ggplot(mm2a, aes(x = Religiosity, y = relpref)) +
  geom_jitter(width = 0.2, height = 0.2) +  # Consistent jitter
  geom_smooth() +
  labs(title = 'Religiosity vs Relative Preference',
       x = 'Religiosity',
       y = 'Relative Preference (Diff - Equiv)') +
  theme_minimal()

# Grouped Means for Strata
strata_means <- ge1 %>%
  group_by(strata) %>%
  summarise(
    mean_rating1 = mean(rating_1, na.rm = TRUE),
    mean_rating2 = mean(rating_2, na.rm = TRUE)
  )

# Reshape Data for Figure 1
fig1 <- mm2a %>%
  select(Difference = rating_1, Equivalence = rating_2, country, politics) %>%
  gather(Difference:Equivalence, key = 'key', value = 'Rating')

# View Results
View(strata_means)
View(fig1)

# Prepare Data for Justification Analysis
just1 <- mm2a %>%
  select(reasonsDiff, reasonsEquiv, Endorsement, PROLIFIC_PID) %>%
  pivot_longer(cols = starts_with('reasons'), names_to = 'key', values_to = 'response', values_drop_na = TRUE) %>%
  select(-key)

write.csv(just1, 'study2a/justifications.csv', row.names = FALSE)

# Function to Process Coder Data
process_coder_data <- function(filename) {
  codes <- read_csv(filename, show_col_types = FALSE)
  codes %>%
    select(justID, PROLIFIC_PID, code1, code2, code3, code4, code5) %>%
    pivot_longer(cols = starts_with('code'), names_to = 'tag', values_to = 'category', values_drop_na = TRUE) %>%
    select(-tag) %>%
    mutate(code = 1) %>%
    filter(category != '', category != 'NA') %>%
    pivot_wider(names_from = category, values_from = code, values_fill = 0) %>%
    arrange(justID)
}

codesI <- process_coder_data('study2a/Do Means Matter Open Ended Responses - Ivar.csv')
codesJ <- process_coder_data('study2a/Do Means Matter Open Ended Responses - Jon.csv')

# Combine and Analyze Coder Data
codesI <- codesI %>% pivot_longer(cols = -c(justID, PROLIFIC_PID), names_to = 'category', values_to = 'c1resp')
codesJ <- codesJ %>% pivot_longer(cols = -c(justID, PROLIFIC_PID), names_to = 'category', values_to = 'c2resp')

codesIJ <- full_join(codesI, codesJ, by = c('justID', 'category', 'PROLIFIC_PID')) %>%
  mutate(status = if_else(c1resp == c2resp, 'Agreement', 'Disagreement'),
         present = case_when(
           c1resp + c2resp == 2 ~ 'Present',
           c1resp + c2resp == 0 ~ 'Absent'
         ))

# Cohen's Kappa
cohen_kappa <- psych::cohen.kappa(x = cbind(codesIJ$c1resp, codesIJ$c2resp))

# Visualization Preserved
tempdt <- codesIJ %>%
  filter(present == 'Present') %>%
  left_join(mm2a, by = 'PROLIFIC_PID') %>%
  mutate(difference = Endorsement == 'Difference')

jmod <- glm(difference ~ category, tempdt, family = 'binomial')
fig1 <- as.data.frame(emmeans(jmod, ~ category, adjust = 'none', type = 'response'))


mm2a = codesIJ %>%
  mutate(just_value = present == 'Present') %>%
  full_join(mm2a, by = 'PROLIFIC_PID')

mod_just = glmer(just_value ~ category * Endorsement + (1 | PROLIFIC_PID), 
               mm2a, family = 'binomial')
emmeans(mod_just, pairwise ~ Endorsement | category, type = 'response')

tempdt %>%
  group_by(category) %>%
  summarise(mean_diff = mean(difference, na.rm = TRUE) - .5, 
            freq = n()) %>%
  ggplot(aes(x = mean_diff, y = freq)) + 
  annotate(geom = 'rect', xmin = -.5, xmax = 0, ymin = 0, ymax = Inf, fill = 'grey', 
           alpha = .3) +
  annotate(geom = 'rect', xmin = 0, xmax = .5, ymin = 0, ymax = Inf, fill = 'firebrick', 
           alpha = .3) +
  geom_vline(xintercept = 0, linetype = 2, color = 'white', linewidth = 1) +
  theme_classic() +
  geom_col(width = .005, position = 'identity') + 
  geom_point(size = 1, fill = 'white', shape = 21, stroke = 1, color = 'black') +
  ggrepel::geom_label_repel(aes(label = category), xlim = c(-.5, .5), 
                           fontface = 'bold') + 
  scale_y_continuous(name = 'Frequency', limits = c(0, 30), expand = c(0, 0)) + 
  scale_x_continuous(name = 'Mean endorsement', limits = c(-.5, .53), 
                     labels = c('Equivalence', 'Both\nequally', 'Difference'), 
                     breaks = c(-.4, 0, .42),
                     expand = c(0, 0)) + 
  theme(axis.ticks = element_blank(),
        axis.title = element_text(size = 12, face = 'bold'),
        axis.text = element_text(size = 12),
        legend.position = 'bottom',
        plot.margin = margin(0.3,1,0.3, 1, "cm"),
        legend.title = element_text(size = 10))

ggsave('Figures/Figure2.jpg', dpi = 300, width = 13, height = 13, units = 'cm')
  
xtabs(~ category, tempdt)

#categories at the top have lower probability of endorsing "Difference" view

ggplot(fig1, aes(x = reorder(category, -prob), y = 1 - prob, label = category, color = prob)) +
  geom_linerange(aes(ymin = 1 - asymp.LCL, ymax = 1 - asymp.UCL), size = 1, alpha = 0.6) +  # Error bars
  geom_point(size = 3) +  # Points
  coord_flip() +  # Flip coordinates
  labs(
    x = 'Category',
    y = 'Probability of Endorsing "Equivalence" View',
    color = 'Probability of\nEndorsing "Difference" View'
  ) +
 # scale_color_viridis(option = "D", direction = -1) +  # Color-blind-friendly gradient
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = 'bottom',
    legend.title = element_text(size = 10)
  )

# more analysis, THIS FEELS LIKE A NEW STUDY
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

# Additional Analysis and Visualization Steps are Preserved
xtabs(~ Endorsement + category, tempdt)

jmod2 <- lm(scale(Conservatism) ~ category, tempdt)
jtools::summ(jmod2, confint = TRUE, digits = 3)
emmeans(jmod2, pairwise ~ category, adjust = 'none')

fig1b <- as.data.frame(emmeans(jmod2, ~ category, adjust = 'none'))

countDt <- tempdt %>%
  group_by(category) %>%
  tally()

fig1b <- left_join(fig1b, fig1, by = 'category') %>%
  left_join(., countDt, by = 'category')


ggplot(fig1b, aes(x = emmean, y = 1 - prob, label = category, color = prob)) +
  # Add a reference line
  geom_abline(slope = 0.5, intercept = 0.5, linetype = 3, color = "gray50") +
  
  # Add translucent error bars for y-axis
  geom_linerange(aes(ymin = 1 - asymp.LCL, ymax = 1 - asymp.UCL), alpha = 0.5) +
  
  # Add translucent error bars for x-axis
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), alpha = 0.5) +
  
  # Add solid points without size scaling
  geom_point(shape = 21, fill = 'white', color = 'black', size = 5) +
  
  # Add text labels with repulsion to avoid overlap
  geom_text_repel(
    box.padding = 0.5, # Adjust padding around text
    max.overlaps = Inf, # Allow more overlaps if necessary
    segment.color = "gray", # Color of the line connecting text to point
    segment.alpha = 0.5, # Translucent connecting lines
    min.segment.length = 0.2, # Shorten connecting lines
    size = 4, color = "black"
  ) +
  
  # Customize the color gradient with a color-blind friendly palette
  scale_color_viridis_c(option = 'plasma', direction = -1) +
  
  # Add axis labels and title
  labs(
    x = "Conservatism (emmean)",
    y = "Probability of Endorsing 'Equivalence' View",
    title = "Category Comparison with Confidence Intervals",
    subtitle = "Error bars are translucent; points are solid for clarity",
    color = "Probability of 'Difference' Endorsement"
  ) +
  
  # Improve theme for better readability
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = 'gray40'),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(size = 12)
  )


# Plot 2: Sample Size by Category as a Table
library(gt)

# Create a table of sample sizes by category
sample_size_table <- fig1b %>%
  select(category, n) %>%
  arrange(desc(n)) %>%
  gt() %>%
  tab_header(
    title = "Sample Size by Category",
    subtitle = "Displaying the number of observations per response category"
  )

sample_size_table

#graph seen in gdocs
# Prepare data for the plot
fig1b <- fig1b %>%
  mutate(
    Endorsement = ifelse(prob > 0.5, "Difference", "Equivalence"),
    Conservatism = emmean,
    Category = category
  )

# Create the conservatism plot
ggplot(fig1b, aes(x = Conservatism, y = 1 - prob, label = Category, color = Endorsement, size = n)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  
  geom_point(shape = 21, fill = 'white', alpha = 0.8) +
  geom_text_repel(
    box.padding = 0.5, 
    max.overlaps = Inf, 
    segment.color = "gray", 
    segment.alpha = 0.5, 
    min.segment.length = 0.2, 
    size = 5, 
    show.legend = FALSE
  ) +
  
  scale_color_manual(values = c('Equivalence' = 'purple', 'Difference' = 'brown')) +
  scale_size(range = c(3, 10), guide = 'none') +
  
  scale_x_continuous(limits = c(-0.6, 0.6)) +

  
  labs(
    x = "Conservatism",
    y = "Endorsement",
    title = "Moral Justifications by Conservatism and Endorsement",
    color = "Endorsement"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "top"
  )

temp_dt


