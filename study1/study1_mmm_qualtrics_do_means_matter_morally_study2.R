# Load necessary libraries
library(tidyverse)
library(emmeans)
library(effectsize)
library(interactions)
library(psych)

#'Do means matter morally?' study 2
# --- Step 1: Load and Clean Data ---

# Load dataset
filename = "mmm_studyA.csv"
headers = read_csv(filename, col_names = FALSE, n_max = 1)
ge2raw = read_csv(filename, skip = 3, col_names = FALSE)
colnames(ge2raw) = headers

# Filter to complete responses
ge2 = ge2raw %>%
  filter(Progress == 100, !is.na(PROLIFIC_PID)) %>%
  mutate(
    Endorsement = case_when(choice == 4 ~ "Difference",
                            choice == 5 ~ "Equivalence"),
    Conservatism = (politics_1 + politics_2) / 2,
    Religiosity = religiosity_1,
    Social = politics_2,
    Fiscal = politics_1
  )

# --- Step 2: Test Whether Endorsements are Balanced (Chi-square, Prop Test) ---

# Chi-square test: Are Difference and Equivalence endorsed equally?
endorsement_table = xtabs(~ Endorsement, ge2)
chisq.test(endorsement_table)

# Proportion test: Is proportion = 0.5?
prop.test(endorsement_table["Equivalence"], sum(endorsement_table), p = 0.5)

# --- Step 3: Prepare Moral Judgment Data Across Scenarios ---

# Select Conventional and Technological moral judgments
convDVs = ge2 %>% select(contains(c("C1", "C2", "C3", "C4")))
techDVs = ge2 %>% select(contains(c("T1", "T2", "T3", "T4")))

# Create composite moral judgment for each participant
ge2 = ge2 %>%
  mutate(
    conv = rowMeans(convDVs, na.rm = TRUE),
    tech = rowMeans(techDVs, na.rm = TRUE),
    resp = if_else(is.na(conv), tech, conv) # Pick whichever is available
  )

# --- Step 4: Compare Each Conventional vs Technological DV (4 pairs) ---

# 1. Reshape to long format and extract scenario labels
convL = ge2 %>%
  select(PROLIFIC_PID, contains(c("C1", "C2", "C3", "C4"))) %>%
  pivot_longer(-PROLIFIC_PID, names_to = "Scenario", values_to = "Value") %>%
  mutate(
    scen = str_sub(Scenario, 1, 2),  # Extract "C1", "C2", etc
    Type = "Conventional"
  )

techL = ge2 %>%
  select(PROLIFIC_PID, contains(c("T1", "T2", "T3", "T4"))) %>%
  pivot_longer(-PROLIFIC_PID, names_to = "Scenario", values_to = "Value") %>%
  mutate(
    scen = str_sub(Scenario, 1, 2),  # Extract "T1", "T2", etc
    Type = "Technological"
  )

# 2. Build the combined dataset properly
compare_df = left_join(
  convL %>% rename(Value_conv = Value),
  techL %>% rename(Value_tech = Value),
  by = c("PROLIFIC_PID", "scen")
)

# 3. Now, Overall average across all Conventional vs Technological scenarios (13 scenarios collapsed)
conv_avg = compare_df %>%
  group_by(PROLIFIC_PID) %>%
  summarise(ConvAvg = mean(Value_conv, na.rm = TRUE))

tech_avg = compare_df %>%
  group_by(PROLIFIC_PID) %>%
  summarise(TechAvg = mean(Value_tech, na.rm = TRUE))

# Merge and t-test overall Conventional vs Technological
comparison_data = left_join(conv_avg, tech_avg, by = "PROLIFIC_PID")
t.test(comparison_data$ConvAvg, comparison_data$TechAvg)

comparison_data_long = comparison_data %>%
  pivot_longer(cols = c(ConvAvg, TechAvg), names_to = "Condition", values_to = "Approval")

# Clean condition names
comparison_data_long = comparison_data_long %>%
  mutate(Condition = recode(Condition,
                            "ConvAvg" = "Conventional",
                            "TechAvg" = "Technological"))

# Now plot
ggplot(comparison_data_long, aes(x = Condition, y = Approval, fill = Condition)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.4) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Overall Moral Approval: Conventional vs Technological Methods",
    y = "Moral Judgment (Average Approval)",
    x = "Condition"
  ) +
  theme(legend.position = "none")

# 4. Compare each C1 vs T1, C2 vs T2, C3 vs T3, C4 vs T4 individually
# Extract Conventional C1-C4
C1 = ge2 %>% select(PROLIFIC_PID, contains("C1"))
C2 = ge2 %>% select(PROLIFIC_PID, contains("C2"))
C3 = ge2 %>% select(PROLIFIC_PID, contains("C3"))
C4 = ge2 %>% select(PROLIFIC_PID, contains("C4"))

# Extract Technological T1-T4
T1 = ge2 %>% select(PROLIFIC_PID, contains("T1"))
T2 = ge2 %>% select(PROLIFIC_PID, contains("T2"))
T3 = ge2 %>% select(PROLIFIC_PID, contains("T3"))
T4 = ge2 %>% select(PROLIFIC_PID, contains("T4"))

scenarios = list(
  list(C = C1, T = T1),
  list(C = C2, T = T2),
  list(C = C3, T = T3),
  list(C = C4, T = T4)
)

for (i in 1:4) {
  cat("\nComparing C", i, " vs T", i, "\n", sep = "")
  print(t.test(scenarios[[i]]$C[[2]], scenarios[[i]]$T[[2]], paired = FALSE))
}


# --- Step 5: Compare Abstract Views (rating_1) Between Conditions ---

#Rating 1: Morally speaking, technological methods of improvement are ______ than/to standard methods.
# Test if abstract approval differs by Vignette order
#determined that it is significant
t.test(rating_1 ~ Vignettes, data = ge2)

# Plot abstract endorsement by Endorsement category
ggplot(ge2, aes(x = rating_1, fill = Endorsement)) +
  geom_density(alpha = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(
    title = "Abstract Moral Approval: Technological vs Standard Methods",
    subtitle = "Comparison by Endorsement Type (Difference vs Equivalence)",
    x = "Moral Judgment: Technological Compared to Standard Methods",
    y = "Density",
    fill = "Endorsement"
  )

# --- Step 6: Abstract Views Predict Concrete Moral Judgments ---

# Does abstract view predict specific judgment (resp)?
#question for Ivar: does it make sense to do this? because we combine C1-C4,T1-T4
#resp is the mean moral judgment across C1–C4/T1-T4
mod_abstract = lm(resp ~ rating_1 * Methods, data = ge2)

#TOOD: Choice * Methods; emmeans(mod_abstract, pairwise ~ Methods | Choice)

summary(mod_abstract)

#break this down by methods

#if you say the methods are equivalent you have more favourable methods to 
#technological enhancement than people who say they are worse

#everyone ultimately thinks that conventional better than tech regardless of what they say
ggplot(ge2, aes(x = rating_1, y = resp, color=Methods)) +
  geom_point(alpha = 0.5) +  # scatter plot of points
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.5) +  # regression line with confidence band
  theme_minimal() +
  labs(
    title = "Abstract Moral Approval Predicts Concrete Judgments",
    subtitle = "Regression of Scenario Judgment (resp) on Abstract Approval (rating_1)",
    x = "Abstract Moral Approval (rating_1)",
    y = "Concrete Moral Judgment (resp)"
  )


# --- Step 7: Still Effect of Condition Among Equivalence Endorsers ---

ge2_equivalence = subset(ge2, Endorsement == "Equivalence")

# Model: Concrete moral judgment predicted by Methods and Abstract Approval
mod_equivalence = lm(resp ~ Methods * rating_1, data = ge2_equivalence)

# ANOVA table (tests if Methods, rating_1, and their interaction are significant)
anova(mod_equivalence)


#USE THE BINARY CHOICE HERE FOLLOWING IVARS CALL
# Estimated marginal means for Methods at different levels of abstract approval
library(emmeans)
em = emmeans(mod_equivalence, ~ Methods | rating_1)
summary(em)
pairs(em)

# Interaction plot: How abstract approval (rating_1) and Methods predict moral judgment (resp)
interactions::interact_plot(
  mod_equivalence,
  pred = "rating_1",
  modx = "Methods",
  interval = TRUE,
  x.label = "Abstract Moral Approval (Technological vs Standard Methods)",
  y.label = "Concrete Moral Judgment (Scenario Rating)",
  main.title = "Interaction: Abstract Approval × Methods (Equivalence Endorsers)"
)

