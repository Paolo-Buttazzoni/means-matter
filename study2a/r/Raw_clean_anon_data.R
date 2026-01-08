library(tidyverse)

#'Do means matter morally?' Study 2a

# Load dataset
filename <- "study2a/data/identificable/study2a.csv"
headers <- read_csv(filename, col_names = FALSE, n_max = 1)
mm2raw <- read_csv(filename, skip = 3, col_names = FALSE)
colnames(mm2raw) <- headers

# Create anon_id mapping
anon_map <- mm2raw %>%
  distinct(PROLIFIC_PID) %>%
  mutate(anon_id = paste0("anon_", row_number()))
# Join anon_id into dataset
mm2raw_Anon <- mm2raw %>%
  left_join(anon_map, by = "PROLIFIC_PID") %>%
  select(-c(PROLIFIC_PID, LocationLatitude, LocationLongitude, IPAddress))

# Save anonymized dataset
write_csv(mm2raw_Anon, "study2a/data/anonymized/study2a_anon.csv")

# Save mapping table separately 
write_csv(anon_map, "study2a/data/identificable/anon_id_map.csv")


# Anonymized data for open ended reponses coders

coder1 = read_csv('study2a/data/identificable/study2a_openEndedResponses_coder_I.csv')
coder2 = read_csv('study2a/data/identificable/study2a_openEndedResponses_coder_II.csv')
code_tiebreaker = read_csv('study2a/data/identificable/study2a_openEndedResponses_code_tiebraker.csv')

coder1_anon <- coder1 %>%
  left_join(anon_map, by = "PROLIFIC_PID") %>%
  select(-PROLIFIC_PID)

coder2_anon <- coder2 %>%
  left_join(anon_map, by = "PROLIFIC_PID") %>%
  select(-PROLIFIC_PID)

code_tiebreaker_anon <- code_tiebreaker %>%
  left_join(anon_map, by = "PROLIFIC_PID") %>%
  select(-PROLIFIC_PID)

write_csv(coder1_anon, "study2a/data/anonymized/coder1_anon.csv")
write_csv(coder2_anon, "study2a/data/anonymized/coder2_anon.csv")
write_csv(code_tiebreaker_anon, "study2a/data/anonymized/code_tiebreaker_anon.csv")





