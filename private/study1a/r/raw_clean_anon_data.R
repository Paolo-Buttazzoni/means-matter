library(tidyverse)

#'Do means matter morally?' Study 1

# Load dataset
filename <- "private/study1/data/study1.csv"
headers <- read_csv(filename, col_names = FALSE, n_max = 1)
mm1raw <- read_csv(filename, skip = 3, col_names = FALSE)
colnames(mm1raw) <- headers

# Create anon_id mapping
anon_map <- mm1raw %>%
  distinct(PROLIFIC_PID) %>%
  mutate(anon_id = paste0("anon_", row_number()))
# Join anon_id into dataset
mm1raw_Anon <- mm1raw %>%
  left_join(anon_map, by = "PROLIFIC_PID") %>%
  select(-c(PROLIFIC_PID, STUDY_ID, LocationLatitude, LocationLongitude))

# Save anonymized dataset
write_csv(mm1raw_Anon, "study1/study1_anon.csv")

# Save mapping table separately (optional)
write_csv(anon_map, "study1/anon_id_map.csv")

