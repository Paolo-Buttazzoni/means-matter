library(tidyverse)

#'Do means matter morally?' Study 1

# Load dataset
filename <- "study2a/study2a.csv"
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
  select(-c(PROLIFIC_PID, LocationLatitude, LocationLongitude))

# Save anonymized dataset
write_csv(mm2raw_Anon, "study2a/study2a_anon.csv")

# Save mapping table separately (optional)
write_csv(anon_map, "study2a/anon_id_map.csv")

