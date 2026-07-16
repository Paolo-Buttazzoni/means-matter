library(tidyverse)

#'Do means matter morally?' Study 2b

# Load dataset
filename <- "private/data/study2b.csv"
headers <- read_csv(filename, col_names = FALSE, n_max = 1)
mm2braw <- read_csv(filename, skip = 3, col_names = FALSE)
colnames(mm2braw) <- headers

# Create anon_id mapping
anon_map <- mm2braw %>%
  distinct(PROLIFIC_PID) %>%
  mutate(anon_id = paste0("anon_", row_number()))

# Join anon_id into dataset
mm2braw_Anon <- mm2braw %>%
  left_join(anon_map, by = "PROLIFIC_PID") %>%
  select(-c(PROLIFIC_PID, STUDY_ID, LocationLatitude, LocationLongitude, IPAddress))

# Save anonymized dataset
write_csv(mm2braw_Anon, "study2b/data/study2b_anon.csv")

# Save mapping table separately 
write_csv(anon_map, "study2a/data/anon_id_map_S2b.csv")



