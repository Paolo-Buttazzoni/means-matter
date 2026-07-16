library(tidyverse)

#'Do means matter morally?' Study 2b

# Load dataset
filename <- "private/study3a/data/study3a.csv"
headers <- read_csv(filename, col_names = FALSE, n_max = 1)
mm3araw <- read_csv(filename, skip = 3, col_names = FALSE)
colnames(mm3araw) <- headers

# Create anon_id mapping
anon_map <- mm3araw %>%
  distinct(PROLIFIC_PID) %>%
  mutate(anon_id = paste0("anon_", row_number()))

# Join anon_id into dataset
mm3araw_Anon <- mm3araw %>%
  left_join(anon_map, by = "PROLIFIC_PID") %>%
  select(-c(PROLIFIC_PID, LocationLatitude, LocationLongitude, IPAddress))

# Save anonymized dataset
write_csv(mm3araw_Anon, "data/study3a/study3a_anon.csv")

# Save mapping table separately 
write_csv(anon_map, "private/study3a/data/anon_id_map_S3a.csv")



