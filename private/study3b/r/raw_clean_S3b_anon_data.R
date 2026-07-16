library(tidyverse)

#'Do means matter morally?' Study 2b

# Load dataset
filename <- "private/study3b/data/study3b.csv"
headers <- read_csv(filename, col_names = FALSE, n_max = 1)
mm3braw <- read_csv(filename, skip = 3, col_names = FALSE)
colnames(mm3braw) <- headers

# Create anon_id mapping
anon_map <- mm3braw %>%
  distinct(PROLIFIC_PID) %>%
  mutate(anon_id = paste0("anon_", row_number()))

# Join anon_id into dataset
mm3braw_Anon <- mm3braw %>%
  left_join(anon_map, by = "PROLIFIC_PID") %>%
  select(-c(PROLIFIC_PID, LocationLatitude, LocationLongitude, IPAddress))

# Save anonymized dataset
write_csv(mm3braw_Anon, "data/study3b/study3b_anon.csv")

# Save mapping table separately 
write_csv(anon_map, "private/study3b/data/anon_id_map_S3b.csv")



