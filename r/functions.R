# Function to Read and Prepare Data
read_qualtrics <- function(filename) {
  headers <- read_csv(filename, col_names = FALSE, n_max = 1) %>% as.character()
  data <- read_csv(filename, skip = 3, col_names = FALSE)
  colnames(data) <- headers
  return(data)
}

# Function to Process Coder Data
process_coder_data <- function(filename) {
  codes <- read_csv(filename, show_col_types = FALSE)
  codes %>%
    select(justID, anon_id, code1, code2, code3, code4, code5) %>%
    pivot_longer(cols = starts_with('code'), names_to = 'tag', values_to = 'category', values_drop_na = TRUE) %>%
    select(-tag) %>%
    mutate(code = 1) %>%
    filter(category != '', category != 'NA') %>%
    pivot_wider(names_from = category, values_from = code, values_fill = 0) %>%
    arrange(justID)
}
