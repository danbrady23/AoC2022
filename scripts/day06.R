library(stringr)
library(dplyr)

# Test Input
#raw_input <- "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

# Load input and split into individual chars
raw_input <- readLines("data/day06.txt", warn = F)
split_input <- tibble(str_split_1(raw_input, ""))

# Set the header length and the number of lagged variables needed
header_len <- 14
N_lag <- seq(from = 0, by = 1, length.out = header_len)

# Created the lagged variables
lagged <- as.data.frame(sapply(N_lag, lag, x = split_input))
# Apply meaningful variable names
colnames(full_input) <- paste0("input_l", N_lag)

# Apply rowwise grouping
row_input <- rowwise(full_input)

parsed_data <- mutate(
  row_input,
  # How many items
  n = length(c_across(starts_with("input"))),
  # How many unique items
  n_unique = length(unique(c_across(starts_with("input")))),
  # Account for NAs from lagged variables
  has_na = any(is.na(c_across(starts_with("input")))),
  # find all rows where n unique and n are the same, accounting for NAs
  all_diff = (n == n_unique) & !has_na) 

# Find first time that all the all_diff variable shows true
min(which(parsed_data$all_diff))
