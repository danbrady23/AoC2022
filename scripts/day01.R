# Part 1

# Get data in, load as dataframe but keep blank lines 
data <- read.table("data/day01.txt", col.names = c("calories"), sep = "\n", blank.lines.skip = F)
# Find NAs and use them to mark the different groups of number
data["elf_no"] <- cumsum(is.na(data))
# Remove NAs
grouped_data <- na.omit(data)

# Summarise by summing by group (this would be significantly easier in tidyverse)
summarised_data <- aggregate(grouped_data$calories, list(grouped_data$elf_no), FUN = sum)
# Find the max value of the summarised data
elf_with_max_cals <- max(summarised_data)

# Part 2

# Find top 3 by sorting the summarised data 
sorted_summary <- sort(summarised_data$x, decreasing = T)

# Then selecting the top 3 values
n_top_elves <- 3
sum(sorted_summary[1:n_top_elves])
