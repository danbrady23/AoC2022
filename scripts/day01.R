# Part 1

data <- read.table("data/day1.txt", sep = "\n", blank.lines.skip = F)
data["elf_no"] <- cumsum(is.na(data))
grouped_data <- na.omit(data)

summarised_data <- aggregate(grouped_data$V1, list(grouped_data$elf_no), FUN = sum)
elf_with_max_cals <- max(summarised_data)

# Part 2
sorted_summary <- sort(summarised_data$x, decreasing = T)

n_top_elves <- 3

sum(sorted_summary[1:n_top_elves])
