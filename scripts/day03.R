data <- c("vJrwpWtwJgWrhcsFMMfFFhFp",
          "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
          "PmmdzqPrVvPwwTWBwg",
          "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
          "ttgJtRGJQctTZtZT",
          "CrZsJsPPZsGzwwsLwLmpwMDw")

data <- read.table("data/day03.txt")$V1

split_data <- strsplit(data, "")


find_common_item <- function(data) {
  data_len <- length(data)
  first_half <- data[1:(data_len/2)]
  second_half <- data[((data_len/2)+1):data_len]
  
  intersect(first_half, second_half)
}

find_item_score <- function(item) {
  which(c(letters, LETTERS) %in% item)
}

common_item <- sapply(split_data, find_common_item)
common_item_pos <- sapply(common_item, find_item_score)
sum(common_item_pos)

# Part 2

common_items <- function(start_n, data) {
  Reduce(intersect, list(data[[start_n]],
                         data[[start_n + 1]],
                         data[[start_n + 2]]))
}

badge <- sapply(as.list(seq(from = 1, to = length(split_data), by = 3)), common_items, split_data)
badge_pos <- sapply(badge, find_item_score)
sum(badge_pos)
