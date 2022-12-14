library(stringr)

data <- read.table("data/day04.txt", col.names = "source")

# Had to use stringr because regex is hard
split_data <- str_split(data$source, "-|,", simplify = T)
# Convert split character matrix to data frame with numeric elements
numeric_data <- data.frame( apply(split_data, 2, as.numeric) )
# Add in colnames
colnames(numeric_data) <- c("E1_start", "E1_end", "E2_start", "E2_end")

# Part 1

# determines whether values for inner_name fall within outer_name
contains <- function(data, inner_name, outer_name){
  
  outer_names <- paste(outer_name, c("start", "end"), sep = "_")
  inner_names <- paste(inner_name, c("start", "end"), sep = "_")
  
  start_within <- (data[[outer_names[1]]] <= data[[inner_names[1]]])
  end_within <- (data[[outer_names[2]]] >= data[[inner_names[2]]])
  
  start_within & end_within
}

# Is E1 in E2?
numeric_data$E1in <- contains(numeric_data, "E1", "E2")
# Is E2 in E1?
numeric_data$E2in <- contains(numeric_data, "E2", "E1")

# Are there overlaps in either? 
numeric_data$eitherIn <- (numeric_data$E1in | numeric_data$E2in)

# Sum it all up
sum(numeric_data$eitherIn)

# Part 2

# do values for inner_name overlap with outer_name
overlap <- function(data, inner_name, outer_name) {
  
  outer_names <- paste(outer_name, c("start", "end"), sep = "_")
  inner_names <- paste(inner_name, c("start", "end"), sep = "_")
  
  start_between <- between(numeric_data[[inner_names[1]]],
                           numeric_data[[outer_names[1]]],
                           numeric_data[[outer_names[2]]])
  
  end_between <- between(numeric_data[[inner_names[2]]],
                         numeric_data[[outer_names[1]]],
                         numeric_data[[outer_names[2]]])
  
  start_between | end_between
  
}

# Does a value fall between the upper and lower bounds
between <- function(values, lower, upper) {
  (values >= lower) & (values <= upper)
}

# E1 overlap with E2?
numeric_data$E1_overlap <- overlap(numeric_data, "E1", "E2")
# E2 overlap with E1?
numeric_data$E2_overlap <- overlap(numeric_data, "E2", "E1")

# Any overlap?
numeric_data$anyOverlap <- (numeric_data$E1_overlap | numeric_data$E2_overlap)

sum(numeric_data$anyOverlap)
