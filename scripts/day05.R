# 2 components:
# Stacks (first 10 lines)
# Instructions (11 - 512)

# Separate raw input
# Read the stacks from the input into a nested list (as above)
# Parse specific info from the instructions
#   N items from stack
#   Origin
#   Target

# Write function that uses instruction info to modify the stacks
# loop (?) through the instructions modifying stacks

# function (?) to get top crate from each stack

library(stringr)

# Load raw data
raw_data <- readLines("data/day05.txt", warn = F)

split_location <- which(raw_data == "")

raw_stacks <- raw_data[1:(split_location-1)]
stack_list <- extract_stacks(raw_stacks)
# May be possible to automate splitting this by finding the blank line,
# just need to find an appropriate way of detecting it

# Get the parts with the instructions
raw_instructions <- raw_data[(split_location+1):length(raw_data)]
instructions_parsed <- extract_instructions(raw_instructions)

# Data parsing functions

extract_instructions <- function(raw_instructions) {

  # Pull all digits from instructions
  instructions_chr <- str_extract_all(raw_instructions, "[:digit:]+", simplify = T)
  # Convert digits from characters to numbers 
  instructions_num <- data.frame( apply(instructions_chr, 2, as.numeric))
  # Add the column names, pulled from the first row of data
  colnames(instructions_num) <- unlist(str_extract_all(raw_instructions[1], "[a-z]+"))
  
  return(instructions_num)
}

extract_stacks <- function(stack) {
  
  # Reverse order of rows
  rev_stack <- rev(stack)
  
  # Get locations of numbers from bottom row
  stackN_locs <- str_locate_all(rev_stack[1], "[:digit:]")[[1]]
  
  # Number of stacks
  n_stacks <- nrow(stackN_locs)  

  # Generate list of stacks
  stack_out <- sapply(seq_len(n_stacks), extract_stack, stackN_locs, rev_stack)
  
  return(stack_out)
}

extract_stack <- function(location, stackN_locs, stack) {
  
  outlist <-list()
  # Pull out all values from a column
  all_vals <- str_sub(stack, stackN_locs[location,1], stackN_locs[location,2])
  # Get name for stack
  stack_number <- all_vals[1]
  # Get the rest of the crates in the stack
  # (reverse, so top crate is first in vector)
  stack_vals <- rev(all_vals[-1])
  
  # Remove empty items
  stack_vals <- stack_vals[stack_vals != " "]
  
  # Put values in list
  outlist[[stack_number]] <- stack_vals
  return(outlist)
}

# Part 1

run_instructions_9000 <- function(inst_row, stack_list){
  # Get crates
  moved_crates <- stack_list[[inst_row$from]][1:inst_row$move]
  # Reverse crates to account for moving single crate at time 
  moved_crates <- rev(moved_crates)
  # Place crates
  stack_list[[inst_row$to]] <- c(moved_crates, stack_list[[inst_row$to]])
  # Remove crates
  stack_list[[inst_row$from]] <- stack_list[[inst_row$from]][-(1:inst_row$move)]
  
  return(stack_list)
}

# Run through instruction list
for (inst_row in seq_len( nrow(instructions_parsed) )) {
  stack_list <- run_instructions_9000(instructions_parsed[inst_row, ], stack_list)
} 

# Get top crates
top_crates <- sapply(stack_list, head, 1)
paste0(top_crates, collapse = "")

# Part 2

# Rewritten without reversal
run_instructions_9001 <- function(inst_row, stack_list){
  # Get crates
  moved_crates <- stack_list[[inst_row$from]][1:inst_row$move]
  # Place crates
  stack_list[[inst_row$to]] <- c(moved_crates, stack_list[[inst_row$to]])
  # Remove crates
  stack_list[[inst_row$from]] <- stack_list[[inst_row$from]][-(1:inst_row$move)]
  
  return(stack_list)
}

# Reset stack list
stack_list <- extract_stacks(raw_stacks)

# Same as above
for (inst_row in seq_len( nrow(instructions_parsed) )) {
  stack_list <- run_instructions_9001(instructions_parsed[inst_row, ], stack_list)
} 

top_crates <- sapply(stack_list, head, 1)
paste0(top_crates, collapse = "")
