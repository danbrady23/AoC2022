# Problem info
# Only one cd /
# cd followed by ls: cd gives name of directory and ls gives contents
# cd .. indicates the end of a branch, more repetitions indicate depth of branch
# Files always preceded by a number indicating size
# Directory always preceded by dir
# Multiple files/directories with the same name, but all have a unique path

library(stringr)
library(dplyr)

# Read data
input <- readLines("data/day07.txt", warn = F)

# Setup regex patterns
cd_patt <- "(?<=cd )([:alpha:]+)" # 'cd ' preceding 1+ letters 
cd_up_patt <- "cd .."

# Starts with 1+ digits followed by a space then 1+ letters/punctuation
file_patt <- "^([:digit:]+) ([:graph:]+)" 

# Starts with 'dir ' followed by 1+ letters
# Not strictly needed but useful to have
dir_patt <- "^dir ([:alpha:]+)"

# Empty data frame for files scan
files_df <- data.frame(name = character(),
                        size = numeric(),
                        path = character(),
                        input_loc = numeric())

# Creates a list of all directories
dir_vec <- c("/")
# Stack for navigating directories
path_stack <- c("/")

# Run through every line of input
# Could be made quicker by skipping ls inputs and dir outputs
for (line_n in seq_along(input)) {

  if (str_detect(input[line_n], cd_patt)) {
    # If there is a line with a change to a directory then add that directory
    # to the stack
    
    dir_name <- paste0(str_extract(input[line_n], cd_patt), "/")
    path_stack <- c(dir_name, path_stack)
    
    # Generate current path
    file_path = paste0(rev(path_stack), collapse = "") 
    
    # If that path isn't in the current list then add it
    if (!(file_path %in% dir_vec)){
      dir_vec <- c(dir_vec, file_path)
    }
    
  } else if (str_detect(input[line_n], cd_up_patt)) {
    # if we cd .. then go back down the path removing the first element from the
    # path stack
    path_stack <- path_stack[-1]
    
  } else if (str_detect(input[line_n], file_patt)) {
    # if the input line referneces a file then get the name and size of the file
    file_match <- str_match(input[line_n], file_patt)
    # Generate the file path
    file_path = paste0(rev(path_stack), collapse = "") 
    
    # And add these details to the files data frame
    files_df <- add_row(files_df,
                        name = file_match[3],
                        size = as.numeric(file_match[2]),
                        path = file_path,
                        input_loc = line_n)
  }
}

dir_output <- data.frame(path = character(),
                         size = numeric())

# Run through the entire directory tree
for (path in dir_vec) {
  # Find all files that sit within the current path (including in subfolders)
  contains_rec <- str_detect(files_df$path, paste0(path, ".*"))
  # Sum up their sizes
  dir_size <- sum(files_df$size[contains_rec])
  
  # Add the details to the dir output dataframe
  dir_output <- add_row(dir_output,
                        path = path,
                        size = dir_size)
}

# To get the answer to Part 1 sort by size, remove anything less than the cutoff
# and then add everything else up

dirs_by_size <- arrange(dir_output, size)
filter(dirs_by_size, size <= 100000) %>%
  summarise(total_size = sum(size))

# Part 2

total_space <- 70000000
required_space <- 30000000
free_space <- total_space - max(dirs_by_size$size)
need_to_find <- required_space - free_space 

# Find the smallest directory that is large enough to free up enough space
closest_required_loc <- min( which( dirs_by_size$size > need_to_find ))
# How big is it
dirs_by_size$size[closest_required_loc]
