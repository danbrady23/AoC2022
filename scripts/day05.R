test_list <- list("1" = c("N", "Z"),
                  "2" = c("D", "C", "M"),
                  "3" = c("P")) 

test_list[["1"]][1:2]

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
