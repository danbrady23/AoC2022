data <- read.table("data/day2.txt", col.names = c("opponent", "you"))

data <- data.frame(opponent = c("A", "B", "C"),
                   you = c("Y", "X", "Z"))
# Part 1

# Calculate score for your selection
data$select_score[data$you == "X"] = 1
data$select_score[data$you == "Y"] = 2
data$select_score[data$you == "Z"] = 3


# Convert letters for both opponent and you to R P and S?
# Opponent: A = R, B = P, C = S
# You: X = R, Y = P, Z = S

# Calculate score for outcome of match
rnames <- sort(unique(data$opponent))
cnames <- sort(unique(data$you))

# Matrix defining score based on opponent section and your selection
outcome_mat <- matrix(c(3,0,6,
                        6,3,0,
                        0,6,3), nrow = 3,
                      dimnames = list(rnames, cnames))

# Uses the outcome_mat to compute the outcome score for each match 
compare_selections <- function(data, outcome_mat) {
  outcome_mat[data["opponent"], data["you"]]
} 

data$outcome_score <- apply(data, 1, compare_selections, outcome_mat)

# Sum up everything
data$round_total <- data$select_score + data$outcome_score
sum(data$round_total)

# Part 2

data <- read.table("data/day2.txt", col.names = c("opponent", "outcome"))

data <- data.frame(opponent = c("A", "B", "C"),
                   outcome = c("Y", "X", "Z"))

# x = L, Y = D, Z = W

# Matrix defining the score based on opponent selection and outcome of the match
select_mat <- matrix(c(3,1,2,
                       1,2,3,
                       2,3,1), nrow = 3,
                      dimnames = list(rnames, cnames))

# Runs through the data and uses the select matrix to compute the section scores
compute_selection <- function(data, select_mat) {
  select_mat[data["opponent"], data["outcome"]]
}

# Calculate score for outcome
data$outcome_score[data$outcome == "X"] = 0
data$outcome_score[data$outcome == "Y"] = 3
data$outcome_score[data$outcome == "Z"] = 6

# Apply the compute_selection function across the data
data$select_score <-apply(data, 1, compute_selection, select_mat)

# Get the sums
data$round_total <- data$select_score + data$outcome_score
sum(data$round_total)
