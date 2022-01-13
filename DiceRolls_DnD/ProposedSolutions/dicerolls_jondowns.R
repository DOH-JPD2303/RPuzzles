# Basic solution ----------------------------------------------------------
# Create lists for the d4, d6, and d8 dice
d4 <- 1:4
d6 <- 1:6
d8 <- 1:8

# Create a data frame of all possible combinations
df <- expand.grid(d4, d6, d8)
colnames(df) <- c('d4', 'd6', 'd8')

# See how often d4 < d6 < d8 by creating a new variable
df$winner <- ifelse(df$d4 < df$d6 & df$d6 < df$d8, TRUE, FALSE)
prop.table(table(df$winner))

# Simulated solution -----------------------------------------------------
# Let's simulate 10,000 dice rolls to see if we come up with roughly 25%
n_rolls <- 10000
sim_df <- data.frame()
roll1 <- sample(d4, n_rolls, replace = TRUE)
roll2 <- sample(d6, n_rolls, replace = TRUE)
roll3 <- sample(d8, n_rolls, replace = TRUE)
winner <- ifelse(roll1 < roll2 & roll2 < roll3, TRUE, FALSE)
sim_df <- rbind(sim_df, data.frame(roll1, roll2, roll3, winner))

# Get the proportion of simulations that were a winner
prop.table(table(sim_df$winner))