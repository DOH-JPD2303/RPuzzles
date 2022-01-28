# Solving puzzle from: https://fivethirtyeight.com/features/can-you-trek-the-triangle/

# Load tidyverse
library(tidyverse)

# The following scenarios are how the 'going for two' scenario would play out
two_point_scenarios <- tribble(
  ~'Scenario #', ~'Try1', ~'Try1_Success', ~'Try2', ~'Try2_Success', ~'Game_Result',
  1, 2, FALSE, 2, FALSE, 0, # Go for it 2x and fail (you lose)
  2, 2, FALSE, 2, TRUE, 0.5, # Go for it 2x and succeed once (OT)
  3, 2, TRUE, 1, TRUE, 1 # Go for it 1x and succeed, then kick extra pt (you win)
)

# Simulate scenarios
outcomes <- data.frame()
for(p in seq(0, 1, by = 0.001)){
  tmp <- two_point_scenarios
  tmp$p <- p
  tmp$win_probs <- c((1-p)**2, p*(1-p), p)
  tmp$wtd_win_prob <- tmp$Game_Result*tmp$win_probs
  outcomes <- rbind(outcomes, win_prob = data.frame(p, wp = sum(tmp$wtd_win_prob)))
}

# Plot the possibilities
ggplot(outcomes, aes(x = p, y = wp)) + geom_area()

# What you need to do algebraically is solve the formula:
# 0.5 = 0*(1-p)^2 + 0.5*(p)(1-p) + 1*p
# Because this would be the average expected wins in each of the 3 'go for it' 
# scenarios (provided p % chance of success on a given 2 pt attempt.)
# Thus, when the average expected wins from 'going for it' is higher than the 
# probability of winning in OT (50%), then going for it is favored.