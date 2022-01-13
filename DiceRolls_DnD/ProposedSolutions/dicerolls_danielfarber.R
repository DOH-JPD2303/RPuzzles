# Riddle - probability of increasing rolls of fair dice
library(tidyverse)

# d4, d6, d8 each equally likely to land on their 4, 6, 8 sides respectively
# total_combos = d4 * d6 * d8 = 192

# df of possible rolls
df <- data.frame(d4_roll = c(rep(1, 48),rep(2,48), rep(3,48), rep(4,48)),
                 d6_roll = rep(c(rep(1,8),rep(2,8), rep(3,8), rep(4,8), rep(5,8), rep(6,8)), 4),
                 d8_roll = rep(1:8,24))

df <- df %>%
  mutate(win = case_when(d8_roll > d6_roll & d6_roll > d4_roll ~ 1,
                         TRUE ~ 0))

win_pct <- length(df$win[df$win == 1])/ length(df$win)
win_pct
