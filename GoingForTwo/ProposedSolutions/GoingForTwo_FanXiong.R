library(tidyverse)

##P is some Probability of winning with a 2 point conversion
##1 - p is the probability of getting a 2 point conversion to tie the game and send it to overtime
##The solution may just be p + (1-p) = 0 so that p = 1 IF you won the game on the first 2 point conversion (IF ONLY it wasn’t tied)
##So, 0.5*p is the probability of winning in overtime if the game was tied after the 2 point conversion to tie the game.
##Multiple the overtime probability with the probability for getting a 2 point conversion.
##You win when the solution is > 0.5.
##Set up equation and solve
##Find the minimum value of p when the solution is greater than 0.5
##So the equation is: p + ((1-p) * (0.5*p)) > 0.5

##Crude brute force method with a small incremental value
all_solutions <- 
  expand.grid( p = seq(0,1, by = 0.0001)) %>%
  dplyr::mutate(solution = p + ((1-p) * (0.5*p)))

##Find the minimum p when the solution is exactly > 0.5
greater_05 <- 
  all_solutions %>%
  dplyr::filter(
    solution > 0.5
  ) %>%
  dplyr::arrange(solution)

##minimum p
greater_05$p[1]
##0.382
