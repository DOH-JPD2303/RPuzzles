library(tidyverse)

#create a df of all possible combinations
d4 <- c(1, 2, 3, 4)
d6 <- c(1, 2, 3, 4, 5, 6)
d8 <- c(1, 2, 3, 4, 5, 6, 7, 8)

df <- expand.grid(d4, d6, d8)

#create a variable indicating whether each combo is a win or a loss
df <- df%>%
    mutate(gamestatus = case_when((Var1 < Var2 & Var2 < Var3) ~ 'win'
            , TRUE ~ 'lose')
           )
#count the frequnecy of possible wins and losses
count(df, gamestatus)

# Out of 192 possible outcomes, 144 are losses, 48 are wins
48/192 = .25

#Probability of winning is 1 in 4, or 25%