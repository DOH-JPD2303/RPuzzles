dice_puzzle <- function(rng) {
  
  d4 <- sample(1:4, 1) + rng
  d6 <- sample(1:6, 1) + rng
  d8 <- sample(1:8, 1) + rng
  
  if (d4 < d6 & d6 < d8) {
    return(1)
  } else {
    return(0)
  }
}

Trials = seq(1:20000)

100*(sum(sapply(Trials, dice_puzzle)) / max(Trials))

################################################################################
dice_puzzle <- function() {
  
  d4 <- sample(1:4, 1)
  d6 <- sample(1:6, 1)
  d8 <- sample(1:8, 1)
  
  if (d4 < d6 & d6 < d8) {
    return(1)
  } else {
    return(0)
  }
}

res <- replicate(20000, dice_puzzle(), simplify = TRUE)
prop.table(table(res))

################################################################################

dice_puzzle <- function(...) {
  
  d4 <- sample(1:4, 1)
  d6 <- sample(1:6, 1)
  d8 <- sample(1:8, 1)
  
  if (d4 < d6 & d6 < d8) {
    return(1)
  } else {
    return(0)
  }
}

Trials = seq(1:20000)

100*(sum(sapply(Trials, dice_puzzle)) / max(Trials))
