# Solution for dice roll puzzle by Marcus Bamberger

# dice
d4 <- c(1:4)
d6 <- c(1:6)
d8 <- c(1:8)

# outcomes
nwin <- 0
nloss <- 0

# solution code
for (a in d4) {
  for(b in d6) {
    for (c in d8) {
      if (a < b & b < c) {
        nwin <- nwin + 1
        # san check
        #print(c(a, b, c))
      } else {
        nloss <- nloss + 1
        # san check
        #print(c(a, b, c))
      }
    }
  }
}

# solution
print(nwin*100/(nloss+nwin))
