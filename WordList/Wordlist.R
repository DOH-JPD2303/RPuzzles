# Setup -------------------------------------------------------------------
# Load ggplot
library(ggplot2)

# Read in the word dictionary
lnk <- 'https://svnweb.freebsd.org/csrg/share/dict/words?revision=61569&view=co'
all_word <- as.list(data.table::fread(lnk, header = FALSE))

# Most used letter in dictionary ------------------------------------------
# Convert to uppercase
words <- lapply(all_word[[1]], toupper)

# Function to turn a list into a single character vector
flatten <- function(lst) {
  do.call(c, lapply(lst, function(x) if(is.list(x)) flatten(x) else list(x)))
}

# Split each letter from our list of words, then flatten into single vector
letter_list_raw <- lapply(words, function(x) strsplit(x, split = ''))
letter_list <- do.call(c, flatten(letter_list_raw))

# Run the frequency chart
letter_freqs <- data.frame(table(letter_list))

# Plot it
ggplot(letter_freqs, aes(x = reorder(letter_list, -Freq), y = Freq)) +
  geom_bar(stat ='identity')

# Letter most frequently following 'b' ------------------------------------
all_word_df <- data.frame(all_word)
all_word_df$V1 <- toupper(all_word_df$V1)

# The regmathces/regexpr functions return empty data in case of failure
# Write a function to provide an 'NA' placeholder so we can add it as a DF col
follows_letter <- function(word, letter){
  result <- regmatches(word, regexpr(paste0('(?<=', letter, ').{1}'), word, perl = TRUE))
  if(identical(result, character(0))){
    return(NA)
  } else{
    return(result)
  }
}

# Add a column indicating the single letter following b in each word (NA if no B)
all_word_df$follows_b <- do.call(c, lapply(all_word_df$V1, function(x) follows_letter(x, 'B')))

# Get frequencies and plot it
follow_b_freqs <- data.frame(table(all_word_df$follows_b))
ggplot(follow_b_freqs, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = 'identity')

