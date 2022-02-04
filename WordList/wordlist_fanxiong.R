library(tidyverse)
library(tidytext)
library(tidyr)

lnk <- 'https://svnweb.freebsd.org/csrg/share/dict/words?revision=61569&view=co'

all_word <- 
  data.table::fread(lnk, header = FALSE) 

##Most Used Letter
##I will need to separate out each letter

letters <- 
  stringr::str_split(all_word$V1, "", n = Inf, simplify = T) %>%
  as.data.frame()

letters2 <- 
  pivot_longer(
    letters,
    cols = colnames(letters),
    values_to = "letter",
  ) %>%
  dplyr::filter(letter != "") %>%
  dplyr::mutate(
    letter = stringr::str_trim(
      stringr::str_to_upper(letter),
      side = c("both")
    )
  ) %>%
  dplyr::group_by(letter) %>%
  dplyr::summarise(total = n()) %>%
  dplyr::arrange(desc(total))


letters3 <- 
  pivot_longer(
    letters,
    cols = colnames(letters),
    values_to = "letter",
  ) %>%
  dplyr::filter(letter != "") %>%
  dplyr::mutate(case_upper = str_detect(letter,"[[:upper:]]"),
                case_lower = str_detect(letter,"[[:lower:]]"),
                case = ifelse(case_upper == T,"Upcase","Lowcase")) %>%
  dplyr::group_by(case,letter) %>%
  dplyr::summarise(total = n()) %>%
  dplyr::arrange(case,desc(total))

##most-used letter if we upper case everything 
letters2$letter[1]
# E

##Most Used Letter as is by case
letters3$letter[letters3$case == "Lowcase"][which.max(letters3$total)]
##e
letters3$letter[letters3$case == "Upcase"][which.max(letters3$total)]
##M

##What letter most often follows b?
##Assuming we are interested in the next letter that follows a lower-case b including b
all_word_b <- all_word[str_detect(all_word$V1,"b") == T]

after_b <- stringr::str_extract_all(all_word_b$V1,
                                    "b[[:alpha:]]",
                                    simplify = F) 

after_b2 <- unlist(after_b) %>% as.data.frame()
names(after_b2) <- c("b_digram")

after_b2$next_letter <- stringr::str_sub(
  stringr::str_trim(after_b2$b_digram, side = c("both")),2,2)

after_b3 <-
  after_b2 %>% 
  dplyr::group_by(next_letter) %>%
  dplyr::summarise(total = n()) %>%
  dplyr::arrange(desc(total)) %>%
  dplyr::mutate(
    L_huh = case_when(next_letter == "I" ~ "Capital I (eye)",
                      next_letter == "l" ~ "Lowercase l (elle)",
                      TRUE ~ NA_character_)
  )
after_b3$next_letter[1]
after_b3$L_huh[1]
##"l"
##"Lowercase l (elle)"  

vowel_freq <- 
  letters2 %>%
  dplyr::mutate(vowel = ifelse(letter %in% c("A","E","I","O","U"),total,0)) 
sum(vowel_freq$vowel) / sum(vowel_freq$total)
##38.5% or 0.3845999

