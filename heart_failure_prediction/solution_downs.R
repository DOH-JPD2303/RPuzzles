# Contest: https://www.kaggle.com/andrewmvd/heart-failure-clinical-data/version/1
# Load in libraries and datafile
library(data.table)
library(tidyverse)
infile <- './heart_failure_prediction/heart_failure_clinical_records_dataset.csv'
dat <- data.table::fread(infile)

# DATA EXPLORATION --------------------------------------------------------
# DO NOT SKIP THIS PART :)

# FIT MODEL ---------------------------------------------------------------
# Fit a logistic model and produce predicted probabilities for each row
mod <- glm(DEATH_EVENT ~ ., data = dat, family = 'binomial')

# Add predicted values to the dataset, assume any modeled prob >0.5 is a yes,
# and set an indicator variable for whether the model result was accurate
dat_out <- dat %>%
  mutate(pred = round(mod$fitted, 2),
         predyesno = ifelse(pred > 0.5, 1, 0),
         pred_correct = predyesno == DEATH_EVENT)

# Let's see how accurate our predictions were by batching them
# Essentially, we want to compare the % of records with a death by the total
# number of people in that bracket.
# For example, for folks in the 5-14% prediction range, we would hope that the prevalence
# of death would also be in the 5-14% range.
sums <- dat_out %>%
  mutate(pred2 = round(pred, 1)) %>%
  group_by(pred2) %>%
  summarize(n = n(),
            nevents = sum(DEATH_EVENT),
            pevents = nevents/n)

# Plot it
ggplot(sums, aes(x = pred2, y = pevents)) + 
  geom_line() +
  geom_abline(slope = 1, color = 'red') +
  xlab('Predicted probability of event') +
  ylab('Percent of records with an event')
