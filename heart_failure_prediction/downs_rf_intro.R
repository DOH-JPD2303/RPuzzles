# Contest: https://www.kaggle.com/andrewmvd/heart-failure-clinical-data/version/1
# Load in libraries and datafile
library(data.table)
library(tidyverse)
library(caret)
library(randomForest)
library(rpart)

# Specify data path and read in file
# Also, need to convert outcome var to factor for RF models
infile <- './heart_failure_prediction/heart_failure_clinical_records_dataset.csv'
dat <- data.table::fread(infile, data.table = FALSE) %>%
  mutate(DEATH_EVENT = factor(DEATH_EVENT))

# Set a seed for reproducibility
set.seed(9000)

# Create training and testing ---------------------------------------------
# Save 20% for testing, 80% for model fit
train_frac <- round(0.8*nrow(dat))
train <- sample_n(dat, train_frac)
test <- dat %>%
  anti_join(train)

# Single tree -------------------------------------------------------------
# Here's are the variables of interest in this problem
f <- as.formula('DEATH_EVENT ~ age + creatinine_phosphokinase + diabetes +
                ejection_fraction + high_blood_pressure + platelets + serum_creatinine + serum_sodium + sex + smoking + anaemia')

# Let's start by constructing a single recursive partition & regression tree
# Essentially, you are asking each row of data a series of yes/no questions
tree <- rpart(f, method = 'class', data = train, control = rpart.control(cp = 0))

# Plot the tree to understand what it's doing
par(mfrow = c(1,1), xpd = NA)
plot(tree)
text(tree, use.n = T)

# And let's get a summary of how well it's predicting the data in training
train$pred_rpart <- predict(tree, newdata = train)[,2] > 0.5
table(train$pred_rpart, train$DEATH_EVENT)
round(prop.table(table(train$pred_rpart, train$DEATH_EVENT)), 2)

# And in test
test$pred_rpart <- predict(tree, newdata = test)[,2] > 0.5
table(test$pred_rpart, test$DEATH_EVENT)
round(prop.table(table(test$pred_rpart, test$DEATH_EVENT)), 2)

# Single random forest model ----------------------------------------------
# A random forest is essentially the process above, repeated 100's of times using
# randomly sampled subsets of the data and putting some constraints on the number of 
  #variables considered at each step

# Look at some of the arguments of the randomForest thoroughly
?randomForest

rf <- randomForest(f, data = train)

# Add predictions to training data (100%, WE DID IT!!!!!)
train$pred_rf <- predict(rf, newdata = train) == 1
table(train$pred_rf, train$DEATH_EVENT)
round(prop.table(table(train$pred_rf, train$DEATH_EVENT)), 2)

# Show the confusion matrix/accuracy
rf$confusion
(rf$confusion[[1]] + rf$confusion[[4]]) / sum(rf$confusion[1:4])

# Next, let's see how this model does on testing data
test$pred_rf <- predict(rf, newdata = test) == 1
table(test$pred_rf, test$DEATH_EVENT)
round(prop.table(table(test$pred_rf, test$DEATH_EVENT)), 2)

# Now, let's plot some of the diagnostics of the random forest
plot(rf)
varImpPlot(rf)

# Cross-validated random forest -------------------------------------------
control <- trainControl(method = 'repeatedcv', number = 10, repeats = 2, search = 'grid')
mod <- train(f, data = train, method = 'rf', metric = 'Accuracy', 
             trControl = control, tuneGrid = expand.grid(.mtry = (5:9)))

# Let's explore some of the results
mod$results
mod$finalModel
varImpPlot(mod$finalModel)
plot(mod$finalModel)

# Accuracy in testing data
test$pred_cvrf <- predict(mod, newdata = test) == 1
table(test$pred_cvrf, test$DEATH_EVENT)
round(prop.table(table(test$pred_cvrf, test$DEATH_EVENT)), 2)