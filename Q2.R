###############################################################################
## Authors: John M. Chambers, Asya Magazinnik, and Rachael McLellan
## Project: POL 573 Problem Set 1
## Question: 2
## Date: 23Sep2015
###############################################################################

## clear the R workspace
#rm(list=ls())

## Load the MIDS data
#mids <- read.table("mids.txt")

###############################################################################
## Section (a) ---- 
## Initial Regression
###############################################################################

## Render procedure as function
conflictLogit <- function(myData) {
  ## Part 1 ---------------------------------------------------------------------
  ## Logistic regression using all explanatory variables
  myLogit <- glm(formula = Conflict ~ MajorPower + Contiguity + Allies +
                   ForeignPolicy + BalanceOfPower + YearsSince + MinTrade + 
                   MaxTrade + MinDem + MaxDem, 
                 family = binomial(link = logit),
                 data = myData)
  
  ## Part 2 --------------------------------------------------------------------
  ## Estimate diff (allies vs. non-allies) in avg predicted probabilities 
  ## of conflict. Set other predictors to their sample means.
  
  ## Separate out x and y
  y <- myData$Conflict
  x <- myData
  x$Conflict <- NULL
  
  ## Generate values for prediction function
  xMeans <- data.frame(t(colMeans(x)))
  alliesLogitValues <- xMeans
  notAlliesLogitValues <- xMeans
  alliesLogitValues["Allies"] <- 1
  notAlliesLogitValues["Allies"] <- 0
  
  ## Predict!
  alliesProbability <- predict(object = myLogit,
                               newdata = alliesLogitValues,
                               type = "response")
  notAlliesProbability <- predict(object = myLogit,
                                  newdata = notAlliesLogitValues,
                                  type = "response")
  diffInProbs <- alliesProbability - notAlliesProbability
  
  ## Part 3 ---------------------------------------------------------------------
  ## Find the standard errors and 95% CIs somehow
  
  ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ## STILL NEED TO Do
  ## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  ## Construct Object for return
  returnObject <- list()
  returnObject$logit <- myLogit
  returnObject$diffInProbs <- diffInProbs
  
  return(returnObject)
}

## Run procedure for full dataset
results2a <- conflictLogit(myData = mids)

###############################################################################
## Section (b) ---- 
## Selecting on the dependent variable
###############################################################################

## Part 1 ---------------------------------------------------------------------
## Select all observations with conflict.
## Randomly select 3000 observations w/o conflict.

conflictObservationIndices <- which(mids$Conflict == 1)
noConflictObservationIndices <- which(mids$Conflict == 0)

noConflictObservationSelection <- sample(x = noConflictObservationIndices,
                                         size = 3000,
                                         replace = FALSE)

allSelectionIndices <- c(conflictObservationIndices, 
                         noConflictObservationSelection)

## Generate dataset with selection on the dependent variable
mids2b <- mids[allSelectionIndices, ]

## Part 2 ---------------------------------------------------------------------
## Use the same logistic regression you fitted in Q2a and compute the same qty
## of interest w/ and w/o the correction outlined in Q1 (w/ std errors and 
## 95% CIs). Compare and discuss

