# Supervised Learning with Text
rm(list = ls())

# load in packages
library(quanteda)
# devtools::install_github("matthewjdenny/SpeedReader")
library(SpeedReader)

# Install som packages we are going to use:
install.packages(
    c("xgboost","glmnet","caret","ROCR","pROC"),
    dependencies = TRUE)

library(xgboost)
library(caret)
library(ROCR)
library(glmnet)
library(pROC)

# load in data from term category associations lab:
load("~/Desktop/Example_Bills_Corpus_Object.RData")

head(summary(bills))

# process our data:
dtm <- dfm(bills,
           remove_punct = TRUE,
           remove = stopwords("english"),
           ngrams = 1:3)

# look at vocabulary size
dtm

# now lets trim the vocabulary to make things easier to work with:
dtm <- dfm_trim(dtm,
                min_termfreq = 50,
                min_docfreq = 50)

# look at vocabulary size
dtm

# pull out the document level covariates:
bill_features <- docvars(dtm)

# add a numeric encoding of the features where
# Education = 0, Health = 1,  Immigration = 2
bill_features$topic_numeric <- 0
bill_features$topic_numeric[which(bill_features$topic == "Health")] <- 1
bill_features$topic_numeric[which(bill_features$topic == "Immigration")] <- 2

# lets also code a numeric party feature:
# Democrat = 0, Republican = 1, Other = 2
bill_features$party_numeric <- 0
bill_features$party_numeric[which(bill_features$party == "Republican")] <- 1
bill_features$party_numeric[which(bill_features$party == "Other")] <- 2


################# GLMNET #################
# lets start by training a supervised classifier for a binary classification
# problem using a lasso (regularized) logistic regression model.
# See: https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html

# For this problem, lets see if we can classify whether a bill section was
# written by a Democrat or a Republican, within the domain of healthcare
# legislation. To start out, we will need to subset our dataset:

# get the indices we want to keep
keep <- which(bill_features$topic_numeric == 1 &
                  (bill_features$party_numeric == 0 |
                       bill_features$party_numeric == 1))

# subset our data:
features_party <- bill_features[keep,]
dtm_party <- dtm[keep,]

# partition our data into train and test sets:
trainIndex <- createDataPartition(features_party$party_numeric,
                                  p = 0.8,
                                  list = FALSE,
                                  times = 1)

# pull out the first column as a vector:
trainIndex <- trainIndex[,1]

train <- dtm_party[trainIndex, ]
test <- dtm_party[-trainIndex, ]

# Create separate vectors of our outcome variable for both our train and test sets
# We'll use these to train and test our model later
train.label  <- features_party$party_numeric[trainIndex]
test.label   <- features_party$party_numeric[-trainIndex]

# train our lasso
cvfit = cv.glmnet(x = train,
                  y = train.label,
                  family = "binomial",
                  type.measure = "class")

pdf(file = "~/Desktop/Optimal_Lasso_Penalty.pdf",
    width = 10,
    height = 5)
plot(cvfit)
dev.off()

# lets take a look at the coefficients:
head(coef(cvfit, s = "lambda.min"),n = 50)


pred <- predict(
    cvfit,
    newx = test,
    s = "lambda.min",
    type = "response")

# select a threshold and generate predcited labels:
pred_vals <- ifelse(pred >= 0.5, 1, 0)

# Create the confusion matrix
confusionMatrix(table(pred_vals, test.label),positive="1")


# Use ROCR package to plot ROC Curve
lasso.pred <- prediction(pred, test.label)
lasso.perf <- performance(lasso.pred, "tpr", "fpr")

pdf(file = "~/Desktop/LASSO_ROC.pdf",
    width = 6,
    height = 6)
plot(lasso.perf,
     avg = "threshold",
     colorize = TRUE,
     lwd = 1,
     main = "ROC Curve w/ Thresholds",
     print.cutoffs.at = c(.9,.8,.7,.6,.5,.4,.3,.2,.1),
     text.adj = c(-0.5, 0.5),
     text.cex = 0.5)
grid(col = "lightgray")
axis(1, at = seq(0, 1, by = 0.1))
axis(2, at = seq(0, 1, by = 0.1))
abline(v = c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
lines(x = c(0, 1), y = c(0, 1), col="black", lty="dotted")
dev.off()

# we can also get the AUC for this predictor:
auc.perf = performance(lasso.pred,
                       measure = "auc")
auc.perf@y.values


# we can also get the AUC for this predictor:
auc.perf = performance(lasso.pred,
                       measure = "auc")
auc.perf@y.values

# and look at accuracy by threshold
acc.perf = performance(lasso.pred, measure = "acc")
plot(acc.perf)

# we can also calculate the optimal accuracy and its associated threshold:
ind = which.max( slot(acc.perf, "y.values")[[1]] )
acc = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))



################# XGBOOST #################
# Now lets move on to training a model with XGBoost
# See: http://jamesmarquezportfolio.com/get_up_and_running_with_xgboost_in_r.html

################# Softmax (multiclass) ####################
# Set the seed to create reproducible train and test sets
set.seed(300)

# Create a stratified random sample to create train and test sets
# Reference the outcome variable
trainIndex <- createDataPartition(bill_features$topic_numeric,
                                  p = 0.75,
                                  list = FALSE,
                                  times = 1)

# pull out the first column as a vector:
trainIndex <- trainIndex[,1]

train <- dtm[trainIndex, ]
test <- dtm[-trainIndex, ]

# Create separate vectors of our outcome variable for both our train and test sets
# We'll use these to train and test our model later
train.label  <- bill_features$topic_numeric[trainIndex]
test.label   <- bill_features$topic_numeric[-trainIndex]



# Set our hyperparameters
param <- list(objective   = "multi:softmax",
              num_class = 3)

set.seed(1234)

# Pass in our hyperparameteres and train the model
xgb <- xgboost(
    params = param,
    data = train,
    label = train.label,
    nrounds = 100,
    print_every_n = 10,
    verbose = 1)

pred <- predict(xgb, test)

# Create the confusion matrix
confusionMatrix(table(pred, test.label))

# Get the trained model
model <- xgb.dump(xgb, with_stats=TRUE)

# Get the feature real names
names <- colnames(train)

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = xgb)[0:30]

# Plot
pdf(file = "~/Desktop/Multiclass_Feature_Importance.pdf",
    width = 6,
    height = 6)
xgb.plot.importance(importance_matrix)
dev.off()

# we can look at the multiclass AUC calcualted from:
# David J. Hand and Robert J. Till (2001). A Simple Generalisation of the Area
# Under the ROC Curve for Multiple Class Classification Problems. Machine
# Learning 45(2), p. 171--186. DOI: 10.1023/A:1010920819831
# Implemented in pROC package:
roc.multi <- pROC::multiclass.roc(test.label, pred)

# get the AUC:
roc.multi

# plot the ROC curves for each pair of outcomes:
rs <- roc.multi[['rocs']]
plot.roc(rs[[1]])
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))


################## Logistic (Binary) ##################

# We can make use of the same dataset for this example by subsetting our data
# to just two topics:
rem <- which(bill_features$topic_numeric == 2)
bin_bill_features <- bill_features[-rem,]
dtm_bin <- dtm[-rem,]

# Set the seed to create reproducible train and test sets
set.seed(300)

# Create a stratified random sample to create train and test sets
# Reference the outcome variable
trainIndex <- createDataPartition(bin_bill_features$topic_numeric,
                                  p = 0.75,
                                  list = FALSE,
                                  times = 1)

# pull out the first column as a vector:
trainIndex <- trainIndex[,1]

train <- dtm_bin[trainIndex, ]
test <- dtm_bin[-trainIndex, ]

# Create separate vectors of our outcome variable for both our train and test sets
# We'll use these to train and test our model later
train.label  <- bin_bill_features$topic_numeric[trainIndex]
test.label   <- bin_bill_features$topic_numeric[-trainIndex]



# Set our hyperparameters
param <- list(objective = "binary:logistic")

set.seed(1234)

# Pass in our hyperparameteres and train the model
xgb <- xgboost(
    params = param,
    data = train,
    label = train.label,
    nrounds = 100,
    print_every_n = 10,
    verbose = 1)

# since this is a binary classification problem, we get out probabilities of
# being in class 1:
pred <- predict(xgb, test)

# select a threshold and generate predcited labels:
pred_vals <- ifelse(pred >= 0.5, 1, 0)

# Create the confusion matrix
confusionMatrix(table(pred_vals, test.label),positive="1")

# we can also try with a different threshold:
pred_vals <- ifelse(pred >= 0.6, 1, 0)

# Create the confusion matrix
confusionMatrix(table(pred_vals, test.label),positive="1")

# Get the trained model
model <- xgb.dump(xgb, with_stats=TRUE)

# Get the feature real names
names <- colnames(train)

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = xgb)[0:30]

# Plot
pdf(file = "~/Desktop/Binary_Feature_Importance.pdf",
    width = 6,
    height = 6)
xgb.plot.importance(importance_matrix)
dev.off()


# Use ROCR package to plot ROC Curve
xgb.pred <- prediction(pred, test.label)
xgb.perf <- performance(xgb.pred, "tpr", "fpr")


pdf(file = "~/Desktop/XGB_ROC.pdf",
    width = 6,
    height = 6)
plot(xgb.perf,
     avg = "threshold",
     colorize = TRUE,
     lwd = 1,
     main = "ROC Curve w/ Thresholds",
     print.cutoffs.at = c(.95,.8,.5,.1),
     text.adj = c(-0.5, 0.5),
     text.cex = 0.5)
grid(col = "lightgray")
axis(1, at = seq(0, 1, by = 0.1))
axis(2, at = seq(0, 1, by = 0.1))
abline(v = c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
abline(h = c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
lines(x = c(0, 1), y = c(0, 1), col="black", lty="dotted")
dev.off()



################ Preprocessing <-> Accuracy #################

# different ways of preprocessing our data can affect prediction accuracy. To
# see how this can be the case, lets return to our party prediction problem
# and now lets try some alternative preprocessing specifications:

# start by writing a function to calculate accuracy given input data:
generate_accuracy <- function(dtm,
                              covariates,
                              keep) {

    set.seed(1234)
    fp <- covariates[keep,]
    dtm_p <- dtm[keep,]

    # partition our data into train and test sets:
    trainIndex <- createDataPartition(fp$party_numeric,
                                      p = 0.8,
                                      list = FALSE,
                                      times = 1)

    # pull out the first column as a vector:
    trainIndex <- trainIndex[,1]

    train <- dtm_p[trainIndex, ]
    test <- dtm_p[-trainIndex, ]

    # Create separate vectors of our outcome variable for both our train and test sets
    # We'll use these to train and test our model later
    train.label  <- fp$party_numeric[trainIndex]
    test.label   <- fp$party_numeric[-trainIndex]

    # train our lasso
    cvfit = cv.glmnet(x = train,
                      y = train.label,
                      family = "binomial",
                      type.measure = "class")

    pred <- predict(
        cvfit,
        newx = test,
        s = "lambda.min",
        type = "response")

    lasso.pred <- prediction(pred, test.label)

    # we can also get the AUC for this predictor:
    auc.perf = performance(lasso.pred,
                           measure = "auc")
    cat("Model AUC:",auc.perf@y.values[[1]],"\n\n")

    cat("Maximum accuracy and corresponding threshold:\n")
    # and look at accuracy by threshold
    acc.perf = performance(lasso.pred, measure = "acc")

    # we can also calculate the optimal accuracy and its associated threshold:
    ind = which.max( slot(acc.perf, "y.values")[[1]] )
    acc = slot(acc.perf, "y.values")[[1]][ind]
    cutoff = slot(acc.perf, "x.values")[[1]][ind]
    print(c(accuracy= acc, cutoff = cutoff))
}


########### Specification 1 #############
# generate unigram DTM with all default arguments:
dtm3 <- dfm(bills)

# calculate accuracy:
generate_accuracy(dtm3,
                  bill_features,
                  keep)

########### Specification 2 #############
# apply specification used earlier in lab:
dtm <- dfm(bills,
           remove_punct = TRUE,
           remove = stopwords("english"),
           ngrams = 1:3)

# trim vocabulary
dtm2 <- dfm_trim(dtm,
                min_termfreq = 50,
                min_docfreq = 50)

# calculate accuracy
generate_accuracy(dtm2,
                  bill_features,
                  keep)

########### Specification 3 #############
# try higher threshold:
dtm2 <- dfm_trim(dtm,
                 min_termfreq = 100,
                 min_docfreq = 100)

# calculate accuracy
generate_accuracy(dtm2,
                  bill_features,
                  keep)

########### Specification 4 #############
# try lower threshold:
dtm2 <- quanteda::dfm_tfidf(dtm)

# calculate accuracy
generate_accuracy(dtm2,
                  bill_features,
                  keep)















