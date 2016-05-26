## 04-Exercises

packs <-  c('ggplot2', 'magrittr', 'dplyr', 'caret', 'AppliedPredictiveModeling')

for( nm in packs ) { 
        # message(nm)
        if( ! nm  %in% installed.packages()[,1]  ) install.packages(nm)
        library(nm, character.only = TRUE)
}

# x is a random variable. 
# We want to not only know what the mean(x) is but want to calculate the uncertainty of mean(x). Measuring the
# uncertainty requires repeated measurements of mean(x).
set.seed(1) 
x <- runif(20,1,20)

# Calculate the mean of x.
x_mean = mean(x)

# Calculte the sd( mean(x) ) using the using 10-fold cross-validation. 
# Create your own folds, show your work. (An example is for the Bootstrap is given as a hint. )


sd_cv <- .. # YOUR ANSWER HERE


# BOOTSTRAP (EXAMPLE)
# function(i) sample(x, replace = TRUE) is essentially a single bootstrap
# running through the mean is a sample mean
# taking the sd is the standard error of the sample means
sd_boot <- sapply(1:k, function(i) sample(x,replace=TRUE) %>% mean ) %>% sd
sd_cv is: r sd_cv
sd_boot is: r sd_boot

# CROSS-VALIDATION
# data
x
# number of folds
k <- 10

# x is input vector, n is size of each slice based on x and k
n <- length(x)/k
slice <- function(x, n) {
        split(seq_along(x), as.integer((seq_along(x) - 1) / n))
}
folds <- slice(x,n)


mean_cv <- sapply(folds, function(fold) mean(x[-(fold)]))
sd(mean_cv)


###### PART II 

set.seed(1)
data(iris)

# visualize petal length, sepal length by class
qplot( data=iris, x=Petal.Length, y=Sepal.Length, color=Species )

# Create Dependent Variable, make cats 'versicolor' or other
# remove information from species
iris$Versicolor <- 
        ifelse( iris$Species == 'versicolor', "versicolor", "other" ) %>% as.factor
iris$Species = NULL 

# create 50/50 train test split. wh are the indices
wh <- sample.int( nrow(iris), size=nrow(iris)/2 )
train <- iris[ wh,]
test <- iris[ -wh, ]

# fit logistic binomial classifier on all but sepal lenth from train...
fit.glm <- glm( Versicolor ~ . - Sepal.Length, data=train, family=binomial )

####
# BINOMIAL EXERCISES ::
####

# define predictin and actual response for test set
y <- iris$Versicolor[-wh]
pred_probs <- predict(fit.glm, newdata=test, type="response")
yhat <- factor(pred_probs >= 0.5, labels = levels(iris$Versicolor))

# prevalence is the portion of positive case in the test set
# yhat is not necessary to make this calc
# prevalence = function(y,yhat) {}
prevalence = (y == 'versicolor') %>% mean

accuracy = function(y, yhat) mean(y == yhat)


error_rate = function(y,yhat) 1 - accuracy(y, yhat)

# TPR is proportion of TP out of all actual positives
tpr = function(y, yhat) {
        pos_mask = y == 'versicolor'
        return(mean(yhat[pos_mask] == 'versicolor'))
}
# FPR is proportion of FP out of all actual negatives
fpr = function(y, yhat) {
        neg_mask = y == 'other'
        return(mean(yhat[neg_mask] != 'other'))
}
# TNR is proportion of TN out of all actual negatives
# this is the complement of FPR
tnr = function(y, yhat) 1 - fpr(y,yhat)

# sensitivity is same as TPR
sensitivity = tpr
# specificity is the proportion of TN out of predicted negatives
specificity = function(y,yhat) {
        neg_preds = yhat == 'other'
        return(mean(y[neg_preds] == 'other'))
}
# recall is same as TPR and sensitivity
recall = tpr
# precision is the proportion TP out of predicted positives
precision = function(y,yhat) {
        pos_preds = yhat == 'versicolor'
        return(mean(y[pos_preds] == 'versicolor'))
}

# EXAMPLE: fpr
# The FPR is THE NUMBER OF FALSE POSITIVES / NEGATIVES (TN+FP)

threshold = 0.5 
y = test$Versicolor == 'versicolor'
yhat = predict(fit.glm, test, type="response") > threshold


fpr = function(y,yhat)
        sum(yhat & !y ) / # FP
        sum(! y)                # N

fpr(y,yhat)