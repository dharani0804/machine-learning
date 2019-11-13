library(fpp2)
library(lattice)
library(gains)
library(e1071)
library(pROC)
library(gains)
library(MASS)
library(caret)
library(ggplot2)
library(tidyverse)
library(dplyr)
theme_set(theme_classic()) # set ggplot theme

spam.df <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data", 
                      header = FALSE,
                      sep = ",")

spam.df$V58 <- factor(spam.df$V58)
agg <- aggregate(spam.df[, 1:57], list(spam.df$V58), mean)
agg <- agg[,-1]
spam1.df=spam.df[spam.df$V58 == 1,]
email.df <- spam.df[spam.df$V58 == 0,]
summary(spam1.df)
summary(email.df)

Sorted <- abs(agg[2,]-agg[1,])
Sorted <- Sorted[,-1]
head(t(sort(Sorted, decreasing = TRUE)),10)


##new column selection
spam2.df <- spam.df[,c(57,56,55,27,19,21,25,16,26,52,58)]
levels(spam2.df$V58) <- c("email","spam")

### Split the data into training (80%) and validation/test set (20%)

set.seed(1207)
index <- 1:nrow(spam2.df)
training.index <- sample(index,trunc(length(index)*0.8))
training.df <- spam2.df[training.index, ]
validation.df <- spam2.df[-training.index, ]

# normalize the data
# Estimate preprocessing parameters
norm.values  <- preProcess(training.df, method = c("center", "scale"))
# Transform the data using the estimated parameters
spam.training.norm.df <- predict(norm.values, training.df)
spam.validation.norm.df <- predict(norm.values, validation.df)


# run lda()
lda1 <- lda(V58 ~ ., data = spam.training.norm.df)
# output
lda1

### Predict propensities
lda.predict <- predict(lda1, spam.validation.norm.df[, -11], type = "response")


# check model accuracy
table(lda.predict$class, spam.validation.norm.df$V58)  # predict v actual
mean(lda.predict$class == spam.validation.norm.df$V58)  # percent accurate

sum(lda.predict$posterior[, 1] >=.5)


### cumulative lift chart
gain <- gains(as.numeric(spam.validation.norm.df$V58), lda.predict$x[,1], groups = 10)

str(lda.predict$posterior)

options(scipen=999)
### Compute gains relative to price
spam<- as.numeric(spam.validation.norm.df$V58)
plot(c(0,gain$cume.pct.of.total*sum(spam))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="Lift Chart", 
     col = "blue1", type="l")
### baseline
lines(c(0,sum(spam))~c(0, dim(validation.df)[1]), lty = 5)

### Plot decile-wise chart
heights <- gain$mean.resp/mean(spam)
midpoints <- barplot(heights, names.arg = gain$depth,  ylim = c(0,9), col = "blue",  
                     xlab = "Percentile", ylab = "Mean Response", 
                     main = "Decile-wise lift chart")