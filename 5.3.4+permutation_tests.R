# Resampling Methods: The Bootstrap and Cross-Validation
#April 30, 2016
#Alexandru Papiu


# the first examples are from : http://www.stat.wisc.edu/~larget/stat302/chap3.pdf
library(Lock5Data) #for some data
library(boot)
library(dplyr)
data(CommuteAtlanta)
train <- CommuteAtlanta

# we'll use the bootstrap to build confidence intervals for the average commute time:
hist(train$Time, breaks = 20)
mean(train$Time)
#sd(train$Time)/sqrt(500)
#t.test(train$Time)

#doing the boostrap:
replicate(1000, {
boot_sample <- sample(1:nrow(train), replace = TRUE)
mean(train$Time[boot_sample])
}) -> boot_statistic

hist(boot_statistic, breaks = 50)
se <- sd(boot_statistic)

#computing confidence intervals:
interval <- mean(train$Time) + c(1,-1)*1.96*se

paste("The confidence interval is", 
      round(mean(train$Time) - 1.96*sd(boot_statistic),4),
      round(mean(train$Time) + 1.96*sd(boot_statistic),4))


#~~~~~~~~~~~~~
#5.3.4 ISLR: Estimating the Accuracy of a Linear Regression Model
#~~~~~~~~~~~~~

library(ISLR)
data(Auto)

plot(Auto$horsepower, Auto$mpg)
model <- lm(mpg ~ horsepower, data = Auto)
summary(model)
model$coefficients[2]

#now let's bootstrap
#ISLR uses the boot package but we'll do it directly:
replicate(1000,{
    boot_sample <- sample(1:nrow(Auto), replace = TRUE)
    data <- Auto[boot_sample,]
    model <- lm(mpg ~ horsepower, data = data)
    model$coefficients[2]
}) -> boot_statistic

hist(boot_statistic)
se <- sd(boot_statistic)
interval <- mean(boot_statistic) + c(-2,2)*se
print(interval)

#We get a se that is slightly larger than what lm gives us
#this makes sense since since the relationship is non-linear:
plot(model$residuals) #you can see a pattern here.

#~~~~~~~~~~~~~~~~~~
#Exercise 9, p 201:
#A look at the Boston dataset - we'll try to estimate
#median value of owner-occupied homes


library(MASS)
data(Boston)
train <- Boston

mean(train$medv)
t.test(train$medv)
data <- train$medv

replicate(1000, {
    boot_samples <- sample(1:length(data), replace = TRUE)
    mean(data[boot_samples])
}) -> boot_statistic

interval <- mean(data) + c(-2,2)*sd(boot_statistic)
#very similar to the t-test

#let's do it for median as well:
replicate(1000, {
    boot_samples <- sample(1:length(data), replace = TRUE)
    median(data[boot_samples])
}) -> boot_statistic

1.96*sd(boot_statistic) 

#Let's look at the tenth quantile:
quantile(data, .9)

replicate(1000, {
    boot_samples <- sample(1:length(data), replace = TRUE)
    quantile(data[boot_samples], .9)
}) -> boot_statistic

1.96*sd(boot_statistic) 

#Let's write a function that does the bootstrap, similar to the one in the 
#boot library:

bootstrap <- function(data, statistic, n = 1000) {
    replicate(n, {
        boot_samples <- sample(1:length(data), replace = TRUE)
        statistic(data[boot_samples])
    }) -> boot_statistic
    boot_statistic
}

sd(bootstrap(data, mean))
sd(bootstrap(data, function(x){quantile(x, 0.9)}))

#~~~~~~~~~~~~~~~~~~~~
#Difference in Means:
#~~~~~~~~~~~~~~~~~~~~

data(StudentSurvey)
train <- StudentSurvey
#Let's look at gender versus MathGPA
train %>% group_by(Gender) %>% summarize(mean(MathSAT))

t.test(train$MathSAT[train$Gender == "F"],
       train$MathSAT[train$Gender == "M"])

#Let's first build confidence intervals for for each gender:
bootstrap(train$MathSAT[train$Gender == "M"], mean) -> boot_statistic_m
bootstrap(train$MathSAT[train$Gender == "F"], mean) -> boot_statistic_f
2*sd(boot_statistic_m)
2*sd(boot_statistic_f)
#it's hard to get a probability from this

#let's compute the difference in means for each boostrap sample:
male <- train$MathSAT[train$Gender == "M"]
female <- train$MathSAT[train$Gender == "F"]
replicate(1000, {
    boot_samples_m <- sample(length(male), replace = TRUE)
    boot_samples_f <- sample(length(female), replace = TRUE)
    mean(male[boot_samples_m] - mean(female[boot_samples_f]))
}) -> boot_statistic

hist(boot_statistic)
2*mean(boot_statistic < 0) #probability the two come from the same population - low

interval <- mean(boot_statistic) + c(-2,2)*sd(boot_statistic)
interval #again very similar to the two sample Welch t-test. 


#Permutation Test:
len <- length(male)
data <- c(male, female)

replicate(1000, {
permutation <- sample(length(data))
data <- data[permutation]
mean(data[1:len]) - mean(data[-(1:len)])
}) -> permutation_stat

hist(permutation_stat) +
abline(v = mean(male)- mean(female)) #the actual value.

mean(perlikmutation_stat >  mean(male)- mean(female)) *2
#slightly larger than the t-test and the bootstrap




