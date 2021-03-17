# Assignment 2
# 1

n <- 999
x <- runif(n, min = 0, max = 1)
mean <- 0
sd <- 10
set.seed(1)
y <- 2 * x + 3 + rnorm(n, mean = mean, sd = sd)
x.1000 <- mean(x) + 13*sd(x)
y.1000 <- mean(y) - 10*sd(y)
lm.fit.999 <- lm(y ~ x)
summary(lm.fit.999)

# data with 1000th observation
new_x <- x[1:999]
new_x[1000] <- x.1000
new_y <- y[1:999]
new_y[1000] <- y.1000
lm.fit.1000 <- lm(new_y ~ new_x)
summary(lm.fit.1000)

# plot
plot(
  x,
  y,
  cex = .2,
  col = "black",
  xlim = c(0, x.1000),
  ylim = c(y.1000, max(y)),
  xlab = "x",
  ylab = "y",
  pch = 19
)
title('Linear Regression Fit for Data With and Without Outlier')
points(x.1000,
       y.1000,
       cex = 1,
       col = "red",
       pch = 19)
abline(lm.fit.999, col = "black", lwd = 4)
abline(lm.fit.1000, col = "red", lwd = 4)
legend(
  0,
  -70,
  c("Without outlier",
    "With outlier"),
  col = c("black", "red"),
  lwd = c(4, 4),
  cex = .9
)

#############
# 2
library(arm)
library(Matching)
data(lalonde)
lm.fit <- lm(re78~age+educ+re74+re75+I(educ*re74)+I(educ*re75)+
               I(age*re74)+I(age*re75)+I(age*age)+I(re74*re75), data=lalonde)
set.seed(2)
sim_results <- sim(lm.fit, n.sims = 10000)
get_expected_value <- function(coefs, vec) {
  # vec is (age, educ, re74, re75)
  age <- vec[1]
  educ <- vec[2]
  re74 <- vec[3]
  re75 <- vec[4]
  new_vec <- c(1, age, educ, re74, re75, educ*re74, 
               educ*re75, age*re74, age*re75, age*age, re74*re75)
  return(sum(coefs*new_vec))
}


get_matrix_for_intervals <- function(educ_val, re74_val, re75_val){
  storage.matrix <- matrix(NA, nrow=10000, ncol=39)
  for (age in c(17:55)) {
    for (i in 1:10000) {
      this_unit <- c(age, educ_val, re74_val, re75_val)
      storage.matrix[i,age-16] <- get_expected_value(sim_results@coef[i,], this_unit)
    }
  }
  return(storage.matrix)
  }

# a
storage.matrix.median <- get_matrix_for_intervals(median(lalonde$educ),
                                                  median(lalonde$re74),
                                                  median(lalonde$re75))
conf.intervals_median <- apply(storage.matrix.median, 2, 
                               quantile, probs = c(0.025, 0.975))



# b
storage.matrix.75 <- get_matrix_for_intervals(quantile(lalonde$educ, prob=.75)[[1]],
                                              quantile(lalonde$re74, prob=.75)[[1]],
                                              quantile(lalonde$re75, prob=.75)[[1]])
conf.intervals_75 <- apply(storage.matrix.75, 2, 
                               quantile, probs = c(0.025, 0.975))
# for c and d
get_matrix_for_prediction_intervals <- function(educ_val, re74_val, re75_val){
  storage.matrix.prediction <- matrix(NA, nrow=10000, ncol=39)
  for (age in c(17:55)) {
    for (i in 1:10000) {
      this_unit <- c(age, educ_val, re74_val, re75_val)
      storage.matrix.prediction[i, age-16] <-
        get_expected_value(sim_results@coef[i,], this_unit) +
        rnorm(1, 0, sim_results@sigma[i])
    }
  }
  return(storage.matrix.prediction)
}

# c
storage.matrix.prediction.median <- 
  get_matrix_for_prediction_intervals(median(lalonde$educ),
                                      median(lalonde$re74),
                                      median(lalonde$re75))
conf.prediction_intervals_median <- apply(storage.matrix.prediction.median, 2, 
                           quantile, probs = c(0.025, 0.975))
# d
storage.matrix.prediction.75 <-
  get_matrix_for_prediction_intervals(quantile(lalonde$educ, prob=.75)[[1]],
                                      quantile(lalonde$re74, prob=.75)[[1]],
                                      quantile(lalonde$re75, prob=.75)[[1]])
conf.prediction_intervals_75 <- apply(storage.matrix.prediction.75, 2, 
                                          quantile, probs = c(0.025, 0.975))
# plots for a and b
plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(0,20000), 
     main = "Expected values for earnings in 1978 by age", xlab = "Age", 
     ylab = "Earning")
for (age in 17:55) {
  segments(
    x0 = age,
    y0 = conf.intervals_median[1, age - 16],
    x1 = age,
    y1 = conf.intervals_median[2, age - 16],
    lwd = 4)
  #arrows(age, conf.intervals_median[1, age - 16], age,
   #      conf.intervals_median[2, age - 16], lwd = 3, angle = 90,
    #     code = 3, length = 0.05)
}

for (age in 17:55) {
  segments(
    x0 = age,
    y0 = conf.intervals_75[1, age - 16],
    x1 = age,
    y1 = conf.intervals_75[2, age - 16],
    lwd = 1,
    lend=3,
    lty='dotted')
  arrows(age, conf.intervals_75[1, age - 16], age,
         conf.intervals_75[2, age - 16], lwd = 1.5, angle = 90,
         code = 3, length = 0.05)
}
abline(v = mean(lalonde$age), col = "red", lwd = 2)
# plot for c and d
plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(-10000,20000), 
     main = "Predicted values for earnings in 1978 by age", xlab = "Age", 
     ylab = "Earning")
for (age in 17:55) {
  segments(
    x0 = age,
    y0 = conf.prediction_intervals_median[1, age - 16],
    x1 = age,
    y1 = conf.prediction_intervals_median[2, age - 16],
    lwd = 4)
  #arrows(age, conf.intervals_median[1, age - 16], age,
  #      conf.intervals_median[2, age - 16], lwd = 3, angle = 90,
  #     code = 3, length = 0.05)
}

for (age in 17:55) {
  segments(
    x0 = age,
    y0 = conf.intervals_75[1, age - 16],
    x1 = age,
    y1 = conf.intervals_75[2, age - 16],
    lwd = 1,
    lend=3,
    lty='dotted')
  arrows(age, conf.prediction_intervals_75[1, age - 16], age,
         conf.prediction_intervals_75[2, age - 16], lwd = 1.5, angle = 90,
         code = 3, length = 0.05)
}
abline(v = mean(lalonde$age), col = "red", lwd = 2)

####################

# 3
data(PlantGrowth)
which.trt2 <- which(PlantGrowth$group == 'trt2')
new_df <- PlantGrowth[-which.trt2,]
encoded_group <- rep(0, length(new_df$group))
which.trt1 <- which(PlantGrowth$group == 'trt1')
encoded_group[which.trt1] <- 1
new_df$group <- encoded_group
lm.fit3 <- lm(weight~group, data=new_df)
confint(lm.fit3)[2,] # analytical CI

# sample (100 ,100 , replace =T))
storage.coef <- c()
storage.coef 
set.seed(1)
for (i in 1:10000) {
  index <- sample(nrow(new_df), nrow(new_df), replace=TRUE)
  weight.boot <- new_df$weight[index]
  group.boot <- new_df$group[index]
  lm.fit.boot <- lm(weight.boot ~ group.boot)
  storage.coef[i] <- lm.fit.boot$coef[2]
}
quantile(storage.coef, probs=c(.025,.975))
hist(storage.coef, main='Histogram of the bootstrapped estimates for the coefficient \nof treatment',
     xlab='Bootstrapped estimate',
     ylab='Frequency',
     col='gray')
mean(storage.coef)
abline(v=mean(storage.coef),col='red',lwd=2)
abline(v=quantile(storage.coef, probs=c(.025,.975))[[1]],
       col='blue',lwd=2)
abline(v=quantile(storage.coef, probs=c(.025,.975))[[2]],
       col='blue',lwd=2)
mean(storage.coef) - quantile(storage.coef, probs=c(.025,.975))[[1]] # 0.577549
quantile(storage.coef, probs=c(.025,.975))[[2]] - mean(storage.coef) # 0.602279

# plot 3 plots with difference numbers of bootstrapping
par(mfrow=c(1,3))
storage.coef.1 <- c()
set.seed(1)
for (i in 1:50) {
  index <- sample(nrow(new_df), nrow(new_df), replace=TRUE)
  weight.boot <- new_df$weight[index]
  group.boot <- new_df$group[index]
  lm.fit.boot <- lm(weight.boot ~ group.boot)
  storage.coef.1[i] <- lm.fit.boot$coef[2]
}
storage.coef.2 <- c()
set.seed(1)
for (i in 1:100) {
  index <- sample(nrow(new_df), nrow(new_df), replace=TRUE)
  weight.boot <- new_df$weight[index]
  group.boot <- new_df$group[index]
  lm.fit.boot <- lm(weight.boot ~ group.boot)
  storage.coef.2[i] <- lm.fit.boot$coef[2]
}
storage.coef.3 <- c()
set.seed(1)
for (i in 1:1000) {
  index <- sample(nrow(new_df), nrow(new_df), replace=TRUE)
  weight.boot <- new_df$weight[index]
  group.boot <- new_df$group[index]
  lm.fit.boot <- lm(weight.boot ~ group.boot)
  storage.coef.3[i] <- lm.fit.boot$coef[2]
}
hist(storage.coef.1,
     main='',
     xlab='Bootstrapped estimate',
     ylab='Frequency',
     col='gray')
hist(storage.coef.2,
     main='',
     xlab='Bootstrapped estimate',
     ylab='Frequency',
     col='gray')
hist(storage.coef.3,
     main='',
     xlab='Bootstrapped estimate',
     ylab='Frequency',
     col='gray')
mtext("Histograms for estimates of treatment's coefficients", side = 3, line = -2, outer = TRUE)


### plot for the length of interval
par(mfrow=c(1,2))
storage.len.interval <- c()
for (i in seq(10,1000,10)){
  storage.coef <- c()
  for (j in 1:i) {
    index <- sample(nrow(new_df), nrow(new_df), replace=TRUE)
    weight.boot <- new_df$weight[index]
    group.boot <- new_df$group[index]
    lm.fit.boot <- lm(weight.boot ~ group.boot)
    storage.coef[j] <- lm.fit.boot$coef[2]
  }
  upper <- quantile(storage.coef, probs=c(.025,.975))[[2]]
  lower <- quantile(storage.coef, probs=c(.025,.975))[[1]]
  storage.len.interval[i/10] <- upper - lower
}
plot(seq(10,1000,10), storage.len.interval,
     main='Range of the 95% confidence interval for
     the estimates of the coefficient for treatment',
     xlab='Number of bootstrapped samples used',
     ylab='Length of the interval')
abline(h=confint(lm.fit3)[2,][[2]]-confint(lm.fit3)[2,][[1]])

# plot for possible convergence of estimate for coefficients
storage.mean.coef <- c()
for (i in seq(10,1000,10)){
  storage.coef <- c()
  for (j in 1:i) {
    index <- sample(nrow(new_df), nrow(new_df), replace=TRUE)
    weight.boot <- new_df$weight[index]
    group.boot <- new_df$group[index]
    lm.fit.boot <- lm(weight.boot ~ group.boot)
    storage.coef[j] <- lm.fit.boot$coef[2]
  }
  storage.mean.coef[i/10] <- mean(storage.coef)
}

plot(seq(10,1000,10), storage.mean.coef,
     main = 'Mean of the bootstrapped estimates of 
     the coefficient for treatment',
     xlab='Number of bootstrapped samples used',
     ylab='Mean of estimates')
abline(h=lm.fit3$coef[2])

########################
# 4
rsquared <- function(y, y_hat) {
  # RSS (residual sum of squares) is the sum
  # of the squares of the deviations of the 
  # predicted ys from ys.
  RSS <- sum((y-y_hat)^2)
  # TSS (total sum of squares) is the sum of the squares
  # of the difference between each observation and the mean
  # of all the observations
  TSS <- sum((y-mean(y))^2)
  # R^2 = 1-RSS/TSS
  return(1-RSS/TSS)
}
rsquared(new_df$weight,predict.lm(lm.fit3))
summary(lm.fit3)$r.squared

##########################
# 5
setwd("/Users/admin/Documents/CS112/Assignment/assignment 2/")
library(foreign)
nsw <- read.dta('nsw.dta')
str(nsw)
glm.fit <- glm(treat~age+education+black+hispanic+married+nodegree+re75,
               data=nsw, family='binomial')
which.treated <- which(nsw$treat == 1)
nrow(nsw[which.treated,]) # 297, the number of treated
nrow(nsw[-which.treated,]) # 425, the number of controls
treat_probs <- predict(glm.fit, newdata=nsw[which.treated,], type='response')
control_probs <- predict(glm.fit, newdata=nsw[-which.treated,], type='response')
par(mfrow=c(1,1))
plot(density(control_probs), col = "blue", lwd = 3,
     main='Density plot for propensity scores',
     xlab='Estimated probability',
     ylab='Density',
     xlim=c(0.27,.6))
lines(density(treat_probs), col = "red", lwd = 3)
legend(
  0.48,
  20,
  c("Treatment group",
    "Control group"),
  col = c("red", "blue"),
  lwd = c(4, 4),
  cex = .9
)
length(which.treated)
nrow(nsw) - length(which.treated)
hist(treat_probs)
