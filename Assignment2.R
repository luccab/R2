### Question 1 ###
# Write your own original code that produces a dataset that conforms to the classic univariate regression model. Your data set should have 99 observations and a Normal error term.
# The slope of the coefficient on your regressor should be positive. 
# Now include a single outlier, such that when you fit a regression to your 100 data points, the slope of your regression line is negative.
# Your answer to this question should consist of:
#  Your original data-generating equation
# Regression results for the original 99 (copy/paste the “summary” output)
# Regression results with the outlier included (copy/paste “summary” output)
# A properly-labeled data visualization that shows a single scatterplot, the regression line based on the original 99 points, and another differentiated regression line based on 100 points.
# No more than 3 sentences that would serve as a caption for your figure if it were to be included in an econometrics textbook to illustrate the dangers of extrapolation.
library("ggplot2")
set.seed(123)

x1 <- runif(99, 1, 100)
x1.data <- 5*x1 + rnorm(99, mean = 20, sd = 20)
intercept_x1 <- summary(lm(x1.data ~ x1))$coef[1]
slope_x1 <- summary(lm(x1.data ~ x1))$coef[2]

summary(x1.data) # V
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 21.81  155.24  274.55  273.51  391.94  513.35 
summary(x1) # V
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.062  25.241  47.130  50.344  75.890  99.433 
lm.99 <- lm(x1.data ~ x1)
summary(lm.99) # V
# Call:
#   lm(formula = x1.data ~ x1)
#
# Residuals:
#    Min     1Q Median     3Q    Max 
# -47.78 -13.00  -1.75  11.64  41.80 
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 25.87283    3.63893    7.11 1.98e-10 ***
#   x1           4.91888    0.06305   78.01  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 17.7 on 97 degrees of freedom
# Multiple R-squared:  0.9843,	Adjusted R-squared:  0.9841 
# F-statistic:  6086 on 1 and 97 DF,  p-value: < 2.2e-16

# Graph on 99 variables
df1 <- data.frame(x1,x1.data)
# Graphing the regression line and standard error without outlier
ggplot(df1, aes(x=x1, y=x1.data)) + geom_point()+ geom_smooth(method=lm, se=TRUE, level=0.95)+
  ggtitle("Regression Line without the outlier") +
  xlab("Predictor") +
  ylab("Dependent variable")

# Adding the outlier
x2 <- append(x1, -200)
x2.data <- append(x1.data, 2500)
df2 <- data.frame(x2,x2.data)

summary(x2.data) # V 
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 21.81  155.37  275.27  295.78  398.48 2500.00 
summary(x2) # V
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -200.00   24.86   46.67   47.84   75.79   99.43 
lm.100 <- lm(x2.data ~ x2)
summary(lm.100) # V
# Call:
#   lm(formula = x2.data ~ x2)
#
# Residuals:
#    Min      1Q  Median      3Q     Max 
# -325.72 -163.58  -21.14  140.23 1915.36 
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 351.5350    42.2154   8.327 5.06e-13 ***
#   x2           -1.1655     0.6943  -1.679   0.0964 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 260.6 on 98 degrees of freedom
# Multiple R-squared:  0.02795,	Adjusted R-squared:  0.01803 
# F-statistic: 2.818 on 1 and 98 DF,  p-value: 0.09639

# Graphing the regression line and standard error with outlier
ggplot(df2, aes(x=x2, y=x2.data)) + geom_point()+ geom_smooth(method=lm, se=TRUE, level=0.95)+
  ggtitle("Regression Line with the outlier") +
  xlab("Predictor") +
  ylab("Dependent variable")

# Graphing 2 lines at the same time with(blue) and without(red) the outlier
ggplot(df2, aes(x=x2, y=x2.data)) + geom_point() + geom_smooth(method=lm, se=TRUE, level=0.95) + 
  geom_abline(intercept = intercept_x1, slope = slope_x1, color="red", size=1.2)+ 
  ggtitle("Regression Line with(blue) and without(red) the outlier") + 
  xlab("Predictor") +
  ylab("Dependent variable")+
  labs(caption = "As we can see a single outlier can have a big effect on the regression line. 
       So the outlier can hide an almost linear relationship and produce a line that will not be representative of the real data. 
       Outliers should be checked for errors, and in case of great impact on the regression line it is reasonable to remove it and report it explaining your results")


#### Question 2 ####
# NOTE: FOR THIS PROBLEM (AND THIS PROBLEM ONLY), USE ONLY THE CONTROL GROUP. 
# DO NOT USE ANY UNITS FOR WHICH TREATMENT == 1.
# Using the Lalonde data set and a linear model that predicts re78 as a linear additive function 
# of age, educ, re74, re75, educ*re74, educ*re75, age*re74, age*re75, and re74*re75, estimate:
#   the 95% prediction interval for re78, for every unit, using simulation 
# (i.e., 10000 simulated predictions for every row from 10000 sets of coefficients). 
# You will need to incorporate simulated sigmas, and you should hold educ, re74, and re75 at their medians 
# (hence only age will vary). 
# the 95% prediction interval for re78, for every unit, using simulation 
# (i.e., 10000 simulated predictions for every row from 10000 sets of coefficients). 
# You will need to incorporate simulated sigmas, and you should hold educ, re74, and re75 at their 90% quantiles 
# (hence only age will vary).
# Your answer to this question should consist of the following:
#   A table with the relevant point estimates 
# (e.g., the bounds of the prediction intervals of y for the different ages, and the medians of the other predictors)
# 2 figures showing the scatterplots (one for the analysis holding predictors at their medians, 
# and other for the analysis holding predictors at their 90% quantiles). 
# Each of these figures should show how the prediction intervals’ change over time. 
# Be sure to label your scatterplot’s features (axis, title, etc.).

library(arm)
library(Matching)
data(lalonde)
lalonde

set.seed(123)
# Using only control group
lalonde1 <- subset(lalonde, treat == 0)
lm1 <- lm(re78 ~ age + educ + re74 + re75 + I(educ*re74) + I(educ*re75) + I(age*re74) + I(age*re75) + I(re74*re75), data= lalonde1)
# Simulating and getting coefficients
sim_results <- sim(lm1, 10000)
coef_results <- coef(sim_results, 10000)

# Getting the median values
educ.median <- median(lalonde1$educ)
re74.median <- median(lalonde1$re74)
re75.median <- median(lalonde1$re75)

# Creating variable to store the simulation results (multiplicating simulated coefficients generated by the median values, age, and summing normal distribution of sigmas (get through 'sim'))
storagedf_1 <- matrix(NA, nrow = 10000, ncol = 39)
# Storing values get through simulated coefficients
for(age in c(17:55)) {
  for(i in 1:10000)
  {
    beta <- sum(coef_results[i,]*c(1, age, educ.median, re74.median, re75.median, educ.median*re74.median, educ.median*re75.median, age*re74.median, age*re75.median, re74.median*re75.median), rnorm(1,0,sim_results@sigma[i])  )
    storagedf_1[i, age - 16] <- beta
  }
}
# Getting confidence intervals for re78 based on the simulated coefficients and variables at their median
conf.intervals1 <- apply(storagedf_1, 2, quantile, probs = c(0.025, 0.975))
conf.intervals1

#Getting qunatile values
educ.percentile <- quantile(lalonde1$educ, .90)
re74.percentile <- quantile(lalonde1$re74, .90)
re75.percentile <- quantile(lalonde1$re75, .90)

# Variable to store results
storagedf_2 <-matrix(NA, nrow = 10000, ncol = 39)

# Storing values from simulated coefficients and holding variables in their percentile (educ, re74, re75) for each age
for(age in c(17:55)) {
  for(i in 1:10000)
  {
    beta1 <- sum(coef_results[i,]*c(1, age, educ.percentile, re74.percentile, re75.percentile, educ.percentile*re74.percentile, educ.percentile*re75.percentile, age*re74.percentile, age*re75.percentile, re74.percentile*re75.percentile), rnorm(1,0,sim_results@sigma[i])  )
    storagedf_2[i, age - 16] <- beta1
  }
}

# Getting confidence intervals for re78 based on the simulated coefficients and variables at their percentila
conf.intervals2 <- apply(storagedf_2, 2, quantile, probs = c(0.005, 0.995))
conf.intervals2

# Graphing based on median for educ, re74, and re75 and age groups
plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(-15000,25000))
counter = 1
for(age in c(17:55)) {
  segments(x0 = age,  y0 = conf.intervals1[counter], x1 = age, y1 = conf.intervals1[counter + 1], col = c("red"))
  counter = counter + 2
}

# Graphing based on .9 perfentile for educ, re74, and re75 and age groups
plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(-15000,25000))
counter = 1
for(age in c(17:55)) {
  segments(x0 = age,  y0 = conf.intervals2[counter], x1 = age, y1 = conf.intervals2[counter + 1], col = c("blue"))
  counter = counter + 2
}

# Graphing based on .9 perfentile and median for educ, re74, and re75 and age groups one above the other
plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(-15000,25000))
counter = 1
for(age in c(17:55)) {
  segments(x0 = age,  y0 = conf.intervals1[counter], x1 = age, y1 = conf.intervals1[counter + 1], col = c("red"))
  counter = counter + 2
}
counter = 1
for(age in c(17:55)) {
  segments(x0 = age,  y0 = conf.intervals2[counter], x1 = age, y1 = conf.intervals2[counter + 1], col = c("blue"))
  counter = counter + 2
}
# Creating the tables

# Table with 95% percentiles (higher and lower bound) for each age group
table1 <- matrix(conf.intervals1, nrow= 39, byrow =TRUE)
colnames(table1) <- c("Lower bound", "Higher bound")
rownames(table1) <- c(17:55)
table1

# Table with median values used to get the confidence interval
table2 <- matrix(c(educ.median, re74.median, re75.median), nrow=1)
colnames(table2) <- c("Education Median", "re74 Median", "re75 Median")
table2

# Table with 90% percentile values used to get the confidence interval
table3 <- matrix(c(educ.percentile, re74.percentile, re75.percentile), nrow=1)
colnames(table3) <- c("Education .9 Percentile", "re74 .9 Percentile", "re75 .9 Percentile")
table3

#### Question 3 ####
#	Obtain the nsw.dta dataset from http://users.nber.org/~rdehejia/data/nswdata2.html. 
# Read the description of this data set provided on the page. If you proceed with this work in R (recommended) use the foreign library to open it (so you can use read.dta).
#
# Specify a regression model in which the dependent variable is re78 and the sole predictor is treatment 
# (and, an intercept should be included automatically, by default). 
# Then, bootstrap the 95% confidence intervals for the value of the coefficient for treatment. 
# Then, obtain the analytical confidence interval for the coefficient value using the standard error that 
# pops out of a regression (or equivalently, in R, you can use the confint function). 
# Compare the two confidence intervals--one obtained via simulation, the other via the formula.
# 
# NOTE: Make sure that you don’t use a ‘canned’ bootstrap function -- please code the bootstrap routine manually.
# 
# Your answer to this question should consist of the following:
#   A table with the relevant results (bounds on the 2 confidence intervals).
# 1 histogram (properly labeled) showing your bootstrap-sample results. How you do this one is up to you.
# No more than 3 sentences summarizing the results and drawing any conclusions you find relevant and interesting.

library(foreign)
library(Matching)
library(boot)

nsw <- read.dta("nsw.dta")

set.seed(1314)
# Setting number of simulations and initializing variables
nsims <- 1000
storage_intercept <- matrix(NA, ncol = 1000, nrow = 100)
storage_treatment_effect <- matrix(NA, ncol = 1000, nrow = 100)

set.seed(2321)
# Bootstrapping and saving the intercept and treatment effect coefficients
for(j in 1:nsims) {
  simulated_original_data <- nsw
  for(i in 1:100)
  {
    index <- sample(1:nrow(nsw), nrow(nsw), replace = TRUE)
    bootstrapped_sample <- nsw[index,]
    bootstrapped_coefs <- coef(summary(lm(re78 ~ treat, data = bootstrapped_sample)))
    
    storage_intercept[i,j] <- bootstrapped_coefs[1]
    storage_treatment_effect[i,j] <- bootstrapped_coefs[2]
  }
  
}
# Taking the quantiles of the saved coefficients
quantile(storage_intercept, na.rm = TRUE, c(0.025, 0.975))
quantile(storage_treatment_effect, na.rm = TRUE, c(0.025, 0.975))

# Making the histogram for treatment effect
hist(storage_treatment_effect)

#ggplot(as.data.frame(storage_treatment_effect), aes(x=storage_treatment_effect))+ geom_histogram()

# Since treat is the single predictor the treatment effect is what is going to the value, it will either be the intercept if assigned to control group or the intercept + treatment effect if assigned to treatment group.
# So we can see that going to the treatment group can either increase or decrease your earnings in 1978 but it is much more likely to increase than decrease (since all the data is concentrated in 1000)


#### Question 4 ####
#	 Write a function (5 lines max) that takes Ys and predicted Ys as inputs, and outputs R2. 
# Copy/paste an example using the nsw.dta data (from #3 above) that shows it working.


linear <- lm(re78 ~ treat, data =nsw)
# Summary function so we can access the R2 value 
summary(linear)
# Sending predicted and the real values to the function which will return R2
determination(predict(linear), nsw$re78)
# We can see that both R2 values are 0.0048

# Calculating the correlation between the variables and then squaring it
determination <- function(y, py) cor(y, py) ^ 2

#### Question 5 ####
# 	Use the nsw.dta dataset from question 3 above to estimate the probability of being assigned to the 
# treatment group (vs. the control group) for every observation in the data set. Your logistic regression 
# model should be a linear additive function of all predictors available to you -- no interaction terms needed. 
# NOTE: re78 is not a predictor because it postdates the treatment. (In other words, it’s an outcome.)

# Your answer to this question should consist of the following:
#   Two properly labeled histograms: one in red 
# (showing the distribution of the treatment group’s estimated probabilities) 
# and one in blue (showing the distribution of the control group’s estimated probabilities). 
# Extra credit for a legend in the plot.
# No more than 3 sentences summarizing the differences between the two distributions of estimated probabilities, 
# and whether/not your results are surprising and/or intuitive.


nt = 0
# For loop to get all the ones who were assigned to treatment group so we can separate the results on the histogram
for (i in 1:length(nsw$treat)){
  if(nsw$treat[i] == 1){
    nt <- nt +1
  }
}
nsw1 <- as.data.frame(nsw$treat[1:nt])
nsw2 <- as.data.frame(nsw$treat[nt+1:length(nsw$treat)])
# Predicting the probability of going to treatment group
glinear <- glm(treat~age+education+black+hispanic+married+nodegree+re75, data=nsw, family="binomial")
probs.glinear <- predict(glinear, nsw, "response")
graphs <- cbind(nsw, probs.glinear)

ggplot(nsw1, aes(x=probs.glinear[1:nt])) +geom_histogram(color= "red", fill="red")
ggplot(nsw2, aes(x=probs.glinear[nt+1:length(nsw$treat)])) +geom_histogram(color= "blue", fill="blue")
# Putting both of them in the same graph and changing color for easy comparison
ggplot(graphs , aes(x=probs.glinear, fill=factor(treat), color=factor(treat))) + geom_histogram(position="identity")+ ggtitle("Probability of being assigned to treatment group\n Treatment group in blue and control in red")+ylab("probabilities")

# We can see that the treatment group has higher probabilities, and more values above 0.5 than the control group, but those 2 groups do not have a big differente.
# This mean that only those predictors it was not good enough to separate them appropriatelly in control and treatment group.
# Although it was not enough we can see that there is more high probabilities in blue than in red in high values (meaning we predicted it to be from the treatment group)

### OPTIONAL QUESTION ###
# Write code that repeatedly randomly selects predictors from the lalonde data set, 
# runs a regression on those predictors (with “re78” as the dependent variable), and saves the treatment effect.  
# Produce a histogram showing the wide range of treatment effects that pop out. 
# (This is much easier if you exclude interaction effects). Produce another histogram that shows 
# the wide range of treatment effects that pop out that are also statistically significant.

data(lalonde)
lalonde
number_of_predictors <- sample(1:11, 1)

nsims <-1000
storing_treatment_effect <- matrix(NA, nrow = nsims)
storing_statistically_significant_treatment_effect <- matrix(NA, nrow = nsims)
a = 1
for(j in 1:nsims){
  number_of_predictors <- sample(1:11, 1)
  predictors <- sample(c("age", "educ", "black", "hisp", "married", "nodegr", "re74", "re75", "u74", "u75", "treat"), number_of_predictors)
  vars <- paste(predictors, collapse = ' + ')
  equation <- paste0("re78 ~ ", vars)
  m2 <- lm(as.formula(equation), data=lalonde)
  treatment_effect <- mean(predict(m2)[1:185]) - mean(predict(m2)[186:445])
  storing_treatment_effect[j] <- treatment_effect
  
  x <- summary(m2)
  statistical <- pf(x$fstatistic[1], x$fstatistic[2], x$fstatistic[3],lower.tail = FALSE)
  if(statistical < 0.05){
    storing_statistically_significant_treatment_effect[a] <- treatment_effect
    a <- a + 1
  }
  
}

hist(storing_treatment_effect, xlim=c(-100,2000), ylim = c(0,1000))

hist(na.omit(storing_statistically_significant_treatment_effect), xlim=c(-100,2000), ylim = c(0, 1000))


