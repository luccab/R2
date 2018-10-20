Write your own original code that produces a dataset that conforms to the classic univariate regression model. Your data set should have 99 observations and a Normal error term.
The slope of the coefficient on your regressor should be positive. 
Now include a single outlier, such that when you fit a regression to your 100 data points, the slope of your regression line is negative.
Your answer to this question should consist of:
  Your original data-generating equation
Regression results for the original 99 (copy/paste the “summary” output)
Regression results with the outlier included (copy/paste “summary” output)
A properly-labeled data visualization that shows a single scatterplot, the regression line based on the original 99 points, and another differentiated regression line based on 100 points.
No more than 3 sentences that would serve as a caption for your figure if it were to be included in an econometrics textbook to illustrate the dangers of extrapolation.

set.seed(123)

x1 <- runif(99, 1, 100)

x1.data <- 5*x1 + rnorm(99, mean = 20, sd = 0)
str(lm(x1.data ~ x1))
summary(x1.data)
summary(x1)

plot(x1, x1.data)
abline(lm(x1.data ~ x1))

x2 <- append(x1, 100)
x2.data <- append(x1.data, -30000)

plot(x2, x2.data)
abline(lm(x2.data ~ x2))
summary(x2.data)
summary(x2)

#### Question 2 ####
NOTE: FOR THIS PROBLEM (AND THIS PROBLEM ONLY), USE ONLY THE CONTROL GROUP. 
DO NOT USE ANY UNITS FOR WHICH TREATMENT == 1.
Using the Lalonde data set and a linear model that predicts re78 as a linear additive function 
of age, educ, re74, re75, educ*re74, educ*re75, age*re74, age*re75, and re74*re75, estimate:
  the 95% prediction interval for re78, for every unit, using simulation 
(i.e., 10000 simulated predictions for every row from 10000 sets of coefficients). 
You will need to incorporate simulated sigmas, and you should hold educ, re74, and re75 at their medians 
(hence only age will vary). 
the 95% prediction interval for re78, for every unit, using simulation 
(i.e., 10000 simulated predictions for every row from 10000 sets of coefficients). 
You will need to incorporate simulated sigmas, and you should hold educ, re74, and re75 at their 90% quantiles 
(hence only age will vary).
Your answer to this question should consist of the following:
  A table with the relevant point estimates 
(e.g., the bounds of the prediction intervals of y for the different ages, and the medians of the other predictors)
2 figures showing the scatterplots (one for the analysis holding predictors at their medians, 
and other for the analysis holding predictors at their 90% quantiles). 
Each of these figures should show how the prediction intervals’ change over time. 
Be sure to label your scatterplot’s features (axis, title, etc.).

library(arm)
library(Matching)
data(lalonde)
lalonde

set.seed(123)
la
lalonde1 <- subset(lalonde, treat == 0)
lalonde1
lm1 <- lm(re78 ~ age + educ + re74 + re75 + I(educ*re74) + I(educ*re75) + I(age*re74) + I(age*re75) + I(re74*re75), data= lalonde)

lm1$coef
mean(sim_results@coef[,2])

sim_results <- sim(lm1, 10000)
sim_results
coef_results <- coef(sim_results, 10000)
head(coef_results)
min(lalonde1$age)
max(lalonde1$age)

sim_results@sigma[2]
coef_results[1,]
sum(1,2)

exp(sum(coef_results[i,]*c(0, 22, educ.median, re74.median, re75.median, educ.median*re74.median, educ.median*re75.median, 1*re74.median, 1*re75.median, re74.median*re75.median)  ) )  /  (1 + exp( sum(coef_results[i,]*c(1, 1, educ.median, re74.median, re75.median, educ.median*re74.median, educ.median*re75.median, 1*re74.median, 1*re75.median, re74.median*re75.median) ) )  ) 

sum(coef_results[1,]*c(0, 22, educ.median, re74.median, re75.median, educ.median*re74.median, educ.median*re75.median, 1*re74.median, 1*re75.median, re74.median*re75.median)  )
exp(sum(coef_results[i,]*c(0, 1, educ.median, re74.median, re75.median, educ.median*re74.median, educ.median*re75.median, 1*re74.median, 1*re75.median, re74.median*re75.median) ))
(1 + exp( sum(coef_results[i,]*c(0, 34, educ.median, re74.median, re75.median, educ.median*re74.median, educ.median*re75.median, 34*re74.median, 34*re75.median, re74.median*re75.median) ) )  )

educ.median <- median(lalonde1$educ)
educ.median
lalonde$re74
re74.median <- median(lalonde1$re74)
re75.median <- median(lalonde1$re75)
re74.median
re75.median

storage <- rep(NA, 10000)


storagedf_1 <- storage <- matrix(NA, nrow = 10000, ncol = 39)

for(age in c(17:55)) {
  for(i in 1:10000)
  {
    beta <- sum(coef_results[i,]*c(1, age, educ.median, re74.median, re75.median, educ.median*re74.median, educ.median*re75.median, age*re74.median, age*re75.median, re74.median*re75.median), sim_results@sigma[i]  )
    storagedf_1[i, age - 16] <- beta
  }
}

conf.intervals1 <- apply(storagedf_1, 2, quantile, probs = c(0.005, 0.995))
conf.intervals1


educ.percentile <- quantile(lalonde1$educ, .90)
re74.percentile <- quantile(lalonde1$re74, .90)
re75.percentile <- quantile(lalonde1$re75, .90)


storagedf_2 <- storage <- matrix(NA, nrow = 10000, ncol = 39)

for(age in c(17:55)) {
  for(i in 1:10000)
  {
    beta1 <- sum(coef_results[i,]*c(1, age, educ.percentile, re74.percentile, re75.percentile, educ.percentile*re74.percentile, educ.percentile*re75.percentile, age*re74.percentile, age*re75.percentile, re74.percentile*re75.percentile), sim_results@sigma[i]  )
    storagedf_2[i, age - 16] <- beta1
  }
}

conf.intervals2 <- apply(storagedf_2, 2, quantile, probs = c(0.005, 0.995))
conf.intervals2


plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(0,20000))


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

####Question 3 ###
3. 	Obtain the nsw.dta dataset from http://users.nber.org/~rdehejia/data/nswdata2.html. 
Read the description of this data set provided on the page. If you proceed with this work in R (recommended) use the foreign library to open it (so you can use read.dta).

Specify a regression model in which the dependent variable is re78 and the sole predictor is treatment 
(and, an intercept should be included automatically, by default). 
Then, bootstrap the 95% confidence intervals for the value of the coefficient for treatment. 
Then, obtain the analytical confidence interval for the coefficient value using the standard error that 
pops out of a regression (or equivalently, in R, you can use the confint function). 
Compare the two confidence intervals--one obtained via simulation, the other via the formula.

NOTE: Make sure that you don’t use a ‘canned’ bootstrap function -- please code the bootstrap routine manually.


Your answer to this question should consist of the following:
  A table with the relevant results (bounds on the 2 confidence intervals).
1 histogram (properly labeled) showing your bootstrap-sample results. How you do this one is up to you.
No more than 3 sentences summarizing the results and drawing any conclusions you find relevant and interesting.

library(foreign)
library(Matching)
library(boot)

nsw <- read.dta("nsw.dta")
nsw

glm.nsw <- glm(re78 ~ treat, data =nsw )
summary(glm.nsw)
coef(glm.nsw)
plot(glm)



# cv.glm performs LOOCV (You can use the K argument to perform K-fold CV)
cv.err <- cv.glm(nsw, glm.nsw)
cv.err$delta

cv.error=rep(0,5)
for (i in 1:1){
  glm.fit=glm(re78~poly(treat ,i),data=nsw)
  cv.error[i]=cv.glm(nsw ,glm.fit)$delta[1]
}
cv.error


set.seed(1314)

nsims <- 10000

#nsw$treat
#mean(sample(nsw$treat, 100, replace = TRUE))

storage_intercept <- matrix(NA, ncol = 10000, nrow = 100)
storage_treatment_effect <- matrix(NA, ncol = 10000, nrow = 100)

# GO THROUGH THE BELOW, LINE BY LINE, & TRY TO UNDERSTAND EVERYTHING
# FOCUS ON WHATEVER YOU FIND CONFUSING


index <- sample(1:nrow(nsw), nrow(nsw), replace = TRUE)
bootstrapped_sample <- nsw[index,]
summary_statistic_of_boot_sample <- summary(lm(re78 ~ treat, data = bootstrapped_sample))
coef(summary_statistic_of_boot_sample)[2]

set.seed(2321)
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
quantile(storage_intercept[2,], na.rm = TRUE, c(0.025, 0.975))
quantile(storage_treatment_effect[2,], na.rm = TRUE, c(0.025, 0.975))

mean_intercepts <- apply(storage_intercept, 2, mean)
print(mean(mean_intercepts))

mean_intercepts <- apply(storage_treatment_effect, 2, mean)
print(mean(mean_treatment_effect))

storage_intercept[1,2]

conf_intervals_intercept <- apply(storage_intercept, 2, quantile, probs = c(0.005, 0.995))
conf_intervals_intercept

var_of_correlated_means <- var(correlated_means)
print(var_of_correlated_means)

confint(glm.nsw)

conf.intervals2 <- apply(storagedf_2, 2, quantile, probs = c(0.005, 0.995))

############  Question 4 ###
4.	Write a function (5 lines max) that takes Ys and predicted Ys as inputs, and outputs R2. 
Copy/paste an example using the nsw.dta data (from #3 above) that shows it working.

linear <- lm(re78 ~ treat, data =nsw)
summary(linear)

summary(predict(glm.nsw))
determination(predict(glm.nsw), nsw$re78)

determination <- function(y, py) cor(y, py) ^ 2

determination(a,b)

######################
5. 	Use the nsw.dta dataset from question 3 above to estimate the probability of being assigned to the 
treatment group (vs. the control group) for every observation in the data set. Your logistic regression 
model should be a linear additive function of all predictors available to you -- no interaction terms needed. 
NOTE: re78 is not a predictor because it postdates the treatment. (In other words, it’s an outcome.)

Your answer to this question should consist of the following:
  Two properly labeled histograms: one in red 
(showing the distribution of the treatment group’s estimated probabilities) 
and one in blue (showing the distribution of the control group’s estimated probabilities). 
Extra credit for a legend in the plot.
No more than 3 sentences summarizing the differences between the two distributions of estimated probabilities, 
and whether/not your results are surprising and/or intuitive.

if nsw$treat == 0{
  
}
nsw.1 <- subset(nsw, treat == 1)
nsw.0
nsw$treat[298:722]

linear <- lm(treat~age+education+black+hispanic+married+nodegree+re75, data=nsw)
glinear <- glm(treat~age+education+black+hispanic+married+nodegree+re75, data=nsw, family="binomial")
probs.glinear <- predict(glinear, nsw, "response")
probs.glinear[1:297]
pred.glinear = rep(0, length(probs.glinear > .5))
pred.glinear[probs.glinear > .5] = probs.glinear
hist(probs.glinear[1:297])
hist(probs.glinear[298:722])

probs.glinear

predict(linear) > 0.5

hist(probs.glinear)
linear1$probs
hist(linear$probs)

### OPTIONAL QUESTION ###
Write code that repeatedly randomly selects predictors from the lalonde data set, 
runs a regression on those predictors (with “re78” as the dependent variable), and saves the treatment effect.  
Produce a histogram showing the wide range of treatment effects that pop out. 
(This is much easier if you exclude interaction effects). Produce another histogram that shows 
the wide range of treatment effects that pop out that are also statistically significant.

data(lalonde)
lalonde()
number_of_predictors <- sample(1:11, 1)

# age    -> 1
# educ   -> 2
# black  -> 3
# hisp   -> 4
# married-> 5
# nodegr -> 6
# re74   -> 7
# re75   -> 8
# u74    -> 9
# u75    -> 10
# treat  -> 11

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


