library(shiny)
library(ggplot2)

# Importing dataset

set.seed(123)

x1 <- runif(99, 1, 100)
x1.data <- 5*x1 + rnorm(99, mean = 20, sd = 20)
df1 <- data.frame(x1,x1.data)
intercept_x1 <- summary(lm(x1.data ~ x1))$coef[1]
slope_x1 <- summary(lm(x1.data ~ x1))$coef[2]

x2 <- append(x1, -200)
x2.data <- append(x1.data, 2500)
df2 <- data.frame(x2,x2.data)

#####
library(arm)
library(Matching)
data(lalonde)
set.seed(123)
lalonde1 <- subset(lalonde, treat == 0)
lm1 <- lm(re78 ~ age + educ + re74 + re75 + I(educ*re74) + I(educ*re75) + I(age*re74) + I(age*re75) + I(re74*re75), data= lalonde1)

sim_results <- sim(lm1, 10000)
coef_results <- coef(sim_results, 10000)

educ.median <- median(lalonde1$educ)
re74.median <- median(lalonde1$re74)
re75.median <- median(lalonde1$re75)
storagedf_1 <- matrix(NA, nrow = 10000, ncol = 39)
for(age in c(17:55)) {
  for(i in 1:10000)
  {
    beta <- sum(coef_results[i,]*c(1, age, educ.median, re74.median, re75.median, educ.median*re74.median, educ.median*re75.median, age*re74.median, age*re75.median, re74.median*re75.median), rnorm(1,0,sim_results@sigma[i])  )
    storagedf_1[i, age - 16] <- beta
  }
}
conf.intervals1 <- apply(storagedf_1, 2, quantile, probs = c(0.025, 0.975))

educ.percentile <- quantile(lalonde1$educ, .90)
re74.percentile <- quantile(lalonde1$re74, .90)
re75.percentile <- quantile(lalonde1$re75, .90)
storagedf_2 <-matrix(NA, nrow = 10000, ncol = 39)
for(age in c(17:55)) {
  for(i in 1:10000)
  {
    beta1 <- sum(coef_results[i,]*c(1, age, educ.percentile, re74.percentile, re75.percentile, educ.percentile*re74.percentile, educ.percentile*re75.percentile, age*re74.percentile, age*re75.percentile, re74.percentile*re75.percentile), rnorm(1,0,sim_results@sigma[i])  )
    storagedf_2[i, age - 16] <- beta1
  }
}
conf.intervals2 <- apply(storagedf_2, 2, quantile, probs = c(0.025, 0.975))

#####
library(foreign)
nsw <- read.dta("nsw.dta")
set.seed(1314)
nsims <- 1000
storage_intercept <- matrix(NA, ncol = 1000, nrow = 100)
storage_treatment_effect <- matrix(NA, ncol = 1000, nrow = 100)
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
####
nt = 0
for (i in 1:length(nsw$treat)){
  if(nsw$treat[i] == 1){
    nt <- nt +1
  }
}
nsw1 <- as.data.frame(nsw$treat[1:nt])
nsw2 <- as.data.frame(nsw$treat[nt+1:length(nsw$treat)])
glinear <- glm(treat~age+education+black+hispanic+married+nodegree+re75, data=nsw, family="binomial")
probs.glinear <- predict(glinear, nsw, "response")
graphs <- cbind(nsw, probs.glinear)

####

nsims <-1000
storing_treatment_effect <- matrix(NA, nrow = nsims)
storing_statistically_significant_treatment_effect <- matrix(NA, nrow = nsims)
for(j in 1:nsims){
  number_of_predictors <- sample(1:66, 1)
  predictors <- sample(c("age", "educ", "black", "hisp", "married", "nodegr", "re74", "re75", "u74", "u75", "treat", "age*educ","age*black","age*hisp","age*married","age*nodegr","age*re74", "age*re75", "age*u74","age*u75", "age*treat",
                         "educ*black", "educ*hisp","educ*married","educ*nodegr", "educ*re74","educ*re75","educ*u74","educ*u75", "educ*treat",
                         "black*hisp","black*married", "black*nodegr","black*re74","black*re75","black*u74","black*u75","black*treat",
                         "hisp*married", "hisp*nodegr","hisp*re74","hisp*re75","hisp*u74","hisp*u75","hisp*treat",
                         "married*nodegr", "married*re74","married*re75","married*u74","married*u75", "married*treat", 
                         "nodegr*re74","nodegr*re75","nodegr*u74","nodegr*u75","nodegr*treat",
                         "re74*re75","re74*u74","re74*u75","re74*treat",
                         "re75*u74","re75*u75", "re75*treat",
                         "u74*u75","u74*treat",
                         "u75*treat"), number_of_predictors)
  vars <- paste(predictors, collapse = ' + ')
  equation <- paste0("re78 ~ ", vars)
  m2 <- lm(as.formula(equation), data=lalonde)
  treatment_effect <- mean(predict(m2)[1:185]) - mean(predict(m2)[186:445])
  storing_treatment_effect[j] <- treatment_effect
  
  x <- summary(m2)
  statistical <- pf(x$fstatistic[1], x$fstatistic[2], x$fstatistic[3],lower.tail = FALSE)
  if(statistical < 0.05){
    storing_statistically_significant_treatment_effect[j] <- treatment_effect
  }
}

function(input, output) {
  
  # Creating an input that let users control the variables
  # It is reactive so you need to call it on the plot of the graph
  # so it becomes reactive and any change on input will change
  # the graph V
  # dateRangeInput<-reactive({
  #   test <- subset(dataset, dataset$approval.date >= as.Date(input$daterange[1]) & dataset$approval.date <= as.Date(input$daterange[2]))
  # })
  
    output$barplot <- renderPlot({
      #dateRangeInput() <calling the reactive part
      # V output that let the graph interactive
      #output$barplot <- renderDataTable(dateRangeInput())
      
      # V plotted at: https://luccabertoncini.shinyapps.io/assignment2/
      #ggplot(df1, aes(x=x1, y=x1.data)) + geom_point()+ geom_smooth(method=lm, se=TRUE, level=0.95)+
      #  ggtitle("Regression Line without the outlier") +
      #  xlab("Predictor") +
      #  ylab("Dependent variable")
      
      # V plotted at: https://luccabertoncini.shinyapps.io/assignment2-2/
      #ggplot(df2, aes(x=x2, y=x2.data)) + geom_point()+ geom_smooth(method=lm, se=TRUE, level=0.95)+
      #  ggtitle("Regression Line with the outlier") +
      #  xlab("Predictor") +
      #  ylab("Dependent variable")
      
      # V plotted at: https://luccabertoncini.shinyapps.io/assignment2-3/
      #ggplot(df2, aes(x=x2, y=x2.data)) + geom_point() + geom_smooth(method=lm, se=TRUE, level=0.95) + 
      #  geom_abline(intercept = intercept_x1, slope = slope_x1, color="red", size=1.2)+ 
      #  ggtitle("Regression Line with(blue) and without(red) the outlier") + 
      #  xlab("Predictor") +
      #  ylab("Dependent variable")+
      #  labs(caption = "As we can see a single outlier can have a big effect on the regression line. 
      #       So the outlier can hide an almost linear relationship and produce a line that will not be representative of the real data. 
      #       Outliers should be checked for errors, and in case of great impact on the regression line it is reasonable to remove it and report it explaining your results")
      
      # V plotted at: https://luccabertoncini.shinyapps.io/assignment2-4/
      # plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(-15000,25000), main="Earnings per Age group (other variables hold at their median)",xlab="Ages", ylab="95% confidence interval for re78 (earnings in 1978)")
      # counter = 1
      # for(age in c(17:55)) {
      #   segments(x0 = age,  y0 = conf.intervals1[counter], x1 = age, y1 = conf.intervals1[counter + 1], col = c("red"))
      #   counter = counter + 2
      # }
      
      # V plotted at: https://luccabertoncini.shinyapps.io/assignment2-5/
      # plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(-15000,25000), main="Earnings per Age group (other variables hold at their .9 percentile)",xlab="Ages", ylab="95% confidence interval for re78 (earnings in 1978)")
      # counter = 1
      # for(age in c(17:55)) {
      #   segments(x0 = age,  y0 = conf.intervals2[counter], x1 = age, y1 = conf.intervals2[counter + 1], col = c("blue"))
      #   counter = counter + 2
      # }
      
      # V plotted at: https://luccabertoncini.shinyapps.io/assignment2-6/
      # plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(-15000,25000), main="Earnings per Age group (.9 percentile (blue) median (red))",xlab="Ages", ylab="95% confidence interval for re78 (earnings in 1978)")
      # counter = 1
      # for(age in c(17:55)) {
      #   segments(x0 = age,  y0 = conf.intervals1[counter], x1 = age, y1 = conf.intervals1[counter + 1], col = c("red"))
      #   counter = counter + 2
      # }
      # counter = 1
      # for(age in c(17:55)) {
      #   segments(x0 = age,  y0 = conf.intervals2[counter], x1 = age, y1 = conf.intervals2[counter + 1], col = c("blue"))
      #   counter = counter + 2
      # }
      
      # V plotted at: https://luccabertoncini.shinyapps.io/assignment2-7/
      # hist(storage_intercept, xlab = "Intercept value", main = "Histogram of intercept")
      
      # V plotted at: https://luccabertoncini.shinyapps.io/assignment2-8/
      # hist(storage_treatment_effect, xlab = "Treatment effect value", main = "Histogram of treatment effect")
      
      # V plotted at: https://luccabertoncini.shinyapps.io/assignment2-9/
      # ggplot(nsw1, aes(x=probs.glinear[1:nt])) +geom_histogram(color= "red", fill="red") +
      #   ggtitle("Histogram of probabilities of being assigned to the treatment group, for variables that were indeed in the treatment group")+
      #   xlab("Probabilities")+
      #   ylab("Count")
      
      # V plotted at: https://luccabertoncini.shinyapps.io/assignment2-10/
      # ggplot(nsw2, aes(x=probs.glinear[nt+1:length(nsw$treat)])) +geom_histogram(color= "blue", fill="blue")+
      #   ggtitle("Histogram of probabilities of being assigned to the treatment group, for variables that were in the control group")+
      #   xlab("Probabilities")+
      #   ylab("Count")
      
      # V plotted at: https://luccabertoncini.shinyapps.io/assignment2-11/
      # ggplot(graphs , aes(x=probs.glinear, fill=factor(treat), color=factor(treat))) + geom_histogram(position="identity")+ 
      #   ggtitle("Probability of being assigned to treatment group\n Treatment group in blue and control in red")+
      #   xlab("Probabilities")+
      #   ylab("Count")
      
      # V plotted at: https://luccabertoncini.shinyapps.io/assignment2-12/
      # ggplot(as.data.frame(storing_treatment_effect), aes(x=storing_treatment_effect)) +geom_histogram(color= "blue", fill="blue")+ 
      #   ggtitle("Treatment effect predicting re78 from lalonde (randomly selecting predictors)")+
      #   xlab("Treatment Effect")+
      #   ylab("Count")
      
      # V plotted at: https://luccabertoncini.shinyapps.io/assignment2-13/
      ggplot(as.data.frame(na.omit(storing_statistically_significant_treatment_effect)), aes(x=na.omit(storing_statistically_significant_treatment_effect))) +geom_histogram(color= "blue", fill="blue")+ 
        ggtitle("Statistically significants treatment effect predicting re78 from lalonde (randomly selecting predictors)")+
        xlab("Treatment Effect")+
        ylab("Count")
      
      
    }, height=700)
  
  
  
  
  
}