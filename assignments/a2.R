print("Josh Janzen")
print("SEIS-763-02")
print("Assign #2")

getwd()
setwd("/Users/a149174/UST_GPS/seis_763/r/seis_763_machine_learning/assignments")
#library(caret)

# 1 & 2: read in data
data <- read.csv("patients.csv", head=T, sep=',', skip = 0)
head(data)
summary(data)

# 3: Build Linear Model
#model <- train(Systolic ~ Age + Gender + Height + Location + SelfAssessedHealthStatus + Smoker + Weight,  data=data, method="lm")
model <- lm(Systolic ~ Age + Gender + Height + Location + SelfAssessedHealthStatus + Smoker + Weight, data=data)

# 4: Thetas
summary(model)
#summary(model2)
summary(model)$coefficients[,1:2]
#(Intercept)                           88.65811329
#Age                                    0.08025966
#`Gender'Male'`                        -1.47939073  3.26574545
#Height                                 0.46962059  0.25390819
#`Location'St. Mary's Medical Center'` -0.85650078  1.29798791
#`Location'VA Hospital'`               -1.73484051  1.13322534
#`SelfAssessedHealthStatus'Fair'`      -2.75096823  1.51063322
#`SelfAssessedHealthStatus'Good'`       0.58637873  1.17832929
#`SelfAssessedHealthStatus'Poor'`       0.45934283  1.67618555
#Smoker                                 9.67308711  1.04590413
#Weight                                -0.01341834  0.05837056

# 5: Interpret Thetas
## For continuous predictors, if all other variables are held constant, the larger absolute value of the theta will have a larger average impact to predicting Systolic.
## For continuous predictors, if all other variables are held constaint, 1 unit of a theta will result in that thetas value change in Systolic. 
## For categorical predictors, a change between in Y will be represent change from the reference group. For example, the reference group for Gender is "Female". 

# 6
# http://www.stat.columbia.edu/~martin/W2024/R7.pdf
# scatter plots
#scatterplot(Systolic ~ Age + Gender + Height + Location + SelfAssessedHealthStatus + Smoker + Weight,  data=data)
pairs(~ Systolic + Age + Gender + Height + Location + SelfAssessedHealthStatus + Smoker + Weight, data=data)
pairs(~ Systolic + Weight + Smoker, data=data)

# leverage (difference from the mean of variables)
lev <- hat(model.matrix(model))
lev
head(model.matrix(model))
plot(lev)
# sort with give values sorted with highest leverage 
sort(lev, decreasing=T)
# order will provide the index of the obs.  1: obs 75, 2: 93
order(lev, decreasing=T)

# cooks (measures the effector of deleting a given observation)
plot(cooks.distance(model))
sort(cooks.distance(model), decreasing=T)[1]
# point 93 with cook's distance of 0.135

# residual plots
#https://statweb.stanford.edu/~candes/acm118/Hw/hw4sol.pdf
#http://www.r-tutor.com/elementary-statistics/simple-linear-regression/normal-probability-plot-residualsqqno
model_res = resid(model)
head(model_res)
length(model_res)
dim(data)
# residual plot
plot(data$Systolic, model_res, ylab="Residuals", xlab="Fitted Values", main="Plot of Residuals vs Fitted Values", abline(0,0))

# residual histogram
hist(model_res)
# normal prob plot of residuals
model_res_stdev <- sd(model_res)
model_res_stdev
model_res_mean <- mean(model_res, trim = 1)

sort(model_res)
model_res_mean + 2 * model_res_stdev
model_res_mean - 2 * model_res_stdev

qqnorm(model_res, ylab="Sample Quartiles", xlab="Theoretical Quartiles",  main="Normal Q-Q Plot")
qqline(model_res)

# advanced regression diagnostics http://www.statmethods.net/stats/rdiagnostics.html
library(car) # companian applied regression package
# Assessing Outliers
outlierTest(model)
#qqplot(model)
leveragePlots(model)

# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(model)-length(model$coefficients)-2)) 
plot(model, which=4, cook.levels=cutoff)
# Influence Plot 
#influencePlot(model, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# Normality of Residuals
# qq plot for studentized resid
qqPlot(model, main="QQ Plot")

# Influential Observations
# added variable plots (if line is near horizontal, then the variable is insignificant) 
avPlots(model)
# weight has flattest line, therefore should be removed

# remove predictor
#Weight has heighest P-value, Gender, Location

# model remove outlier
model_no_outlier <- lm(Systolic ~ Age + Gender + Height + Location + SelfAssessedHealthStatus + Smoker + Weight, data=data[-93,])

# change in adj r-squared
summary(model_no_outlier)$adj.r.squared - summary(model)$adj.r.squared

# model remove outlier and weight
model_no_outlier_weight <- lm(Systolic ~ Age + Gender + Height + Location + SelfAssessedHealthStatus + Smoker, data=data[-93,])

# change in adj r-squared
summary(model_no_outlier_weight)$adj.r.squared - summary(model_no_outlier)$adj.r.squared





# good to know, but not used for assignment
# distribution of studentized residuals
library(MASS)
sresid <- studres(model) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit) 
lines(xfit, yfit)

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(model)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(model)

# Evaluate Collinearity
vif(model) # variance inflation factors 
sqrt(vif(model)) > 2 # problem?

# Test for Autocorrelated Errors
durbinWatsonTest(model)

# Global test of model assumptions
install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(model) 
summary(gvmodel)
