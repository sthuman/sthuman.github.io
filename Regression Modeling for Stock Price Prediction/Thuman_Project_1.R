#Final Project 1 

#import packages
install.packages("readxl")
install.packages("TSstudio")
install.packages("BSDA")
install.packages('distributions3')
install.packages('rcompanion')
install.packages('car')
install.packages('lmtest')
library(readxl)
library(TSstudio)
library(BSDA)
library(distributions3)
library(rcompanion)
library(car)
library(lmtest)

#import data
setwd("C:/Users/sarah/OneDrive/Desktop/Grad School/MA-541 (Statistical Methods)/Project_1/")
stock_data <- read_xlsx("MA 541 Course Project Data.xlsx")

attach(stock_data)

#Part 1: Meet the data.

#Sample means
ETF_mean <- mean(ETF)
oil_mean <- mean(oil)
gold_mean <- mean(gold)
JPM_mean <- mean(JPM)

#Sample standard deviations
ETF_std <- sd(ETF)
oil_std <- sd(oil)
gold_std <- sd(gold)
JPM_std <- sd(JPM)

#Sample correlation
corr_ETF_oil <- cor(ETF, oil)
corr_ETF_gold <- cor(ETF, gold)
corr_ETF_JPM <- cor(ETF, JPM)
corr_oil_gold <- cor(oil, gold)
corr_oil_JPM <- cor(oil, JPM)
corr_gold_JPM <- cor(gold, JPM)

#Part 2: Describe the data.

#Histograms
hist(ETF, breaks=50, col="blue")
hist(oil, breaks=50, col="grey", main = "Histogram of Crude Oil")
hist(gold, breaks=50, col="orange", main = "Histogram of Gold")
hist(JPM, breaks=50, col="green")

#Time Series Plots Individual Columns
ts.plot(ETF, col="blue", main = "ETF Time Series Plot")
ts.plot(oil, col="gray", main = "Oil Time Series Plot")
ts.plot(gold, col="orange", main = "Gold Time Series Plot")
ts.plot(JPM, col="green", main = "JPM Time Series Plot")

#Time Series Plot for all
plot(ETF, type="l",col="blue",xlab="Time", ylab="Daily Relative Change", main = "Time Series of all Variables")
lines(oil, type="l", col="gray")
lines(gold, type="l", col="orange")
lines(JPM, type="l", col="green")
legend("bottomleft", c("ETF", "oil","gold","JPM"),lty=1,col=c("blue","gray","orange","green"))

#Scatter Plots
#ETF vs Oil
plot(ETF, oil, main="Daily Relative Change ETF vs Oil", xlab="ETF", ylab="Oil", pch=20)
#ETF vs Gold
plot(ETF, gold, main="Daily Relative Change ETF vs Gold", xlab="ETF", ylab="Gold", pch=20)
#ETF vs JPM
plot(ETF, JPM, main="Daily Relative Change ETF vs JPM", xlab="ETF", ylab="JPM", pch=20)

#Part 3: What distribution does data follow?

# Hypothesis/assumption: based on plots (mainly the histograms) from step 2, I think that
# ETF, oil, gold, and JPM follow Normal distributions centered 
# at 0 and with variances ETF = 0.02, oil = 0.08, gold = 0.04, 
# and JPM = 0.04

#seed to make test reproducible
set.seed(100)

#Shapiro-Wilk normality test
ETF_ntest <- shapiro.test(ETF)
oil_ntest <- shapiro.test(oil)
gold_ntest <- shapiro.test(gold)
JPM_ntest <- shapiro.test(JPM)

print(JPM_ntest)
#result none of the variables are normally distributed

#Q-Q Plot 
qqnorm(ETF, pch = 1, frame = FALSE, main = "Normal Q-Q Plot: ETF")
qqline(ETF, col = "blue", lwd = 2)

qqnorm(oil, pch = 1, frame = FALSE, main = "Normal Q-Q Plot: Oil")
qqline(oil, col = "blue", lwd = 2)

qqnorm(gold, pch = 1, frame = FALSE, main = "Normal Q-Q Plot: Gold")
qqline(gold, col = "blue", lwd = 2)

qqnorm(JPM, pch = 1, frame = FALSE, main = "Normal Q-Q Plot: JPM")
qqline(JPM, col = "blue", lwd = 2)

#Part 4: Break Data into Small groups and use to show importance of CLT

#1 Using ETF as the population calculate mean and standard deviation
cat("Population mean is", ETF_mean)
cat('Population standard deviation is', ETF_std)

#2 Split population into 50 groups of 20 values
sample_pop50 <- split(ETF, seq(1,1000,by=20))

#3 Sample mean by group
for(x_bar50 in sample_pop50) {
  mean(x_bar50);
  hist(x_bar50, main = "Sample Histograms 50 Groups")
}
    #Based on the histograms, the distributions seem to all be either 
    #slightly right or left skewed with the occasional normal
#4
#Mean of sample means
mean_xbar50 <- mean(x_bar50)
std_xbar50 <- sd(x_bar50)

#Compare x_bar and mean_xbar
mux_muxbr50 <- abs(ETF_mean-mean_xbar50) #mean pop - mean xbar
stdbyn50 <- ETF_std/(sqrt(length(x_bar50))) #std pop/ squareroot number sample means

stdx_stdxbr50 <- abs(stdbyn50-std_xbar50)

#5: Are result from 3 and 4 consistent with CLT? Why?
#Yes these results are consistent with the CLT because even with only 
#20 observation per group the histograms are trending toward Gaussian.

#6/7 Break population into 10 groups and repeat steps 3-5
sample_pop10 <- split(ETF, seq(1,1000,by=100))

for(x_bar10 in sample_pop10) {
  mean(x_bar10);
  hist(x_bar10, main = "Sample Histograms 10 Groups")
}

mean_xbar10 <- mean(x_bar10)
std_xbar10 <- sd(x_bar10)

mux_muxbr10 <- abs(ETF_mean-mean_xbar10) #mean pop - mean xbar
stdbyn10 <- ETF_std/(sqrt(length(x_bar10))) #std pop/ squareroot number sample means

stdx_stdxbr10 <- abs(stdbyn10-std_xbar10)

#Yes these results are consistent with the CLT because for the most part 
#the histograms are trending toward Gaussian.

#8/9 50 groups simple random sample with replacement. Repeat steps 3-5
sample_pop50r <- split(ETF,sample(ETF, 50, replace = TRUE))

for(x_bar50r in sample_pop50r) {
  mean(x_bar50r);
  hist(x_bar50r, main = "Sample Histograms 50 Random Groups")
}

mean_xbar50r <- mean(x_bar50r)
std_xbar50r <- sd(x_bar50r)

mux_muxbr50r <- abs(ETF_mean-mean_xbar50r) #mean pop - mean xbar
stdbyn50r <- ETF_std/(sqrt(length(x_bar50r))) #std pop/ squareroot number sample means

stdx_stdxbr50r <- abs(stdbyn50r-std_xbar50r)
#Yes these results are consistent with the CLT because even with only 
#20 observation per group the histograms appear to trend towards Gaussian.

#10/11 10 groups simple random sample with replacement. Repeat steps 3-5
sample_pop10r <- split(ETF,sample(ETF, 10, replace = TRUE))

for(x_bar10r in sample_pop10r) {
  mean(x_bar10r);
  hist(x_bar10r, main = "Sample Histograms 10 Random Groups")
}

mean_xbar10r <- mean(x_bar10r)
std_xbar10r <- sd(x_bar10r)

mux_muxbr10r <- abs(ETF_mean-mean_xbar10r) #mean pop - mean xbar
stdbyn10r <- ETF_std/(sqrt(length(x_bar10r))) #std pop/ squareroot number sample means

stdx_stdxbr10r <- abs(stdbyn10r-std_xbar10r)

#Yes these results are consistent with the CLT because the histograms 
#appear to trend towards Gaussian.

#12 Is this information consistent with distribution in part 3?

#Yes this information is consistent with the distribution found in part 3.
#The CLT isn't saying anything about the distribution of the population
#just that as you sample more and more, the distributions will look
#like the normal distribution, which is being shown in part 4.

#Part 5: Construct a confidence interval with your data

#Confidence interval level and critical value
level = .95
alpha = 1-level
#Select random element from step 10 of Part 4
sample10 <- sample_pop10r[[sample(1:length(sample_pop10r),1)]] #maybe choose a specific element so it doesn't change all the time

#variables needed for confidence interval calculation
mu10 <- mean(sample10)
n10 <- 100
sd10 <- sd(sample10)

#95% Confidence interval
margin10 <- qt(level, df=n10-1)*sd10/sqrt(n10)

upperinterval10 <- mu10 + margin10
lowerinterval10 <- mu10 - margin10
cat("95% Confidence Interval is", "(", lowerinterval10, ",", upperinterval10, ")")
print(upperinterval10-ETF_mean)
print(ETF_mean)
#Select random element from step 10 of Part 4
sample50 <- sample_pop50r[[sample(1:length(sample_pop50r),1)]] #maybe choose a specific element so it doesn't change all the time

#variables needed for confidence interval calculation
mu50 <- mean(sample50)
n50 <- 20
sd50 <- sd(sample50)

#95% Confidence interval
margin50 <- qt(level, df=n10-1)*sd50/sqrt(n50)

upperinterval50 <- mu50 + margin50
lowerinterval50 <- mu50 - margin50
cat("95% Confidence Interval is", "(", lowerinterval50, ",", upperinterval50, ")")
print(upperinterval50-ETF_mean)

#Both confidence intervals captures the population mean
#Part 4: Step 8 captures the population mean better since it is closer to 
#the center of the interval than it is for the Part 4: Step 10 interval.
#This makes sense because Step 8 had more elements per group, which 
#means it will have a better chance of capturing the population mean. 
#One thing to note is that just because this randomly selected sample
#captured the population mean doesn't mean that they all would. In a 
#previous run through of my code neither interval captured the population
#mean, however the Part 4: Step 8 interval was closer to capturing it and
#still more accurate.

#Part 6: Form a hypothesis and test with data

#Split data into 80/20 for Steps 1 and 2
split10 <- length(sample10)*0.80
split50 <- length(sample50)*0.80
train10 <- sample10[1:split10]
train50 <- sample50[1:split50]

#Calculate sample mean and variance
sample10_mu <- mean(train10)
sample10_sd <- sd(train10)

sample50_mu <- mean(train50)
sample50_sd <- sd(train50)

#Null Hypothesis: mu = sample_mu vs Alt. Hypothesis mu =/ sample_mu
#Two-Tail z-test

z.test(sample10,y=NULL,  alternative = 'two.sided', mu = sample10_mu, sigma.x = sample10_sd, sigma.y = NULL, conf.level = 0.95)
shapiro.test(sample10) #normality check
#Fail to reject Null Hypothesis

z.test(sample50, y=NULL, alternative = 'two.sided', mu = sample50_mu, sigma.x = sample50_sd, sigma.y = NULL, conf.level = 0.95)
shapiro.test(sample50)
#Fail to reject Null Hypothesis

#F-test for Null: sigma = sample_sd vs Alt: sigma =/ sample_sd
var.test(ETF, sample50, alternative = 'two.sided')

#Since p-value > 0.05, fail to reject the null hypothesis.

#F-test for Null: sigma = sample_sd vs Alt: sigma < sample_sd
var.test(ETF, sample50, alternative = 'less')

#Since p-value > 0.05, fail to reject the null hypothesis.

##Part 7: Compare Data with a different data set

#Entire Gold column considered to be a RV from first population
#Entire Oil column considered to be a RV from the second population

#Null Hypothesis: mean gold - mean oil = 0; 
#Alternative hypothesis: mean gold - mean oil =/ 0
#Significance level = 0.05
z.test(sample(gold,100),sample(oil,100),alternative = 'two.sided', mu=0, sigma.x = gold_std, sigma.y = oil_std,conf.level = 0.95)
#Fail to reject Null Hypothesis since p-value = 0.4845 > significance level = 0.05

#Alt code for z-test (not needed)
#z_stat <- ((gold_mean - oil_mean)-0) / (sqrt((gold_std^2/1000)+(oil_std^2/1000)))
#Z <- Normal(0,1)
#1-cdf(Z,abs(z_stat))+cdf(Z,-abs(z_stat))

#Null Hypothesis: mean diff = 0; 
#Alternative hypothesis: mean diff =/ 0
#Significance level = 0.05
diff <- mapply('-',gold,oil)
sample_diff <- sample(diff, 100, replace = TRUE)

z.test(sample_diff, y=NULL, alternative = 'two.sided', mu = 0, sigma.x = sd(sample_diff), sigma.y = NULL, conf.level = 0.95)
#Fail to reject Null Hypothesis since p-value > 0.05

#Null Hypothesis: sigma gold - sigma oil = 0; 
#Alternative hypothesis: sigma gold - sigma oil =/ 0
#Critical F-value = 1.1097 for df1 = 999 and df2 = 999

var.test(sample(gold,100),sample(oil,100), alternative = 'two.sided', conf.level = 0.95)
#Reject the Null Hypothesis since p-value < 0.05

#Part 8: Fitting the line to the data
#using only EFT and Gold data

plot(gold, ETF, main="Scatter Plot Gold vs. ETF", xlab="Gold Daily Relative Change", ylab="ETF Daily Relative Change")
#There is no linear relationship between ETF and Gold that can be 
#observed from the scatter plot.

print(cor(gold,ETF))
#Based on the correlation coefficient between ETF and gold, which is 0.09,
#there is no correlation between ETF and gold.

#Split data
sample <- sample(c(TRUE, FALSE), nrow(stock_data), replace = T, prob = c(0.8,0.2))
train <- stock_data[sample, ]
test <- stock_data[!sample, ]

xtrain <- as.matrix(train)[,4]
ytrain <- train[,2] 

xtest <- as.matrix(test)[,4]
ytest <- test[,2]

#Linear Regression Model
lin_regress <- lm(train$ETF~train$gold)
lin_sum <- summary(lin_regress)
print(lin_sum)
plot(train$gold, train$ETF, xlab = 'Gold', ylab= "ETF", 
     main = "Scatter plot with Linear Regression Line", col='blue')
abline(lin_regress, lwd =2)
#The intercept is 0.00055 and the slope is 0.040. We interpret them to be
#where the intercept intersects the y-axis (so when gold is 0, ETF 
#is 0.00055) and the slope tells what the relationship between the two
#variables is. In this case, there is a slight positive correlation
#between ETF and gold but because the line is almost horizontal, there
#isn't a significant level of correlation between the two.

#Two-tailed t-test where Null Hypothesis: beta1 = 0
#Based on the summary table from the linear regression model, the 
#p-value for the slope variable is 0.0542 which is greater than 0.01. 
#This tells us that we fail to reject the Null Hypothesis that the 
#beta1 = 0.

#The p-value is 0.054. The linear relationship between ETF and gold
#is NOT significant since the p-value > 0.01 indicating that there is
#not significant evidence that we should reject the null hypothesis

#Based on the coefficient of determination from the regression model
#(multiple R^2 and Adjusted R^2), this is not a good model since both
#versions of the coefficient of determination are almost 0. This makes
#sense, since looking at the scatter plot there is no indication of 
#linearity. The points look like they are clustered around a center
#point instead.

#Some of the assumptions that I made when performing the regression
#analysis are that there is a linear relationship between the two 
#variables, there are no outliers, and the variables are normally 
#distributed.

#mu gold = 0.005127 at 99% confidence interval and 99% prediction 
#interval of daily ETF return

yhat <- 0.0005465 + 0.0399699*0.005127
t005 <- 2.582091
se <- 0.0002416
ci_up <- yhat + t005*se
ci_low <- yhat - t005*se
cat('The confidence interval is', ci_low,', ',ci_up)
# The 99% confidence interval is (0.00013,  0.00138).

tpred <- 0.01253745
n <- length(train$ETF)
p_up <- yhat + tpred*(se/sqrt(n))
p_low <- yhat - tpred*(se/sqrt(n))
cat('The 99% prediction interval is (',p_low,',',p_up,')')
# The 99% prediction interval is (0.0007513178 , 0.0007515335)

#Part 9: Does model predict?

#Split data
xtrain <- as.matrix(train)[,3:4]
ytrain <- train[,2] 

xtest <- as.matrix(test)[,3:4]
ytest <- test[,2]

#gold and oil are explanatory variables; ETF is response variable
multi_linregres <- lm(ETF ~ gold + oil, data = train)
summary(multi_linregres)

#Since adjusted R^2 is 0.0162, the model's predictive power is very low

#Part 10: Check residuals and model selection

#prediction model
y_pred <- predict(multi_linregres,newdata = test)
y <- unlist(ytest,use.names = FALSE)
res <- y-y_pred

#residuals plot
plot(y_pred,res, xlab="Predicted ETF", ylab="Residuals", main="ETF Predicted vs Residuals")

#Error term assumptions check
#normality check
plotNormalHistogram( res, prob = FALSE,
                     main = "Normal Distribution overlay on Residuals Histogram",
                     length = length(res) )
shapiro.test(res)
qqnorm(res, pch = 1, frame = FALSE, main = "Normal Q-Q Plot: Residuals")
qqline(res, col = "blue", lwd = 2)

#Check Linearity
plot(xtest[,1],y, xlab = "Oil", ylab = 'ETF', main = 'Oil vs ETF Daily Change')
plot(xtest[,2],y, xlab = 'Gold', ylab = 'ETF', main = 'Gold vs ETF Daily Change')

#Check mean = 0
z.test(res,alternative = 'two.sided', mu = 0, sigma.x = sd(res),conf.level = 0.95)
#Fail to rject null hypothesis mu = 0

#Check Homoscedasticity
plot(y_pred,res, xlab="Predicted ETF", ylab="Residuals", main="ETF Predicted vs Residuals")
bptest(multi_linregres)
#fail to reject null hypothesis that homoscedasticity is present

#Check Independence
durbinWatsonTest(multi_linregres)
#result is 2.05 which is approx 2 meaning there is no autocorrelation
#so yes independent

#Some ways to imporve the quality of my regression model would be to
#implement Lasso, Ridge, and Stepwise regression to determine 1) which
#fits best and 2) if any variables could be removed resulting in a
#better model. After that, if there is a better model available,
#I would then implement it.