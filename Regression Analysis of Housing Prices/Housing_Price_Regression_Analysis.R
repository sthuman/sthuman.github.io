#MA-541 Statistical Methods: Project 2 Alternative

#Packages and libraries
install.packages("TSstudio")
install.packages("BSDA")
install.packages('distributions3')
install.packages('fastDummies')
install.packages("ggplot2")
install.packages('glmnet')
install.packages('npreg')
install.packages('rcompanion')
install.packages('lmtest')
install.packages('car')
install.packages('ggcorrplot')
install.packages('FactoMineR')
install.packages('factoextra')
install.packages("tidyverse")
install.packages('caret')
library(TSstudio)
library(BSDA)
library(distributions3)
library(fastDummies)
library(ggplot2)
library(glmnet)
library(npreg)
library(rcompanion)
library(lmtest)
library(car)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(tidyverse)
library(caret)

#Import Data
setwd("C:/Users/sarah/OneDrive/Desktop/Grad School/MA-541 (Statistical Methods)/Project_2/data_housing/")
housing_data <- read.csv("Housing.csv")
housing <- data.frame(housing_data)

#Get a look at data
head(housing,5)

#Create Dummy Variables
housings <- dummy_cols(housing)
housings <- housings[-c(7:14,16,18,20,22,24,26:28)]
head(housings)

#add unit price column
unit_price <- housings[ ,1]/housings[ ,2]
housing_dm <- cbind(housings, unit_price)
head(housing_dm)

#Histograms
hist_labels <- c("Price","Area","Bedrooms","Bathrooms","Stories","Parking",
                 "Main Road","Guestroom","Basement","Hot Water Heating",
                 "AC","Preferred Area", "Unit Price")

for(i in 1:length(housing_dm)) {
  hist(housing_dm[ ,i],50, main = hist_labels[i], xlab = hist_labels[i])
}

#Means
price_mean <- mean(housing_dm[ ,1])
area_mean <- mean(housing_dm[,2])
bed_mean <- mean(housing_dm[,3])
bath_mean <- mean(housing_dm[,4])
stories_mean <- mean(housing_dm[,5])
parking_mean <- mean(housing_dm[,6])
mainroad_mean <- mean(housing_dm[,7])
guestroom_mean <- mean(housing_dm[,8])
basement_mean <- mean(housing_dm[,9])
heating_mean <- mean(housing_dm[,10])
AC_mean <- mean(housing_dm[,11])
prefarea_mean <- mean(housing_dm[,12])
unitprice_mean <- mean(housing_dm[,13])

#standard Deviations
price_sd <- sd(housing_dm[,1])
area_sd <- sd(housing_dm[,2])
bed_sd <- sd(housing_dm[,3])
bath_sd <- sd(housing_dm[,4])
stories_sd <- sd(housing_dm[,5])
parking_sd <- sd(housing_dm[,6])
mainroad_sd <- sd(housing_dm[,7])
guestroom_sd <- sd(housing_dm[,8])
basement_sd <- sd(housing_dm[,9])
heating_sd <- sd(housing_dm[,10])
AC_sd <- sd(housing_dm[,11])
prefarea_sd <- sd(housing_dm[,12])
unitprice_sd <- sd(housing_dm[,13])

#Correlation
cor(housing_dm[,1],housing_dm[,2])
cor(housing_dm[,1],housing_dm[,3])
cor(housing_dm[,1],housing_dm[,4])
cor(housing_dm[,1],housing_dm[,5])
cor(housing_dm[,1],housing_dm[,6])
cor(housing_dm[,1],housing_dm[,7])
cor(housing_dm[,1],housing_dm[,8])
cor(housing_dm[,1],housing_dm[,9])
cor(housing_dm[,1],housing_dm[,10])
cor(housing_dm[,1],housing_dm[,11])
cor(housing_dm[,1],housing_dm[,12])
cor(housing_dm[,1],housing_dm[,13])
cor(housing_dm[,2],housing_dm[,3])
cor(housing_dm[,2],housing_dm[,4])
cor(housing_dm[,2],housing_dm[,5])
cor(housing_dm[,2],housing_dm[,6])
cor(housing_dm[,2],housing_dm[,7])
cor(housing_dm[,2],housing_dm[,8])
cor(housing_dm[,2],housing_dm[,8])
cor(housing_dm[,2],housing_dm[,10])
cor(housing_dm[,2],housing_dm[,11])
cor(housing_dm[,2],housing_dm[,12])
cor(housing_dm[,2],housing_dm[,13])
cor(housing_dm[,3],housing_dm[,4])
cor(housing_dm[,3],housing_dm[,5])
cor(housing_dm[,3],housing_dm[,6])
cor(housing_dm[,3],housing_dm[,7])
cor(housing_dm[,3],housing_dm[,8])
cor(housing_dm[,3],housing_dm[,9])
cor(housing_dm[,3],housing_dm[,10])
cor(housing_dm[,3],housing_dm[,11])
cor(housing_dm[,3],housing_dm[,12])
cor(housing_dm[,3],housing_dm[,13])
cor(housing_dm[,4],housing_dm[,5])
cor(housing_dm[,4],housing_dm[,6])
cor(housing_dm[,4],housing_dm[,7])
cor(housing_dm[,4],housing_dm[,8])
cor(housing_dm[,4],housing_dm[,9])
cor(housing_dm[,4],housing_dm[,10])
cor(housing_dm[,4],housing_dm[,11])
cor(housing_dm[,4],housing_dm[,12])
cor(housing_dm[,4],housing_dm[,13])
cor(housing_dm[,5],housing_dm[,6])
cor(housing_dm[,5],housing_dm[,7])
cor(housing_dm[,5],housing_dm[,8])
cor(housing_dm[,5],housing_dm[,9])
cor(housing_dm[,5],housing_dm[,10])
cor(housing_dm[,5],housing_dm[,11])
cor(housing_dm[,5],housing_dm[,12])
cor(housing_dm[,5],housing_dm[,13])
cor(housing_dm[,6],housing_dm[,7])
cor(housing_dm[,6],housing_dm[,8])
cor(housing_dm[,6],housing_dm[,9])
cor(housing_dm[,6],housing_dm[,10])
cor(housing_dm[,6],housing_dm[,11])
cor(housing_dm[,6],housing_dm[,12])
cor(housing_dm[,6],housing_dm[,13])
cor(housing_dm[,7],housing_dm[,8])
cor(housing_dm[,7],housing_dm[,9])
cor(housing_dm[,7],housing_dm[,10])
cor(housing_dm[,7],housing_dm[,11])
cor(housing_dm[,7],housing_dm[,12])
cor(housing_dm[,7],housing_dm[,13])
cor(housing_dm[,8],housing_dm[,9])
cor(housing_dm[,8],housing_dm[,10])
cor(housing_dm[,8],housing_dm[,11])
cor(housing_dm[,8],housing_dm[,12])
cor(housing_dm[,8],housing_dm[,13])
cor(housing_dm[,9],housing_dm[,10])
cor(housing_dm[,9],housing_dm[,11])
cor(housing_dm[,9],housing_dm[,12])
cor(housing_dm[,9],housing_dm[,13])
cor(housing_dm[,10],housing_dm[,11])
cor(housing_dm[,10],housing_dm[,12])
cor(housing_dm[,10],housing_dm[,13])
cor(housing_dm[,11],housing_dm[,12])
cor(housing_dm[,11],housing_dm[,13])
cor(housing_dm[,12],housing_dm[,13])

#Scatterplots
plot(housing_dm[,1],housing_dm[,2],xlab = "Price",ylab = "Area", main = "Price vs Area" )
plot(housing_dm[,1],housing_dm[,13],xlab = "Price",ylab = "Unit Price", main = "Price vs Unit Price" )
plot(housing_dm[,2],housing_dm[,13],xlab = "Area",ylab = "Unit Price", main = "Area vs Unit Price" )
# plot(housing_dm[,1], housing_dm[,3],xlab = "Price",ylab = "Bedrooms", main = "Price vs Bedrooms")
# plot(housing_dm[,1],housing_dm[,4],xlab = "Price",ylab = "Bathrooms", main = "Price vs Bathrooms")
# plot(housing_dm[,1],housing_dm[,5],xlab = "Price",ylab = "Stories", main = "Price vs Stories")
# plot(housing_dm[,1],housing_dm[,6],xlab = "Price",ylab = "Parking", main = "Price vs Parking")
# plot(housing_dm[,1],housing_dm[,7],xlab = "Price",ylab = "Mainroad", main = "Price vs Mainroad")
# plot(housing_dm[,1],housing_dm[,8],xlab = "Price",ylab = "Guestroom", main = "Price vs Guestroom")
# plot(housing_dm[,1],housing_dm[,9],xlab = "Price",ylab = "Basement", main = "Price vs Basement")
# plot(housing_dm[,1],housing_dm[,10],xlab = "Price",ylab = "Hot Water", main = "Price vs Hot Water")
# plot(housing_dm[,1],housing_dm[,11],xlab = "Price",ylab = "AC", main = "Price vs AC")
# plot(housing_dm[,1],housing_dm[,12],xlab = "Price",ylab = "Preferred Area", main = "Price vs Preferred Area")
# plot(housing_dm[,2],housing_dm[,3],xlab = "Area",ylab = "Bedrooms", main = "Area vs Bedrooms")
# plot(housing_dm[,2],housing_dm[,4],xlab = "Area",ylab = "Bathrooms", main = "Area vs Bathrooms")
# plot(housing_dm[,2],housing_dm[,5],xlab = "Area",ylab = "Stories", main = "Area vs Stories")
# plot(housing_dm[,2],housing_dm[,6],xlab = "Area",ylab = "Parking", main = "Area vs Parking")
# plot(housing_dm[,2],housing_dm[,7],xlab = "Area",ylab = "Mainroad", main = "Area vs Mainroad")
# plot(housing_dm[,2],housing_dm[,8],xlab = "Area",ylab = "Guestroom", main = "Area vs Guestroom")
# plot(housing_dm[,2],housing_dm[,9],xlab = "Area",ylab = "Basement", main = "Area vs Basement")
# plot(housing_dm[,2],housing_dm[,10],xlab = "Area",ylab = "Hot Water", main = "Area vs Hot Water")
# plot(housing_dm[,2],housing_dm[,11],xlab = "Area",ylab = "AC", main = "Area vs AC")
# plot(housing_dm[,2],housing_dm[,12],xlab = "Area",ylab = "Preferred Area", main = "Areas vs Preferred Area")
# plot(housing_dm[,3],housing_dm[,4],xlab = "Bedrooms",ylab = "Bathrooms", main = "Bedrooms vs Bathrooms")
# plot(housing_dm[,3],housing_dm[,5],xlab = "Bedrooms",ylab = "Stories", main = "Bedrooms vs Stories")
# plot(housing_dm[,3],housing_dm[,6],xlab = "Bedrooms",ylab = "Parking", main = "Bedrooms vs Parking")
# plot(housing_dm[,3],housing_dm[,7],xlab = "Bedrooms",ylab = "Mainroad", main = "Bedrooms vs Mainroad")
# plot(housing_dm[,3],housing_dm[,8],xlab = "Bedrooms",ylab = "Guestroom", main = "Bedrooms vs Guestroom")
# plot(housing_dm[,3],housing_dm[,9],xlab = "Bedrooms",ylab = "Basement", main = "Bedrooms vs Basement")
# plot(housing_dm[,3],housing_dm[,10],xlab = "Bedrooms",ylab = "Hot Water", main = "Bedrooms vs Hot Water")
# plot(housing_dm[,3],housing_dm[,11],xlab = "Bedrooms",ylab = "AC", main = "Bedrooms vs AC")
# plot(housing_dm[,3],housing_dm[,12],xlab = "Bedrooms",ylab = "Preferred Area", main = "Bedrooms vs Preferred Area")
# plot(housing_dm[,4],housing_dm[,5],xlab = "Bathrooms",ylab = "Stories", main = "Bathrooms vs Stories")
# plot(housing_dm[,4],housing_dm[,6],xlab = "Bathrooms",ylab = "Parking", main = "Bathrooms vs Parking")
# plot(housing_dm[,4],housing_dm[,7],xlab = "Bathrooms",ylab = "Mainroad", main = "Bathrooms vs Mainroad")
# plot(housing_dm[,4],housing_dm[,8],xlab = "Bathrooms",ylab = "Guestroom", main = "Bathrooms vs Guestroom")
# plot(housing_dm[,4],housing_dm[,9],xlab = "Bathrooms",ylab = "Basement", main = "Bathrooms vs Basement")
# plot(housing_dm[,4],housing_dm[,10],xlab = "Bathrooms",ylab = "Hot Water", main = "Bathrooms vs Hot Water")
# plot(housing_dm[,4],housing_dm[,11],xlab = "Bathrooms",ylab = "AC", main = "Bathrooms vs AC")
# plot(housing_dm[,4],housing_dm[,12],xlab = "Bathrooms",ylab = "Preferred Area", main = "Bathrooms vs Preferred Area")
# plot(housing_dm[,5],housing_dm[,6],xlab = "Stories",ylab = "Parking", main = "Stories vs Parking")
# plot(housing_dm[,5],housing_dm[,7],xlab = "Stories",ylab = "Mainroad", main = "Stories vs Mainroad")
# plot(housing_dm[,5],housing_dm[,8],xlab = "Stories",ylab = "Guestroom", main = "Stories vs Guestroom")
# plot(housing_dm[,5],housing_dm[,9],xlab = "Stories",ylab = "Basement", main = "Stories vs Basement")
# plot(housing_dm[,5],housing_dm[,10],xlab = "Stories",ylab = "Hot Water", main = "Stories vs Hot Water")
# plot(housing_dm[,5],housing_dm[,11],xlab = "Stories",ylab = "AC", main = "Stories vs AC")
# plot(housing_dm[,5],housing_dm[,12],xlab = "Stories",ylab = "Preferred Area", main = "Stories vs Preferred Area")
# plot(housing_dm[,6],housing_dm[,7],xlab = "Parking",ylab = "Mainroad", main = "Parking vs Mainroad" )
# plot(housing_dm[,6],housing_dm[,8],xlab = "Parking",ylab = "Guestrooom", main = "Parking vs Guestroom")
# plot(housing_dm[,6],housing_dm[,9],xlab = "Parking",ylab = "Basement", main = "Parking vs Basement")
# plot(housing_dm[,6],housing_dm[,10],xlab = "Parking",ylab = "Hot Water", main = "Parking vs Hot Water")
# plot(housing_dm[,6],housing_dm[,11],xlab = "Parking",ylab = "AC", main = "Parking vs AC")
# plot(housing_dm[,6],housing_dm[,12],xlab = "Parking",ylab = "Preferred Area", main = "Parking vs Preferred Area")
# plot(housing_dm[,7],housing_dm[,8],xlab = "Mainroad",ylab = "Guestroom", main = "Mainroad vs Guestroom")
# plot(housing_dm[,7],housing_dm[,9],xlab = "Mainroad",ylab = "Basement", main = "Mainroad vs Basement")
# plot(housing_dm[,7],housing_dm[,10],xlab = "Mainroad",ylab = "Hot Water", main = "Mainroad vs Hot Water")
# plot(housing_dm[,7],housing_dm[,11],xlab = "Mainroad",ylab = "AC", main = "Mainroad vs AC")
# plot(housing_dm[,7],housing_dm[,12],xlab = "Mainroad",ylab = "Preferred Area", main = "Mainroad vs Preferred Area")
# plot(housing_dm[,8],housing_dm[,9],xlab = "Guestroom",ylab = "Basement", main = "Guestroom vs Basement")
# plot(housing_dm[,8],housing_dm[,10],xlab = "Guestroom",ylab = "Hot Water", main = "Guestroom vs Hot Water")
# plot(housing_dm[,8],housing_dm[,11],xlab = "Guestroom",ylab = "AC", main = "Guestroom vs AC")
# plot(housing_dm[,8],housing_dm[,12],xlab = "Guestroom",ylab = "Preferred Area", main = "Guestroom vs Preferred Area")
# plot(housing_dm[,9],housing_dm[,10],xlab = "Basement",ylab = "Hot Water", main = "Basement vs Hot Water")
# plot(housing_dm[,9],housing_dm[,11],xlab = "Basement",ylab = "AC", main = "Basemenet vs AC")
# plot(housing_dm[,9],housing_dm[,12],xlab = "Basement",ylab = "Preferred Area", main = "Basement vs Preferred Area")
# plot(housing_dm[,10],housing_dm[,11],xlab = "Hot Water",ylab = "AC", main = "Hot Water vs AC")
# plot(housing_dm[,10],housing_dm[,12],xlab = "Hot Water",ylab = "Preferred Area", main = "Hot Water vs Preferred Area")
# plot(housing_dm[,11],housing_dm[,12],xlab = "AC",ylab = "Preferred Area", main = "AC vs Preferred Area")

#Normaility Checks
print("Normality check price:")
shapiro.test(housing_dm[,1])
print("Normality check area:") 
shapiro.test(housing_dm[,2])
print("Normality check bedrooms:")
shapiro.test(housing_dm[,3])
print("Normality check bathrooms:")
shapiro.test(housing_dm[,4])
print("Normality check stories:")
shapiro.test(housing_dm[,5])
print("Normality check parking:")
shapiro.test(housing_dm[,6])
print("Normality check mainroad:")
shapiro.test(housing_dm[,7])
print("Normality check guestroom:")
shapiro.test(housing_dm[,8])
print("Normality check basement:")
shapiro.test(housing_dm[,9])
print("Normality check hot water heating:")
shapiro.test(housing_dm[,10])
print("Normality check AC:")
shapiro.test(housing_dm[,11])
print("Normality check preferred area:")
shapiro.test(housing_dm[,12])
print("Normality check Unit Price:")
shapiro.test(housing_dm[,13])

#Conclusion: none of the variables are normally distributed.
#So need to normalize data

#Data Normalization
scale_housing <- scale(housing_dm)
scale_housing <- scale_housing[,-13]
log <- c(log(housing_dm[c(1:5,13)]))
log_house <- cbind(log,housing_dm[6:12])

#Set seed
set.seed(123)

#Hypothesis testing

# 1) price of houses with low unit prices is lower than those with high unit prices
#Null Hypothesis: mean price when unit price low - mean price when unit price high = 0
#Alternative hypothesis: mean price when unit price low - mean price when unit price high < 0

#Hypothesis test data splits
lowu <- c()
for (i in 1:545) { if(housing_dm[i,13]<=unitprice_mean)
  lowu <- c(lowu,housing_dm[i,1])
}

length(low)

highu <- c()
for (i in 1:545) { if(housing_dm[i,13]>unitprice_mean)
  highu <- c(highu,housing_dm[i,1])
}
length(high)

t.test(sample(low,100, replace = TRUE), sample(high,100, replace = TRUE),
       alternative = 'less', mu = 0, conf.level = 0.95)
#Conclusion: we reject the null hypothesis

# 2) Hypothesis: houses with basements have a higher unit price than those without 
#Null hypothesis: mean unit price with basement = mean unit price without basement
# Alternative hypothesis: mean unit price with basement > mean unit price without basement
nob <- c()
for (i in 1:545) { if(housing_dm[i,9]==0)
  nob <- c(nob,housing_dm[i,13])
}

nob

yesb <- c()
for (i in 1:545) { if(housing_dm[i,9]==1)
  yesb <- c(yesb,housing_dm[i,13])
}
yesb

t.test(sample(yesb,100, replace = TRUE), sample(nob,100, replace = TRUE),
       alternative = 'greater', mu = 0, conf.level = 0.95)
#Conclusion: we reject the null hypothesis

# 3) Hypothesis: houses in preferred area have a higher unit price than those not
#Null hypothesis: mean unit price preferred area = mean unit price not preferred area
# Alternative hypothesis: mean unit price preferred area > mean unit price not preferred area
nop <- c()
for (i in 1:545) { if(housing_dm[i,12]==0)
  nop <- c(nop,housing_dm[i,13])
}

nop

yesp <- c()
for (i in 1:545) { if(housing_dm[i,12]==1)
  yesp <- c(yesp,housing_dm[i,13])
}
yesp

t.test(sample(yesp,100, replace = TRUE), sample(nop,100, replace = TRUE),
       alternative = 'greater', mu = 0, conf.level = 0.95)
#Conclusion: we fail to reject the null hypothesis

# 4) Hypothesis: unit price of house with bedrooms less than mean will be less than unit price of house with bedrooms greater than mean
#Null hypothesis: mean unit price preferred area = mean unit price not preferred area
# Alternative hypothesis: mean unit price preferred area > mean unit price not preferred area
nobed <- c()
for (i in 1:545) { if(housing_dm[i,3] < bed_mean)
  nobed <- c(nobed,housing_dm[i,13])
}

nobed

yesbed <- c()
for (i in 1:545) { if(housing_dm[i,3] > bed_mean)
  yesbed <- c(yesbed,housing_dm[i,13])
}
yesbed

t.test(sample(nobed,100, replace = TRUE), sample(yesbed,100, replace = TRUE),
       alternative = 'less', mu = 0, conf.level = 0.95)
#Conclusion: we reject the null hypothesis

#Split Data for regression analysis
sample <- sample(c(TRUE, FALSE), nrow(log_house), replace = T, prob = c(0.8,0.2))
train <- log_house[sample, ]
test <- log_house[!sample, ]

xtrain <- as.matrix(train)[,2:13]
xtrain <- xtrain[,-5]
ytrain <- train[,1] 

xtest <- as.matrix(test)[,2:13]
xtest <- xtest[,-5]
ytest <- test[,1]

#Simple linear regression: Price ~ Area
slin <- lm(train[,1]~train[,2])
summary(slin)
AIC(slin)

#Multiple linear regression all variables
mlin <- lm(ytrain~xtrain, data = train)
summary(mlin)
AIC(mlin)

#Stepwise forward selection
intercept_only <- lm(ytrain~1, data = train)

fstep <- step(intercept_only, direction = 'forward', 
              scope = formula(mlin), trace = 0)
summary(fstep)
fstep$coefficients
AIC(fstep)
#conclusion all variables needed for multiple regression

#Lasso Regression
lmodel <- cv.glmnet(xtrain, ytrain, alpha = 1)
best_lambda <- lmodel$lambda.min

lasso_model <- glmnet(xtrain, ytrain, alpha = 1, lambda = best_lambda)
summary(lasso_model)
coef(lasso_model)
head(xtrain)

#Conclusion for attempted dimension reduction of multi-lin regresss is can't

#Polynomial Regression
poly_lin <- lm(ytrain~poly(train[,2],4), data = train)
summary(poly_lin)
coef(poly_lin)
AIC(poly_lin)

poly_log <- glm(I(ytrain>mean(ytrain))~poly(train[,2],4), data = train)
summary(poly_log)
AIC(poly_log)

#Select multiple linear regression model

#Prediction based on selected model
yhat <- mlin %>% predict(test)

#residuals and checks
res <- resid(mlin)
hist(res, main = "Histogram of Residuals", xlab = "Residuals")

#PCA
corr_matrix <- cor(scale_housing)
ggcorrplot(corr_matrix)

housing.pca <- princomp(corr_matrix)
summary(housing.pca)

#interpret PCs
housing.pca$loadings[,1:8]

#Scree Plot
fviz_eig(housing.pca, addlabels = TRUE, main = "Housing Scree Plot")

#Biplot
fviz_pca_var(housing.pca, col.var = "black")

#Contribution of each variable
fviz_cos2(housing.pca, choice = 'var', axes = 1:2)

#Biplot Cos2 Combo
fviz_pca_var(housing.pca, col.var = 'cos2', 
             gradient.cols = c('black','orange','green'),repel = TRUE)
