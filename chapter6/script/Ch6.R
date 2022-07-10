#### Table 6.3
require(ggplot2)
require(psych)
require(forecast)

car.df <- read.csv("data/ToyotaCorolla.csv")
# use first 1000 rows of data
car.df <- car.df[1:1000, ]
describe(car.df)
# select variables for regression
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)
car.df = car.df[,selected.var]
car.df$Fuel_Type = factor(car.df$Fuel_Type)
pairs(car.df)




# Linear Regression in Traditional way
options(scipen = 999)
model1 = lm(Price ~ ., data = car.df)
summary(model1)





selectvar = c(1,2,3,10,11)
car.df = car.df[,selectvar]
describe(car.df)
round(cor(car.df),2)

selectvar = c(1,2,3,5)
car.df = car.df[,selectvar]
describe(car.df)
pairs(car.df)
round(cor(car.df),2)

model2 = lm(Price ~ Age_08_04 + KM +Weight, data = car.df)
confint(model2)


# y = b0 + b1 *x +b2 *x2 + b3 * x3
Age = 3
KM = 5000
WT = 1079

model2$coefficients[1]
y = model2$coefficients[1]+
    (model2$coefficients[2]*Age)+
    (model2$coefficients[3]*KM)+
    (model2$coefficients[4]*WT)
y

car.df <- read.csv("data/ToyotaCorolla.csv")
# use first 1000 rows of data
car.df <- car.df[1:1000, ]
describe(car.df)
# select variables for regression
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)
car.df = car.df[,selected.var]
car.df$Fuel_Type = factor(car.df$Fuel_Type)
set.seed(1234)
train.index <- sample(c(1:1000), 600)  
train.df <- car.df[train.index,]
valid.df <- car.df[-train.index,]
car.lm <- lm(Price ~ ., data = train.df)
summary(car.lm)

library(forecast)
# use predict() to make predictions on a new set. 
car.lm.pred <- predict(car.lm, valid.df)
# options(scipen=999, digits = 0)
some.residuals <- valid.df$Price[1:20] - car.lm.pred[1:20]
com = data.frame("Predicted" = car.lm.pred[1:20], "Actual" = valid.df$Price[1:20],
           "Residual" = some.residuals)

hist(com$Residual)
# use accuracy() to compute common accuracy measures.

accuracy(car.lm.pred, valid.df$Price)




data <- read.csv("data/ToyotaCorolla.csv")
price =  data$Price
km = data$KM
age = data$Age_08_04
fit = lm(price ~ km + age)
summary(fit)
plot(km, price,xlab = "KM",ylab = "Price")
curve(coef(fit)[1] + coef(fit)[2]*km + coef(fit)[3]*age,add = TRUE)

# partition data
set.seed(1)  # set seed for reproducing the partition
train.index <- sample(c(1:1000), 600)  
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]

# use lm() to run a linear regression of Price on all 11 predictors in the
# training set. 
# use . after ~ to include all the remaining columns in train.df as predictors.
car.lm <- lm(Price ~ ., data = train.df)

#  use options() to ensure numbers are not displayed in scientific notation.
#options(scipen = 999)
summary(car.lm)

#### Table 6.4

library(forecast)
# use predict() to make predictions on a new set. 
car.lm.pred <- predict(car.lm, valid.df)
options(scipen=999, digits = 0)
some.residuals <- valid.df$Price[1:20] - car.lm.pred[1:20]
data.frame("Predicted" = car.lm.pred[1:20], 
           "Actual" = valid.df$Price[1:20],
           "Residual" = some.residuals)

options(scipen=999, digits = 3)
# use accuracy() to compute common accuracy measures.
accuracy(car.lm.pred, valid.df$Price)



#### Figure 6.1

library(forecast)
car.lm.pred <- predict(car.lm, valid.df)
all.residuals <- valid.df$Price - car.lm.pred
length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)])/400
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")



#### Table 6.5

# use regsubsets() in package leaps to run an exhaustive search. 
# unlike with lm, categorical predictors must be turned into dummies manually.
library(leaps)
search <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],
                     method = "exhaustive")
sum <- summary(search)

# show models
sum$which

# show metrics
sum$rsq
sum$adjr2
sum$Cp



#### Table 6.6
# use step() to run stepwise regression.
car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step)  # Which variables were dropped?
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

#### Table 6.7
# create model with no predictors
car.lm.null <- lm(Price~1, data = train.df)
# use step() to run forward regression.
car.lm.step <- step(car.lm.null, scope=list(lower=car.lm.null, upper=car.lm), direction = "forward")
summary(car.lm.step)  # Which variables were added?
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

#### Table 6.8
# use step() to run stepwise regression.
car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step)  # Which variables were dropped/added?
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)

