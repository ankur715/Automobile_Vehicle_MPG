#### Automobile Vehicle MPG Analysis
#### Date: 10/10/18

#####################################################################################

# This file only includes the codes for this project. 
# All details and decriptions can be seen in the Rmd or Docx file.
# The plots are in a folder
# A brief overview: read, summarize, clean, train & test, multiple r-squared, adjusted r-squared,
# complete linear regression equation, predict & compare, residual plot, histogram, log, bonus.

### Read, View, Structure, Summary  
auto_mpg <- read.csv(file.choose())
# auto_mpg_read <- read.csv("/Users/apatel8/Documents/DS 510/Labs/auto-mpg.csv")
View(auto_mpg)
str(auto_mpg)
summary(auto_mpg)
# 398 observations with 9 variables

### Clean Date
auto_mpg1 <- auto_mpg[-c(33,127,331,337,355,375),]
auto_mpg_clean <- auto_mpg1[,-c(9)]
View(auto_mpg_clean)
auto_mpg_clean$horsepower <- as.numeric(auto_mpg_clean$horsepower)
model <- lm(mpg~., data = auto_mpg)
summary(model)
model1 <- lm(mpg~., data = auto_mpg_clean)
summary(model1)
str(auto_mpg_clean)
# clean, summary of dataset

### Train Dataset & Test Dataset
View(auto_mpg_clean)
train <- auto_mpg_clean[1:300,]
View(train) 
# test <- auto_mpg_clean[301:398,]
test <- auto_mpg_clean[301:392,]
View(test)
# test dataset only upto 392 because of the 6 NA values

### Multiple R-squared
traincylinder <- lm(train$mpg ~ train$cylinder, data = train)
summary(traincylinder)
plot(train$cylinder, train$mpg, main = "MPG vs. Cylinder", xlab = "cylinder", ylab = "mpg")
traindisplacement <- lm(train$displacement ~ train$mpg, data = train)
summary(traindisplacement)
plot(train$displacement, train$mpg, main = "MPG vs. Displacement", xlab = "displacement", ylab = "mpg")
trainhorsepower <- lm(train$horsepower ~ train$mpg, data = train)
summary(trainhorsepower)
plot(train$horsepower, train$mpg, main = "MPG vs. Displacement", xlab = "displacement", ylab = "mpg")
trainweight <- lm(train$weight ~ train$mpg, data = train)
summary(trainweight)
plot(train$weight, train$mpg, main = "MPG vs. Weight", xlab = "weight", ylab = "mpg")
trainacceleration <- lm(train$acceleration ~ train$mpg, data = train)
summary(trainacceleration)
plot(train$acceleration, train$mpg, main = "MPG vs. Acceleration", xlab = "acceleration", ylab = "mpg")
trainmodel.year <- lm(train$model.year ~ train$mpg, data = train)
summary(trainmodel.year)
plot(train$model.year, train$mpg, main = "MPG vs. Model Year", xlab = "model.year", ylab = "mpg")
trainorigin <- lm(train$origin ~ train$mpg, data = train)
summary(trainorigin)
plot(train$origin, train$mpg, main = "MPG vs. Origin", xlab = "origin", ylab = "mpg")
# summary and plots to test significance and R-squared

### Adjusted R-squared
attach(train)
adjustedmodel <- lm(mpg ~ cylinder + displacement + weight, data = train)
summary(adjustedmodel)
# cylinder showed no significance 
adjustedmodel2 <- lm(mpg ~ displacement + weight, data = train)
summary(adjustedmodel2)
# didn't attach() because it showed message indicating that it's repeating
adjustedmodel3 <- lm(mpg ~ cylinder + displacement + horsepower + weight + acceleration, data = train)
summary(adjustedmodel3)
# trying all variables to just see

### Complete Linear Regression Model
attach(train)
adjustedmodel2 <- lm(mpg ~ displacement + weight, data = train)
# adjustedmodel2 showed best results 

### Predict & Compare
pred <- coef(trainweight)[1] + coef(trainweight)[2]*test$mpg
View(pred)
test_mpg <- test[,1]
View(test_mpg)
error <-  pred - test_mpg
summary(error)
# include displacement
pred1 <- coef(adjustedmodel2)[1] + coef(adjustedmodel2)[2]*test$displacement + coef(adjustedmodel2)[3]*test$weight
View(pred1)
test_mpg1 <- test[,1]
error1 <- test_mpg1 - pred1
View(error1)
summary(error1)
error1 <- error1 - 6.764
summary(error1)
# mostly normal distributed

### Residual Plot
plot(error1, main = 'Prediction Errors', xlab = 'Error1', ylab = 'Residual')
abline(0,0)

### Histogram
hist(error1, prob = T, breaks = 20, main = 'Error1- pred vs. test', xlab = 'Residual', ylab = 'Density')
lines(density(error1), col='red')
mean_e <- mean(error1)
var_e <- var(error1)
sd_e <- sqrt(var_e)
x_e <- seq(-20,20, length=92)
y_e <- dnorm(x_e, mean_e, sd_e)
lines(x_e, y_e, col = 'blue')
# blue normal distributed curve similar when compared to red error curve

## BONUS
# new 'esquisse' package for vizualizations
source("https://install-github.me/dreamRs/esquisse")
esquisse::esquisser()
ggplot(data = test) +
  aes() +
  geom_blank() +
  geom_smooth(span = 1) +
  labs(title = "MPG vs Weight, Displacement",
       x = "weight",
       y = "mpg",
       caption = "ggplot2 builder",
       subtitle = "Adjusted R-squared: 0.77") +
  theme_classic()
# amazing tool to create several graphs to observe and use


### Thank you for taking the time to read this report!
  