##-------------------------------Question 1-------------------------------------

##Consider the following data with x as the predictor and y as as the outcome.

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

##Give a P-value for the two sided hypothesis test of whether ??1 from a linear 
##regression model is 0 or not.

linearmod = lm(y ~ x) ##outcome/predictor

summary(linearmod)

0.05296

##-------------------------------Question 2-------------------------------------

##Consider the previous problem, give the estimate of the residual standard 
##deviation.

summary(linearmod)

0.223

##-------------------------------Question 3-------------------------------------

##In the mtcars data set, fit a linear regression model of weight (predictor) on
##mpg (outcome). Get a 95% confidence interval for the expected mpg at the 
##average weight. What is the lower endpoint?

x = mtcars$wt
y = mtcars$mpg

linearmod = lm(y ~ x) ##outcome/predictor

newdata = data.frame(x=mean(x))

predict(linearmod, newdata, interval = ("confidence"))

18.99098

##-------------------------------Question 4-------------------------------------

##Refer to the previous question. Read the help file for mtcars. What is the 
##weight coefficient interpreted as?

?mtcars

The estimated expected change in mpg per 1,000 lb increase in weight.

##-------------------------------Question 5-------------------------------------

##Consider again the mtcars data set and a linear regression model with mpg as 
##predicted by weight (1,000 lbs). A new car is coming weighing 3000 pounds. 
##Construct a 95% prediction interval for its mpg. What is the upper endpoint?

x = mtcars$wt
y = mtcars$mpg

linearmod = lm(y ~ x) ##outcome/predictor

newdata = data.frame(x=3)

predict(linearmod, newdata, interval = ("prediction"))

27.57355

help(predict.lm)

##-------------------------------Question 6-------------------------------------

##Consider again the mtcars data set and a linear regression model with mpg as 
##predicted by weight (in 1,000 lbs). A "short" ton is defined as 2,000 lbs. 
##Construct a 95% confidence interval for the expected change in mpg per 1 short
##ton increase in weight. Give the lower endpoint.

x = mtcars$wt
y = mtcars$mpg*2 ##double for short ton

linearmod = lm(y ~ x) ##outcome/predictor

summary(linearmod)

##The 95% confidence interval for the slope is the estimated coefficient 
##(-10.689) ± two standard errors (1.118).

confint(linearmod, x, level=0.95)[1]

-12.97262

##-------------------------------Question 7-------------------------------------

##If my X from a linear regression is measured in centimeters and I convert it 
##to meters what would happen to the slope coefficient?


It would get multiplied by 100.

##-------------------------------Question 8-------------------------------------

##I have an outcome, Y, and a predictor, X and fit a linear regression model 
##with Y=??0+??1X+?? to obtain ??^0 and ??^1. What would be the consequence to the 
##subsequent slope and intercept if I were to refit the model with a new 
##regressor, X+c for some constant, c?

Y=??0+??1X+??

BO = intercept
B1 = slope

Shifting your X values by value 'a' changes the intercept, but not the slope.

The new intercept would be ??^0 - c??^1


##-------------------------------Question 9-------------------------------------

##Refer back to the mtcars data set with mpg as an outcome and weight (wt) as 
##the predictor. About what is the ratio of the the sum of the squared errors, 
##???ni=1(Yi???Y^i)2 when comparing a model with just an intercept (denominator) to 
##the model with the intercept and slope (numerator)?

The sum of square errors is also known as the sum of squared residuals.

x = mtcars$wt
y = mtcars$mpg


interceptonly = lm(y ~ 1) ##intercept only model (representing total variation)
linearmod = lm(y ~ x) ##outcome/predictor (intercept and slope)

modelcomp = anova(linearmod, interceptonly)

modelcomp[2]

278.32/1126.05

0.2471649


##------------------------------Question 10-------------------------------------

##Do the residuals always have to sum to 0 in linear regression?

If an intercept is included, then they will sum to 0.