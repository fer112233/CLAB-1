library(ggplot2)
library(readxl)
library(broom)
library(olsrr)
library(MASS)

# 1 - 11a)

db <- data.frame(
  x = c(408,408,554,554,680,680,812,812),
  y = c(1.1,1.3,1.6,2.5,3.0,4.3,4.2,4.7)
)

model <- lm(y~x, data=db)
model

pred.int <- predict(model, interval = "prediction")
db <- cbind(db, pred.int)

p <- ggplot(db, aes(x, y, col="pink")) +
  geom_point() +
  stat_smooth(method = lm, aes(col="lightblue")) +
  theme(legend.position = "none")
p

# 1 - 11b)

conf_int450 <- predict(model, data.frame(x=450), interval='confidence')
conf_int450

# 1 - 11c)

pred_int650 <- predict(model, data.frame(x=650), interval='prediction')
pred_int650

# 1 Final Graph

p + geom_line(aes(y = lwr), color = "purple", linetype = "dashed")+
  geom_line(aes(y = upr), color = "purple", linetype = "dashed")


# 2

age_height_db <- read_excel("Datasets/ageandheight.xls", sheet="Hoja2")
lmHeight = lm(height~age, data = age_height_db)
summary(lmHeight)

# Equation -> height = 64.9283 + 0.635 * age

p <- ggplot(age_height_db, aes(age, height, col="pink")) +
  geom_point() +
  stat_smooth(method = lm, aes(col="lightblue")) +
  theme(legend.position = "none")
p

# 2 - Q1

cint_age <- confint(lmHeight, "age", level=0.99)
cint_age


# 3

cov(age_height_db$height, age_height_db$age)
cor(age_height_db$height, age_height_db$age)

# 3 - Q2

# Since the covariance is positive we can tell that when the height parameter increases, the age one also increases, same when decreasing.
# With the correlation coefficient of 0.9943 we can tell that there is a really strong linear relationship, we can tell that there is a strong one when we get a value between 0.8 and 1, which is the maximum and would indicate that they are perfectly related.


# 4

cor.test(age_height_db$height, age_height_db$age)

# 4 - Q3
  # 1. Given a p-value of 4.428e-11, there is a probability of 4.428e-11 that the correlation coefficient between the 2 variables is 0, which would mean that there would not be a relationship between the 2.
  # 2. The best estimate would be 0.9943661
  # 3.
    # 3.1 The interval for a 5% probability of beign wrong would be [0.9793465, 0.9984716], since those values would give a 0.05 p-value.
    # 3.2
    cor.test(age_height_db$height, age_height_db$age, conf.level = 0.99)
    # With a probability of 1% of beign wrong, the interval gets bigger, to [0.9690234, 0.9989860].


# 5

ggplot(lmHeight, aes(x = .fitted, y = .resid)) + geom_point() + geom_hline(yintercept=0)

# 5 - Q4

plot(lmHeight, which=1)
plot(lmHeight, which=2)

# 5 - Q5

# In the first plot we can see that the residuals are randomly distributed, hence, there is no relation whatsoever between them, that indicates that they are mostly normal errors, if for example we saw a pattern like a curve, we would know that our model is not good since that relation should be included in our model.
# In the second plot we check the normality of the residuals with a qq-plot, we see a linear trend which indicates that the error terms are normally distributed, the normal distribution is the distribution that explains random deviation, hence supporting what we saw on the 1st plot.


# 6

ols_plot_resid_qq(lmHeight)

ols_test_normality(lmHeight)
# From this test we get a Shaphiro-Wilk normality test with a 0.3154 p-value which is higher than the standard 0.05, that means that the distribution of the data is not significantly different from a normal distribution, hence, it follows a normal distribution, the same thing our eyes predicted.

ols_test_breusch_pagan(lmHeight)
# In this second test we test whether we have or not heteroscedasticity in our data, which would mean that the residuals are not homogeneous, hence they are not random and our linear regression model is not good. With a p value of 0.63369, we conclude that since it is way higher than our standard 0.05, there is no enough evidence to claim that our data is heteroscedasticity, so our linear model is good for now. Backing up our findings from the visual inspection in Ex. 5.


# 7

pressure_db <- read_excel("Datasets/pressure.xlsx")

# 7 - Q7

temp_model <- lm(Pressure~Temperature, data=pressure_db)
summary(temp_model)

plot(pressure_db$Temperature, pressure_db$Pressure, pch = 16, cex = 1.3, col = "blue", main = "PRESSURE PLOTTED AGAINST TEMPERATURE", xlab = "TEMPERATURE", ylab = "PRESSURE")
abline(temp_model)

plot(temp_model, which=1)

# From the summary we see that the Adjusted R-Squared is really high, with a value of 0.8898, it would mean that the model can predict 88.98% of the outcomes, but if we see the plot we can clearly see an issue with the model.
# The Temperature - Pressure is not a linear relation but our model is. Our model is not precise at all and it is not a good fit regardless of the Adjusted R-Squared value, we need to try to find a better model by adding a quadratic or cubic term.
# We confirm this by looking at the residuals vs fitted plot, we can clearly see a non linear trend on the residuals.

# 7 - Q8

temp_model_cubic <- lm(Pressure~Temperature+I(Temperature^2), data=pressure_db)
summary(temp_model_cubic)

plot(temp_model_cubic, which=1)

# By adding a quadratic term to the model we get a better model, with an Adjusted R-Squared of 0.9994 and a residuals vs fitted plot wich is flat now.


# 8

delivery_db <- read.csv("Datasets/delivery.csv")

# 8 - a)

shop_model <- lm(time ~ shop, data = delivery_db)
summary(shop_model)

# Dummy variables are used when we have categorical variables in our data, if we have k types of a categorical variable, we need to add k-1 dummy variables to our model, since the last one would not give any extra information to the model, "if it is not any of this is has to be that".
# In this case we have 3 types of shop, centre, north and south, R ommits the centre one and computes 2 dummy variables in our model, for the north and for the south.
# We get a -5.2464 coefficient for the north shop and a -1.1182 for the south one, this implies that the fatest shop is the north one, followed by the south one and lastly the centre one. The smaller the coefficient, the lesser time is added to the prediction of time with our linear model.

# 8 - b)

model_time <- lm(time ~ temperature+operator+bill+day+shop+rider+pizzas+discount, data = delivery_db)
summary_model_time <- summary(model_time)
summary_model_time

# Again, since we are modeling time and want the fastest variables, we look for the smallest ones:
# The most efficient shop would be the north one, with a coefficient of -1.6.
# The fastest rider would be rider Peter with a coefficient of -2.59.

# By looking at the significance codes we can easily see the variables that are not significant at 5% == p-value > 0.05:
# Not significant variables: operator, day and discount.

# 8 - c)

coefficients <- summary_model_time[["coefficients"]]
coefficients_names <- rownames(coefficients)

cat("Coefficient confidence intervals at 99%:", "\n")
for (i in 1:nrow(coefficients)) {
  coef_name <- coefficients_names[i]
  
  conf_int_coeff <- confint(model_time, coef_name, level=0.99)
  
  cat(paste(paste(paste(paste(paste(coef_name, " -> ["), conf_int_coeff[[1]], ", ", conf_int_coeff[[2]], "]")))), "\n")
}

# Using the data from the model summary, we compute every confidence interval.

# 8 - d)

sum(residuals(model_time)^2)
summary(residuals(model_time))

# With the sum of residuals to the square we can get an idea of the fit of the model on real data, if it were 0 it would mean that the model is a perfect fit and can predict the time precisely.
# Having an SSE of over 36000 tells us that the fit may not be good, but we need to do further checks. In the summary we can see that most residuals are between [-3.768, 3.504], IQR.

# 8 - e)

summary_model_time

# The best indicator for the goodnes of fit for our linear model is the adjusted R-Squared that we get from the summary, it indicates how much % of the data is explained by the model.
# In our case the Ajusted R-Squared is 0.3085 so, our model only predicts 30.86% of the outcomes, which is usable but we can improve it. If we see that number increase that means our model is better at predicting outcomes.

# 8 - f)

optimal_model <- stepAIC(model_time, direction = "backward")
optimal_model

# 8 - g)

summary_optimal <- summary(optimal_model)
summary_optimal

# Yes it is an improvement, first, we have removed 2 variables, operator and discount, that simplifies the model.
# By observing the Adjusted R-Squared and residual standard error we also see an improvement, from 0.3085 to 0.3092 and from 5.373 to 5.37 respectively. The model has improved.

# 8 - h)

plot(optimal_model, which=2)
plot(optimal_model, which=1)

ols_test_normality(optimal_model)
ols_test_breusch_pagan(optimal_model)

ols_vif_tol(optimal_model)

# The test for normality gives us a p-value of 0 which makes no sense with what we see on the qq plot, therefore, we claim that there is normality indeed, we trust our eyes since the test for normality does not give us a reasonable output.
# The test for heteroscedasticity gives us a p value of 0.00589, which is quite small and can indicate heteroscedasticity, but we do not see a clear heteroscedasticity in the residuals vs fitted plot. On the residuals vs fitted we actually see a slight curve which could mean that there is a term that should be quadratic, also know as, non linear.
# The VIF test which indicates multicorrelation gives us values between 1 and 2 approximately, a value between 1 and 5 indicates that the variables are indeed moderately correlated.

# 8 - i)

summary_optimal

# From the summary, we see that not all variables are within the 99.9% of confidence, which means that for those variables we cannot prove that they affect the delivery time.
# The variables that we know for sure with 99.9% confidence that they affect the delivery time are: temperature, bill, shop, rider and pizzas.
# The variable that is left out of the 99.9% confidence range is the day, which could be fixed if we lowered the confidence range to 90%, which would still be high are within the standard for statistics.

# 8 - j)

quadratic_model <- lm(formula = time ~ temperature + I(temperature^2) + bill + day + shop + rider + pizzas, data = delivery_db)
summary(quadratic_model)

plot(quadratic_model, which=1)

# By adding a quadratic dependence on temperature we see that our model actually improves.
# The Adjusted R-Squared improves to 0.3341 from the original 0.3092.
# The Standard error also decreases to 5.273 from 5.37.
# Finally if we plot again the Residuals vs Fitted graph, we see that the curve that we saw earlier has almost disappear, meaning we improved the model.

# 8 - k)

last_row <- tail(delivery_db, n=1)
real_time <- last_row[["time"]]
last_row["time"] <- NULL

predicted_time <- predict(model_time, newdata = last_row)

cat("Predicted time vs Real time:\n", predicted_time, "   vs   ", real_time)

# It is indeed a good prediction for our usecase, we want to predict when a pizza order will arrive to a customer house, we predicted it with an error of 1 minute which doesn't imply a difference in this situation.


# 9

