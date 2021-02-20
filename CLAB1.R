library(ggplot2)
library(readxl)

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
    # With a probability of 1% of beign wrong, the interval gets smaller, to [0.9690234, 0.9989860].

    
