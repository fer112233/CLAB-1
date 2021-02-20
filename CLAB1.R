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

p <- ggplot(age_height_db, aes(age, height, col="pink")) +
  geom_point() +
  stat_smooth(method = lm, aes(col="lightblue")) +
  theme(legend.position = "none")
p








