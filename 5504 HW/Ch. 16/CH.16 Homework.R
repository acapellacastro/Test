# Clear the environment
rm(list = ls())

# Set the working directory
setwd("/Users/andreaalejandra/Desktop/5504 HW/Ch. 16")

##Q1
library(readxl)
Q1 <- read_excel("Q1.xlsx")


q1m <- lm(y ~ x, data = Q1)
q1m$coefficients # coefficients
summary(q1m)
plot(q1m)

library(tidyverse)

Q1 %>% ggplot(aes(x= x, y= y)) +
  geom_point() +
  geom_smooth(method = lm)


# First Method
Q1$x2 <- Q1$x^2
q1lm <- lm(y ~ x + x2, data = Q1)
summary(q1lm)
q1lm$coefficients

predict(q1lm, data.frame(x=25, x2=25^2) ) 

predict(q1lm, interval = "prediction", level = .95)

# Second Method (More Efficient Method)
reynolds_model_b <- lm(Sales ~ poly(Months, 2, degree = 2, raw = T), data = reynolds)
# raw = T is the polynomial model
# If raw = F Orthogonal Model (Beyond Scope)

##Q2
library(readxl)
Q2 <- read_excel("Q2.xlsx")

q2m <- lm(y ~ x, data = Q2 )
summary(q2m)


library(car) # companion to applied regression
anova(q2m)

Q2$x2 <- Q2$x^2
q2lm <- lm(y ~ x + x2, data = Q2)
summary(q2lm)
q2lm$coefficients

anova(q2lm)

predict(q2lm, data.frame(x=10, x2=10^2) ) 

##  Q3
Q3 <- read_excel("Q3.xlsx")

q3m <- lm(y ~ x, data = Q3 )
summary(q3m)
q3m$coefficients
anova(q3m)

28234.4 + 292.4 

##Q4

Q4 <- read_excel("Q4.xlsx")

q4m <- lm(y ~ x, data = Q4 )
summary(q4m)


library(car) # companion to applied regression
anova(q2m)

Q4$x2 <- Q4$x^2
q4lm <- lm(y ~ x + x2, data = Q4)
summary(q4lm)
q4lm$coefficients
predict(q4lm, data.frame(x=38, x2=38^2) ) 

##Q5

Q5 <- read_excel("Q5.xlsx")

q5m <- lm(y ~ x, data = Q5 )
summary(q5m)

Q5 %>% ggplot(aes(x= x, y= y)) +
  geom_point() +
  geom_smooth(method = lm)

Q5$x2 <- Q5$x^2
q5lm <- lm(y ~ x + x2, data = Q5)
summary(q5lm)
q5lm$coefficients
predict(q5lm, data.frame(x=38, x2=38^2) ) 

##6
Q6 <- read_excel("Q6.xlsx")

Q6m <- lm(Price ~ Rating, data = Q6 )
summary(Q6m)

Q6 %>% ggplot(aes(x= x, y= y)) +
  geom_point() +
  geom_smooth(method = lm)

Q6$Rating2 <- Q6$Rating^2
Q6lm <- lm(Price ~ Rating + Rating2, data = Q6)
summary(Q6lm)
Q6lm$coefficients

Q6$LnPrice <- log(Q6$Price)
Q6lm2 <- lm(LnPrice ~ Rating, data = Q6)
summary(Q6lm2)
Q6lm2$coefficients

mpg_model_a <- lm(LnMPG ~ Weight, data = mpg)
summary(mpg_model_a)


mpg_model <- lm(MPG ~ Weight, data = mpg)
mpg$LnMPG <- log(mpg$MPG)

##Q9

q9 <- read_excel("q9.xlsx")
head(q9)

q9m <- lm(risk ~ age + bloodpressure, data = q9)
q9m$coefficients # coefficients
summary(q9m)
plot(q9m)
anova(q9m)



aov(q9m)

q9$predicted <- fitted(q9m)
q9$residuals <- residuals(q9m)
q9$std_residuals <- rstandard(q9m)

13.97961 + 3322.38

q9lm <- glm(risk ~ age + bloodpressure + smoker + age*bloodpressure, data = q9)
summary(q9lm)
q9lm$coefficients # coefficients

aov(q9lm)
anova(q9lm)

Anova <- aov(risk ~ age + bloodpressure + smoker + age*bloodpressure, data = q9)
summary(Anova)

##10

library(readxl)
head(q9)
q10 <- read_excel("q10.xlsx")
glimpse(q10)

# This format is not useful in R, we need to reshape
install.packages(reshape2)
library(reshape2)
q10a <- melt(q10)

q10a$method <- c(rep("1",4), rep("2",4), rep("3", 4))


# convert to factor
q10a$method <- as.factor(q10a$method)
levels(q10a$method)

colnames(q10a)[2] <- "units"
q10_model <- lm(units ~ method, data = q10a)
anova(q10_model)
summary(q10_model)