
# Clear the environment
rm(list = ls())


# Set the working directory
setwd("~/Documents/5504/Ch_15")

# Section 1 Multiple Regression Coeffecients ----

# Read Excel file in R
library(readxl)
butler <- read_excel("/Users/andreaalejandra/Downloads/butler.xlsx")



summary(butler)

# multiple regression
butler_model <- lm(Time ~ Miles + Deliveries, data = butler)
butler_model$coefficients # coefficients

# Section 2 R2 and Adjusted R2 ----
summary(butler_model) 
summary(butler_model)$r.squared # R2
summary(butler_model)$adj.r.squared # R2

# alt method rsq package
install.packages(rsq)
library(rsq)
Y
rsq(butler_model, adj = FALSE) # R2
rsq(butler_model, adj = TRUE) # adjusted RR2

library(readxl)
sat <- read_excel("/Users/andreaalejandra/Downloads/satisfaction.xlsx")

summary(sat)

# Change col names
colnames(sat) <- c("global" , "job", "pay", "org")

# a. Develop the estimated multiple regression equation that can be used to predict the Global Satisfaction score using the Job Satisfaction, Pay Satisfaction, and Organization Satisfaction scores.
sat_model <- lm(global ~ job + pay + org, data = sat)
summary(sat_model)

sat_model$coefficients

#b. Predict the overall Global Satisfaction score for an employee with a Job Satisfaction score of 72, a Pay Satisfaction score of 54, and an Organization Satisfaction score of 53.
predict(sat_model, data.frame(job = 72, pay = 54, org = 53))


#problem 24 
library(readxl)
nfl <- read_excel("/Users/andreaalejandra/Downloads/nfl2011.xlsx")
plot(nfl) 

colnames(nfl) <- c("team", "off", "def","win")

nfl_model <- lm(win ~ off + def, data=nfl) 
summary(nfl_model)

coef(nfl_model)
confint(nfl_model, level = .95)

install.packages(GGally)

#problem 52

library(readxl)
nba <- read_excel("/Users/andreaalejandra/Downloads/nbastats.xlsx")
plot(nba)

nba_model <- lm( Win ~ FG, data = nba )
summary(nba_model)
colnames(nba) <- c("Team",  "Win" , "FG",   "threeP",   "FT",   "RBOff", "RBDef")
simmons_model <- glm(Coupon ~ Card + Spending, data = simmons, family ='binomial')
summary(simmons_model)

nba_model2 <- lm(Win ~ FG + threeP + FT + RBOff + RBDef  , data = nba)
summary(nba_model2)