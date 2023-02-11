
df <- read_excel("/Users/andreaalejandra/Desktop/Test/5504 HW/Case Study/wineratings.xlsx")
head(df)

#Q1: Develop a table that shows the number of wines that were classified as classic,outstanding, very good, good, mediocre, and not recommended and the average price. Does there appear to be any relationship between the price of the wine and the Wine Spectator rating? Are there any other aspects of your initial summary of the data that stand out?

# Simple linear regression
winelm <- lm(Price ~ Rating, data = df)
summary(winelm)

plot(winelm)
#The higher the rating, the more expensive the mean price per bottle. In the sample, there are no bottles of wine under a score of 75 and only one bottle under the mediocre rating. 


####
#Q2: Develop a scatter diagram with price on the horizontal axis and the Wine Spectator score on the vertical axis. Does the relationship between price and score appear to be linear?

ggplot(df) +
  aes(x = Price, y = Score) +
  geom_jitter() +
  geom_smooth(method = lm) +
  theme_minimal()

#The relationship between Price and score appears to be linear.


###
#Q3: Using linear regression, develop an estimated regression equation that can be used to predict the score given the price of the wine.

Q3_model <- lm(Score ~ Price, df)
summary(Q3_model)
#Score = 87.76 + (0.027995*Price)


###
#Q4: Using a second-order model, develop an estimated regression equation that can be used to predict the score given the price of the wine.

Q4_model <- lm(Score ~ poly(Price, 2, degree = 2, raw = T), df)
summary(Q4_model)


###
#Q5: Compare the results from fitting a linear model and fitting a second-order model.

plot(Q3_model, which = 3)
plot(Q4_model, which = 3)


###
#Q6: As an alternative to fitting a second-order model, fit a model using the natural logarithm of price as the independent variable. Compare the results with the second-order model.

df$price_log <- log(df$Price)
Q6_model <- lm(Score ~ price_log, df)
summary(Q6_model)
plot(Q6_model, which = 3)


###
#Q7: Based upon your analysis, would you say that spending more for a bottle of wine will provide a better wine?

#Yes. Based on the results from all three models, the price of the bottle has a strong statistical significance of predicting spending more will grant you a better (higher score) bottle of wine.


###
#Q8: Suppose that you want to spend a maximum of $30 for a bottle of wine. In this case, will spending closer to your upper limit for price result in a better wine than a much lower price?
predict(Q3_model, data.frame(Price=30), interval = "prediction")

#Yes. Spending closer to your upper limit will result in a better wine than a much lower price


