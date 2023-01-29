
library(readxl)
df <- read_excel("/Users/andreaalejandra/Desktop/5504 HW/Ch15.Case Study/nascar.xlsx")


# Problem #1
#Suppose you wanted to predict Winnings ($) using only the number of poles won (Poles), the number of wins (Wins), the number of top five finishes (Top 5), or the number of top ten finishes (Top 10). Which of these four variables provides the best single predictor of winnings?
summary.lm(lm(winnings ~ top10, data = df))
summary.lm(lm(winnings ~ top5, data = df))
summary.lm(lm(winnings ~ poles, data = df))
summary.lm(lm(winnings ~ wins, data = df))

# Top 10 provides the strongest predictive power of winnings out of the four variables with a p-value of 2.712e-13 which is smaller than top 5(3.18e-11), poles (0.0155), and wins (1.51e-05)

# Problem #2
#Develop an estimated regression equation that can be used to predict Winnings ($) given the number of poles won (Poles), the number of wins (Wins), the number of top five finishes (Top 5), and the number of top ten (Top 10) finishes. Test for individual significance and discuss your findings and conclusions.

 winnings_model <- lm(winnings ~ poles + wins + top5 + top10 , data = df)
 summary.lm(winnings_model)
#Regression Model --> winnings = 3140367 + (poles*(-12939)) + (wins*13545) + (top5*71629) + (top10*117071)
#Explanation: When all the variables are placed in the model together, there is no significance for poles, wins, or top5, leaving top10 as the sole significant predictor of winnings. Top10 has a p-value of 0.00147.

# Problem #3
#Create two new independent variables: Top 2–5 and Top 6–10. Top 2–5 represents the number of times the driver finished between second and fifth place and Top 6–10 represents the number of times the driver finished between sixth and tenth place. Develop an estimated regression equation that can be used to predict Winnings ($) using Poles, Wins, Top 2–5, and Top 6–10. Test for individual significance and discuss your findings and conclusions.

df$top2_5 <- df$top5 - df$wins
df$top6_10 <- df$top10 - df$top2_5 - df$wins

modeling_top <- lm(winnings ~ poles + wins + top2_5 + top6_10 , data = df)
summary.lm(modeling_top)

#Top 2_5 is more significant than the Top 6_10. Poles is not significant. 
#Regression Model --> #Winnings = 3140367 + (poles*(-12939)) + (wins*202245) + (top2_5*188700) + (top6_10*117071)
#Explanation: 

# Problem #4
#Based upon the results of your analysis, what estimated regression equation would you recommend using to predict Winnings ($)? Provide an interpretation of the estimated regression coefficients for this equation.

# I would use the model from question 3 as it  has more significant results compared to the prior model with just top 5 and top 10 finishes.
