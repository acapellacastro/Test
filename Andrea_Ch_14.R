##-------------------------------------------------
## Andrea_Ch_14.r
##
## Title: Andrea Capella-Castro Chapter 14 Case Study
## Author: Andrea Capella-Castro
## Date: 01/24/2023
##-------------------------------------------------
#Loading Packages
library(tidyverse)
library(ggplot2)

# reading the excel file
library(readxl)
df <- read_excel("/Users/andreaalejandra/Downloads/buckeyecreek.xlsx")
glimpse(df)

# 1. compute descriptive statistics and construct a scatter diagram for the data. Discuss your findings.
summary(df)
plot(df)

ggplot(df) +
  aes(x = `Population`, y = `Season Pass Holders`) +
  geom_point() +
  geom_smooth(span = 1)

# The descriptive statistics show there to be an mean population of 15,738 people per zip code, with a mean season pass ownership of 128 per zip code. Through the use of a scatter plot specifically looking at the number of season pass holders by population, we can see that the larger a population, the more season passes are purchased. 


# 2. Using simple linear regression, develop an estimated regression equation that could be used to predict the number of season pass holders in a zip code given the total population of the zip code.
buckeyecreek_lm <- lm(`Season Pass Holders` ~ Population, data = df)
buckeyecreek_lm$coefficients
# 0.00918295x - 16.25836695 = Season Pass Holders

# 3. Test for a significant relationship at the .05 level of significance.
summary(buckeyecreek_lm)

#  Population is statistically significance (p < 0.001) representing that there is significant difference between the number of season pass holders and the population in each zip code.

# 4. Did the estimated regression equation provide a good fit?
# Yes, The estimated regression equation does provide a good fit as we can see the R^2 value is 0.6374 representing ~64% of the variance in our data.

# 5. Use residual analysis to determine whether the assumed regression model is appropriate.
df$predicted <- fitted(buckeyecreek_lm)
df$residuals <- residuals(buckeyecreek_lm)
df$std_residuals <- rstandard(buckeyecreek_lm)
plot(df$predicted)

plot(buckeyecreek_lm, which = 1)

# 6. Discuss if/how the estimated regression equation should be used to guide the marketing campaign.
#At this point, I believe that Greg can use the regression equation to run his mail campaign to try and target those zip codes with lower than expected season pass ownership. However, I believe it is important to take into account other factors (mentioned in question 7) as they may provide better insight into why season pass ownership differs by zip code.

# 7. What other data might be useful to predict the number of season pass holders in a zip code?
#The average duration of how long it takes for people from each area code to get to Buckeye Creek.
#As it is not given in this dataset, the distance of the Zip Codes from Buckeye Creek so we can understand if there is a decrease in season passes for those living further away.
