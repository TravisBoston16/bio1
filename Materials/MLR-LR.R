library(ggplot2)
library(MASS)
library(tidyverse)
library(car)
library(stargazer)
#checking normality for regression 
data(Boston)
#First build a model
model <- lm(medv ~ rm + age + tax + lstat + indus + nox, data = Boston)
summary(model)
# DIAGNOSTIC 1: Correlation Matrix 
# The simplest first step is to look at the correlations between predictors.
# We create a dataframe with only the predictor variables from our model.
predictors <- Boston[, c("rm", "age", "tax", "lstat", "indus", "nox")]
cor_matrix <- cor(predictors)
print(round(cor_matrix, 2)) # Round to 2 decimal places for readability
# - For example, 'indus' and 'nox' have a correlation of 0.76. This is high.
# - 'age' and 'nox' have a correlation of 0.73. Also high.
# - 'tax' and 'indus' have a correlation of 0.72.
# DIAGNOSTIC 2: Calculate VIF for our model
#VIF: calculates how much each regression coefficient is inflated due to co-linearity
vif_values <- vif(model)
print(vif_values)
barplot(vif_values, main = "VIF Values", horiz = FALSE, col = "steelblue",
        ylab = "VIF", las = 2) 
abline(h = 5, col = "red", lwd = 3, lty = 2) 
abline(h = 10, col = "darkred", lwd = 3, lty = 2)

#Demo of interaction terms 
summary(Boston[c("medv", "rm", "crim")])
p1 <- ggplot(Boston, aes(x = rm, y = medv)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Rooms vs. Home Value",
       x = "Average Number of Rooms",
       y = "Median Home Value ($1000s)") +
  theme_minimal()
p1
p2 <- ggplot(Boston, aes(x = crim, y = medv)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Crime Rate vs. Home Value",
       x = "Crime Rate (per capita)",
       y = "Median Home Value ($1000s)") +
  theme_minimal()
p2

# Model 1: Simple linear regression with rooms only
model1 <- lm(medv ~ rm, data = Boston)
summary(model1)

# Model 2: Multiple regression with rooms and crime
model2 <- lm(medv ~ rm + crim, data = Boston)
summary(model2)

# Interaction: Using the * operator (automatically includes main effects)
#The regression formula for this would be:
#Y = beta0 + beta1 * rm + beta2 * crim + beta3 *rm * crim
model3 <- lm(medv ~ rm * crim, data = Boston)
summary(model3)

stargazer(model1, model2, model3, 
          type = "text",
          title = "Comparison of Regression Models",
          dep.var.labels = "Median Home Value",
          covariate.labels = c("Rooms", "Crime", "Rooms × Crime", "Intercept"))

Boston <- Boston %>%
  mutate(crime_level = cut(crim, 
                           breaks = quantile(crim, probs = c(0, 0.33, 0.67, 1)),
                           labels = c("Low Crime", "Medium Crime", "High Crime")))
interaction_plot <- ggplot(Boston, aes(x = rm, y = medv, color = crime_level)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Interaction Effect: Rooms × Crime on Home Value",
       x = "Average Number of Rooms",
       y = "Median Home Value ($1000s)",
       color = "Crime Level") +
  theme_minimal() +
  scale_color_manual(values = c("green", "orange", "red"))
interaction_plot


#inspecting whether interaction is statistically significant
model_no_interaction <- lm(medv ~ rm + crim, data = Boston)
model_with_interaction <- lm(medv ~ rm * crim, data = Boston)

anova_test <- anova(model_no_interaction, model_with_interaction)
print(anova_test)













