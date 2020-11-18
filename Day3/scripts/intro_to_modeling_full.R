## This script was developed by Michael F Meyer (michael.f.meyer@wsu.edu)
## for the Reproducible Research Techniques with R workshop. 
## The goal of this script is to serve as a tutorial for different 
## types of modeling. The script can be broken into a few main 
## sections: 
## 1. One-way Linear Modeling
## 2. Multiple Linear Regression
## 3. Non-linear Modeling
## 3.1 Polynomial Regression
## 3.2 General Additive Models
## 3.3 Logistic Regression

# Install necessary packages (for the entire script)
install.packages("tidyverse")
install.packages("mgcv")
install.packages("ploynom")
install.packages("hexbin")

library(tidyverse)
library(mgcv)
library(polynom)
library(rcompanion)

# Load and inspect the data

bike_data <- read.csv("../data/day.csv",
                      header = TRUE)

head(bike_data)

ggplot(bike_data, aes(atemp*50, (cnt))) +
  geom_point() +
  xlab("Feeling Temperature") +
  ylab("Total bikes rented")


# 1. One-way linear regression --------------------------------------------

# Create a subset of hte data 
bike_linear_data <- bike_data %>%
  mutate(feeling_temperature = atemp*50) %>%
  select(feeling_temperature, cnt)

# Build a linear model
linear_model <- lm(formula = cnt ~ feeling_temperature,
                   data = bike_linear_data)


# Assess the model
summary(linear_model)

# Assess model object structure
str(linear_model)

# Extract R-squared
r_squared <- summary(linear_model)$r.squared

# Extract p-value
p_value <- summary(linear_model)$coefficients[2,4]

# Extract slope
slope <- summary(linear_model)$coefficients[2,1]

# Extract y-intercept
y_intercept <- summary(linear_model)$coefficients[1,1]


# Challenge Number 1
summary(linear_model)$residuals
hist(summary(linear_model)$residuals)
median(summary(linear_model)$residuals)

residuals <- summary(linear_model)$residuals

challenge1 <- bike_linear_data %>%
  mutate(residuals)

ggplot(challenge1, aes(feeling_temperature, residuals)) +
  geom_point() +
  xlab("Feeling Temperature") +
  ylab("Residuals")


# Visualize residual deviation from the modeled predictions 3 ways

# 1. Visualize model and points 
ggplot(data = bike_linear_data,
       mapping = aes(feeling_temperature, cnt)) + 
  geom_point(alpha = 0.33) + 
  geom_smooth(method = "lm", se = TRUE) + theme_minimal()

# 2. Group points into hexbins
ggplot(data = bike_linear_data,
       mapping = aes(feeling_temperature,cnt)) +
  geom_hex(bins = 10)+ 
  geom_smooth(method = 'lm', se = TRUE) + 
  scale_fill_viridis_c(option = "plasma", name = "Number of \nPoints") +
  theme_minimal()
  

# 3. Lines and color points for residuals. 
# We will build this plot incrementally.
bike_linear_data$predicted_cnt <- predict(linear_model)

bike_linear_data$residuals <- residuals(linear_model)

head(bike_linear_data)

ggplot(data = bike_linear_data,
       mapping = aes(x = feeling_temperature, y = cnt)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_segment(mapping = aes(xend = feeling_temperature,
                             yend = predicted_cnt),
               alpha = 0.2) +
  geom_point(mapping = aes(color = abs(residuals))) +
  scale_color_viridis_c(option = "plasma", name = "Residual \nCount") + 
  theme_minimal()

## Challenge Number 2

prediction_data <- data.frame(feeling_temperature = seq(from = 0,
                                                        to = 50, 
                                                        by = 0.25))
head(prediction_data)

prediction_data$predicted_cnt <- predict(object = linear_model,
                                         newdata = prediction_data)

ggplot() + 
  geom_point(data = bike_linear_data, aes(feeling_temperature,
                                          cnt))+
  geom_point(data = prediction_data, aes(feeling_temperature,
                                         predicted_cnt),
             shape = 18, color = "blue")

# 2. Multiple Linear Regression -------------------------------------------

# Multiple continuous predictors

# Create a new dataset for multiple linear regression 
# with continuous variables. 
multiple_bike_data <- bike_data %>% 
  mutate(feeling_temperature = atemp * 50,
          humidity = hum * 100,
         windiness = windspeed * 67) %>% 
  select(feeling_temperature, humidity, windiness, cnt)

head(multiple_bike_data)


# First make a model where interactions are considered. 
mult_lm <- lm(formula = cnt ~ feeling_temperature * humidity * windiness,
              data = multiple_bike_data)


# Assess the pairs plot for cross-correlations
summary(mult_lm)



# Create a function to put correlation value in the upper panel.
# This function can be found in the documentation for pairs.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Rerun pairs with upper panel showing correlation values.
pairs(multiple_bike_data[ , 1:3], upper.panel = panel.cor)

# Second make a model where interactions are NOT considered. 
mult_lm_noint <- lm(formula = cnt ~ feeling_temperature + humidity + windiness,
                    data = multiple_bike_data)

summary(mult_lm_noint)

# Calculate the AIC of each linear model
# i.e., all interactions vs no interactions

AIC(mult_lm)
AIC(mult_lm_noint)


## Challenge Number 3
mult_lm_chal <- lm(formula = cnt ~ feeling_temperature + humidity*windiness - windiness,
                    data = multiple_bike_data)
summary(mult_lm_chal)
AIC(mult_lm_chal)

# Mutliple linear regression with continuous and categorical predictors

# Create data subset
mixed_bike_data <- bike_data %>%
  mutate(feeling_temperature = atemp * 50,
         humidity = hum * 100,
         windiness = windspeed * 67,
         weather_type = ifelse(weathersit == 1, "Clear", NA),
         weather_type = ifelse(weathersit == 2, "Misty_Cloudy", weather_type),
         weather_type = ifelse(weathersit == 3, "Light_precip", weather_type),
         weather_type = ifelse(weathersit == 4, "Heavy_precip", weather_type),
         weather_type = as.factor(weather_type)) %>%
  select(feeling_temperature, humidity, windiness, weather_type, cnt)

# Check to be sure that there are no NAs
summary(mixed_bike_data)

# Build the model

mixed_type_model <- lm(cnt ~ feeling_temperature * humidity * windiness * weather_type,
                       data =mixed_bike_data)
summary(mixed_type_model)


# 3. Non-Linear Modeling --------------------------------------------------


# 3.1 Polynomial regression -----------------------------------------------

# Create a special dataset
bike_nonlinear_data <- bike_data %>%
  mutate(feeling_temperature = atemp*50) %>%
  select(feeling_temperature, cnt)

# Remind ourselves what these data looked like
ggplot(bike_nonlinear_data, aes(feeling_temperature,cnt))+geom_point()

# Build the model
poly_bike_model <- lm(cnt ~ I(feeling_temperature^3),
                      data = bike_nonlinear_data)

summary(poly_bike_model)
# To build a polynomial equation without second and first order 
# terms, followup this workflow:




# Build the model



# 3.2 General Additive Models ---------------------------------------------

# Build the model 
# We will rerun this same R code multiple times using different 
# values for gamma. 
# Remember: Gamma is like a wiggliness factor (per Gavin Simpson),
# where gamma closer to zero is very wiggly, and gamma greater than 1
# less wiggly. 

bike_gam_model <- gam(cnt ~ s(feeling_temperature),
                      data = bike_nonlinear_data,
                      method = "REML", gamma = 10)

summary(bike_gam_model)

plot(bike_gam_model)
# 3.3 Logistic Regresssion ------------------------------------------------

# First look at some data that we will be modeling
ggplot(bike_data, aes(registered)) +
  geom_histogram() +
  facet_grid(~workingday)

ggplot(bike_data, aes(registered)) +
  geom_density() +
  facet_grid(~workingday)

bike_data %>%
  group_by(workingday) %>%
  summarize(mean_registered = mean(registered))

# Build the model 
logistic_model <- glm(formula = workingday ~ registered,
                      data = bike_data, family = binomial())
summary(logistic_model)

## Challenge 4
prediction_data <- data.frame(registered = seq(from = 0, to = 7000, by = 10))

prediction_data$predicted_cnt <- predict(object = logistic_model,
                                         newdata = prediction_data,
                                         type = "response")
ggplot() + 
  geom_point(data = bike_data, aes(registered, (workingday)))+
  geom_point(data = prediction_data, aes(registered,
                                         predicted_cnt),
             shape = 18, color = "blue") +
  xlab("Is it a working day?") + 
  ylab("Registered Bike Registrations")
