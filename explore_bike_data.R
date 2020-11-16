# Explore Bike Data to see if there's a relationship

# Note: Incomplete due to helper duties

#### Load DAta ----
library(tidyverse)
library(tidyselect)
df <- read_csv("data/daily_bike_data.csv")
df


# Time Trend of ridership ----
ggplot(data = df) +
  geom_line(aes(x = dteday, y = cnt))


# Relationship between ridership and temperature
ggplot(data = df,aes( x = temp, y = cnt)) + 
  geom_point()+
  geom_smooth()

# What is weathersit?
summary(df$weathersit)
unique(df$weathersit)

# dplyr 

df2 <- df %>% 
  dplyr:: mutate(
    weather_fac = factor(weathersit,
                         levels = c(1,2,3,4),
                         labels = c("Clear", "Cloudy", "Rainy", "Heavy Rain"))
  )
df2

df2 %>% dplyr::select(dteday, weathersit, weather_fac)

df2 %>%  dplyr::filter(weather_fac == "Rainy") %>% 
  ggplot(aes(x = temp, y = cnt)) +
  geom_point() +
  geom_smooth()

#### Transforming Data with Tidyr ----

df2 %>% 
  dplyr::select(mnth, temp, season) %>% 
  dplyr::group_by(season,mnth) %>% 
  dplyr::summarize(temp_mean = mean(temp)) %>% 
  dplyr::select(-season)
  tidyr::pivot_wider(names_prefix = "temp_", names_from = mnth,
                     values_from = temp)
  
df2_a <- df2 %>%  dplyr::select(season, mnth, temp)
df2_b <- df2_a %>% 
  dplyr::group_by(season, mnth) %>% 
  dplyr::summarize(temp_mean = mean(temp)) %>% 
  dplyr:: ungroup() %>% 
  dplyr:: select(-season)

df2_b # Need to include year!
months = c("January", "February", "March","April",
           "May","June","July","August", "September", "October",
           "November","December")
df_wide <-df2 %>% 
  dplyr::mutate(mnth = factor(mnth, levels = months,
                              labels = months)) %>% 
  dplyr::rename(year = yr) %>% 
  dplyr::select(year, mnth, temp) %>% 
  dplyr::group_by(year, mnth) %>% 
  dplyr::summarize(temp_mean = mean(temp)) %>% 
  tidyr:: pivot_wider(names_prefix = "temp", names_from = mnth,
                      values_from = temp_mean) %>% 
  dplyr::rename_with(tolower)



# Transform to create separate temp variables for each month
months <- c("January", "February", "March", "April", "May", "June", "July", "August","September", "October", "November", "December")
df_wide <- df2 %>%
  dplyr::mutate(mnth = factor(mnth, levels = months, labels = months))%>%
  dplyr::rename(year = yr) %>%
  dplyr::select(year, mnth, temp) %>%
  dplyr::group_by(year, mnth) %>%
  dplyr::summarize(temp_mean = mean(temp)) %>% 
tidyr::pivot_wider(names_prefix = "temp_", names_from = mnth, values_from = temp_mean)

df_long <- df2 %>% 
  tidyr::pivot_longer(cols = c(temp, atemp, hum, windspeed),
                      values_to = "values", names_to = "variable")

df_wide2 <- df_long %>% 
  tidyr:: pivot_wider(names_prefix = "v_", names_from = variable, 
                      values_from = values)


## Plotting with a longer data frame
ggplot(data = df2, aes(x = temp, y = cnt)) +
  geom_point(shape = 21) + geom_smooth(method = "lm", color = "steelblue") +
  facet_wrap(~ weather_fac, scales = "free_y")+
  theme_linedraw() + labs(title = "SICK", x = "ENTROPY", y = "BIKES")+
  theme(strip.text = element_text(size = 14,face = "bold"))

ggplot(data = df_long, aes(x = values, y = cnt, color = variable)) +
  geom_point(size = .8)+ 
  geom_smooth(se = FALSE)+ facet_wrap(~weather_fac)


