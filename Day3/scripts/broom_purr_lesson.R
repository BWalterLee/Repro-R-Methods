# Nick Potter broom/purr lesson Day 3 part 2
library(broom)


bike_data <- read.csv("../data/day.csv",
                      header = TRUE)

bd2 <- bike_data %>% 
  dplyr::mutate(weather_type = factor(weathersit, 
                                      levels = c(1,2,3,4),
                                      labels = c("Clear","Cloudy", "Rainy", "Snowy")))


m = lm(cnt ~ season + workingday + weather_type + temp, data = bd2)
summary(m)

broom::tidy(m) # dataframe of estimates and summary stats

broom::glance(m) # df of model summary statistics

broom:: augment(m) # df of individual residuals per point

ggplot(broom::tidy(m)) +
  geom_point(aes(x = term, y = estimate)) + 
  geom_errorbar(aes(x = term, 
                    ymin = estimate- std.error, ymax = estimate + std.error), 
                width = .3)

install.packages("purrr")
library(purrr)
summary(bd2$temp)

bd3 <- bd2 %>% 
  dplyr::mutate(temp_bin = cut(temp,
                              breaks = c(0, seq(0.2, 0.9, by = 0.1)),
                              labels = seq(0.2,0.9, by = 0.1))) 

bd3 %>%   dplyr::select(temp, temp_bin)

bd4 <- bd3 %>% 
  tidyr::nest(data = -temp_bin) %>% 
  dplyr::mutate(model = purrr::map(data, ~lm(cnt ~ temp, .x))) %>% 
  dplyr::mutate(tidy_model = purrr::map(model, broom::tidy)) %>% 
  tidyr::unnest(tidy_model) %>% 
  dplyr::arrange(temp_bin)

ggplot(bd4 %>% filter(term == "temp")) + 
  geom_hline(aes(yintercept = 0), color = "gray50")+
  geom_point(aes(x = temp_bin, y = estimate)) +
  geom_errorbar(aes(x = temp_bin, 
                    ymin = estimate- std.error, ymax = estimate + std.error), 
                width = .3) 

ggplot(bd3) + 
  geom_point(aes(x = temp, y= cnt)) +
  geom_smooth(aes(x = temp, y = cnt, group = temp_bin), method = "lm")

#### purrr::reduce
temp <- bike_data %>%  select(dteday, temp)
atemp <- bike_data %>%  select(dteday, atemp)
hum <- bike_data %>%  select(dteday, hum)

merge(temp,atemp, by = "dteday")

purrr::reduce(
  list(temp,atemp,hum),
  function(a,b) {merge(a,b, by = "dteday")}  # Way to combine everything!
)
