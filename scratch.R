library(tidyverse)
library(glmnet)
library(caret)
library(elasticnet)

df <- as_tibble(read.table("winequality-red.csv", sep=";", header = TRUE))
summary(df$quality)

df_white <- as_tibble(read.table("winequality-white.csv", sep=";", header = TRUE))
summary(df_white$quality)

summary(df$fixed.acidity)
summary(df$sulphates)

data_red <- as.data.frame(scale(df))
summary(data_red$fixed.acidity)

data_white <- as.data.frame(scale(df_white))
summary(data_white$sulphates)

hist(df$quality,
     main="Histogram of Red Wine Quality Ratings", 
     xlab="Rating", 
     las=1, 
     breaks=4)

hist(df_white$quality,
     main="Histogram of White Wine Quality Ratings", 
     xlab="Rating", 
     las=1, 
     breaks=5)

ggplot(data_red, aes(x=fixed.acidity, y=quality)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(data_red, aes(x=volatile.acidity, y=quality)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(data_red, aes(x=citric.acid, y=quality)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(data_red, aes(x=residual.sugar, y=quality)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(data_red, aes(x=chlorides, y=quality)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(data_red, aes(x=free.sulfur.dioxide, y=quality)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(data_red, aes(x=total.sulfur.dioxide, y=quality)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(data_red, aes(x=density, y=quality)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(data_red, aes(x=pH, y=quality)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(data_red, aes(x=sulphates, y=quality)) + 
  geom_point()+
  geom_smooth(method=lm)

ggplot(data_red, aes(x=alcohol, y=quality)) + 
  geom_point()+
  geom_smooth(method=lm)

plot(data_red$fixed.acidity, data_red$quality)

corr_red <- as.data.frame(cor(data_red))
corr_red$quality








pairs(~fixed.acidity+volatile.acidity+citric.acid+quality, data=data_red)

longer_data <- data_red %>%
  pivot_longer(fixed.acidity:quality, names_to = "feature", values_to = "measurement")
print(longer_data)

data_red %>%
  pivot_longer(fixed.acidity:quality, names_to = "feature", values_to = "measurement") %>%
    ggplot(aes(x = measurement)) +
      geom_bar() +
      facet_wrap(vars(feature)) +
      labs(x = "Response (on a 1 to 5 scale)", y = "Number of respondents")







pivot_longer(Q1:Q6, names_to = "question", values_to = "response") %>%
  ggplot(aes(x = response)) +
  geom_bar() +
  facet_wrap(vars(question), ncol = 3) +
  labs(x = "Response (on a 1 to 5 scale)", y = "Number of respondents")


plot(data_red$fixed.acidity, data_red$quality)

corr <- cor(df)
corr_red <- cor(data_red)

pivot_longer()

plot(data_red)