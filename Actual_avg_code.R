df = read.csv("big_island1000.csv")
library(tidyverse)
library(tidyr)
library(dplyr)
library(purrr)
library(scales)
library(ggplot2)
df2 <- select(df, Date, Look_Ahead, Observed)
df2$Date <- as.Date(df2$Date, format("%m/%d/%y"))
df2 <- mutate(df2, month = format(Date, "%m"))
df2$month <- factor(df2$month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"),
                    labels = c("Jan", "Feb","Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
df2 <- filter(df2, Look_Ahead > 0 & Look_Ahead <= 4)
df2 <- filter(df2, Observed > 0)
df3 <- aggregate(Observed ~ Look_Ahead + month, df2, mean)
df4 <- df3 %>%
  spread(month, Observed)
df4  <- mutate(df4,Jun = (May + Jul)/ 2 )
df5 <- df4[c(1,2,3,4,5,6,13,7,8,9,10,11,12)]
write.csv(df5, file = "time_avg")
df6 <- gather(df4, month, Actual, 2:13)
write.csv(df6, file = "gather")
ggplot(df6, aes(x = Look_Ahead, y = Actual, col = month)) + geom_line() + geom_point() + 
  labs(title = "Actual Average by Look Ahead Time and Month", x = "Look Ahead", y = "Average Actual") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,4, by = 0.25)) + 
  scale_y_continuous(breaks = seq(34, 54, by = 2))
df_all <- merge(df2,df6, by = c("Look_Ahead", "month"))
df_all <- df_all %>%
  arrange(by = month) %>%
  arrange(by = Look_Ahead) %>%
  arrange(by = Date)
names(df_all)[5] <- "Avg_Actual"
names(df_all)[4] <- "Actual"   
df_all$Avg_Actual = round(df_all$Avg_Actual, 2)
df_all = mutate(df_all, error = Avg_Actual - Actual)
df_all = mutate(df_all, abs_error = abs(Avg_Actual - Actual))
df9 <- df_all[c(1,2,3,6,7)]
range(df9$abs_error)
range(df9$error)
summ <- df9 %>% 
  group_by(Look_Ahead) %>% 
  summarize(mean = mean(abs_error))

ggplot(df9, aes(x = Look_Ahead, y = error, group = Look_Ahead ))  + geom_boxplot() + 
  labs(title = "Boxplot Actual Average Error ", x = "Look Ahead", y = "Actual Average Error") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = seq(0,4, by = 0.25)) +
  scale_y_continuous(breaks = seq(-10,26, by = 2)) +
  theme(legend.position = "none") 

ggplot(df9, aes(x = Look_Ahead, y = abs_error, group = Look_Ahead ))  + geom_boxplot() + 
  labs(title = "Boxplot Actual Average Absolute Error ", x = "Look Ahead", y = "Actual Average Absolute Error") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = seq(0,4, by = 0.25)) +
  scale_y_continuous(breaks = seq(0,26, by = 2)) +
  theme(legend.position = "none") +
  geom_label(data = summ, aes(x = Look_Ahead, y = mean, 
                              label = paste("Mean: ", round(mean, 2))))
#######
############### Stats ################
df10 <- sum_stats(df9)
write.csv(df10, file = "both")
summary(df9$error)
summary(df9$abs_error)
df_error <- df9[c(1,2,3,4)]
df_abs_error <- df9[c(1,2,3,5)]

pct_stats <- function(x){
  probs <- c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90)
  by_month <- x %>%
    group_by(month) %>%
    summarise(pct = list(probs), value = list(quantile(abs_error, probs))) %>%
    unnest()
  df1 <- spread(by_month, month, value)
  df2 <- quantile(x$pct_50, probs)
  return(df1)
}
df_error2 <- pct_stats(df_error)
write.csv(df_error2, file = "errror2")
probs <- c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90)
quantile(df_error$error, probs)
df_abs_error2 <- pct_stats(df_abs_error)
write.csv(df_abs_error2, file = "abs_errror2")
quantile(df_abs_error$abs_error, probs)
#################################################
df11 <- select(df, Date, Look_Ahead, Observed, pct_50)
df11$Date <- as.Date(df11$Date, format("%m/%d/%y"))
df11 <- mutate(df11, month = format(Date, "%m"))
df11$month <- factor(df11$month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"),
                     labels = c("Jan", "Feb","Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
df11 <- filter(df11, Look_Ahead > 0 & Look_Ahead <= 4)
df11 <- filter(df11, Observed > 0 )
df11 <- filter(df11 , pct_50 > 0)
df11 <- mutate(df11, Error_forecast = pct_50 - Observed)
df12 <- merge(df11, df9, by = c("Date", "month","Look_Ahead"))
df12$Observed <- NULL
df12$pct_50 <- NULL
df12$abs_error <- NULL
write.csv(df12, file = "forecast_error_error")