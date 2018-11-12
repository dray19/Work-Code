library(tidyverse)
library(tidyr)
library(dplyr)
library(purrr)
library(scales)
setwd("~/Desktop")
df <- read.csv("bigsolar_1300.csv")
df$DateTime <- as.Date(df$DateTime, format = "%m/%d/%y")
df1 <- df %>%
  filter(Look_ahead >= 2 & Look_ahead <= 4.75)
na_df <- df1[!complete.cases(df1),]
colnames(df1)
df2 <- select(df1, DateTime, Look_ahead, pct_50, Actual)
df_avg <- aggregate(cbind(Actual,pct_50)  ~ DateTime, df1, mean)
df_zero <- df %>%
  select(DateTime,Look_ahead, Actual, pct_50) %>%
  filter(Look_ahead == 0.25) 
df3 <- merge(df_avg, df_zero, by = "DateTime")
df3$Look_ahead <- NULL
df3 <- mutate(df3, Actual = Actual.x - Actual.y)
df3 <- mutate(df3, pct_50 = pct_50.x - pct_50.y)
df4 <-select(df3, DateTime, Actual, pct_50)
df4 <- mutate(df4, month = format(DateTime, "%m"))
df4$month <- factor(df4$month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"),
                    labels = c("Jan", "Feb","Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
df4 <- filter(df4, Actual != 0)
df4 <- drop_na(df4)
df4$Actual_index <- ifelse(df4$Actual < -26.77, 1, ifelse(df4$Actual > -14.75,3,2))
df4$Forecasted_index <- ifelse(df4$pct_50 < -26.77, 1, ifelse(df4$pct_50 > -16.84,3,2))
df4 %>%
  group_by(Actual_index) %>%
  summarise(count = n()) %>%
  mutate(pct = count/309 * 100)

df4 %>%
  group_by(Forecasted_index) %>%
  summarise(count = n()) %>%
  mutate(pct = count/309 * 100)
df4$month <- NULL
write.csv(df4, file = "New_solor_index.csv")
df_actual <- df4 %>%
  group_by(month) %>%
  summarise(mean = mean(Actual),
            median = median(Actual), 
            SD = sd(Actual),
            max = max(Actual),
            min = min(Actual),
            `10%`=quantile(Actual, probs=0.10),
            `20%`=quantile(Actual, probs=0.20),
            `25%`=quantile(Actual, probs=0.25),
            `30%`=quantile(Actual, probs=0.30),
            `40%`=quantile(Actual, probs=0.40),
            `50%`=quantile(Actual, probs=0.5),
            `60%`=quantile(Actual, probs=0.60),
            `70%`=quantile(Actual, probs=0.70),
            `75%`=quantile(Actual, probs=0.75),
            `80%`=quantile(Actual, probs=0.80),
            `90%`=quantile(Actual, probs=0.90))
write.csv(df_actual, "actual_month.csv")
df_actual_all <- df4 %>%
  summarise(mean = mean(Actual),
            median = median(Actual), 
            SD = sd(Actual),
            max = max(Actual),
            min = min(Actual),
            `10%`=quantile(Actual, probs=0.10),
            `20%`=quantile(Actual, probs=0.20),
            `25%`=quantile(Actual, probs=0.25),
            `30%`=quantile(Actual, probs=0.30),
            `40%`=quantile(Actual, probs=0.40),
            `50%`=quantile(Actual, probs=0.5),
            `60%`=quantile(Actual, probs=0.60),
            `70%`=quantile(Actual, probs=0.70),
            `75%`=quantile(Actual, probs=0.75),
            `80%`=quantile(Actual, probs=0.80),
            `90%`=quantile(Actual, probs=0.90))
write.csv(df_actual, "actual_month.csv")
df_pct <- df4 %>%
  group_by(month) %>%
  summarise(mean = mean(pct_50),
            median = median(pct_50), 
            SD = sd(pct_50),
            max = max(pct_50),
            min = min(pct_50),
            `10%`=quantile(pct_50, probs=0.10),
            `20%`=quantile(pct_50, probs=0.20),
            `25%`=quantile(pct_50, probs=0.25),
            `30%`=quantile(pct_50, probs=0.30),
            `40%`=quantile(pct_50, probs=0.40),
            `50%`=quantile(pct_50, probs=0.5),
            `60%`=quantile(pct_50, probs=0.60),
            `70%`=quantile(pct_50, probs=0.70),
            `75%`=quantile(pct_50, probs=0.75),
            `80%`=quantile(pct_50, probs=0.80),
            `90%`=quantile(pct_50, probs=0.90))
write.csv(df_pct, "pct_month.csv")
df_pct_all <- df4 %>%
  summarise(mean = mean(pct_50),
            median = median(pct_50), 
            SD = sd(pct_50),
            max = max(pct_50),
            min = min(pct_50),
            `10%`=quantile(pct_50, probs=0.10),
            `20%`=quantile(pct_50, probs=0.20),
            `25%`=quantile(pct_50, probs=0.25),
            `30%`=quantile(pct_50, probs=0.30),
            `40%`=quantile(pct_50, probs=0.40),
            `50%`=quantile(pct_50, probs=0.5),
            `60%`=quantile(pct_50, probs=0.60),
            `70%`=quantile(pct_50, probs=0.70),
            `75%`=quantile(pct_50, probs=0.75),
            `80%`=quantile(pct_50, probs=0.80),
            `90%`=quantile(pct_50, probs=0.90))
##################################
range(df4$Actual)
range(df4$pct_50)
df4 <- filter(df4, Actual > -50)
ggplot(df4, aes(x = Actual)) + geom_density(fill = "red", alpha = 0.5) +
  labs(title = "Solar Avg Actual minus Look Ahead Time Zero for Each Day", y = "Density", x = "Avg Actual minus Actual at Look Ahead Zero") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(-35,0, by = 5))
ggsave("Density_Actual_solar.png", width = 13 , height = 8)

ggplot(df4, aes(x = Actual, fill = month)) + geom_density( alpha = 0.5) +
  labs(title = "Solar Avg Actual minus Look Ahead Time Zero for Each Day", y = "Density", x = "Avg Actual minus Actual at Look Ahead Zero") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(-35,0, by = 5)) + facet_wrap(~ month) + 
  theme(legend.position = "none")
ggsave("Density__month_solar.png", width = 13 , height = 8)


ggplot(df4, aes(x = pct_50)) + geom_density(fill = "darkgreen", alpha = 0.5) +
  labs(title = " Solar Avg Forecasted minus Look Ahead Time Zero for Each Day", y = "Density", x = "Avg Forecasted minus Forecasted at Look Ahead Zero") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(-35,0, by = 5))
ggsave("Density_Forecasted_solar.png", width = 13 , height = 8)


ggplot(df4, aes(x = Forecasted, fill = month)) + geom_density(alpha = 0.5) +
  labs(title = " Solar Avg Forecasted minus Look Ahead Time Zero for Each Day", y = "Density", x = "Avg Forecasted minus Forecasted at Look Ahead Zero") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(-35,0, by = 5)) + facet_wrap(~ month) + 
  theme(legend.position = "none")
ggsave("Density_Forecasted_month_solar.png", width = 13 , height = 8)

names(df4)[3] <- "Forecasted"
write.csv(df4, file = "solar.csv")
df5 <- gather(df4, type, value, -DateTime)
df5$value <-  as.numeric(df5$value)
str(df5)
df5 <- drop_na(df5)
range(df5$value)
df4 <- drop_na(df4)
df4[!complete.cases(df4),]

ggplot(df5, aes(x = value, fill = type)) + geom_density(alpha = 0.5) +
  labs(title = "Solar Actual vs. Solar Forecasted", y = "Density", x = "Value") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(-35,0, by = 5))
ggsave("Density_Forecasted_vs_Actual_solar.png", width = 13 , height = 8)

names(df4)[4] <- "Month"

ggplot(df4, aes(x = Forecasted, y = Actual, col = Month)) + geom_point() +
  labs(title = "Forecasted vs Actual Afternoon Solar Decrease Parameter (ASDP) for 2017", y = "Actual ASDP (MW)", x = "Forecasted ASDP (MW)") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(limits = c(-35,0), breaks = c(-35,-30,-25,-20,-15,-10,-5,0)) + 
  scale_y_continuous(limits = c(-35,0), breaks = c(-35,-30,-25,-20,-15,-10,-5,0)) + geom_abline(intercept = 0, slope = 1) + 
  geom_vline(xintercept = -26.8, col = "darkgreen", linetype="dashed") + geom_vline(xintercept = -14.7, col = "red", linetype="dashed") +
  geom_hline(yintercept = -26.8, col = "darkgreen", linetype="dashed") + geom_hline(yintercept = -14.7, col = "red", linetype="dashed") + 
  geom_text(aes(x = -7, y = -6, label = "Forecasted = Actual"), col = "black", angle = 30, vjust = 0.5) + theme(axis.text=element_text(size=15)) + 
  theme(axis.title.x = element_text(size = 15)) + theme(axis.title.y = element_text(size = 15)) + theme(title = element_text(size = 15)) + 
  theme(legend.text = element_text(size = 12)) + theme(legend.title = element_text(size = 12))
ggsave("Forecasted_vs_Actual_point_solar.png", width = 13 , height = 8, )


ggplot(df4, aes(x = Forecasted, y = Actual, col = month)) + geom_point() +
  labs(title = "Solar Forecasted Change vs Solar Actual Change", y = "Actual Change", x = "Forecasted Change") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(limits = c(-35,0), breaks = c(-35,-30,-25,-20,-15,-10,-5,0)) + 
  scale_y_continuous(limits = c(-35,0), breaks = c(-35,-30,-25,-20,-15,-10,-5,0)) + facet_wrap(~ month) + theme(legend.position = "none")
ggsave("Forecasted_vs_Actual_point_month_solar.png", width = 13 , height = 8)
#################################
df2 <- df %>%
  filter(Look_ahead > 0) %>%
  select(Date, Look_ahead,pct_50, Actual) %>%
  mutate(value = pct_50 - Actual)
df2 <- mutate(df2, month = format(Date, "%m"))
df2$month <- factor(df2$month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"),
                    labels = c("Jan", "Feb","Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
df2 <- drop_na(df2)
df_value <- df2 %>%
  group_by(month) %>%
  summarise(mean = mean(value),
            median = median(value), 
            SD = sd(value),
            max = max(value),
            min = min(value),
            `10%`=quantile(value, probs=0.10),
            `20%`=quantile(value, probs=0.20),
            `25%`=quantile(value, probs=0.25),
            `30%`=quantile(value, probs=0.30),
            `40%`=quantile(value, probs=0.40),
            `50%`=quantile(value, probs=0.5),
            `60%`=quantile(value, probs=0.60),
            `70%`=quantile(value, probs=0.70),
            `75%`=quantile(value, probs=0.75),
            `80%`=quantile(value, probs=0.80),
            `90%`=quantile(value, probs=0.90))
write.csv(df_value, "fore_minus_act.csv")
df_value_all <- df2 %>%
  summarise(mean = mean(value),
            median = median(value), 
            SD = sd(value),
            max = max(value),
            min = min(value),
            `10%`=quantile(value, probs=0.10),
            `20%`=quantile(value, probs=0.20),
            `25%`=quantile(value, probs=0.25),
            `30%`=quantile(value, probs=0.30),
            `40%`=quantile(value, probs=0.40),
            `50%`=quantile(value, probs=0.5),
            `60%`=quantile(value, probs=0.60),
            `70%`=quantile(value, probs=0.70),
            `75%`=quantile(value, probs=0.75),
            `80%`=quantile(value, probs=0.80),
            `90%`=quantile(value, probs=0.90))