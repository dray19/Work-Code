library(purrr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(chron)
setwd("~/Desktop")
df <- read.csv("clean_data.csv")
df$X <- NULL
str(df)
df1 <- df
df1 <- drop_na(df1)
df1 <- separate(data = df, col = time, into = c("Date", "Time"), sep = " ")
df1$Date <- as.Date(df1$Date, format = "%Y-%m-%d")
df1 <- mutate(df1, month = format(Date, "%m"))
df1$month <- factor(df1$month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"),
                    labels = c("Jan", "Feb","Mar", "Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
df1 <- mutate(df1, Year = format(Date, "%Y"))
df2 <- df1 %>%
  filter(avg_wind_speed_2min <= 75) %>%
  filter(max_wind_speed_5sec <= 75)
range(df2$avg_wind_speed_2min)
range(df2$max_wind_speed_5sec)
df2 <- filter(df2, month != "NA")
df_month_max <- df2 %>%
  group_by(month) %>%
  summarise(mean = mean(max_wind_speed_5sec),
            median = median(max_wind_speed_5sec), 
            SD = sd(max_wind_speed_5sec),
            max = max(max_wind_speed_5sec),
            min = min(max_wind_speed_5sec),
            `10%`=quantile(max_wind_speed_5sec, probs=0.10),
            `20%`=quantile(max_wind_speed_5sec, probs=0.20),
            `25%`=quantile(max_wind_speed_5sec, probs=0.25),
            `30%`=quantile(max_wind_speed_5sec, probs=0.30),
            `40%`=quantile(max_wind_speed_5sec, probs=0.40),
            `50%`=quantile(max_wind_speed_5sec, probs=0.5),
            `60%`=quantile(max_wind_speed_5sec, probs=0.60),
            `70%`=quantile(max_wind_speed_5sec, probs=0.70),
            `75%`=quantile(max_wind_speed_5sec, probs=0.75),
            `80%`=quantile(max_wind_speed_5sec, probs=0.80),
            `90%`=quantile(max_wind_speed_5sec, probs=0.90))
write.csv(df_month_max, "stats_max_wind_speed_5sec_month.csv")
df_year_max <- df2 %>%
  group_by(Year) %>%
  summarise(mean = mean(max_wind_speed_5sec),
            median = median(max_wind_speed_5sec), 
            SD = sd(max_wind_speed_5sec),
            max = max(max_wind_speed_5sec),
            min = min(max_wind_speed_5sec),
            `10%`=quantile(max_wind_speed_5sec, probs=0.10),
            `20%`=quantile(max_wind_speed_5sec, probs=0.20),
            `25%`=quantile(max_wind_speed_5sec, probs=0.25),
            `30%`=quantile(max_wind_speed_5sec, probs=0.30),
            `40%`=quantile(max_wind_speed_5sec, probs=0.40),
            `50%`=quantile(max_wind_speed_5sec, probs=0.5),
            `60%`=quantile(max_wind_speed_5sec, probs=0.60),
            `70%`=quantile(max_wind_speed_5sec, probs=0.70),
            `75%`=quantile(max_wind_speed_5sec, probs=0.75),
            `80%`=quantile(max_wind_speed_5sec, probs=0.80),
            `90%`=quantile(max_wind_speed_5sec, probs=0.90))
write.csv(df_year_max, "stats_max_wind_speed_5sec_year.csv")
total_max <- df2 %>%
  summarise(mean = mean(max_wind_speed_5sec),
            median = median(max_wind_speed_5sec), 
            SD = sd(max_wind_speed_5sec),
            max = max(max_wind_speed_5sec),
            min = min(max_wind_speed_5sec),
            `10%`=quantile(max_wind_speed_5sec, probs=0.10),
            `20%`=quantile(max_wind_speed_5sec, probs=0.20),
            `25%`=quantile(max_wind_speed_5sec, probs=0.25),
            `30%`=quantile(max_wind_speed_5sec, probs=0.30),
            `40%`=quantile(max_wind_speed_5sec, probs=0.40),
            `50%`=quantile(max_wind_speed_5sec, probs=0.5),
            `60%`=quantile(max_wind_speed_5sec, probs=0.60),
            `70%`=quantile(max_wind_speed_5sec, probs=0.70),
            `75%`=quantile(max_wind_speed_5sec, probs=0.75),
            `80%`=quantile(max_wind_speed_5sec, probs=0.80),
            `90%`=quantile(max_wind_speed_5sec, probs=0.90))
write.csv(total_max, "total_max.csv")
#############################
df_month_avg <- df2 %>%
  group_by(month) %>%
  summarise(mean = mean(avg_wind_speed_2min),
            median = median(avg_wind_speed_2min), 
            SD = sd(avg_wind_speed_2min),
            max = max(avg_wind_speed_2min),
            min = min(avg_wind_speed_2min),
            `10%`=quantile(avg_wind_speed_2min, probs=0.10),
            `20%`=quantile(avg_wind_speed_2min, probs=0.20),
            `25%`=quantile(avg_wind_speed_2min, probs=0.25),
            `30%`=quantile(avg_wind_speed_2min, probs=0.30),
            `40%`=quantile(avg_wind_speed_2min, probs=0.40),
            `50%`=quantile(avg_wind_speed_2min, probs=0.5),
            `60%`=quantile(avg_wind_speed_2min, probs=0.60),
            `70%`=quantile(avg_wind_speed_2min, probs=0.70),
            `75%`=quantile(avg_wind_speed_2min, probs=0.75),
            `80%`=quantile(avg_wind_speed_2min, probs=0.80),
            `90%`=quantile(avg_wind_speed_2min, probs=0.90))
write.csv(df_month_avg, "stats_avg_wind_speed_2min_month.csv")
df_year_avg <-df2 %>%
  group_by(Year) %>%
  summarise(mean = mean(avg_wind_speed_2min),
            median = median(avg_wind_speed_2min), 
            SD = sd(avg_wind_speed_2min),
            max = max(avg_wind_speed_2min),
            min = min(avg_wind_speed_2min),
            `10%`=quantile(avg_wind_speed_2min, probs=0.10),
            `20%`=quantile(avg_wind_speed_2min, probs=0.20),
            `25%`=quantile(avg_wind_speed_2min, probs=0.25),
            `30%`=quantile(avg_wind_speed_2min, probs=0.30),
            `40%`=quantile(avg_wind_speed_2min, probs=0.40),
            `50%`=quantile(avg_wind_speed_2min, probs=0.5),
            `60%`=quantile(avg_wind_speed_2min, probs=0.60),
            `70%`=quantile(avg_wind_speed_2min, probs=0.70),
            `75%`=quantile(avg_wind_speed_2min, probs=0.75),
            `80%`=quantile(avg_wind_speed_2min, probs=0.80),
            `90%`=quantile(avg_wind_speed_2min, probs=0.90))
write.csv(df_year_avg, "stats_avg_wind_speed_2min_year.csv")
total_avg <- df2 %>%
  summarise(mean = mean(avg_wind_speed_2min),
            median = median(avg_wind_speed_2min), 
            SD = sd(avg_wind_speed_2min),
            max = max(avg_wind_speed_2min),
            min = min(avg_wind_speed_2min),
            `10%`=quantile(avg_wind_speed_2min, probs=0.10),
            `20%`=quantile(avg_wind_speed_2min, probs=0.20),
            `25%`=quantile(avg_wind_speed_2min, probs=0.25),
            `30%`=quantile(avg_wind_speed_2min, probs=0.30),
            `40%`=quantile(avg_wind_speed_2min, probs=0.40),
            `50%`=quantile(avg_wind_speed_2min, probs=0.5),
            `60%`=quantile(avg_wind_speed_2min, probs=0.60),
            `70%`=quantile(avg_wind_speed_2min, probs=0.70),
            `75%`=quantile(avg_wind_speed_2min, probs=0.75),
            `80%`=quantile(avg_wind_speed_2min, probs=0.80),
            `90%`=quantile(avg_wind_speed_2min, probs=0.90))

ggplot(df17, aes(x = avg_wind_speed_2min, y = max_wind_speed_5sec)) + geom_point(col = "red") + 
  labs(title = "KTEB Avg Wind Speed 2min vs. Max Wind Speed 5sec 2017 ",
       x = "Avg Wind Speed 2min", y = "Max Wind Speed 5sec") + theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = seq(0,75, by= 5)) + scale_y_continuous(breaks = seq(0,75, by= 5))
ggsave("KTEB.png", width = 13 , height = 8)




df3 <- separate(data = df2, col = Time, into = c("Hour", "Min", "Sec"), sep = ":")
df17 <- filter(df3, Year == "2017")
df17$Min <-  as.numeric(df17$Min)
str(df17)
df17$Bin <- cut(df17$Min, breaks = c(-1,5,10,15,20,25,30,35,40,45,50,55,59), labels = c("0,5", "5,10", "10,15", "15,20", "20,25", "25,30", "30,35","35,40", "40,45", "45,50","50,55","55,59"))
dfmax <- select(df17,Date, Hour, Min, max_wind_dir_5sec, max_wind_speed_5sec, Bin)
dfavg <- select(df17, Date, Hour, Min, avg_wind_dir_2min, avg_wind_speed_2min,Bin)
dfmax2 <- dfmax %>%
  group_by(Date, Hour, Bin) %>%
  slice(which.max(max_wind_speed_5sec))

dfavg2 <- dfavg %>%
  group_by(Date, Hour, Bin) %>%
  slice(which.max(avg_wind_speed_2min))
names(dfavg2)[3] <- "Avg_Min"
names(dfmax2)[3] <- "Max_Min"
df_all <- merge(dfmax2, dfavg2, by= c("Date","Hour", "Bin"))
df_all <- df_all %>%
  arrange(Date, Hour, Max_Min)
df_all$Location <- "KTEB" 
df_all <- select(df_all,Location, Date, Hour, Bin, Max_Min, max_wind_dir_5sec, max_wind_speed_5sec, Avg_Min, avg_wind_dir_2min, avg_wind_speed_2min)
write.csv(df_all, file = "KFRG_5min_wind_2017.csv")
top30 <- df_all %>%
  group_by(Date) %>%
  slice(which.max(max_wind_speed_5sec))
df_30 <- head(arrange(top30,desc(max_wind_speed_5sec)), n = 30)
write.csv(df_30, file = "top30_max_wind_speed_5sec.csv")




ggplot(df2, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "red") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "Total Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))


df_month <- df2 %>%
  filter(month == "Jan")
ggplot(df_month, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "red") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "January Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))

df_month <- df2 %>%
  filter(month == "Feb")
ggplot(df_month, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "red") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "February Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("Feb.png", width = 13 , height = 8)

df_month <- df2 %>%
  filter(month == "Mar")
ggplot(df_month, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "red") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "March Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("March.png", width = 13 , height = 8)

df_month <- df2 %>%
  filter(month == "Apr")
ggplot(df_month, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "red") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "April Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("Apr.png", width = 13 , height = 8)

df_month <- df2 %>%
  filter(month == "May")
ggplot(df_month, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "red") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "May Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("May.png", width = 13 , height = 8)


df_month <- df2 %>%
  filter(month == "Jun")
ggplot(df_month, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "red") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "June Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("June.png", width = 13 , height = 8)

df_month <- df2 %>%
  filter(month == "Jul")
ggplot(df_month, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "red") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "July Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("July.png", width = 13 , height = 8)

df_month <- df2 %>%
  filter(month == "Aug")
ggplot(df_month, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "red") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "August Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("Aug.png", width = 13 , height = 8)


df_month <- df2 %>%
  filter(month == "Sep")
ggplot(df_month, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "red") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "September Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("Sep.png", width = 13 , height = 8)

df_month <- df2 %>%
  filter(month == "Oct")
ggplot(df_month, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "red") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "October Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("Oct.png", width = 13 , height = 8)

df_month <- df2 %>%
  filter(month == "Nov")
ggplot(df_month, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "red") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "November Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("Nov.png", width = 13 , height = 8)


df_month <- df2 %>%
  filter(month == "Dec")
ggplot(df_month, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "red") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "December Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("Dec.png", width = 13 , height = 8)
#################################
df_year <- df2 %>%
  filter(Year == "2005")
ggplot(df_year, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "blue") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2005 Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("2005.png", width = 13 , height = 8)


df_year <- df2 %>%
  filter(Year == "2006")
ggplot(df_year, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "blue") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2006 Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("2006.png", width = 13 , height = 8)

df_year <- df2 %>%
  filter(Year == "2007")
ggplot(df_year, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "blue") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2007 Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("2007.png", width = 13 , height = 8)

df_year <- df2 %>%
  filter(Year == "2008")
ggplot(df_year, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "blue") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2008 Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("2008.png", width = 13 , height = 8)

df_year <- df2 %>%
  filter(Year == "2009")
ggplot(df_year, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "blue") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2009 Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("2009.png", width = 13 , height = 8)


df_year <- df2 %>%
  filter(Year == "2010")
ggplot(df_year, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "blue") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2010 Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("2010.png", width = 13 , height = 8)

df_year <- df2 %>%
  filter(Year == "2011")
ggplot(df_year, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "blue") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2011 Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("2011.png", width = 13 , height = 8)

df_year <- df2 %>%
  filter(Year == "2012")
ggplot(df_year, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "blue") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2012 Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("2012.png", width = 13 , height = 8)

df_year <- df2 %>%
  filter(Year == "2013")
ggplot(df_year, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "blue") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2013 Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("2013.png", width = 13 , height = 8)


df_year <- df2 %>%
  filter(Year == "2014")
ggplot(df_year, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "blue") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2014 Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("2014.png", width = 13 , height = 8)

df_year <- df2 %>%
  filter(Year == "2015")
ggplot(df_year, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "blue") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2015 Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("2015.png", width = 13 , height = 8)

df_year <- df2 %>%
  filter(Year == "2016")
ggplot(df_year, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "blue") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2016 Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("2016.png", width = 13 , height = 8)

df_year <- df2 %>%
  filter(Year == "2017")
ggplot(df_year, aes(x = avg_wind_speed_2min)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "blue") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2017 Average Wind Speed 2 min", x = "Average Wind Speed 2 min", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,65, by = 5))
ggsave("2017.png", width = 13 , height = 8)

###########################
ggplot(df2, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "darkgreen") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "Total Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("total.png", width = 13 , height = 8)

df_month <- df2 %>%
  filter(month == "Jan")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "darkgreen") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "January Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("Jan.png", width = 13 , height = 8)

df_month <- df2 %>%
  filter(month == "Feb")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "darkgreen") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "February Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("Feb.png", width = 13 , height = 8)

df_month <- df2 %>%
  filter(month == "Mar")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "darkgreen") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "March Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("Mar.png", width = 13 , height = 8)

df_month <- df2 %>%
  filter(month == "Apr")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "darkgreen") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "April Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("Apr.png", width = 13 , height = 8)

df_month <- df2 %>%
  filter(month == "May")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "darkgreen") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "May Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("May.png", width = 13 , height = 8)

df_month <- df2 %>%
  filter(month == "Jun")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "darkgreen") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "June Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("June.png", width = 13 , height = 8)

df_month <- df2 %>%
  filter(month == "Jul")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "darkgreen") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "July Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("July.png", width = 13 , height = 8)

df_month <- df2 %>%
  filter(month == "Aug")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "darkgreen") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "August Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("Aug.png", width = 13 , height = 8)

df_month <- df2 %>%
  filter(month == "Sep")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "darkgreen") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "September Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("Sep.png", width = 13 , height = 8)

df_month <- df2 %>%
  filter(month == "Oct")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "darkgreen") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "October Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("Oct.png", width = 13 , height = 8)

df_month <- df2 %>%
  filter(month == "Nov")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "darkgreen") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "November Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("Nov.png", width = 13 , height = 8)

df_month <- df2 %>%
  filter(month == "Dec")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "darkgreen") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "December Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("Dec.png", width = 13 , height = 8)

###############################

df_year <- df2 %>%
  filter(Year == "2005")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "orange") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2005 Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("2005.png", width = 13 , height = 8)

df_year <- df2 %>%
  filter(Year == "2006")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "orange") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2006 Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("2006.png", width = 13 , height = 8)

df_year <- df2 %>%
  filter(Year == "2007")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "orange") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2007 Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("2007.png", width = 13 , height = 8)

df_year <- df2 %>%
  filter(Year == "2008")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "orange") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2008 Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("2008.png", width = 13 , height = 8)

df_year <- df2 %>%
  filter(Year == "2009")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "orange") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2009 Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("2009.png", width = 13 , height = 8)

df_year <- df2 %>%
  filter(Year == "2010")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "orange") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2010 Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("2010.png", width = 13 , height = 8)

df_year <- df2 %>%
  filter(Year == "2011")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "orange") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2011 Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("2011.png", width = 13 , height = 8)

df_year <- df2 %>%
  filter(Year == "2012")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "orange") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2012 Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("2012.png", width = 13 , height = 8)

df_year <- df2 %>%
  filter(Year == "2013")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "orange") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2013 Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("2013.png", width = 13 , height = 8)

df_year <- df2 %>%
  filter(Year == "2014")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "orange") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2014 Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("2014.png", width = 13 , height = 8)

df_year <- df2 %>%
  filter(Year == "2015")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "orange") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2015 Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("2015.png", width = 13 , height = 8)

df_year <- df2 %>%
  filter(Year == "2016")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "orange") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2016 Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("2016.png", width = 13 , height = 8)

df_year <- df2 %>%
  filter(Year == "2017")
ggplot(df_month, aes(x = max_wind_speed_5sec)) + geom_histogram(aes(y = ..density..),bins = 25, col = "black", fill = "orange") +
  theme(axis.line = element_line(size=1, colour = "black")) + labs(title = "2017 Max Wind Speed 5 sec", x = "Max Wind Speed 5 sec", y = "Count") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks = seq(0,75, by = 5))
ggsave("2017.png", width = 13 , height = 8)



