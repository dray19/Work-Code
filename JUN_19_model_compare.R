library(tidyr)
library(dplyr)
library(chron)
library(ggplot2)
library(reshape2)
library(data.table)
library(lubridate)
library(tibble)
setwd("~/Desktop/jun18_model")
data <- lapply(dir(), read.csv)
model_fun <- function(x){
  df1 <- x
  df2 <- select(df1 ,Date, u.component_of_wind_.m.s..1.HYBL, v.component_of_wind_.m.s..1.HYBL, Type)
  df2 <- mutate(df2, wind_speed = sqrt((u.component_of_wind_.m.s..1.HYBL^2) + (v.component_of_wind_.m.s..1.HYBL^2)))
  df2$Date <- as.POSIXct(as.character(df1$Date), format =  "%Y%m%d%H%M")
  df2$Date <- df2$Date - 18000
  df3 <- mutate(df2, WRF = wind_speed * 1.93)
  df4 <- df3[, c("Type","Date" ,"WRF")]
  return(df4)
}

data_model <- lapply(data, model_fun)
df_model <- do.call("rbind", data_model)
#################
setwd("~/Desktop/mesonet_15min")
data_mn <- lapply(dir(), read.csv)
meso_fun <- function(x){
  df1 <- x
  df1$Date <- df1$Date <- as.POSIXct(as.character(df1$Date), format =  "%m/%d/%y %H:%M")
  df1$Date <- df1$Date - 18000
  date1 <- as.POSIXct("2017-06-19 07:00:00")
  date2 <- as.POSIXct("2017-06-20 01:00:00")
  int <- interval(date1, date2)
  df_all <- df1[df1$Date %within% int,]
  df10 <- df_all[,c(1,2,3,4,5)]
  df10$New_Date <- NULL
  df10 <- mutate(df10, knots_Avg = avg_wind_speed * 1.94384)
  df10 <- mutate(df10, knots_Max = max_wind_speed * 1.94384)
  df10$avg_wind_speed <- NULL
  df10$max_wind_speed <- NULL
  names(df10)[3] <- "avg_wind_speed"
  names(df10)[4] <- "max_wind_speed"
  return(df10)
}
data_mesonet  <- lapply(data_mn, meso_fun)
df_meso <- do.call("rbind", data_mesonet)
length(unique(df_meso$Type))
setwd("~/Desktop")
write.csv(df_meso, file = "MESONET_20170619_all_models.csv")
###############
setwd("~/Desktop/Aeso_15min")
data2 <- lapply(dir(), read.csv)
asos_fun <- function(x){
  df1 <- x
  df1$Date <- as.POSIXct(as.character(df1$Date), format =  "%m/%d/%y %H:%M")
  date1 <- as.POSIXct("2017-06-19 07:00:00")
  date2 <- as.POSIXct("2017-06-20 01:00:00")
  int <- interval(date1, date2)
  df10 <- df1[df1$Date %within% int,]
  return(df10)
}
data_aeso <- lapply(data2, asos_fun)
df_asos <- do.call("rbind", data_aeso)
length(unique(df_asos$Type))
setwd("~/Desktop")
write.csv(df_asos, file = "ASOS_20170619_all_models.csv")
df20 <- rbind(df_meso, df_asos)
write.csv(df20, file = "Jun19_all.csv")
#####################################################################
get_meso <- function(x){
  df11 <- filter(df_model, Type == x)
  df12  <- filter(df_meso, Type == x)
  df13 <- merge(df11, df12, by = c("Date", "Type"))
  return(df13)
}
ande <- get_meso("ANDE")
beac <- get_meso("BEAC")
bkln <- get_meso("BKLN")
brew <- get_meso("BREW")
clar <- get_meso("CLAR")
depo <- get_meso("DEPO")
dove <- get_meso("DOVE")
eldr <- get_meso("ELDR")
hfal <- get_meso("HFAL")
nbra <- get_meso("NBRA")
otis <- get_meso("OTIS")
quee <- get_meso("QUEE")
some <- get_meso("SOME")
stat <- get_meso("STAT")
suff <- get_meso("SUFF")
wall <- get_meso("WALL")
walt <- get_meso("WALT")
want <-  get_meso("WANT")
warw <- get_meso("WARW")
wbou <- get_meso("WBOU")
get_asos <- function(x){
  df11 <- filter(df_model, Type == x)
  df14  <- filter(df_asos, Type == x)
  df13 <- merge(df11, df14, by = c("Date", "Type"))
  return(df13)
}
k12n <- get_asos("K12N")
kbgm <- get_asos("KBGM")
kcdw <- get_asos("KCDW")
kewr <- get_asos("KEWR")
kfrg <- get_asos("KFRG")
kfwn <- get_asos("KFWN")
khpn <- get_asos("KHPN")
kjfk <- get_asos("KJFK")
klga <- get_asos("KLGA")
kmgj <- get_asos("KMGJ")
knyc <- get_asos("KNYC")
kpou <- get_asos("KPOU")
ksmq <- get_asos("KSMQ")
kteb <- get_asos("KTEB")
#######################
plot_set <- function(x){
  df15 <- x
  df17 <- gather(df15,Model, Value, 3:5)
  df17 <-  df17 %>%
    arrange(Date)
  return(df17)
}

w <- plot_set(kteb)
range(w$Value)

ggplot(w, aes(x = Date, y = Value, col = Model)) + geom_line() + 
  labs(title ="JUNE 19-20, 2017  Station: KTEB", x = "Time (Eastern Prevailing)", y ="Wind Speed (Knots)") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.x = element_text(size = 14)) + 
  scale_y_continuous(breaks = seq(0,38, by = 2))  +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20, face = "bold")) + 
  scale_x_datetime(limits =c(mdy_hms("2017-02-25 13:00:00"),mdy_hms("2017-05-26 07:00:00")),
                   breaks = "1 hour", date_labels = "%H:%M")
#################################

error_fun <- function(x,y){
  df20 <-  x
  df20 <-  mutate(df20, error = WRF - avg_wind_speed)
  df20 <- mutate(df20, abs_error = abs(error))
  df21 <- df20 %>%
    summarise(Avg_Error = mean(error),
              ABS_Avg_Error = mean(abs_error))
  df21$Type <- y
  return(df21)
}
df_1 <- error_fun(kteb, "KTEB")
df_2 <- rbind(df_2, df_1)
df_2

###############
setwd("~/Desktop")
write.csv(df_2, file = "20170619_avg_error.csv")
###############
setwd("~/Desktop")
dis <- read.csv("dis.csv")
df_all <-  rbind(df_meso, df_asos)
  df2 <- df_all[, c(1,2,4)]
  df3 <- df2 %>%
    spread(Type, max_wind_speed)
  df3$Date <- NULL
  df6 <- cor(df3, use="pairwise.complete.obs")
  df7 <- data.frame(df6)
  df7 <-  rownames_to_column(df7, "Models")
  df8 <- gather(df7, "Model_2", "Cor_max", 2:35)
  names(df8)[2] <- "Prim"
  names(df8)[1] <- "Second"
  df10 <- merge(df8, dis, by = c("Prim","Second"))
  
  df_avg <- df_all[, c(1,2,3)]
  df_avg2 <- df_avg %>%
    spread(Type, avg_wind_speed)
  df_avg2$Date <- NULL
  avg <- cor(df_avg2, use="pairwise.complete.obs")
  avg2 <- data.frame(avg)
  avg2 <-  rownames_to_column(avg2, "Models")
  avg3 <- gather(avg2, "Model_2","Cor_avg", 2:35)
  names(avg3)[2] <- "Prim"
  names(avg3)[1] <- "Second"
  df11 <- merge(df10, avg3, by = c("Prim","Second"))
  df_final <- df11[,c(1,2,3,11,4,5,6,7,8,9,10)]

write.csv(df_final, file = "cor_jun19.csv")
##########
setwd("~/Desktop")
dis <- read.csv("dis.csv")
df3 <- df_model %>%
  spread(Type, WRF)
df3$Date <- NULL
df6 <- cor(df3, use="pairwise.complete.obs")
df7 <- data.frame(df6)
df7 <-  rownames_to_column(df7, "Models")
df8 <- gather(df7, "Model_2", "Cor_WRF", 2:38)
names(df8)[2] <- "Prim"
names(df8)[1] <- "Second"
df10 <- merge(df8, dis, by = c("Prim","Second"))
write.csv(df10, file = "cor_wrf_jun19.csv")
