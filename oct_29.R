library(tidyr)
library(dplyr)
library(chron)
library(ggplot2)
library(reshape2)
library(data.table)
library(lubridate)
library(tibble)
setwd("~/Desktop")
data <- read.csv("model_Oct_29.csv")
  df1 <- data
  df1$X <- NULL
  str(df1)
  df1$Date <- as.POSIXct(as.character(df1$Date), format =  "%Y-%m-%d %H:%M:%S")
  df1$Date <- df1$Date - 18000

#################
setwd("~/Desktop/mesonet_15min")
data_mn <- lapply(dir(), read.csv)
meso_fun <- function(x){
  df1 <- x
  df1$Date <- df1$Date <- as.POSIXct(as.character(df1$Date), format =  "%m/%d/%y %H:%M")
  df1$Date <- df1$Date - 18000
  date1 <- as.POSIXct("2017-10-29 13:00:00")
  date2 <- as.POSIXct("2017-10-30 07:00:00")
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
write.csv(df_meso, file = "MESONET_20171029_all_models.csv")
###############
setwd("~/Desktop/Aeso_15min")
data2 <- lapply(dir(), read.csv)
asos_fun <- function(x){
  df1 <- x
  df1$Date <- as.POSIXct(as.character(df1$Date), format =  "%m/%d/%y %H:%M")
  date1 <- as.POSIXct("2017-10-29 13:00:00")
  date2 <- as.POSIXct("2017-10-30 07:00:00")
  int <- interval(date1, date2)
  df10 <- df1[df1$Date %within% int,]
  return(df10)
}
data_aeso <- lapply(data2, asos_fun)
df_asos <- do.call("rbind", data_aeso)
length(unique(df_asos$Type))
setwd("~/Desktop")
write.csv(df_asos, file = "ASOS_20171029_all_models.csv")
df20 <- rbind(df_meso, df_asos)
write.csv(df20, file = "Oct29_all.csv")
#####################################################################
get_meso <- function(x){
  df11 <- filter(df1, Type == x)
  df12  <- filter(df_meso, Type == x)
  df13 <- merge(df11, df12, by = c("Date", "Type"))
  return(df13)
}
ande <- get_meso("ANDE")
beac <- get_meso("BEAC")
#bkln <- get_meso("BKLN")
#brew <- get_meso("BREW")
clar <- get_meso("CLAR")
depo <- get_meso("DEPO")
dove <- get_meso("DOVE")
eldr <- get_meso("ELDR")
hfal <- get_meso("HFAL")
nbra <- get_meso("NBRA")
otis <- get_meso("OTIS")
#quee <- get_meso("QUEE")
some <- get_meso("SOME")
#stat <- get_meso("STAT")
suff <- get_meso("SUFF")
wall <- get_meso("WALL")
walt <- get_meso("WALT")
want <-  get_meso("WANT")
warw <- get_meso("WARW")
wbou <- get_meso("WBOU")
get_asos <- function(x){
  df11 <- filter(df1, Type == x)
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
df20 <- rbind(df_asos, df_meso)
df12 <- merge(df1, df20, by = c("Date", "Type"))
df12 <- arrange(df12, Type)
df17 <- gather(df12,Model, Value, 3:5)
range(df17$Value)
gg_fun <- function(parameter, dt){
  plot_z <- ggplot(df17[df17$Type == parameter,], aes(x = Date, y = Value, col = Model)) + geom_line() + 
   ggtitle(paste0("October 29, 2017 Station:", parameter)) +  labs( x = "Time (Eastern Prevailing)", y ="Wind Speed (Knots)") + 
    theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.x = element_text(size = 14)) + 
    scale_y_continuous(limits = c(0,50), breaks = c(0,5,10,15,20,25,30,35,40,45,50)) + 
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
          plot.title = element_text(size = 20, face = "bold")) + 
    scale_x_datetime( breaks = "1 hour", date_labels = "%H:%M")
  return(plot_z)
}
plot_list <- lapply(unique(df17$Type), gg_fun, dt = df17)
plot_list
#################################
head(df12)
df20 <-  mutate(df12, error = WRF - avg_wind_speed)
df20 <- mutate(df20, abs_error = abs(error))
df21 <- df20 %>%
  group_by(Type) %>%
  summarise(Avg_Error = mean(error),
            ABS_Avg_Error = mean(abs_error))

setwd("~/Desktop")
write.csv(df21, file = "20171029_avg_error.csv")
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
  df8 <- gather(df7, "Model_2", "Cor_max", 2:36)
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
  avg3 <- gather(avg2, "Model_2","Cor_avg", 2:36)
  names(avg3)[2] <- "Prim"
  names(avg3)[1] <- "Second"
  df11 <- merge(df10, avg3, by = c("Prim","Second"))
  df_final <- df11[,c(1,2,3,11,4,5,6,7,8,9,10)]
  
write.csv(df_final, file = "cor_oct_29.csv")
colnames(df_final)
df_final <- drop_na(df_final)
##############
setwd("~/Desktop")
dis <- read.csv("dis.csv")
df3 <- df1 %>%
  spread(Type, WRF)
df3$Date <- NULL
df6 <- cor(df3, use="pairwise.complete.obs")
df7 <- data.frame(df6)
df7 <-  rownames_to_column(df7, "Models")
df8 <- gather(df7, "Model_2", "Cor_WRF", 2:38)
names(df8)[2] <- "Prim"
names(df8)[1] <- "Second"
df10 <- merge(df8, dis, by = c("Prim","Second"))
write.csv(df10, file = "cor_wrf_oct29.csv")
colnames(df10)
range(df10$Cor_WRF)
ggplot(df10, aes(x = Total_Distance, y = Cor_WRF, col = comb)) + geom_point(size = 3)  + 
  labs(title = "October 29, 2017, WRF Correlation", x = "Total Distance", y = "Correlation") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(breaks = seq(-0.5, 1, by = 0.1)) + 
  scale_x_continuous(breaks = seq(0,300, by = 25)) + geom_hline(yintercept = 0, lwd = 0.5, col = "black") + 
  scale_color_manual(values=c("red", "blue", "blue", "darkgreen")) + theme(axis.title.x = element_text(size = 16, face = "bold")) + 
  theme(axis.text.x = element_text(size = 13)) + theme(axis.title.y = element_text(size = 16, face = "bold")) + 
  theme(axis.text.y = element_text(size = 13)) + theme(title = element_text(size = 16, face = "bold")) 
