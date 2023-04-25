#load necessary packages
library("dplyr")
library("ggplot2")
library("scales")
library("plotly")
library("lubridate")
library("zoo")
library("RColorBrewer")
library("gridExtra")
library("patchwork")
library("ggpubr")
library("cowplot")


# Import datasets
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))

# Make participant column
df01 <- mutate(P01_testdata.csv, Participant = 'P01')
df02 <- mutate(P02_testdata.csv, Participant = 'P02')
df04 <- mutate(P04_testData.csv, Participant = 'P04')
df05 <- mutate(P05_testData.csv, Participant = 'P05')
df06 <- mutate(P06_testdata.csv, Participant = 'P06')
df07 <- mutate(P07_testdata.csv, Participant = 'P07')
df08 <- mutate(P08_testdata.csv, Participant = 'P08')
df09 <- mutate(P09_testdata.csv, Participant = 'P09')
df12 <- mutate(P12_testData.csv, Participant = 'P10')
df13 <- mutate(P13_testdata.csv, Participant = 'P11')
df15 <- mutate(P15_testdata.csv, Participant = 'P12')
df16 <- mutate(P16_TestData.csv, Participant = 'P13')
df17 <- mutate(P17_testdata.csv, Participant = 'P14')
df23 <- mutate(P23_testData.csv, Participant = 'P15')
df25 <- mutate(P25_testData.csv, Participant = 'P16')
df29 <- mutate(P29_testdata.csv, Participant = 'P17')
df35 <- mutate(P35_testData.csv, Participant = 'P18')

##Change the timestamp
# Changing the UTC.Timestamp column into a POSIXct format and storing it in a new column
format <- "%d/%m/%Y %H:%M:%S"

# First, convert to a POSIX time stamp for ONE participant
tmp <- as.POSIXct(df01$UTC.Timestamp, format=format, tz="UTC")

# We only need the first date so let's discard evertyhing else
tmp1 <- tmp[1]

# Get the POSIX time of that date
tmp2 <- as.numeric(as.POSIXct(as.Date(tmp1)))

# Subtract this from all the time stamps
tmp3 <- as.numeric(tmp) - tmp2

# Convert back to a real time stamp
tmp4 <- as.POSIXct(tmp3, tz="UTC", origin = "1970-01-01")

# Fold everything back to each participant
df01$UTC.NormTimeStamp <- as_datetime(tmp4)

#Repeat same for each participant
#participant 2
tmp <- as.POSIXct(df02$UTC.Timestamp, format=format, tz="UTC")
tmp1 <- tmp[1]
tmp2 <- as.numeric(as.POSIXct(as.Date(tmp1)))
tmp3 <- as.numeric(tmp) - tmp2
tmp4 <- as.POSIXct(tmp3, tz="UTC", origin = "1970-01-01")
df02$UTC.NormTimeStamp <- as_datetime(tmp4)

#participant 4
tmp <- as.POSIXct(df04$UTC.Timestamp, format=format, tz="UTC")
tmp1 <- tmp[1]
tmp2 <- as.numeric(as.POSIXct(as.Date(tmp1)))
tmp3 <- as.numeric(tmp) - tmp2
tmp4 <- as.POSIXct(tmp3, tz="UTC", origin = "1970-01-01")
df04$UTC.NormTimeStamp <- as_datetime(tmp4)

#participant 5
tmp <- as.POSIXct(df05$UTC.Timestamp, format=format, tz="UTC")
tmp1 <- tmp[1]
tmp2 <- as.numeric(as.POSIXct(as.Date(tmp1)))
tmp3 <- as.numeric(tmp) - tmp2
tmp4 <- as.POSIXct(tmp3, tz="UTC", origin = "1970-01-01")
df05$UTC.NormTimeStamp <- as_datetime(tmp4)

#participant 6
tmp <- as.POSIXct(df06$UTC.Timestamp, format=format, tz="UTC")
tmp1 <- tmp[1]
tmp2 <- as.numeric(as.POSIXct(as.Date(tmp1)))
tmp3 <- as.numeric(tmp) - tmp2
tmp4 <- as.POSIXct(tmp3, tz="UTC", origin = "1970-01-01")
df06$UTC.NormTimeStamp <- as_datetime(tmp4)

#participant 7
tmp <- as.POSIXct(df07$UTC.Timestamp, format=format, tz="UTC")
tmp1 <- tmp[1]
tmp2 <- as.numeric(as.POSIXct(as.Date(tmp1)))
tmp3 <- as.numeric(tmp) - tmp2
tmp4 <- as.POSIXct(tmp3, tz="UTC", origin = "1970-01-01")
df07$UTC.NormTimeStamp <- as_datetime(tmp4)

#participant 8
tmp <- as.POSIXct(df08$UTC.Timestamp, format=format, tz="UTC")
tmp1 <- tmp[1]
tmp2 <- as.numeric(as.POSIXct(as.Date(tmp1)))
tmp3 <- as.numeric(tmp) - tmp2
tmp4 <- as.POSIXct(tmp3, tz="UTC", origin = "1970-01-01")
df08$UTC.NormTimeStamp <- as_datetime(tmp4)

#participant 9
tmp <- as.POSIXct(df09$UTC.Timestamp, format=format, tz="UTC")
tmp1 <- tmp[1]
tmp2 <- as.numeric(as.POSIXct(as.Date(tmp1)))
tmp3 <- as.numeric(tmp) - tmp2
tmp4 <- as.POSIXct(tmp3, tz="UTC", origin = "1970-01-01")
df09$UTC.NormTimeStamp <- as_datetime(tmp4)

#participant 10
tmp <- as.POSIXct(df12$UTC.Timestamp, format=format, tz="UTC")
tmp1 <- tmp[1]
tmp2 <- as.numeric(as.POSIXct(as.Date(tmp1)))
tmp3 <- as.numeric(tmp) - tmp2
tmp4 <- as.POSIXct(tmp3, tz="UTC", origin = "1970-01-01")
df12$UTC.NormTimeStamp <- as_datetime(tmp4)

#participant 11
tmp <- as.POSIXct(df13$UTC.Timestamp, format=format, tz="UTC")
tmp1 <- tmp[1]
tmp2 <- as.numeric(as.POSIXct(as.Date(tmp1)))
tmp3 <- as.numeric(tmp) - tmp2
tmp4 <- as.POSIXct(tmp3, tz="UTC", origin = "1970-01-01")
df13$UTC.NormTimeStamp <- as_datetime(tmp4)

#participant 12
tmp <- as.POSIXct(df15$UTC.Timestamp, format=format, tz="UTC")
tmp1 <- tmp[1]
tmp2 <- as.numeric(as.POSIXct(as.Date(tmp1)))
tmp3 <- as.numeric(tmp) - tmp2
tmp4 <- as.POSIXct(tmp3, tz="UTC", origin = "1970-01-01")
df15$UTC.NormTimeStamp <- as_datetime(tmp4)

#participant 13
tmp <- as.POSIXct(df16$UTC.Timestamp, format=format, tz="UTC")
tmp1 <- tmp[1]
tmp2 <- as.numeric(as.POSIXct(as.Date(tmp1)))
tmp3 <- as.numeric(tmp) - tmp2
tmp4 <- as.POSIXct(tmp3, tz="UTC", origin = "1970-01-01")
df16$UTC.NormTimeStamp <- as_datetime(tmp4)

#participant 14
tmp <- as.POSIXct(df17$UTC.Timestamp, format=format, tz="UTC")
tmp1 <- tmp[1]
tmp2 <- as.numeric(as.POSIXct(as.Date(tmp1)))
tmp3 <- as.numeric(tmp) - tmp2
tmp4 <- as.POSIXct(tmp3, tz="UTC", origin = "1970-01-01")
df17$UTC.NormTimeStamp <- as_datetime(tmp4)

#participant 15
tmp <- as.POSIXct(df23$UTC.Timestamp, format=format, tz="UTC")
tmp1 <- tmp[1]
tmp2 <- as.numeric(as.POSIXct(as.Date(tmp1)))
tmp3 <- as.numeric(tmp) - tmp2
tmp4 <- as.POSIXct(tmp3, tz="UTC", origin = "1970-01-01")
df23$UTC.NormTimeStamp <- as_datetime(tmp4)

#participant 16
tmp <- as.POSIXct(df25$UTC.Timestamp, format=format, tz="UTC")
tmp1 <- tmp[1]
tmp2 <- as.numeric(as.POSIXct(as.Date(tmp1)))
tmp3 <- as.numeric(tmp) - tmp2
tmp4 <- as.POSIXct(tmp3, tz="UTC", origin = "1970-01-01")
df25$UTC.NormTimeStamp <- as_datetime(tmp4)

#participant 17
tmp <- as.POSIXct(df29$UTC.Timestamp, format=format, tz="UTC")
tmp1 <- tmp[1]
tmp2 <- as.numeric(as.POSIXct(as.Date(tmp1)))
tmp3 <- as.numeric(tmp) - tmp2
tmp4 <- as.POSIXct(tmp3, tz="UTC", origin = "1970-01-01")
df29$UTC.NormTimeStamp <- as_datetime(tmp4)

#participant 18
tmp <- as.POSIXct(df35$UTC.Timestamp, format=format, tz="UTC")
tmp1 <- tmp[1]
tmp2 <- as.numeric(as.POSIXct(as.Date(tmp1)))
tmp3 <- as.numeric(tmp) - tmp2
tmp4 <- as.POSIXct(tmp3, tz="UTC", origin = "1970-01-01")
df35$UTC.NormTimeStamp <- as_datetime(tmp4)

#Visualise the visual score
visualscore <- read.table(file = "clipboard",
                          sep = "\t", header=TRUE)

df01$Ev_mel_D65.in.lx[df01$Ev_mel_D65.in.lx <= 1] <- NaN
df01$Ev.in.lx[df01$Ev.in.lx <= 1] <- NaN

df02$Ev_mel_D65.in.lx[df02$Ev_mel_D65.in.lx <= 1] <- NaN
df02$Ev.in.lx[df02$Ev.in.lx <= 1] <- NaN

#df03$Ev_mel_D65.in.lx[df03$Ev_mel_D65.in.lx <= 1] <- NaN
#df03$Ev.in.lx[df03$Ev.in.lx <= 1] <- NaN

df04$Ev_mel_D65.in.lx[df04$Ev_mel_D65.in.lx <= 1] <- NaN
df04$Ev.in.lx[df04$Ev.in.lx <= 1] <- NaN

df05$Ev_mel_D65.in.lx[df05$Ev_mel_D65.in.lx <= 1] <- NaN
df05$Ev.in.lx[df05$Ev.in.lx <= 1] <- NaN

df06$Ev_mel_D65.in.lx[df06$Ev_mel_D65.in.lx <= 1] <- NaN
df06$Ev.in.lx[df06$Ev.in.lx <= 1] <- NaN

df07$Ev_mel_D65.in.lx[df07$Ev_mel_D65.in.lx <= 1] <- NaN
df07$Ev.in.lx[df07$Ev.in.lx <= 1] <- NaN

df08$Ev_mel_D65.in.lx[df08$Ev_mel_D65.in.lx <= 1] <- NaN
df08$Ev.in.lx[df08$Ev.in.lx <= 1] <- NaN

df09$Ev_mel_D65.in.lx[df09$Ev_mel_D65.in.lx <= 1] <- NaN
df09$Ev.in.lx[df09$Ev.in.lx <= 1] <- NaN

df12$Ev_mel_D65.in.lx[df12$Ev_mel_D65.in.lx <= 1] <- NaN
df12$Ev.in.lx[df12$Ev.in.lx <= 1] <- NaN

df13$Ev_mel_D65.in.lx[df13$Ev_mel_D65.in.lx <= 1] <- NaN
df13$Ev.in.lx[df13$Ev.in.lx <= 1] <- NaN

df15$Ev_mel_D65.in.lx[df15$Ev_mel_D65.in.lx <= 1] <- NaN
df15$Ev.in.lx[df15$Ev.in.lx <= 1] <- NaN

df16$Ev_mel_D65.in.lx[df16$Ev_mel_D65.in.lx <= 1] <- NaN
df16$Ev.in.lx[df16$Ev.in.lx <= 1] <- NaN

df17$Ev_mel_D65.in.lx[df17$Ev_mel_D65.in.lx <= 1] <- NaN
df17$Ev.in.lx[df17$Ev.in.lx <= 1] <- NaN

df23$Ev_mel_D65.in.lx[df23$Ev_mel_D65.in.lx <= 1] <- NaN
df23$Ev.in.lx[df23$Ev.in.lx <= 1] <- NaN

df25$Ev_mel_D65.in.lx[df25$Ev_mel_D65.in.lx <= 1] <- NaN
df25$Ev.in.lx[df25$Ev.in.lx <= 1] <- NaN

df29$Ev_mel_D65.in.lx[df29$Ev_mel_D65.in.lx <= 1] <- NaN
df29$Ev.in.lx[df29$Ev.in.lx <= 1] <- NaN

df35$Ev_mel_D65.in.lx[df35$Ev_mel_D65.in.lx <= 1] <- NaN
df35$Ev.in.lx[df35$Ev.in.lx <= 1] <- NaN

#PLOT

lim_ind <- as.POSIXct(c("1970-01-01 10:00:00", "1970-01-02 16:10:00"), format = "%Y-%m-%d %H:%M:%S", tz='UTC')
options(scipen = 100000)
#P01
plot1 <- ggplot(data = df01, aes(UTC.NormTimeStamp, Ev_mel_D65.in.lx)) +
  
  scale_y_log10(breaks=c(1, 10, 100, 1000, 10000, 100000), labels =c(1, 10, 100, 1000, 10000, 100000), limits = c(1, 82000)) +
  
  geom_point(color = "darkgray",size = 0.2) +
  
  labs(title = "P01", x = "Time", y = "Melanopic EDI [lux]") +
  
  scale_x_datetime(breaks = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-01 15:00:00", "1970-01-01 18:00:00", "1970-01-01 21:00:00", "1970-01-02 00:00:00", "1970-01-02 03:00:00", "1970-01-02 06:00:00", "1970-01-02 09:00:00", "1970-01-02 09:00:00", "1970-01-02 12:00:00", "1970-01-02 15:00:00"), tz = 'UTC'),
                   date_labels = "%H:00", limits = lim_ind, expand = c(0, 0)) +
  theme_classic() + 
  
  geom_vline(xintercept = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-02 12:00:00"), tz = 'UTC'), 
             color = "black", size = 0.2, linetype =2) +
  
  annotate("rect", xmin = as.POSIXct("1970-01-02 02:01:13", tz = "UTC"), xmax = as.POSIXct("1970-01-02 08:40:33", tz = "UTC"), ymin = 0, ymax = Inf, alpha = .6, fill = "yellow") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  
  theme(legend.position = "none") 

#P02
plot2 <- ggplot(data = df02, aes(UTC.NormTimeStamp, Ev_mel_D65.in.lx)) +
  scale_y_log10(breaks=c(1, 10, 100, 1000, 10000, 100000), labels =c(1, 10, 100, 1000, 10000, 100000), limits = c(1, 82000)) +
  geom_point(color = "darkgray",size = 0.2) +
  
  labs(title = "P02", x = "Time", y = "Melanopic EDI [lux]") +
  
  scale_x_datetime(breaks = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-01 15:00:00", "1970-01-01 18:00:00", "1970-01-01 21:00:00", "1970-01-02 00:00:00", "1970-01-02 03:00:00", "1970-01-02 06:00:00", "1970-01-02 09:00:00", "1970-01-02 09:00:00", "1970-01-02 12:00:00", "1970-01-02 15:00:00"), tz = 'UTC'),
                   date_labels = "%H:00", limits = lim_ind, expand = c(0, 0)) +
  theme_classic() + 
  
  geom_vline(xintercept = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-02 12:00:00"), tz = 'UTC'), 
             color = "black", size = 0.2, linetype =2) +
  
  annotate("rect", xmin = as.POSIXct("1970-01-02 00:48:36", tz = "UTC"), xmax = as.POSIXct("1970-01-02 09:41:26", tz = "UTC"), ymin = 0, ymax = Inf, alpha = .6, fill = "yellow") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  
  theme(legend.position="none")

#PARTICIPANT4
plot4 <- ggplot(data = df04, aes(UTC.NormTimeStamp, Ev_mel_D65.in.lx)) +
  scale_y_log10(breaks=c(1, 10, 100, 1000, 10000, 100000), labels =c(1, 10, 100, 1000, 10000, 100000), limits = c(1, 82000)) +
  geom_point(color = "darkgray",size = 0.2) +
  
  scale_x_datetime(breaks = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-01 15:00:00", "1970-01-01 18:00:00", "1970-01-01 21:00:00", "1970-01-02 00:00:00", "1970-01-02 03:00:00", "1970-01-02 06:00:00", "1970-01-02 09:00:00", "1970-01-02 09:00:00", "1970-01-02 12:00:00", "1970-01-02 15:00:00"), tz = 'UTC'),
                   date_labels = "%H:00", limits = lim_ind, expand = c(0, 0)) +
  theme_classic() + 
  
  labs(title = "P04", x = "Time", y = "Melanopic EDI [lux]") +
  
  geom_vline(xintercept = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-02 12:00:00"), tz = 'UTC'), 
             color = "black", size = 0.2, linetype =2) +
  
  annotate("rect", xmin = as.POSIXct("1970-01-01 22:45:41", tz = "UTC"), xmax = as.POSIXct("1970-01-02 07:25:51", tz = "UTC"), ymin = 0, ymax = Inf, alpha = .6, fill = "yellow") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  
  theme(legend.position="none")

##PARTICIPANT5
plot5 <- ggplot(data = df05, aes(UTC.NormTimeStamp, Ev_mel_D65.in.lx)) +
  scale_y_log10(breaks=c(1, 10, 100, 1000, 10000, 100000), labels =c(1, 10, 100, 1000, 10000, 100000), limits = c(1, 82000)) +
  geom_point(color = "darkgray",size = 0.2) +
  
  labs(title = "P05", x = "Time", y = "Melanopic EDI [lux]") +
  
  scale_x_datetime(breaks = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-01 15:00:00", "1970-01-01 18:00:00", "1970-01-01 21:00:00", "1970-01-02 00:00:00", "1970-01-02 03:00:00", "1970-01-02 06:00:00", "1970-01-02 09:00:00", "1970-01-02 09:00:00", "1970-01-02 12:00:00", "1970-01-02 15:00:00"), tz = 'UTC'),
                   date_labels = "%H:00", limits = lim_ind, expand = c(0, 0)) +
  theme_classic() + 
  
  geom_vline(xintercept = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-02 12:00:00"), tz = 'UTC'), 
             color = "black", size = 0.2, linetype =2) +
  
  annotate("rect", xmin = as.POSIXct("1970-01-02 02:32:11", tz = "UTC"), xmax = as.POSIXct("1970-01-02 10:18:21", tz = "UTC"), ymin = 0, ymax = Inf, alpha = .6, fill = "yellow") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  
  theme(legend.position="none")

##PARTICIPANT6
plot6 <- ggplot(data = df06, aes(UTC.NormTimeStamp, Ev_mel_D65.in.lx)) +
  scale_y_log10(breaks=c(1, 10, 100, 1000, 10000, 100000), labels =c(1, 10, 100, 1000, 10000, 100000), limits = c(1, 82000)) +
  geom_point(color = "darkgray",size = 0.2) +
  
  labs(title = "P06", x = "Time", y = "Melanopic EDI [lux]") +
  
  scale_x_datetime(breaks = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-01 15:00:00", "1970-01-01 18:00:00", "1970-01-01 21:00:00", "1970-01-02 00:00:00", "1970-01-02 03:00:00", "1970-01-02 06:00:00", "1970-01-02 09:00:00", "1970-01-02 09:00:00", "1970-01-02 12:00:00", "1970-01-02 15:00:00"), tz = 'UTC'),
                   date_labels = "%H:00", limits = lim_ind, expand = c(0, 0)) +
  theme_classic() + 
  
  geom_vline(xintercept = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-02 12:00:00"), tz = 'UTC'), 
             color = "black", size = 0.2, linetype =2) +
  
  annotate("rect", xmin = as.POSIXct("1970-01-01 22:20:02", tz = "UTC"), xmax = as.POSIXct("1970-01-02 07:32:42", tz = "UTC"), ymin = 0, ymax = Inf, alpha = .6, fill = "yellow") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

#PARTICIPANT7
plot7 <- ggplot(data = df07, aes(UTC.NormTimeStamp, Ev_mel_D65.in.lx)) +
  scale_y_log10(breaks=c(1, 10, 100, 1000, 10000, 100000), labels =c(1, 10, 100, 1000, 10000, 100000), limits = c(1, 82000) ) +
  geom_point(color = "darkgray",size = 0.2) +
  
  labs(title = "P07", x = "Time", y = "Melanopic EDI [lux]") +
  
  scale_x_datetime(breaks = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-01 15:00:00", "1970-01-01 18:00:00", "1970-01-01 21:00:00", "1970-01-02 00:00:00", "1970-01-02 03:00:00", "1970-01-02 06:00:00", "1970-01-02 09:00:00", "1970-01-02 09:00:00", "1970-01-02 12:00:00", "1970-01-02 15:00:00"), tz = 'UTC'),
                   date_labels = "%H:00", limits = lim_ind, expand = c(0, 0)) +
  theme_classic() + 
  
  geom_vline(xintercept = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-02 12:00:00"), tz = 'UTC'), 
             color = "black", size = 0.2, linetype =2) +
  
  annotate("rect", xmin = as.POSIXct("1970-01-02 01:46:47", tz = "UTC"), xmax = as.POSIXct("1970-01-02 07:47:37", tz = "UTC"), ymin = 0, ymax = Inf, alpha = .6, fill = "yellow") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

#PARTICIPANT8
plot8 <- ggplot(data = df08, aes(UTC.NormTimeStamp, Ev_mel_D65.in.lx)) +
  scale_y_log10(breaks=c(1, 10, 100, 1000, 10000, 100000), labels =c(1, 10, 100, 1000, 10000, 100000), limits = c(1, 82000)) +
  geom_point(color = "darkgray",size = 0.2) +
  
  labs(title = "P08", x = "Time", y = "Melanopic EDI [lux]") +
  
  scale_x_datetime(breaks = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-01 15:00:00", "1970-01-01 18:00:00", "1970-01-01 21:00:00", "1970-01-02 00:00:00", "1970-01-02 03:00:00", "1970-01-02 06:00:00", "1970-01-02 09:00:00", "1970-01-02 09:00:00", "1970-01-02 12:00:00", "1970-01-02 15:00:00"), tz = 'UTC'),
                   date_labels = "%H:00", limits = lim_ind, expand = c(0, 0)) +
  theme_classic() + 
  
  geom_vline(xintercept = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-02 12:00:00"), tz = 'UTC'), 
             color = "black", size = 0.2, linetype =2) +
  
  annotate("rect", xmin = as.POSIXct("1970-01-01 21:46:34", tz = "UTC"), xmax = as.POSIXct("1970-01-02 08:09:14", tz = "UTC"), ymin = 0, ymax = Inf, alpha = .6, fill = "yellow") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

##PARTICIPANT9
plot9 <- ggplot(data = df09, aes(UTC.NormTimeStamp, Ev_mel_D65.in.lx)) +
  scale_y_log10(breaks=c(1, 10, 100, 1000, 10000, 100000), labels =c(1, 10, 100, 1000, 10000, 100000), limits = c(1, 82000)) +
  geom_point(color = "darkgray",size = 0.2) +
  
  labs(title = "P09", x = "Time", y = "Melanopic EDI [lux]" ) +
  
  scale_x_datetime(breaks = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-01 15:00:00", "1970-01-01 18:00:00", "1970-01-01 21:00:00", "1970-01-02 00:00:00", "1970-01-02 03:00:00", "1970-01-02 06:00:00", "1970-01-02 09:00:00", "1970-01-02 09:00:00", "1970-01-02 12:00:00", "1970-01-02 15:00:00"), tz = 'UTC'),
                   date_labels = "%H:00", limits = lim_ind, expand = c(0, 0)) +
  theme_classic() + 
  
  geom_vline(xintercept = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-02 12:00:00"), tz = 'UTC'), 
             color = "black", size = 0.2, linetype =2) +
  
  annotate("rect", xmin = as.POSIXct("1970-01-02 01:28:18", tz = "UTC"), xmax = as.POSIXct("1970-01-02 07:49:58", tz = "UTC"), ymin = 0, ymax = Inf, alpha = .6, fill = "yellow") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

##PARTICIPANT10
plot10 <- ggplot(data = df12, aes(UTC.NormTimeStamp, Ev_mel_D65.in.lx)) +
  scale_y_log10(breaks=c(1, 10, 100, 1000, 10000, 100000), labels =c(1, 10, 100, 1000, 10000, 100000), limits = c(1, 82000)) +
  geom_point(color = "darkgray",size = 0.2) +
  
  labs(title = "P10", x = "Time", y = "Melanopic EDI [lux]") +
  
  scale_x_datetime(breaks = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-01 15:00:00", "1970-01-01 18:00:00", "1970-01-01 21:00:00", "1970-01-02 00:00:00", "1970-01-02 03:00:00", "1970-01-02 06:00:00", "1970-01-02 09:00:00", "1970-01-02 09:00:00", "1970-01-02 12:00:00", "1970-01-02 15:00:00"), tz = 'UTC'),
                   date_labels = "%H:00", limits = lim_ind, expand = c(0, 0)) +
  theme_classic() + 
  
  geom_vline(xintercept = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-02 12:00:00"), tz = 'UTC'), 
             color = "black", size = 0.2, linetype =2) +
  
  annotate("rect", xmin = as.POSIXct("1970-01-02 02:21:00", tz = "UTC"), xmax = as.POSIXct("1970-01-02 07:42:00", tz = "UTC"), ymin = 0, ymax = Inf, alpha = .6, fill = "yellow") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

##PARTICIPANT11
plot11 <- ggplot(data = df13, aes(UTC.NormTimeStamp, Ev_mel_D65.in.lx)) +
  scale_y_log10(breaks=c(1, 10, 100, 1000, 10000, 100000), labels =c(1, 10, 100, 1000, 10000, 100000), limits = c(1, 82000)) +
  geom_point(color = "darkgray",size = 0.2) +
  
  labs(title = "P11", x = "Time", y = "Melanopic EDI [lux]") +
  
  scale_x_datetime(breaks = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-01 15:00:00", "1970-01-01 18:00:00", "1970-01-01 21:00:00", "1970-01-02 00:00:00", "1970-01-02 03:00:00", "1970-01-02 06:00:00", "1970-01-02 09:00:00", "1970-01-02 09:00:00", "1970-01-02 12:00:00", "1970-01-02 15:00:00"), tz = 'UTC'),
                   date_labels = "%H:00", limits = lim_ind, expand = c(0, 0)) +
  theme_classic() + 
  
  geom_vline(xintercept = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-02 12:00:00"), tz = 'UTC'), 
             color = "black", size = 0.2, linetype =2) +
  
  annotate("rect", xmin = as.POSIXct("1970-01-02 02:57:55", tz = "UTC"), xmax = as.POSIXct("1970-01-02 09:00:05", tz = "UTC"), ymin = 0, ymax = Inf, alpha = .6, fill = "yellow") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

##PARTICIPANT12
plot12 <- ggplot(data = df15, aes(UTC.NormTimeStamp, Ev_mel_D65.in.lx)) +
  scale_y_log10(breaks=c(1, 10, 100, 1000, 10000, 100000), labels =c(1, 10, 100, 1000, 10000, 100000), limits = c(1, 82000)) +
  geom_point(color = "darkgray",size = 0.2) +
  
  labs(title = "P12", x = "Time", y = "Melanopic EDI [lux]") +
  
  scale_x_datetime(breaks = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-01 15:00:00", "1970-01-01 18:00:00", "1970-01-01 21:00:00", "1970-01-02 00:00:00", "1970-01-02 03:00:00", "1970-01-02 06:00:00", "1970-01-02 09:00:00", "1970-01-02 09:00:00", "1970-01-02 12:00:00", "1970-01-02 15:00:00"), tz = 'UTC'),
                   date_labels = "%H:00", limits = lim_ind, expand = c(0, 0)) +
  theme_classic() + 
  
  geom_vline(xintercept = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-02 12:00:00"), tz = 'UTC'), 
             color = "black", size = 0.2, linetype =2) +
  
  annotate("rect", xmin = as.POSIXct("1970-01-02 04:03:18", tz = "UTC"), xmax = as.POSIXct("1970-01-02 12:27:38", tz = "UTC"), ymin = 0, ymax = Inf, alpha = .6, fill = "yellow") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

##PARTICIPANT13
plot13 <- ggplot(data = df16, aes(UTC.NormTimeStamp, Ev_mel_D65.in.lx)) +
  scale_y_log10(breaks=c(1, 10, 100, 1000, 10000, 100000), labels =c(1, 10, 100, 1000, 10000, 100000), limits = c(1, 82000)) +
  geom_point(color = "darkgray",size = 0.2) +
  
  labs(title = "P13", x = "Time", y = "Melanopic EDI [lux]") +
  
  scale_x_datetime(breaks = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-01 15:00:00", "1970-01-01 18:00:00", "1970-01-01 21:00:00", "1970-01-02 00:00:00", "1970-01-02 03:00:00", "1970-01-02 06:00:00", "1970-01-02 09:00:00", "1970-01-02 09:00:00", "1970-01-02 12:00:00", "1970-01-02 15:00:00"), tz = 'UTC'),
                   date_labels = "%H:00", limits = lim_ind, expand = c(0, 0)) +
  theme_classic() + 
  
  geom_vline(xintercept = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-02 12:00:00"), tz = 'UTC'), 
             color = "black", size = 0.2, linetype =2) +
  
  annotate("rect", xmin = as.POSIXct("1970-01-02 00:45:36", tz = "UTC"), xmax = as.POSIXct("1970-01-02 08:52:46", tz = "UTC"), ymin = 0, ymax = Inf, alpha = .6, fill = "yellow") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

##PARTICIPANT14
plot14 <- ggplot(data = df17, aes(UTC.NormTimeStamp, Ev_mel_D65.in.lx)) +
  scale_y_log10(breaks=c(1, 10, 100, 1000, 10000, 100000), labels =c(1, 10, 100, 1000, 10000, 100000), limits = c(1, 82000)) +
  geom_point(color = "darkgray",size = 0.2) +
  
  labs(title = "P14", x = "Time", y = "Melanopic EDI [lux]") +
  
  scale_x_datetime(breaks = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-01 15:00:00", "1970-01-01 18:00:00", "1970-01-01 21:00:00", "1970-01-02 00:00:00", "1970-01-02 03:00:00", "1970-01-02 06:00:00", "1970-01-02 09:00:00", "1970-01-02 09:00:00", "1970-01-02 12:00:00", "1970-01-02 15:00:00"), tz = 'UTC'),
                   date_labels = "%H:00", limits = lim_ind, expand = c(0, 0)) +
  theme_classic() + 
  
  geom_vline(xintercept = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-02 12:00:00"), tz = 'UTC'), 
             color = "black", size = 0.2, linetype =2) +
  
  annotate("rect", xmin = as.POSIXct("1970-01-02 02:08:40", tz = "UTC"), xmax = as.POSIXct("1970-01-02 07:52:50", tz = "UTC"), ymin = 0, ymax = Inf, alpha = .6, fill = "yellow") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

##PARTICIPANT15
plot15 <- ggplot(data = df23, aes(UTC.NormTimeStamp, Ev_mel_D65.in.lx)) +
  scale_y_log10(breaks=c(1, 10, 100, 1000, 10000, 100000), labels =c(1, 10, 100, 1000, 10000, 100000), limits = c(1, 82000)) +
  geom_point(color = "darkgray",size = 0.2) +
  
  labs(title = "P15", x = "Time", y = "Melanopic EDI [lux]") +
  
  scale_x_datetime(breaks = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-01 15:00:00", "1970-01-01 18:00:00", "1970-01-01 21:00:00", "1970-01-02 00:00:00", "1970-01-02 03:00:00", "1970-01-02 06:00:00", "1970-01-02 09:00:00", "1970-01-02 09:00:00", "1970-01-02 12:00:00", "1970-01-02 15:00:00"), tz = 'UTC'),
                   date_labels = "%H:00", limits = lim_ind, expand = c(0, 0)) +
  theme_classic() + 
  
  geom_vline(xintercept = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-02 12:00:00"), tz = 'UTC'), 
             color = "black", size = 0.2, linetype =2) +
  
  annotate("rect", xmin = as.POSIXct("1970-01-02 01:25:35", tz = "UTC"), xmax = as.POSIXct("1970-01-02 	
06:40:15", tz = "UTC"), ymin = 0, ymax = Inf, alpha = .2, fill = "yellow") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

##PARTICIPANT16
plot16 <- ggplot(data = df25, aes(UTC.NormTimeStamp, Ev_mel_D65.in.lx)) +
  scale_y_log10(breaks=c(1, 10, 100, 1000, 10000, 100000), labels =c(1, 10, 100, 1000, 10000, 100000), limits = c(1, 82000)) +
  geom_point(color = "darkgray",size = 0.2) +
  
  labs(title = "P16", x = "Time", y = "Melanopic EDI [lux]") +
  
  scale_x_datetime(breaks = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-01 15:00:00", "1970-01-01 18:00:00", "1970-01-01 21:00:00", "1970-01-02 00:00:00", "1970-01-02 03:00:00", "1970-01-02 06:00:00", "1970-01-02 09:00:00", "1970-01-02 09:00:00", "1970-01-02 12:00:00", "1970-01-02 15:00:00"), tz = 'UTC'),
                   date_labels = "%H:00", limits = lim_ind, expand = c(0, 0)) +
  theme_classic() + 
  
  geom_vline(xintercept = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-02 12:00:00"), tz = 'UTC'), 
             color = "black", size = 0.2, linetype =2) +
  
  annotate("rect", xmin = as.POSIXct("1970-01-01 22:21:31", tz = "UTC"), xmax = as.POSIXct("1970-01-02 	
09:33:31", tz = "UTC"), ymin = 0, ymax = Inf, alpha = .2, fill = "yellow") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

##PARTICIPANT17
plot17 <- ggplot(data = df29, aes(UTC.NormTimeStamp, Ev_mel_D65.in.lx)) +
  scale_y_log10(breaks=c(1, 10, 100, 1000, 10000, 100000), labels =c(1, 10, 100, 1000, 10000, 100000), limits = c(1, 82000)) +
  geom_point(color = "darkgray",size = 0.2) +
  
  labs(title = "P17", x = "Time", y = "Melanopic EDI [lux]") +
  
  scale_x_datetime(breaks = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-01 15:00:00", "1970-01-01 18:00:00", "1970-01-01 21:00:00", "1970-01-02 00:00:00", "1970-01-02 03:00:00", "1970-01-02 06:00:00", "1970-01-02 09:00:00", "1970-01-02 09:00:00", "1970-01-02 12:00:00", "1970-01-02 15:00:00"), tz = 'UTC'),
                   date_labels = "%H:00", limits = lim_ind, expand = c(0, 0)) +
  theme_classic() + 
  
  geom_vline(xintercept = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-02 12:00:00"), tz = 'UTC'), 
             color = "black", size = 0.2, linetype =2) +
  
  annotate("rect", xmin = as.POSIXct("1970-01-02 02:14:36", tz = "UTC"), xmax = as.POSIXct("1970-01-02 	
07:37:46", tz = "UTC"), ymin = 0, ymax = Inf, alpha = .2, fill = "yellow") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

##PARTICIPANT18
plot18 <- ggplot(data = df35, aes(UTC.NormTimeStamp, Ev_mel_D65.in.lx)) +
  scale_y_log10(breaks=c(1, 10, 100, 1000, 10000, 100000), labels =c(1, 10, 100, 1000, 10000, 100000),limits = c(1, 82000) ) +
  geom_point(color = "darkgray",size = 0.2) +
  
  labs(title = "P18", x = "Time", y = "Melanopic EDI [lux]") +
  
  scale_x_datetime(breaks = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-01 15:00:00", "1970-01-01 18:00:00", "1970-01-01 21:00:00", "1970-01-02 00:00:00", "1970-01-02 03:00:00", "1970-01-02 06:00:00", "1970-01-02 09:00:00", "1970-01-02 09:00:00", "1970-01-02 12:00:00", "1970-01-02 15:00:00"), tz = 'UTC'),
                   date_labels = "%H:00", limits = lim_ind, expand = c(0, 0)) +
  theme_classic() + 
  
  geom_vline(xintercept = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-02 12:00:00"), tz = 'UTC'), 
             color = "black", size = 0.2, linetype =2) +
  
  annotate("rect", xmin = as.POSIXct("1970-01-01 23:34:32", tz = "UTC"), xmax = as.POSIXct("1970-01-02 	
07:07:02", tz = "UTC"), ymin = 0, ymax = Inf, alpha = .2, fill = "yellow") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")

##MULTIPANEL PLOT
plot_all <- (plot1 + plot2 + plot4 + plot5 + plot6 + plot7 + plot8 + plot9 + plot10 + plot11 + plot12 + plot13 + plot14 + plot15 + plot16 + plot17 + plot18) +
  plot_annotation(title = "Melanopic light exposure", subtitle = "Objective light measurement") +
  plot_layout(ncol = 5, nrow = 4) &
  ylab(NULL) & xlab(NULL) 


# Use the tag label as an x-axis label
multi_panel <- wrap_elements(panel = plot_all) +
  labs(tag = "Melanopic equivalent daylight illuminance (mEDI) [lux]") +
  theme(
    plot.tag = element_text(size = rel(1), angle = 90),
    plot.tag.position = "left"
  )

#Save this plot as pdf
ggsave("2023.04.16_Multiplot_yellow.pdf",
       plot = multi_panel,
       width = 270,
       height = 184,
       unit = "mm")