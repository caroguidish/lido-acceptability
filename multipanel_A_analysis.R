#load necessary packages
library("dplyr")
library("ggplot2")
library("scales")
library("plotly")
library("lubridate")
library("zoo")
library("RColorBrewer")
library("gridExtra")
library("ggtext")

#Combining datasets (already imported)
dftotal <- rbind(df01, df02, df04, df05, df06, df07, df08, df09, df12, df13, df15, df16, df17, df23, df25, df29, df35)

#Threshold data to eliminate <1 or = 1 lux in Ev_mel_D65.in.lx and Ev.in.lx
dftotal$Ev_mel_D65.in.lx[dftotal$Ev_mel_D65.in.lx <= 1] <- NaN
dftotal$Ev.in.lx[dftotal$Ev.in.lx <= 1] <- NaN

#Plot the moving average per participant
##create empty column
dftotal$ma_360_Evmel = array(data = NA, dim = nrow(dftotal)) 

#create loop to do this for each participant
for (i in 1:18){
  
  id = as.factor(sprintf("P%02d", i)) #trasforming every n from 1 to 18 into P01:P18
  par_index <- which(dftotal$Participant == id, arr.ind =TRUE) #find all rows n corresp to values of that participants
  
  par_data = dftotal$Ev_mel_D65.in.lx[par_index] # specifying that we want only the rows of Ev_mel_D65.in.lx were we know the participant
  
  par_ma = data.table::frollmean(par_data, n = 360, align = "center", na.rm =TRUE) #calculating moving av filter for n=360 measurements, centred, ignoring NA values
  
  dftotal$ma_360_Evmel[par_index] <- par_ma} #assigning output of par_ma to all participant rows which are stored in par_index


##Plot
#color palette
nb.cols <- 18
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)


lims <- as.POSIXct(c("1970-01-01 10:00:00", "1970-01-02 16:10:00"), format = "%Y-%m-%d %H:%M:%S", tz='UTC')

indiv_lines <- ggplot(data = dftotal, aes(UTC.NormTimeStamp, ma_360_Evmel)) +
  
  geom_line(aes(color = Participant), size = 0.8) +
 
   geom_point(data = startpoint, aes(x = first_timestamp, y = (first_ma_360_Evmel)), color = "black") + 
  
  geom_point(data = stoppoint, aes(x = last_timestamp, y = (last_ma_360_Evmel)), color = "black") + 
  
  geom_point(data = startpoint_day2, aes(x = d2first_timestamp, y = d2first_ma_360_Evmel), color = "black") +
  
  geom_point(data = stoppoint_day2, aes(x = d2last_timestamp, y = (d2last_ma_360_Evmel)), color = "black") +
  
  scale_color_manual(values = mycolors) +
  
  scale_y_log10(breaks=c(1, 10, 100, 1000, 10000), labels =c(1, 10, 100, 1000, 10000)) +
  
  labs(title = "Melanopic light exposure", subtitle = "Objective light measurement", x = "Time") + 
  
  ylab("Melanopic equivalent daylight<br>illuminance (mEDI) [lux]") +
  
  scale_x_datetime(breaks = as.POSIXct(c("1970-01-01 12:00:00", "1970-01-01 15:00:00", "1970-01-01 18:00:00", "1970-01-01 21:00:00", "1970-01-02 00:00:00", "1970-01-02 03:00:00", "1970-01-02 06:00:00", "1970-01-02 09:00:00", "1970-01-02 12:00:00", "1970-01-02 15:00:00"), tz = 'UTC'),
                   date_labels = "%H:00", limits = lims, expand = c(0, 0)) +
  theme_classic() + 
  theme(aspect.ratio = 1) +
  theme(axis.title.y = element_markdown()) +
  theme(legend.position = 'none') + 
  annotate("text", x= as.POSIXct("1970-01-01 14:00:00"), y=1, label= "n = 17", size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


##Understanding the start and lights off time for each participant, to add point on the plot
startpoint <- dftotal %>%
  group_by(Participant) %>%
  summarise(first_ma_360_Evmel = ma_360_Evmel[which.max(!is.na(ma_360_Evmel))],
            first_timestamp = UTC.NormTimeStamp[which.max(!is.na(ma_360_Evmel))],
            .groups = "drop")

stoppoint <- dftotal %>%
 filter(UTC.NormTimeStamp <= as.POSIXct("1970-01-02 06:30:54", tz = "UTC")) %>%
  group_by(Participant) %>%
  summarise(last_ma_360_Evmel = tail(ma_360_Evmel[!is.na(ma_360_Evmel)], 1),
           last_timestamp = tail(UTC.NormTimeStamp[!is.na(ma_360_Evmel)], 1),
         .groups = "drop")

#firsttimepointday2
startpoint_day2 <- dftotal %>%
  filter(UTC.NormTimeStamp >=  as.POSIXct("1970-01-02 06:30:54", tz = "UTC")) %>%
  group_by(Participant) %>%
  summarise(d2first_ma_360_Evmel = ma_360_Evmel[which.max(!is.na(ma_360_Evmel))],
            d2first_timestamp = UTC.NormTimeStamp[which.max(!is.na(ma_360_Evmel))],
            .groups = "drop")


#final timepoint in total
stoppoint_day2 <- dftotal %>%
  group_by(Participant) %>%
  summarise(d2last_ma_360_Evmel = tail(ma_360_Evmel[!is.na(ma_360_Evmel)], 1),
            d2last_timestamp = tail(UTC.NormTimeStamp[!is.na(ma_360_Evmel)], 1),
            .groups = "drop")


#save plot as pdf
ggsave("indiv_lines.pdf",
       device = png, 
       width = 210, 
       height = 297, 
       units = "mm")

