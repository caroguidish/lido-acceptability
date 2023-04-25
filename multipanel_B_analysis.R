#load necessary packages
library("dplyr")
library("tidyverse")
library("ggplot2")
library("scales")
library("plotly")
library("lubridate")
library("zoo")
library("RColorBrewer")


dftotal <- rbind(df01, df02, df04, df05, df06, df07, df08, df09, df12, df13, df15, df16, df17, df23, df25, df29, df35)

#Threshold data to eliminate <1 or = 1 lux in Ev_mel_D65.in.lx and Ev.in.lx
dftotal$Ev_mel_D65.in.lx[dftotal$Ev_mel_D65.in.lx <= 1] <- NaN
dftotal$Ev.in.lx[dftotal$Ev.in.lx <= 1] <- NaN

##Calculate day periods

visualscore <- read.table(file = "clipboard",
                          sep = "\t", header=TRUE)
dftotal<- dftotal %>%
  left_join(visualscore, by = "Participant") 


dftotal$Lights.off <- as.POSIXct(dftotal$Lights.off, tz = 'UTC')
dftotal$Lights.on <- as.POSIXct(dftotal$Lights.on, tz = 'UTC')

#calculated night time and day time for each participant
dftotal <- dftotal %>%
  group_by(Participant) %>%
  mutate(day.vs.night = ifelse(UTC.NormTimeStamp >= Lights.off & UTC.NormTimeStamp <= Lights.on, "Night", "Day"))


df_day <- dftotal[dftotal$day.vs.night == "Day",]
df_night <- dftotal[dftotal$day.vs.night == "Night",]



#Plot the correlation of EV_med and EV (mean values)
summ_val_dayLog <- df_day %>%
  group_by(Participant) %>%
  summarise(
    mean_medi = mean(log(Ev_mel_D65.in.lx, 10), na.rm = TRUE),
    sd_medi = sd(log(Ev_mel_D65.in.lx, 10), na.rm = TRUE),
    mean_Ev = mean(log(Ev.in.lx, 10), na.rm = TRUE),
    sd_Ev = sd(log(Ev.in.lx, 10), na.rm = TRUE), 
  )

#plot

nb.cols <- 18
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)

corr_plot <- ggplot(data = summ_val_dayLog, aes(x = mean_Ev, y = mean_medi)) +
  
  geom_point(aes(color = Participant), size = 2) +
  
  scale_color_manual(values = mycolors)  +
  
  geom_abline(intercept = 0, slope = 1, color = "black") +
  
  scale_x_continuous( breaks = c(1.0, 2.0, 3.0), labels = c(10, 100, 1000), limits = c(0.7, 3.3)) +
  
  scale_y_continuous( breaks = c(1.0, 2.0, 3.0), labels = c(10, 100, 1000), limits = c(0.7, 3.3)) +
  
  stat_poly_line(method='lm', linetype = 2, se = FALSE, color = "black", linewidth = 0.5, fullrange = TRUE) +
  
  stat_poly_eq(use_label(c("eq", "R2"))) +
  
  ylab("Mean melanopic equivalent daylight<br>illuminance (mEDI) [lux]") +
  
  xlab("Mean photopic illuminance [lux]") +
  
  ggtitle( "Relationship between\nmEDI and illuminance") +
  
  theme_bw() +
  
  theme(aspect.ratio = 1) +
  theme(axis.title.y = element_markdown()) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))  +
  
  
  theme(legend.position = 'none') 

class(summ_val_day$mean_Ev)


#Save plot
ggsave("corr_plot.png",
       device = png, 
       width = 210, 
       height = 297, 
       units = "mm")





