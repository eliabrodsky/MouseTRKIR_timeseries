library(ggfortify)
library(changepoint)
library(dplyr)
library(forecast)
library(colorspace)
library(hexbin)
library(gganimate)
library(ggExtra)
library(GGally)

setwd("~/Dropbox (Dream Team)/MouseProject-workfolder/2021 experiments")

MouseT1 <- read.table('example3mice1.csv', sep=',', header=TRUE, stringsAsFactors=TRUE)
mouse_select <- 'm27'
MouseT1 <- filter(MouseT1,target1==mouse_select)
MouseT1 <- filter(MouseT1, date != '10/21/20' & date != '10/22/20' & date != '10/23/20' & date != '10/24/20' & date != '10/25/20' & date != '10/26/20' & date != '10/27/20' & date != '10/28/20' & date != '10/29/20' & date != '10/30/20' & date != '10/31/20')
#MouseT1 <- filter(MouseT1, speed > 2)

#features: 6-area, 7-size	8-speed	9-x_head	10-y_head	11-rotation	12-x_center	13-y_center	14-temperature	15-temperature_speed
MouseT <- MouseT1[6:7]
MouseTr <- t(MouseT)

df_all <- as.data.frame(MouseT)
df_allT <- t(df_all)
colnames(df_allT) <- MouseT1$date
colnames1 <- as.factor(colnames(df_all))



#location
library(ggplot2)
library(viridis)
library(gifski)
library(tidyquant)

Mouse_sick <- filter(MouseT1, is_sick == TRUE)
Mouse_healthy <- filter(MouseT1, is_sick == FALSE)

ggpairs(MouseT1[,c("temperature","area","size", "speed")], 
        mapping = ggplot2::aes(colour=MouseT1$is_sick),
        lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1), 
                     discrete = "blank", combo="blank"), 
        diag = list(discrete="barDiag", 
                    continuous = wrap("densityDiag", alpha=0.5 )), 
        upper = list(combo = wrap("box_no_facet", alpha=0.5),
                     continuous = wrap("cor", size=4))) + 
        theme(panel.grid.major = element_blank()
)

MouseT2 <- MouseT1
MouseT2$speed<- log1p(MouseT1$speed)
SickColor <- as.factor(MouseT2$is_sick)
plot(SickColor)

linechart1 <- ggplot(data=MouseT2, aes(x=date1, y=speed, color=temperature)) +
  geom_line(alpha=0.4, linetype=1)+
  scale_color_viridis(option="plasma") +
  ggtitle(paste("Temperature and speed for",mouse_select)) +
  geom_ma(data=MouseT2, aes(temperature), ma_fun = SMA, n = 100, color = "black", linetype=1)+
  theme_bw()
  #geom_point(size=1)
ggsave(plot = linechart1, width = 15, height = 6, dpi = 150, filename = paste("linechart_temp_speed1_",mouse_select,".png"))


ggplot(MouseT1, aes(area)) +
  geom_density(y=MouseT1$area) +
  ggtitle(mouse_select)

ggplot(MouseT1, aes(x_center, y_center, color = date1, size=speed, width=600, height=300)) + 
  #geom_line() +
  #geom_tile() +
  geom_point(alpha = 0.5) + 
  scale_color_viridis() +
  ggtitle(mouse_select)

#position by date
titlePlot <- paste("Position by Day and Temperature, Mouse:", mouse_select, sep=" ")
PosDate <- ggplot(MouseT1, aes(x=x_center, y=y_center, color=temperature, shape=is_sick)) +
  geom_point(alpha=0.6, size=2) +
  scale_color_viridis() +
  ggtitle(titlePlot)+
  theme_bw()+
  facet_wrap(~ date, ncol = 2)

PosDate

ggMarginal(PosDate, groupColour = TRUE, groupFill = TRUE)

#temperature by date (density plot)
titlePlot <- paste("Temperature by Day, Mouse:", mouse_select, sep=" ")

# compute mean
mean_temp <- mean(MouseT1$temperature)

ggplot(MouseT1, aes(x=temperature, fill=is_sick)) +
  geom_density(alpha=0.5) +
  scale_color_viridis() +
  ggtitle(titlePlot)+
  theme(legend.position="bottom")+
  facet_wrap(~ date, ncol = 2)+
  geom_vline(xintercept=mean_temp, size=0.5, color="black",
             linetype="dashed")

#area by date (density plot)
titlePlot <- paste("Area by Day, Mouse:", mouse_select, sep=" ")

# compute mean
mean_speed <- mean(MouseT1$speed)

#by day
ggplot(MouseT1, aes(x=speed, fill=is_sick)) +
  geom_density(alpha=0.5) +
  scale_color_viridis() +
  ggtitle(titlePlot)+
  theme(legend.position="bottom")+
  facet_wrap(~ date, ncol = 2)+
  geom_vline(xintercept=mean_speed, size=0.5, color="black",
             linetype="dashed")

#by condition
ggplot(MouseT1, aes(x=speed, fill=is_sick)) +
  geom_density(alpha=0.5) +
  scale_color_viridis() +
  ggtitle(titlePlot)+
  theme(legend.position="bottom")+
  facet_wrap(~ is_sick, ncol = 2)+
  geom_vline(xintercept=mean_area, size=0.5, color="black",
             linetype="dashed")


