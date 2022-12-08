library(ggfortify)
library(changepoint)
library(dplyr)
library(forecast)
library(colorspace)
library(hexbin)
library(gganimate)
library(ggExtra)


setwd("~/Dropbox (Dream Team)/MouseProject-workfolder/2021 experiments")

MouseT1 <- read.table('example3mice1.csv', sep=',', header=TRUE, stringsAsFactors=TRUE)
mouse_select <- 'm26'
MouseT1 <- filter(MouseT1,target1==mouse_select)
MouseT1 <- filter(MouseT1, date != '10/21/20' & date != '10/22/20' & date != '10/23/20' & date != '10/24/20' & date != '10/25/20' & date != '10/26/20' & date != '10/27/20' & date != '10/28/20' & date != '10/29/20' & date != '10/30/20' & date != '10/31/20')
#MouseT1 <- filter(MouseT1, speed > 2)

#features: 6-area, 7-size	8-speed	9-x_head	10-y_head	11-rotation	12-x_center	13-y_center	14-temperature	15-temperature_speed
MouseT <- MouseT1[6]
MouseTr <- t(MouseT)

df_all <- as.data.frame(MouseT)
df_allT <- t(df_all)
colnames(df_allT) <- MouseT1$date
colnames1 <- as.factor(colnames(df_all))

#location
library(ggplot2)
library(viridis)
library(gifski)

#pair plot
library(psych)
pairs.panels(MouseT, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             main=mouse_select
)

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
mean_area <- mean(MouseT1$area)

#by day
ggplot(MouseT1, aes(x=area, fill=is_sick)) +
  geom_density(alpha=0.5) +
  scale_color_viridis() +
  ggtitle(titlePlot)+
  theme(legend.position="bottom")+
  facet_wrap(~ date, ncol = 2)+
  geom_vline(xintercept=mean_area, size=0.5, color="black",
             linetype="dashed")

#by condition
ggplot(MouseT1, aes(x=area, fill=is_sick)) +
  geom_density(alpha=0.5) +
  scale_color_viridis() +
  ggtitle(titlePlot)+
  theme(legend.position="bottom")+
  facet_wrap(~ is_sick, ncol = 2)+
  geom_vline(xintercept=mean_area, size=0.5, color="black",
             linetype="dashed")

#animated plot
p <- ggplot(MouseT1, aes(x_center, y_center, color = temperature, size=size)) + 
  #geom_line() +
  #geom_tile() +
  geom_point(alpha = 0.5) + 
  scale_color_viridis() +
  ggtitle(mouse_select) +
  labs(subtitle = "{closest_state}") +
  transition_states(states = MouseT1$date1, transition_length = 2) +
  #transition_reveal(MouseT1$date1) +
  ease_aes('linear') +
  shadow_wake(wake_length = 1)

animate(p, nframes = 10, width = 600, height = 300, renderer = gifski_renderer("gganim25.gif"))

#Load normalized data
MouseT1 <- read.table('example3mice_sd300.csv', sep=',', header=TRUE, stringsAsFactors=TRUE)
mouse_select <- 'm27'
MouseT1 <- filter(MouseT1,target1==mouse_select)
MouseT1 <- filter(MouseT1, date != '10/21/20' & date != '10/22/20' & date != '10/23/20' & date != '10/24/20' & date != '10/25/20' & date != '10/26/20' & date != '10/27/20' & date != '10/28/20' & date != '10/29/20' & date != '10/30/20' & date != '10/31/20')
#MouseT1 <- filter(MouseT1, speed > 2)

#features: 5-area, 6-size	7-speed	8-x_head	9-y_head	10-rotation	11-x_center	12-y_center	13-temperature	14-temperature_speed
MouseT <- MouseT1[5:14]
columnSelect <- toString(colnames(MouseT))
MouseTr <- t(MouseT)

df_all <- as.data.frame(MouseT)
df_allT <- t(df_all)
colnames(df_allT) <- MouseT1$date
colnames1 <- as.factor(colnames(df_all))

#position by date
titlePlot <- paste("Position by Day and Temperature, Mouse:", mouse_select, sep=" ")
ggplot(MouseT1, aes(x=x_center, y=y_center, color=temperature, size=area)) +
  geom_point(alpha=0.5) +
  scale_color_viridis() +
  ggtitle(titlePlot) +
  facet_wrap(~ is_sick, nrow = 2)

#area by date (density plot)
titlePlot <- paste("Area by Day, Mouse:", mouse_select, sep=" ")

# compute mean
mean_area <- mean(MouseT1$area)

#by day
ggplot(MouseT1, aes(x=area, fill=is_sick)) +
  geom_density(alpha=0.5) +
  scale_color_viridis() +
  ggtitle(titlePlot)+
  theme(legend.position="bottom")+
  facet_wrap(~ date, ncol = 2)+
  geom_vline(xintercept=mean_area, size=0.5, color="black",
             linetype="dashed")

#by condition
ggplot(MouseT1, aes(x=area, fill=is_sick)) +
  geom_density(alpha=0.5) +
  scale_color_viridis() +
  ggtitle(titlePlot)+
  theme(legend.position="bottom")+
  facet_wrap(~ is_sick, ncol = 2)+
  geom_vline(xintercept=mean_area, size=0.5, color="black",
             linetype="dashed")

#change point detection
autoplot(cpt.meanvar(df_allT), ts.geom = 'bar', fill = 'blue', cpt.width=10, main=colnames(MouseT), legend=TRUE, xlab=MouseT1$date, ylab = deparse(substitute(date)))

#ARIMA
ggtsdiag(auto.arima(MouseT1$size))
autoplot(stl(MouseTr[3,], s.window = 7), ts.colour = 'blue')

#heatmap 1
#par(mar = c(2, 2, 10, 2))
#heatmap(as.matrix(df_allT), scale="row", Colv=NA, col = sequential_hcl(90, palette="Plasma"), 
#        main="Heatmap for mouse 26", margins = c(5, 5))
#legend(x="bottomleft", legend=c("min", "ave", "max"), 
#       fill=sequential_hcl(3, palette="Plasma"))

#changepoint individually
#for(levels in colnames1) {
#  plot1 <- autoplot(cpt.meanvar(t(df_all[,levels])), ts.geom = 'bar', fill = 'blue', cpt.width=10, main=levels, legend=TRUE)
#  print(plot1)
#}

#autoplot(cpt.meanvar(t(df_all$area)), ts.geom = 'bar', fill = 'blue', cpt.width=10, main=levels, legend=TRUE)

library(transformr)
p <- ggplot(MouseT1, aes(area, fill="red")) +
  #geom_point(alpha=0.5) +
  geom_density(fill="red", alpha=0.5) +
  scale_color_viridis() +
  ggtitle(titlePlot)
  labs(subtitle = "{closest_state}")+
  transition_states(states = MouseT1$date, transition_length = 3)+
  ease_aes('linear') +
  shadow_wake(wake_length = 5)
p

animate(p, nframes = 10, width = 600, height = 300, renderer = gifski_renderer("gganim123.gif"))


#+++++++++++++++++++++++++++++++++++++++++++++#
#PCA - regular, normalized, by mouse, by feature
#+++++++++++++++++++++++++++++++++++++++++++++#

#Load RAW data: 
MouseT1 <- read.table('example3mice.csv', sep=',', header=TRUE, stringsAsFactors=TRUE)
MouseT1 <- filter(MouseT1, date != '10/21/20' & date != '10/22/20' & date != '10/23/20' & date != '10/24/20' & date != '10/25/20' & date != '10/26/20' & date != '10/27/20' & date != '10/28/20' & date != '10/29/20' & date != '10/30/20' & date != '10/31/20')

df_all <- as.data.frame(MouseT1[5:14])
df_allT <- t(df_all)
colnames(df_allT) <- MouseT1$date
colnames1 <- as.factor(colnames(df_all))

#pca
titlePCA <- "PCA: all mice with all features, RAW data"
pca_res <- prcomp(df_all, scale. = TRUE, center=TRUE)
#pca_res <- theme_set(theme_bw())
autoplot(pca_res, data = MouseT1, main=titlePCA, colour = 'is_sick', x=1, y=3, loadings=TRUE, loadings.label = TRUE, loadings.colour = 'black',frame = TRUE, frame.type = 'norm')
autoplot(pca_res, data = MouseT1, main=titlePCA, colour = 'date', x=1, y=3,) # frame = TRUE, label = TRUE, label.size = 3)


#Load normalized data: Normalization - sliding window

#Load normalized data: Seasonality Decomposition
MouseT1_SDnorm <- read.table('example3mice_av1.csv', sep=',', header=TRUE, stringsAsFactors=TRUE)
MouseT1_SDnorm <- filter(MouseT1_SDnorm, date != '10/21/20' & date != '10/22/20' & date != '10/23/20' & date != '10/24/20' & date != '10/25/20' & date != '10/26/20' & date != '10/27/20' & date != '10/28/20' & date != '10/29/20' & date != '10/30/20' & date != '10/31/20')

df_all <- as.data.frame(MouseT1_SDnorm[5:14])
df_allT <- t(df_all)
#df_allT <- df_all
colnames(df_allT) <- MouseT1_SDnorm$target1
colnames1 <- as.factor(colnames(df_all))

#pca
titlePCA <- "PCA: all mice with all features, SD data"
pca_res <- prcomp(df_all, scale. = TRUE, center=TRUE)
autoplot(pca_res, data = MouseT1_SDnorm, main=titlePCA, colour = 'is_sick', x=1, y=2, loadings=TRUE, loadings.label = TRUE, loadings.colour = 'black',frame = TRUE, frame.type = 'norm')
autoplot(pca_res, data = MouseT1_SDnorm, main=titlePCA, colour = 'date', x=1, y=2)

#pca
titlePCA <- "PCA: all mice with all features, SD data"
pca_res <- prcomp(df_allT, scale. = TRUE, center=TRUE)

#plot scatterplot
ggplot(pca_res$x, aes(x= PC1, y=PC2, colour = Group)) + geom_point() + theme(legend.position='bottom') + ggtitle("PCA after Quantile Normalization, PC1:16.01%, PC2:7.76%")

crossprod(df_allT[1,], pca_res$x[,1])




autoplot(pca_res, data = df_all, main=titlePCA,  x=1, y=2, loadings=TRUE, loadings.label = TRUE, loadings.colour = 'black', frame = TRUE, frame.type = 'norm')

autoplot(pca_res, data = df_all, main=titlePCA, colour = 'date', x=1, y=2)


#select mouse
mouse_select <- 'm25'
MouseT1 <- filter(MouseT1,target1==mouse_select)
MouseT1 <- filter(MouseT1, date != '10/21/20' & date != '10/22/20' & date != '10/23/20' & date != '10/24/20' & date != '10/25/20' & date != '10/26/20' & date != '10/27/20' & date != '10/28/20' & date != '10/29/20' & date != '10/30/20' & date != '10/31/20')

mouse_select <- 'm26'
MouseT1 <- filter(MouseT1,target1==mouse_select)
MouseT1 <- filter(MouseT1, date != '10/21/20' & date != '10/22/20' & date != '10/23/20' & date != '10/24/20' & date != '10/25/20' & date != '10/26/20' & date != '10/27/20' & date != '10/28/20' & date != '10/29/20' & date != '10/30/20' & date != '10/31/20')

mouse_select <- 'm27'
MouseT1 <- filter(MouseT1,target1==mouse_select)
MouseT1 <- filter(MouseT1, date != '10/21/20' & date != '10/22/20' & date != '10/23/20' & date != '10/24/20' & date != '10/25/20' & date != '10/26/20' & date != '10/27/20' & date != '10/28/20' & date != '10/29/20' & date != '10/30/20' & date != '10/31/20')

#select feature
MouseT1 <- read.table('example3mice_sd300.csv', sep=',', header=TRUE, stringsAsFactors=TRUE)
mouse_select <- 'm26'
MouseT1 <- filter(MouseT1,target1==mouse_select)
MouseT1 <- filter(MouseT1, date != '10/21/20' & date != '10/22/20' & date != '10/23/20' & date != '10/24/20' & date != '10/25/20' & date != '10/26/20' & date != '10/27/20' & date != '10/28/20' & date != '10/29/20' & date != '10/30/20' & date != '10/31/20')
#MouseT1 <- filter(MouseT1, speed > 2)

#features: 5-area, 6-size	7-speed	8-x_head	9-y_head	10-rotation	11-x_center	12-y_center	13-temperature	14-temperature_speed
MouseT <- MouseT1[c(5,6,7,10,13)]
columnSelect <- toString(colnames(MouseT))
MouseTr <- t(MouseT)

df_all <- as.data.frame(MouseT)
df_allT <- t(df_all)
colnames(df_allT) <- MouseT1$date
colnames1 <- as.factor(colnames(df_all))

plot(df_all, main=mouse_select)

plot(df_all$speed, main=mouse_select)

#change point detection
autoplot(cpt.meanvar(df_allT), ts.geom = 'bar', fill = 'blue', cpt.width=10, main=colnames(MouseT), legend=TRUE, xlab=MouseT1$date, ylab = deparse(substitute(date)))


#pca
titlePCA <- paste("PCA: mouse:", mouse_select, "features", columnSelect, sep=" ")
pca_res <- prcomp(df_all, scale. = TRUE, center=TRUE)
autoplot(pca_res, data = MouseT1, main=titlePCA, colour = 'is_sick', x=1, y=3, loadings=TRUE, loadings.label = TRUE, loadings.colour = 'black',frame = TRUE, frame.type = 'norm')
autoplot(pca_res, data = MouseT1, main=titlePCA, colour = 'date', x=1, y=3, frame = TRUE, label = TRUE, label.size = 3)

#location
library(ggplot2)
library(viridis)
library(gifski)

#pair plot
library(psych)
pairs.panels(MouseT1, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             main=mouse_select
)

ggplot(MouseT1, aes(area)) +
  geom_density(y=MouseT1$area) +
  ggtitle(mouse_select)

#position
ggplot(MouseT1, aes(x_center, y_center, color = date1, size=speed, width=600, height=300)) + 
  #geom_line() +
  #geom_tile() +
  geom_point(alpha = 0.5) + 
  scale_color_viridis() +
  ggtitle(mouse_select)

#position by date
titlePlot <- paste("Position by Day and Speed, Mouse:", mouse_select, sep=" ")
PosDate <- ggplot(MouseT1, aes(x=x_center, y=y_center, color=speed, shape=is_sick)) +
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
mean_area <- mean(MouseT1$area)

#by day
ggplot(MouseT1, aes(x=speed, fill=is_sick)) +
  geom_density(alpha=0.5) +
  scale_color_viridis() +
  ggtitle(titlePlot)+
  theme(legend.position="bottom")+
  facet_wrap(~ date, ncol = 2)+
  geom_vline(xintercept=mean_area, size=0.5, color="black",
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

#animated plot
p <- ggplot(MouseT1, aes(x_center, y_center, color = temperature, size=size)) + 
  #geom_line() +
  #geom_tile() +
  geom_point(alpha = 0.5) + 
  scale_color_viridis() +
  ggtitle(mouse_select) +
  labs(subtitle = "{closest_state}") +
  transition_states(states = MouseT1$date1, transition_length = 2) +
  #transition_reveal(MouseT1$date1) +
  ease_aes('linear') +
  shadow_wake(wake_length = 1)

animate(p, nframes = 10, width = 600, height = 300, renderer = gifski_renderer("gganim25.gif"))

