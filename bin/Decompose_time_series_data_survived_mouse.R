### Data considered: Polio infected mouse (5 weeks old) that survived (October 2019)
### Script to visualize the data smoothening and decomposition of time series data
### Script to automate STL decomposition for all the data from October 2019 measurements
### input from the data folder
### prints out to the figures folder

### !!! Warning
#library(ggfortify) ### don't load this, it messes up with decomposition output settings in autoplot 
###If you have loaded this library eariler, the only way is to reload Rstudio

### Load libraries
library(tidyverse)
library(anytime)
library(ggplot2)
library(tseries)
library(xts)
library(forecast)

### Set data and figures directories location
figures_output = "/Users/Raghav/Pine Biotech/Projects/Time Series Mouse Project/Publication/figures/"
data_dir = "/Users/Raghav/Pine Biotech/Projects/Time Series Mouse Project/Publication/data/"

### set output directory to figures
setwd(figures_output)

### Mouse type considered for analysis: 
# Polio infected mouse (5 weeks old) that survived (October 2019)

### Load one normalized data for visualization
M_oct2019_hlth <- read.csv(paste0(data_dir, "October2019/normalized data/october_healthy_1_ucsf.csv"))

### Modify the date and time in proper format
M_oct2019_hlth$date_mod <- anydate(M_oct2019_hlth$date)
M_oct2019_hlth$time_mod <- as.POSIXct(paste(M_oct2019_hlth$date_mod, format(strptime(M_oct2019_hlth$time, "%I:%M:%S %p"), "%H:%M:%S")), format = "%Y-%m-%d %H:%M:%S")

### Visualize the decomposition for sick (dead) mouse from July 2019

### Calculate the time difference for decomposition
time_diff <- (M_oct2019_hlth$time_mod[nrow(M_oct2019_hlth)] - M_oct2019_hlth$time_mod[1])
print(time_diff)

### Feature selection: temperature M_oct2019_hlth$temp
### To decompose, we can create a time series by defining the frequency as the number of days times hours the samples are measured
M_jul2019_temp_ts <- ts(M_oct2019_hlth$temp, frequency=as.numeric(time_diff)*60)

### convert the above time series to include time stamps
OFS_xts <- xts(M_jul2019_temp_ts, order.by = M_oct2019_hlth$time_mod)

### Calculate a jumping average of the above time series for 3 hours window
temp_start = 1
temp_counter = 1
temp_avg_3hr = c()
temp_avg_3hr_time = c()
for (i in 1:nrow(M_oct2019_hlth)){
  if (M_oct2019_hlth$time_mod[i] > (M_oct2019_hlth$time_mod[temp_start] + 60 * 60 * 3)) {
    temp_i <- i - 1
    temp_avg_3hr[temp_counter] <- mean(M_jul2019_temp_ts[temp_start:temp_i]) 
    temp_avg_3hr_time = c(temp_avg_3hr_time,temp_i)
    temp_start = i
    temp_counter = temp_counter+1
    
  }
}
if(temp_start < nrow(M_oct2019_hlth)) { 
  temp_avg_3hr[temp_counter] <- mean(M_jul2019_temp_ts[temp_start:nrow(M_oct2019_hlth)])
  temp_avg_3hr_time = c(temp_avg_3hr_time,nrow(M_oct2019_hlth))
}

### convert the above averaged time series to include time stamps.
OFS_xts_avg_3hr <- xts(temp_avg_3hr, order.by = M_oct2019_hlth$time_mod[temp_avg_3hr_time])

### Plot the time series and its moving average
g1 <- ggplot() +
  geom_line(data = OFS_xts, aes(x=as.POSIXct(row.names(as.data.frame(OFS_xts))), y=as.data.frame(OFS_xts)$V1, color = 'Data')) + 
  geom_line(data = OFS_xts_avg_3hr, aes(x=as.POSIXct(row.names(as.data.frame(OFS_xts_avg_3hr))), y=as.data.frame(OFS_xts_avg_3hr)$V1, color = 'AVG(3hr)')) + 
  scale_color_manual(labels = c("AVG(3hr)", "Data"), values=c('red', 'grey')) +
  labs(title = "Data Smoothing for Temperature", x = "Date (2019)", y = "Temperature") + theme_bw(base_size = 10) +
  theme(legend.position = "none")

### Visualize the decomposition of the time series object (temperature feature)
temp_avg_3hr_ts <- ts(temp_avg_3hr, frequency=as.numeric(time_diff))

### Plot the decomposition of time series object into seasonal, trend and remainder components
a1 <- autoplot(stl(temp_avg_3hr_ts, t.window = 6, s.window = "periodic", robust=FALSE), range.bars = TRUE) +
  labs(title = "Data decomposition: Temperature", x = "Days (July 2019)", y = "Temperature")

### Feature selection: body speed M_oct2019_hlth$body_speed
### To decompose, we can create a time series by defining the frequency as the number of days times hours the samples are measured
M_jul2019_temp_ts <- ts(M_oct2019_hlth$body_speed, frequency=as.numeric(time_diff)*60)

### convert the above time series to include time stamps
OFS_xts_1 <- xts(M_jul2019_temp_ts, order.by = M_oct2019_hlth$time_mod)

### Calculate a jumping average of the above time series for 3 hours window
temp_start = 1
temp_counter = 1
temp_avg_3hr = c()
temp_avg_3hr_time = c()
for (i in 1:nrow(M_oct2019_hlth)){
  if (M_oct2019_hlth$time_mod[i] > (M_oct2019_hlth$time_mod[temp_start] + 60 * 60 * 3)) {
    temp_i <- i - 1
    temp_avg_3hr[temp_counter] <- mean(M_jul2019_temp_ts[temp_start:temp_i]) 
    temp_avg_3hr_time = c(temp_avg_3hr_time,temp_i)
    temp_start = i
    temp_counter = temp_counter+1
    
  }
}
if(temp_start < nrow(M_oct2019_hlth)) { 
  temp_avg_3hr[temp_counter] <- mean(M_jul2019_temp_ts[temp_start:nrow(M_oct2019_hlth)])
  temp_avg_3hr_time = c(temp_avg_3hr_time,nrow(M_oct2019_hlth))
}

### convert the above averaged time series to include time stamps.
OFS_xts_avg_3hr_1 <- xts(temp_avg_3hr, order.by = M_oct2019_hlth$time_mod[temp_avg_3hr_time])

### Plot the time series and its moving average
g2 <- ggplot() +
  geom_line(data = OFS_xts_1, aes(x=as.POSIXct(row.names(as.data.frame(OFS_xts_1))), y=as.data.frame(OFS_xts_1)$V1, color = 'Data')) + 
  geom_line(data = OFS_xts_avg_3hr_1, aes(x=as.POSIXct(row.names(as.data.frame(OFS_xts_avg_3hr_1))), y=as.data.frame(OFS_xts_avg_3hr_1)$V1, color = 'AVG(3hr)')) + 
  scale_color_manual(labels = c("AVG(3hr)", "Data"), values=c('red', 'grey')) +
  labs(title = "Data Smoothing for Body Speed", x = "Date (2019)", y = "Body Speed") + theme_bw(base_size = 10) +
  theme(legend.position = "none")

### Visualize the decomposition of the time series object (body speed feature)
temp_avg_3hr_ts <- ts(temp_avg_3hr, frequency=as.numeric(time_diff))

### Plot the decomposition of time series object into seasonal, trend and remainder components
a2 <- autoplot(stl(temp_avg_3hr_ts, t.window = 6, s.window = "periodic", robust=FALSE), range.bars = TRUE) +
  labs(title = "Data decomposition: Body Speed", x = "Days (July 2019)", y = "Body Speed")

### Plot for smoothening the data
# combine both plot using plot_grid()
combined_plot<-plot_grid(g1, g2, ncol=2, labels = "AUTO")

# extract legend from plot1
legend <- get_legend(
  g1 +
    guides(colour=guide_legend(title=NULL, nrow = 1, byrow = TRUE)) + theme(legend.position = "bottom")
)

png('data_smooth_healthy.png', width = 8, height = 4, units = "in", res = 300, pointsize = 12)
# Combine combined plot and legend using plot_grid()
p <- plot_grid(combined_plot, legend,ncol=1,rel_heights = c(1, .12))
title <- ggdraw() + draw_label("Time series data for polio infected mice that survived (OCT 2019)", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
ttemp <- dev.off()

###Plot for visualizing the decomposition of time series
png('data_decomp_healthy.png', width = 10, height = 4, units = "in", res = 300, pointsize = 12)
p <- plot_grid(a1, a2, labels = "AUTO")
title <- ggdraw() + draw_label("Time series decomposition for polio infected mice that survived (OCT 2019)", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
ttemp <- dev.off()

### automate the above process for all features in all the files in October2019/normalized data/ folder
lf <- list.files(paste0(data_dir, "October2019/normalized data"), pattern=("*.csv"))

for ( file.lst in lf) {
  print (paste0("Processing: ",data_dir, "July2019/normalized data/",file.lst))
  M_oct2019_hlth <- read.csv(paste0(data_dir, "October2019/normalized data/",file.lst))
  M_oct2019_hlth$date_mod <- anydate(M_oct2019_hlth$date)
  M_oct2019_hlth$time_mod <- as.POSIXct(paste(M_oct2019_hlth$date_mod, format(strptime(M_oct2019_hlth$time, "%I:%M:%S %p"), "%H:%M:%S")), format = "%Y-%m-%d %H:%M:%S")
  
  time_diff <- (M_oct2019_hlth$time_mod[nrow(M_oct2019_hlth)] - M_oct2019_hlth$time_mod[1])
  
  if(time_diff >=2) {
    M_oct2019_hlth_trend <- c()
    M_oct2019_hlth_seas <- c()
    for (i in 3:(ncol(M_oct2019_hlth)-2)){
      temp_ts <- ts(M_oct2019_hlth[,i], frequency=as.numeric(time_diff)*60)
      
      temp_start = 1
      #print(temp_start)
      temp_counter = 1
      temp_avg_3hr = c()
      temp_avg_3hr_time = c()
      for (j in 1:nrow(M_oct2019_hlth)){
        if (M_oct2019_hlth$time_mod[j] > (M_oct2019_hlth$time_mod[temp_start] + 60 * 60 * 3)) {
          temp_j <- j - 1
          temp_avg_3hr[temp_counter] <- mean(temp_ts[temp_start:temp_j]) 
          temp_avg_3hr_time = c(temp_avg_3hr_time,temp_j)
          #print(temp_j - temp_start)
          temp_start = j
          temp_counter = temp_counter+1
          
        }
      }
      if(temp_start < nrow(M_oct2019_hlth)) { 
        temp_avg_3hr[temp_counter] <- mean(temp_ts[temp_start:nrow(M_oct2019_hlth)])
        temp_avg_3hr_time = c(temp_avg_3hr_time,nrow(M_oct2019_hlth))
      }
      
      temp_avg_3hr_ts <- ts(temp_avg_3hr, frequency=as.numeric(time_diff))
      
      temp_ts_dec <- stl(temp_avg_3hr_ts, t.window = 6, s.window = "periodic", robust=FALSE)
      
      M_oct2019_hlth_trend <- cbind(M_oct2019_hlth_trend, trendcycle(temp_ts_dec))
      M_oct2019_hlth_seas <- cbind(M_oct2019_hlth_seas, seasonal(temp_ts_dec))
    }
    
    colnames(M_oct2019_hlth_trend) <- paste0(colnames(M_oct2019_hlth[,c(-1,-2,-(ncol(M_oct2019_hlth)-1),-ncol(M_oct2019_hlth))]),"_tr")
    colnames(M_oct2019_hlth_seas) <- paste0(colnames(M_oct2019_hlth[,c(-1,-2,-(ncol(M_oct2019_hlth)-1),-ncol(M_oct2019_hlth))]),"_ss")
    
    write.table(M_oct2019_hlth_trend,file=paste0(data_dir, "October2019/decomposed data/",gsub(".csv","_tr.csv",file.lst)), sep=',',  quote = F, row.names = FALSE)
    write.table(M_oct2019_hlth_seas,file=paste0(data_dir, "October2019/decomposed data/",gsub(".csv","_ss.csv",file.lst)), sep=',',  quote = F, row.names = FALSE)
  } 
}

