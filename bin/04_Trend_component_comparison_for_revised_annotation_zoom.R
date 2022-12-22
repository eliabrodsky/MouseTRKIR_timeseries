### Data considered: trend component of temperature feature of all data from July2019 vs October2019
### Script to compare trend component of temperature feature between mouse that died (July 2019) and survived (October 2019)
### This comparison leads to identification that some of the surivived mice were wrongly assumed to be dead in July2019 data 
### input from the data folder (normalized data and decomposed data required)
### prints out to the figures folder with change in scale

### Load libraries
library(tidyverse)
library(anytime)
library(ggplot2)
library(cowplot)
library(viridis)

### Set data and figures directories location
figures_output = "/Users/Raghav/Pine Biotech/Projects/Time Series Mouse Project/Publication/figures/"
data_dir = "/Users/Raghav/Pine Biotech/Projects/Time Series Mouse Project/Publication/data/"

### set output directory to figures
setwd(figures_output)

### combine all survived mouse(October 2019), trend component only 
lf <- list.files(paste0(data_dir, "October2019/decomposed data/"), pattern=glob2rx("*healthy_*_tr.csv"))

file_ctr = 0
for ( file.lst in lf) {
  print (paste0("Processing: ", data_dir, "October2019/decomposed data/", file.lst))
  file_ctr = file_ctr + 1
  
  ### Read the source file before decomposition for time stamps
  M_oct2019_hlth <- read.csv(paste0(data_dir, "October2019/normalized data/",gsub("_tr","",file.lst)))
  M_oct2019_hlth$date_mod <- anydate(M_oct2019_hlth$date)
  M_oct2019_hlth$time_mod <- as.POSIXct(paste(M_oct2019_hlth$date_mod, format(strptime(M_oct2019_hlth$time, "%I:%M:%S %p"), "%H:%M:%S")), format = "%Y-%m-%d %H:%M:%S")
  
  temp_avg_3hr_time = c()
  temp_start = 1
  for (j in 1:nrow(M_oct2019_hlth)){
    if (M_oct2019_hlth$time_mod[j] > (M_oct2019_hlth$time_mod[temp_start] + 60 * 60 * 3)) {
      temp_j <- j - 1
      temp_avg_3hr_time = c(temp_avg_3hr_time,temp_j)
      temp_start = j
    }
    if (M_oct2019_hlth$time_mod[j] > (M_oct2019_hlth$time_mod[1] + 60 * 60 * 24 * 4)) {
      
    }
  }
  if(temp_start < nrow(M_oct2019_hlth)) { 
    temp_avg_3hr_time = c(temp_avg_3hr_time,nrow(M_oct2019_hlth))
  }
  
  ### Read the trend component files and merge with the time stamps
  M_oct2019_hlth_tr <- read.csv(paste0(data_dir, "October2019/decomposed data/",file.lst))
  temp_str = gsub("_tr.csv","",file.lst)
  temp_str = gsub("october_healthy_","",temp_str)
  temp_df <- M_oct2019_hlth_tr
  temp_df$Mouse = temp_str
  temp_df$Date = anydate(M_oct2019_hlth[temp_avg_3hr_time,1])
  
  temp_df$hr_cntr = 1:nrow(temp_df)
  temp_df$is_sick = "healthy_S"
  temp_df$Days = 1
  temp_df <- temp_df %>% dplyr::select(Mouse, Date, Days, hr_cntr, is_sick, everything()) %>% as_tibble()
  
  ###### Calculate Days as numeric vector
  temp_df <- temp_df %>% mutate(Days = if_else(temp_df$Date == (temp_df$Date[1]),1, as.numeric(temp_df$Date - temp_df$Date[1])))

  ###### Rename temp_df$is_sick first three days: healthy, next days as sick, last day as survived
  temp_df <- temp_df %>% mutate(is_sick = if_else(temp_df$Date > (temp_df$Date[1]+3),"sick_S", "healthy_S"))
  temp_df[nrow(temp_df),'is_sick'] <- "survived_S"
  
  if(file_ctr == 1){ 
    comb_M_oct2019_hlth_tr = temp_df
  } else {
    comb_M_oct2019_hlth_tr = rbind(comb_M_oct2019_hlth_tr, temp_df)
  }
}  


### combine all dead mouse(july 2019), trend component only  
lf <- list.files(paste0(data_dir, "July2019/decomposed data/"), pattern=glob2rx("*sick_*_tr.csv"))

file_ctr = 0
for ( file.lst in lf) {
  print (paste0("Processing: ", data_dir, "July2019/decomposed data/", file.lst))
  file_ctr = file_ctr + 1

  ### Read the source file before decomposition for time stamps
  M_jul2019_sick <- read.csv(paste0(data_dir, "July2019/normalized data/",gsub("_tr","",file.lst)))
  M_jul2019_sick$date_mod <- anydate(M_jul2019_sick$date)
  M_jul2019_sick$time_mod <- as.POSIXct(paste(M_jul2019_sick$date_mod, format(strptime(M_jul2019_sick$time, "%I:%M:%S %p"), "%H:%M:%S")), format = "%Y-%m-%d %H:%M:%S")
  
  temp_avg_3hr_time = c()
  temp_start = 1
  for (j in 1:nrow(M_jul2019_sick)){
    if (M_jul2019_sick$time_mod[j] > (M_jul2019_sick$time_mod[temp_start] + 60 * 60 * 3)) {
      temp_j <- j - 1
      temp_avg_3hr_time = c(temp_avg_3hr_time,temp_j)
      temp_start = j
    }
  }
  if(temp_start < nrow(M_jul2019_sick)) { 
    temp_avg_3hr_time = c(temp_avg_3hr_time,nrow(M_jul2019_sick))
  }
  
  
  ### Read the trend component files and merge with the time stamps
  M_jul2019_sick_tr <- read.csv(paste0(data_dir, "July2019/decomposed data/",file.lst))
  temp_str = gsub("_tr.csv","",file.lst)
  temp_str = gsub("avg_july_sick_","",temp_str)
  temp_df <- M_jul2019_sick_tr
  temp_df$Mouse = temp_str
  temp_df$Date = anydate(M_jul2019_sick[temp_avg_3hr_time,1])
  
  temp_df$hr_cntr = 1:nrow(temp_df)
  temp_df$is_sick = "sick"
  temp_df$Days = 1
  temp_df <- temp_df %>% dplyr::select(Mouse, Date, Days, hr_cntr, is_sick, everything()) %>% as_tibble()
  
  ###### Calculate Days as numeric vector
  temp_df <- temp_df %>% mutate(Days = if_else(temp_df$Date == (temp_df$Date[1]),1, as.numeric(temp_df$Date - temp_df$Date[1])))

  ###### Rename temp_df$is_sick first three days: healthy, next days as sick, last day as survived
  temp_df <- temp_df %>% mutate(is_sick = if_else(temp_df$Date > (temp_df$Date[1]+3),"sick_D", "healthy_D"))
  temp_df[nrow(temp_df),'is_sick'] <- "dead_D"
  
  if(file_ctr == 1){ 
    comb_M_jul2019_sick_tr = temp_df
  } else {
    comb_M_jul2019_sick_tr = rbind(comb_M_jul2019_sick_tr, temp_df)
  }
  
}  


### merge healthy and sick trend data into a combined data frame
MouseT1 <- rbind(comb_M_oct2019_hlth_tr, comb_M_jul2019_sick_tr)

### Numerical encoding for the ucsf and pc cage types
temp_col <- MouseT1$Mouse
temp_col <- gsub("_ucsf","",temp_col)
temp_col <- gsub("1_pc","5",temp_col)
temp_col <- gsub("2_pc","6",temp_col)
temp_col <- gsub("3_pc","7",temp_col)
temp_col <- gsub("4_pc","8",temp_col)
temp_col <- as.numeric(temp_col)
MouseT1$temp_col <- temp_col


### Temperature
## Survived mice from July2019 and October2019
g1 <- ggplot()+
  geom_line(data = (MouseT1 %>% filter(Mouse=="1_ucsf") %>% filter(grepl("_S",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  geom_line(data = (MouseT1 %>% filter(Mouse=="2_ucsf") %>% filter(grepl("_S",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  geom_line(data = (MouseT1 %>% filter(Mouse=="3_ucsf") %>% filter(grepl("_S",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) + 
  geom_line(data = (MouseT1 %>% filter(Mouse=="4_ucsf") %>% filter(grepl("_S",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  geom_line(data = (MouseT1 %>% filter(Mouse=="1_pc") %>% filter(grepl("_S",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  geom_line(data = (MouseT1 %>% filter(Mouse=="2_pc") %>% filter(grepl("_S",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  geom_line(data = (MouseT1 %>% filter(Mouse=="3_pc") %>% filter(grepl("_S",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  geom_line(data = (MouseT1 %>% filter(Mouse=="4_pc") %>% filter(grepl("_S",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  scale_colour_viridis(limits = c(1,max(temp_col)), 
                       breaks = c(1:max(temp_col)),
                       #labels = c("UCSF1", "UCSF2", "UCSF3", "UCSF4", "PC1", "PC2", "PC3", "PC4"),
                       labels = c("Mouse1", "Mouse2", "Mouse3", "Mouse4", "Mouse5", "Mouse6", "Mouse7", "Mouse8"), 
                       option = "inferno") +
  #xlim(0, 80) + ylim(-2.5, 1) + geom_vline(xintercept = 25, linetype="dotted") +
  xlim(0, 80) + ylim(-0.5, 0.5) + geom_vline(xintercept = 25, linetype="dotted") +
  labs(x = "Time (Hours)",
       y = "Trend component of Temperature",
       title = "Survived Mice (old annotation)") + 
  theme_bw(base_size = 10) +
  theme(legend.position = "none")

## Dead mice from July2019 and October2019
g2 <- ggplot()+
  geom_line(data = (MouseT1 %>% filter(Mouse=="1_ucsf") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  geom_line(data = (MouseT1 %>% filter(Mouse=="2_ucsf") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  geom_line(data = (MouseT1 %>% filter(Mouse=="3_ucsf") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  geom_line(data = (MouseT1 %>% filter(Mouse=="4_ucsf") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  geom_line(data = (MouseT1 %>% filter(Mouse=="1_pc") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  geom_line(data = (MouseT1 %>% filter(Mouse=="2_pc") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  geom_line(data = (MouseT1 %>% filter(Mouse=="3_pc") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  geom_line(data = (MouseT1 %>% filter(Mouse=="4_pc") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  scale_colour_viridis(limits = c(1,max(temp_col)), 
                       breaks = c(1:max(temp_col)),
                       #labels = c("UCSF1", "UCSF2", "UCSF3", "UCSF4", "PC1", "PC2", "PC3", "PC4"), 
                       labels = c("Mouse1", "Mouse2", "Mouse3", "Mouse4", "Mouse5", "Mouse6", "Mouse7", "Mouse8"), 
                       option = "inferno") +
  #xlim(0, 80) + ylim(-2.5, 1) + geom_vline(xintercept = 25, linetype="dotted") +
  xlim(0, 80) + ylim(-0.5, 0.5) + geom_vline(xintercept = 25, linetype="dotted") +
  labs(x = "Time (Hours)",
       y = "Trend component of Temperature",
       title = "Dead Mice (old annotation)") + 
  theme_bw(base_size = 10) +
  theme(legend.position = "none")

# combine both plot using plot_grid()
combined_plot<-plot_grid(g1, g2, ncol=2, labels = "AUTO")

# extract legend from plot1
legend <- get_legend(
  g1 +
    guides(colour=guide_legend(title="System2:\nSystem1:", nrow = 2, byrow = TRUE)) + 
    theme(legend.position = "bottom", legend.key.height = unit(0.3, "cm"))
)


#png('Temperature_trend_old_annotn.png', width = 10, height = 4, units = "in", res = 300, pointsize = 12)
png('Temperature_trend_old_annotn_zoom.png', width = 10, height = 4, units = "in", res = 300, pointsize = 12)
# Combine combined plot and legend using plot_grid()
p <- plot_grid(combined_plot, legend,ncol=1,rel_heights = c(1, .12))
title <- ggdraw() + draw_label("Trend feature of polio infected mice that surivived vs died (JUL, OCT 2019)", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
ttemp <- dev.off()

## Survived mice from July2019 (revised annotation)
g1 <- ggplot()+
  geom_line(data = (MouseT1 %>% filter(Mouse=="1_ucsf") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  geom_line(data = (MouseT1 %>% filter(Mouse=="2_ucsf") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  geom_line(data = (MouseT1 %>% filter(Mouse=="3_ucsf") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  geom_line(data = (MouseT1 %>% filter(Mouse=="4_ucsf") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  # geom_line(data = (MouseT1 %>% filter(Mouse=="1_pc") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  # geom_line(data = (MouseT1 %>% filter(Mouse=="2_pc") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  # geom_line(data = (MouseT1 %>% filter(Mouse=="3_pc") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  # geom_line(data = (MouseT1 %>% filter(Mouse=="4_pc") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  scale_colour_viridis(limits = c(1,max(temp_col)), 
                       breaks = c(1:max(temp_col)),
                       #labels = c("UCSF1", "UCSF2", "UCSF3", "UCSF4", "PC1", "PC2", "PC3", "PC4"), 
                       labels = c("Mouse1", "Mouse2", "Mouse3", "Mouse4", "Mouse5", "Mouse6", "Mouse7", "Mouse8"), 
                       option = "inferno") +
  #xlim(0, 80) + ylim(-2.5, 1) + geom_vline(xintercept = 25, linetype="dotted") +
  xlim(0, 80) + ylim(-0.5, 0.5) + geom_vline(xintercept = 25, linetype="dotted") +
  labs(x = "Time (Hours)",
       y = "Trend component of Temperature",
       title = "Survived Mice (revised annotation)") + 
  theme_bw(base_size = 10) +
  #guides(colour=guide_legend(title="UCSF:\nPC:", nrow = 2, byrow = TRUE)) + theme(legend.position = "bottom", legend.key.height = unit(0.4, "cm")) +
  theme(legend.position = "none")

## Dead mice from July2019 (revised annotation)
g2 <- ggplot()+
  # geom_line(data = (MouseT1 %>% filter(Mouse=="1_ucsf") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  # geom_line(data = (MouseT1 %>% filter(Mouse=="2_ucsf") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  # geom_line(data = (MouseT1 %>% filter(Mouse=="3_ucsf") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  # geom_line(data = (MouseT1 %>% filter(Mouse=="4_ucsf") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  geom_line(data = (MouseT1 %>% filter(Mouse=="1_pc") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  geom_line(data = (MouseT1 %>% filter(Mouse=="2_pc") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  geom_line(data = (MouseT1 %>% filter(Mouse=="3_pc") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  geom_line(data = (MouseT1 %>% filter(Mouse=="4_pc") %>% filter(grepl("_D",is_sick))), aes(x=hr_cntr,y=temp_tr, colour=temp_col)) +
  scale_colour_viridis(limits = c(1,max(temp_col)), 
                       breaks = c(1:max(temp_col)),
                       #labels = c("UCSF1", "UCSF2", "UCSF3", "UCSF4", "PC1", "PC2", "PC3", "PC4"), 
                       labels = c("Mouse1", "Mouse2", "Mouse3", "Mouse4", "Mouse5", "Mouse6", "Mouse7", "Mouse8"), 
                       option = "inferno") +
  #xlim(0, 80) + ylim(-2.5, 1) + geom_vline(xintercept = 25, linetype="dotted") +
  xlim(0, 80) + ylim(-0.5, 0.5) + geom_vline(xintercept = 25, linetype="dotted") +
  labs(x = "Time (Hours)",
       y = "Trend component of Temperature",
       title = "Dead Mice (revised annotation)") + 
  theme_bw(base_size = 10) +
  theme(legend.position = "none")

# combine both plot using plot_grid()
combined_plot<-plot_grid(g1, g2, ncol=2, labels = "AUTO")

# extract legend from plot1
legend <- get_legend(
  g1 +
    guides(colour=guide_legend(title="System2:\nSystem1:", nrow = 2, byrow = TRUE)) + 
    theme(legend.position = "bottom", legend.key.height = unit(0.3, "cm"))
)


#png('Temperature_trend_revised_annotn.png', width = 10, height = 4, units = "in", res = 300, pointsize = 12)
png('Temperature_trend_revised_annotn_zoom.png', width = 10, height = 4, units = "in", res = 300, pointsize = 12)
# Combine combined plot and legend using plot_grid()
p <- plot_grid(combined_plot, legend,ncol=1,rel_heights = c(1, .12))
title <- ggdraw() + draw_label("Trend feature of polio infected mice that surivived vs died (JUL 2019)", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
ttemp <- dev.off()

