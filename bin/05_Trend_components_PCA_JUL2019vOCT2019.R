### Data considered: trend component of all the features of all data from July2019 vs October2019
### Script to perform principal component analysis (PCA) on the trend component of all the feature between mouse that died (July 2019) and survived (October 2019)
### input from the data folder (normalized data and decomposed data required)
### prints out to the PCA plot and PCA loadings plot to the figures folder

### Load libraries
library(tidyverse)
library(anytime)
library(ggplot2)
library(cowplot)
library(viridis)
library(ggfortify)

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


### Update annotation for the July2019 mice that survived and died

### Create a copy of existing annotation
MouseT1$upd_is_sick <- MouseT1$is_sick

### Display the old annotation as table for verification
print (table(MouseT1$Mouse, MouseT1$is_sick))

### Changes to be observed for the old vs new annotation
print (table(MouseT1$is_sick, MouseT1$upd_is_sick))

### update annotation for both pc and ucsf mouse of July2019 data
### This is learnt from analysing the output of the script Trend_component_comparison_for_revised_annotation.R
##### For meaningful comparison reduce to 4 categories
#### update annotation for both pc and ucsf mouse

MouseT1 <- MouseT1 %>% mutate(upd_is_sick = case_when(grepl("_pc", Mouse) & is_sick == "dead_D" ~ "sick_D",
                                                      grepl("_pc", Mouse) & is_sick == "healthy_S" ~ "healthy_D",
                                                      grepl("_pc", Mouse) & is_sick == "sick_S" ~ "sick_D",
                                                      grepl("_pc", Mouse) & is_sick == "survived_S" ~ "sick_D",
                                                      grepl("_ucsf", Mouse) & is_sick == "dead_D" ~ "sick_S",
                                                      grepl("_ucsf", Mouse) & is_sick == "healthy_D" ~ "healthy_S",
                                                      grepl("_ucsf", Mouse) & is_sick == "sick_D" ~ "sick_S",
                                                      grepl("_ucsf", Mouse) & is_sick == "survived_S" ~ "sick_S",
                                                      grepl("_ucsf", Mouse) & is_sick == "uninfected" ~ "healthy_S",
                                                      TRUE ~ upd_is_sick
                                                    ))

### Changes that are observed for the old vs new annotation
print (table(MouseT1$is_sick, MouseT1$upd_is_sick))

### PCA for trend component
### PCA between mouse that died (July 2019) and survived (October 2019)

### Create matrix for PCA leaving out unnecessary columns
df_all <- MouseT1[c(23,1,3,6:21)]

#pca
pca_res <- prcomp(df_all[,c(-1,-2, -3)], scale. = FALSE, center=FALSE)

### PCA plot
g1 <- autoplot(pca_res, data = df_all, colour ='Days', shape = 'upd_is_sick', frame = FALSE) + 
  scale_colour_viridis(limits = c(1,max(df_all$Days)), 
                       breaks = c(1:max(df_all$Days)),
                       labels = c(1:max(df_all$Days))) +
  scale_shape_discrete(labels = c("Healthy --> Died", "Healthy --> Survived", "Sick --> Died", "Sick --> Survived")) +
  theme_bw(base_size=14) + 
  theme(legend.position = "none")

# extract legend from plot g1
legend <- get_legend(
  g1 +
    guides(colour = guide_colourbar(title = "Days", barheight = unit(9, "cm"), reverse = TRUE)) + 
    guides(shape = guide_legend(title = "Mice Health Status")) +
    theme(legend.position = "right") 
)

### Save the PCA plot
png('PCA_Healthy_Sick_trend_all.png', width = 9, height = 7, units = "in", res = 300, pointsize = 12)
# Combine combined plot and legend using plot_grid()
p <- plot_grid(g1, legend, ncol=2, rel_widths = c(1, .35))
title <- ggdraw() + draw_label("PCA on decomposed trend features of \n polio infected mice that survived vs died (JUL, OCT 2019)", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
ttemp <- dev.off()

### rename the UCSF and PC as System1 and System2
df_all$Mouse <- as.factor(gsub("ucsf","System2",gsub("pc","System1",df_all$Mouse)))

g2 <- autoplot(pca_res, data =df_all, colour ='Mouse', shape = 'upd_is_sick', frame = FALSE, 
               loadings = TRUE, loadings.label = TRUE, loadings.colour = "Black", loadings.label.colour = "Black", loadings.label.repel=TRUE) + 
  scale_color_viridis(discrete = TRUE, option = "inferno")+
  scale_shape_discrete(labels = c("Healthy --> Died", "Healthy --> Survived", "Sick --> Died", "Sick --> Survived")) +
  theme_bw(base_size=14) + 
  theme(legend.position = "none")

# extract legend from plot g2
legend <- get_legend(
  g2 +
    guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
    guides(shape = guide_legend(title = "Mice Health Status")) +
    theme(legend.position = "right") 
)

### Save the PCA loadingsplot
png('PCA_loadings_Healthy_Sick_trend_all.png', width = 9, height = 7, units = "in", res = 300, pointsize = 12)
# Combine combined plot and legend using plot_grid()
p <- plot_grid(g2, legend, ncol=2, rel_widths = c(1, .35))
title <- ggdraw() + draw_label("PCA loadings plot on important trend features of \n polio infected mice that vary by measurement system (JUL, OCT 2019)", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
ttemp <- dev.off()


