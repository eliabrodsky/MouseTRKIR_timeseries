### Data considered: trend component of all the features of all data from July2019 vs February2019
### Script to perform Random Forest Classification (RF) on the trend component of all the feature between mouse that infected (July 2019) and uninfected (February 2019)
### input from the data folder (normalized data and decomposed data required)
### prints out to the RF confusion matrix, AUC-ROC plot and RF feature importance plot to the figures folder

### Load libraries
library(tidyverse)
library(anytime)
library(ggplot2)
library(cowplot)
library(viridis)
library(ggfortify)
#library(devtools)
#install_github('fawda123/ggord') 
library(ggord)
library(caret)
library(randomForest)
library(scales)
library(yardstick)
library(ROCR)
library(pROC)
library(data.table)

### Set data and figures directories location
figures_output = "/Users/Raghav/Pine Biotech/Projects/Time Series Mouse Project/Publication/figures/"
data_dir = "/Users/Raghav/Pine Biotech/Projects/Time Series Mouse Project/Publication/data/"

### set output directory to figures
setwd(figures_output)

### combine all uninfected mouse(February 2019), trend component only 
lf <- list.files(paste0(data_dir, "February2019/decomposed data/"), pattern=glob2rx("*healthy_*_tr.csv"))

file_ctr = 0
for ( file.lst in lf) {
  print (paste0("Processing: ", data_dir, "February2019/decomposed data/", file.lst))
  file_ctr = file_ctr + 1
  
  ### Read the source file before decomposition for time stamps
  M_feb2019_hlth <- read.csv(paste0(data_dir, "February2019/normalized data/",gsub("_tr","",file.lst)))
  DT <- as.data.table(M_feb2019_hlth)
  M_feb2019_hlth <- as.data.frame(DT[, lapply(.SD,mean), by=c("date","time")])
  M_feb2019_hlth$date_mod <- anydate(M_feb2019_hlth$date)
  M_feb2019_hlth$time_mod <- as.POSIXct(paste(M_feb2019_hlth$date_mod, format(strptime(M_feb2019_hlth$time, "%I:%M:%S %p"), "%H:%M:%S")), format = "%Y-%m-%d %H:%M:%S")
  
  temp_avg_3hr_time = c()
  temp_start = 1
  for (j in 1:nrow(M_feb2019_hlth)){
    if (M_feb2019_hlth$time_mod[j] > (M_feb2019_hlth$time_mod[temp_start] + 60 * 60 * 3)) {
      temp_j <- j - 1
      temp_avg_3hr_time = c(temp_avg_3hr_time,temp_j)
      temp_start = j
    }
    if (M_feb2019_hlth$time_mod[j] > (M_feb2019_hlth$time_mod[1] + 60 * 60 * 24 * 4)) {
      
    }
  }
  if(temp_start < nrow(M_feb2019_hlth)) { 
    temp_avg_3hr_time = c(temp_avg_3hr_time,nrow(M_feb2019_hlth))
  }
  
  ### Read the trend component files and merge with the time stamps
  M_feb2019_hlth_tr <- read.csv(paste0(data_dir, "February2019/decomposed data/",file.lst))
  temp_str = gsub("_tr.csv","",file.lst)
  temp_str = gsub("bottom_|top_","",temp_str)
  temp_str = gsub("left_healthy_|right_healthy_","",temp_str)
  temp_df <- M_feb2019_hlth_tr
  temp_df$Mouse = temp_str
  temp_df$Date = anydate(M_feb2019_hlth[temp_avg_3hr_time,1])
  
  temp_df$hr_cntr = 1:nrow(temp_df)
  temp_df$is_sick = "uninfected"
  temp_df$Days = 1
  temp_df <- temp_df %>% dplyr::select(Mouse, Date, Days, hr_cntr, is_sick, everything()) %>% as_tibble()
  
  ###### Calculate Days as numeric vector
  temp_df <- temp_df %>% mutate(Days = if_else(temp_df$Date == (temp_df$Date[1]),1, as.numeric(temp_df$Date - temp_df$Date[1])))
  
  if(file_ctr == 1){ 
    comb_M_feb2019_hlth_tr = temp_df
  } else {
    comb_M_feb2019_hlth_tr = rbind(comb_M_feb2019_hlth_tr, temp_df)
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
MouseT1 <- rbind(comb_M_feb2019_hlth_tr, comb_M_jul2019_sick_tr)

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

##### RF for trend components with plots
df_all <- MouseT1[c(23,6:21)]

#### RF with feature selection:
### make a 70/30 split of train and test data
set.seed(3456)
train.index <- createDataPartition(df_all$upd_is_sick, p = .7, list = FALSE)
train <- df_all[train.index,]
test  <- df_all[-train.index,]

train.data = train[,-1] #Set the training set
test.data = test[,-1] #Set the testing set

train.output = train[,1]  #store the labels of train data
test.output = test[,1]  # store the true labels of test data

train.output$upd_is_sick <- as.factor(train.output$upd_is_sick)
test.output$upd_is_sick <- as.factor(test.output$upd_is_sick)
### Fitting Random Forest to the train dataset
set.seed(234)
classifier_RF = randomForest(x = train.data,
                             y = train.output$upd_is_sick,
                             ntree = 500,
                             importance = TRUE)

### Print out details of the classiier
print(classifier_RF)

### Prediction on test.data as a whole
test.predict = predict(classifier_RF, test.data)

### Store the confusion matrix for accessibility
result <- confusionMatrix(as.factor(test.predict), test.output$upd_is_sick,
                          positive = "pos")

# Get importance values as a data frame
imp = as.data.frame(importance(classifier_RF))
imp = cbind(vars=rownames(imp), imp)
imp = imp[order(imp$MeanDecreaseAccuracy),]
imp$vars = factor(imp$vars, levels=unique(imp$vars))

barplot(imp$MeanDecreaseAccuracy, names.arg=imp$vars, las=2)

png('RF_feat_imp_uninfected_sick.png', width = 7, height = 10, units = "in", res = 300, pointsize = 12)
p <- imp %>% 
  pivot_longer(cols=matches("Mean")) %>%
  ggplot(aes(value, vars)) +
  geom_col() +
  geom_text(aes(label=round(value), x=0.5*value), size=5, colour="white") +
  facet_grid(. ~ name, scales="free_x") +
  scale_x_continuous(expand=expansion(c(0,0.04))) +
  theme_bw(base_size = 14) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.title=element_blank())

title <- ggdraw() + draw_label("Feature importance plot for Random Forest classification of polio \ninfected and uninfected mice health status (JUL, FEB 2019)", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
ttemp <- dev.off()

### Validation set assessment: ROC curves and AUC
## Calculate the probability of new observations belonging to each class
## prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
prediction_for_roc_curve <- predict(classifier_RF, test.data, type = "prob")

### Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF", "#D39200")

### Specify the different classes 
classes <- levels(test.output$upd_is_sick)
acc = c()
png('RF_ROC_uninfected_sick.png', width = 6.5, height = 6, units = "in", res = 300, pointsize = 12)

### For each class
for (i in 1:length(classes)) {
  # Define which observations belong to class[i]
  true_values <- ifelse(test.output$upd_is_sick==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    p1 <- plot(perf,main="ROC Curve for Random Forest classification of polio \ninfected and uninfected mice health status (JUL, FEB 2019)",
               col=pretty_colours[i], lwd=2)
  }
  else
  {
    p1 <- plot(perf,main="ROC Curve for Random Forest classification of polio \ninfected and uninfected mice health status (JUL, FEB 2019)",
               col=pretty_colours[i],add=TRUE, lwd=2) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  acc[i] = label_percent()(auc.perf@y.values[[1]])
  print(auc.perf@y.values)
}

# Adding a legend to the graph
# defining the lines 
legend(x = "bottomright", box.lwd = 2 , title="Classes", 
       legend=c(paste("Healthy --> Dead",acc[1]), 
                paste("Healthy/Unfected --> Survived", acc[2]), 
                paste("Sick --> Dead",acc[3]), 
                paste("Sick --> Survived", acc[4])),
       fill = pretty_colours)
ttemp <- dev.off()

### visualize confusion matrix using the caret package
truth_predicted <- data.frame( obs = test.output$upd_is_sick, pred = test.predict)
cm <- conf_mat(truth_predicted, obs, pred)
png('RF_CM_uninfected_sick.png', width = 6, height = 5, units = "in", res = 300, pointsize = 12)
autoplot(cm, type = "heatmap", ) +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1") +
  labs(x="True Class", y="Predicted Class", 
       title="Confusion Matrix for Random Forest classification of polio \ninfected and uninfected mice health status (JUL, FEB 2019)")
ttemp <- dev.off()
