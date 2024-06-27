rm(list=ls())
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)
library(tidyverse)

setwd("C:/Users/xcarrj/Desktop/turtleacc_analysis/loggers_lab_accdata/pos2")


files<-list.files()
files<-paste0("C:/Users/xcarrj/Desktop/turtleacc_analysis/loggers_lab_accdata/pos2/", files)

tables <- lapply(files, read.csv, header = TRUE)
data_full <- do.call(rbind , tables)


data_short<-data_full[,c(2,3,4,8,9,11)]

acc.data<-data_short

colnames(acc.data)[colnames(acc.data) == "Behavior"] ="behaviour"

table(acc.data$id)

write.csv(acc.data, "acc.data_IDscombined_p2.csv")

#Label each behavioural segment by when the behaviour changes ####
acc.data<-acc.data %>%
  mutate(segID = cumsum(behaviour != lag(behaviour) | row_number() == 1))
table(acc.data$segID)

write.csv(acc.data, "acc.data_segslabs_p2.csv")



###############################################################################################################
#now process and get different metrics for different rollign means
rm(list=ls())
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)
library(tidyverse)
library(zoo)
graphics.off()

my.data<-read.csv("acc.data_segslabs_p2.csv")
table(my.data$segID)
pos="p2"
Hz<-100

#1,2,3,4,5 seconds
rolling50hz<-c(100,200,300,400,500)

#loop through to get metrics of different segment lengths
rolling<-rolling50hz
############################################################
#this section of code will trim segments to specific length#
############################################################
for(r in 1:length(rolling)){
  #################parms begin###############
  attach(my.data)
  bl_len = rolling[r]
  
  #################parms end###############
  
  #################split data begin##############
  
  lab1 = unique(segID)
  
  empty = data.frame(matrix(nrow = 0, ncol = (ncol(my.data)+1)))
  names(empty) = c(names(my.data),"Block_ID")
  Block_ID_v = 1
  
  
  
  for(i in 1:length(lab1)){
    
    seg = my.data[which(my.data$segID == lab1[i]),]
    
    if(nrow(seg)>bl_len){
      
      trm =  floor(nrow(seg)/bl_len)*bl_len
      seg = seg[1:trm,]
      
      Block_ID = rep(NA,nrow(seg))
      
      blks = nrow(seg)/bl_len  
      
      for(j in 1:blks){
        
        Block_ID[(1 + bl_len*(j-1)):(bl_len+bl_len*(j-1))] = Block_ID_v
        Block_ID_v = Block_ID_v + 1
        print(paste("block",Block_ID_v-1,seg[1,]$id,seg[1,]$behaviour,sep = " "))
        
      }
      
      seg = cbind(seg,Block_ID)
      
      empty = rbind(empty,seg)
      
    }
    
    
  }  
  
  #################split data end##############
  
  ############test begin#################
  
  blocks = unique(empty$Block_ID)
  
  hold = c()
  for(i in 1:length(blocks)){
    
    print(length(
      which(
        empty$Block_ID == blocks[i]
      )
    ))
    hold[i] = length(which(empty$Block_ID == blocks[i]))
    
  }
  
  unique(hold)
  
  ############test end#################
  
  
  write.csv(empty, paste0("100hzsegmented_accData",bl_len,pos, "seglength_loggers.csv"))
  
  detach(my.data)
  
}

##############################################
#### resample different frequnecies here  ####
##############################################

rm(list=ls())
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)
library(tidyverse)
library(zoo)


full_freq<-c(100,200,300,400,500)

for(s in 1:length(full_freq)){
  
  input_file <- paste0("C:/Users/xcarrj/Desktop/turtleacc_analysis/loggers_lab_accdata/pos2/100hzsegmented_accData", full_freq[s],"p2seglength_loggers.csv")
  
  my.data<-read.csv(input_file)
  
  #add row nums to check
  my.data$rownum<-1:nrow(my.data)
  #format datetime to arrange by datetime just in case
  my.data$time2<-as.POSIXct(my.data$time, "%Y-%m-%d %H:%M:%OS",tz = "UTC")
  
  # #resample
    slice_50<-my.data %>%
    group_by(Block_ID) %>%
    arrange(time2) %>%
    slice(seq(1, n(), by =2))
  
  write.csv(slice_50,paste0("resamp_50hz_", full_freq[s],"_p2seglength_loggers.csv"))
  
  slice_25<-slice_50 %>%
    group_by(Block_ID) %>%
    arrange(time2) %>%
    slice(seq(1, n(), by =2))
  
  write.csv(slice_25,paste0("resamp_25hz_", full_freq[s],"_p2seglength_loggers.csv"))
  
  
  # #resample
  slice_10<-my.data %>%
    group_by(Block_ID) %>%
    arrange(time2) %>%
    slice(seq(1, n(), by =(10)))
  
  write.csv(slice_10,paste0("resamp_10hz_", full_freq[s],"_p2seglength_loggers.csv"))
  

  
  #add new row for original 50hz
  data_len<-nrow(my.data)/100
  my.data$fhz<-rep(1:data_len, each=100)
    
  
  
  #now resample small freqs.
  slice_12<-my.data %>%
    group_by(fhz) %>%
    arrange(time2) %>%
    slice(1:(n()-2)) %>%
    slice(seq(0, n(), by =(8)))
  
  
  write.csv(slice_12,paste0("resamp_12hz_", full_freq[s],"_p2seglength_loggers.csv"))
  
  #now resample small freqs.
  slice_8<-my.data %>%
    group_by(fhz) %>%
    arrange(time2) %>%
    slice(1:(n()-2)) %>%
    slice(seq(0, n(), by =(12)))
  
  
  write.csv(slice_8,paste0("resamp_8hz_", full_freq[s],"_p2seglength_loggers.csv"))
  
  
  slice_6<-my.data %>%
    group_by(fhz) %>%
    arrange(time2) %>%
    slice(1:(n()-2))%>%
    slice(seq(0, n(), by =(16)))
  
  write.csv(slice_6,paste0("resamp_6hz_", full_freq[s],"_p2seglength_loggers.csv"))
  
  #resample
  slice_4<-my.data %>%
    group_by(fhz) %>%
    arrange(time2) %>%
    slice(1:(n()-2))%>%
    slice(seq(0, n(), by =(24)))
  
  write.csv(slice_4,paste0("resamp_4hz_", full_freq[s],"_p2seglength_loggers.csv"))
  
  
  #resample
  slice_2<-my.data %>%
    group_by(fhz) %>%
    arrange(time2) %>%
    slice(1:(n()-2))%>%
    slice(seq(0, n(), by =(24)))
  
  write.csv(slice_2,paste0("resamp_2hz_", full_freq[s],"_p2seglength_loggers.csv"))
  
}



###############################################
##########calculate training metrics###########
###############################################

rm(list=ls())
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)
library(tidyverse)

files<-paste0("C:/Users/xcarrj/Desktop/turtleacc_analysis/loggers_lab_accdata/pos2/")

#1,2,3,4,5,6,7,8 seconds

# now extract metrics
#need to do

hz_rp<-rep(c(100,50,25,12,10,8,6,4,2),5)

samp_rep<-c(rep(c(100),9), rep(c(200),9),rep(c(300),9),rep(c(400),9),rep(c(500),9))

met_df<-as.data.frame(cbind(hz_rp,samp_rep))

pos="p2"

for (m in 1:nrow(met_df)){
  
  if(met_df$hz_rp[m]==100){
  input_file <- paste0("100hzsegmented_accData", met_df[m,2], pos,"seglength_loggers.csv")
  }else{
  input_file <- paste0("resamp_", met_df[m,1], "hz_", met_df[m,2],"_", pos, "seglength_loggers.csv")
  }
  
  data<-read.csv(input_file)
  
  print(paste("calculating metrics","smooth",met_df[m,2],"freq",met_df[m,1]))
  
  
  # empty<-read.csv("C:/Users/user/Documents/R/Accelerometers/2Hz/2hzsegmented_accData.csv")
  
  #empty<-my.data
  Hz<-met_df[m,1]
  
  runmean<-met_df[m,2]
  
  #Set the require segment length
  
  freq <- Hz# The Frequency of accelerometry data (Hz)
  secs <- runmean/100 # The number of seconds over which to calculate the desired metrics.
  # The manuscript says to use 1 second intervals, but Phil said "to capture 
  # gliding flight as well I've found that a longer period is needed."
  numrows <- freq*secs # The number of rows required to calculate metrics over the chosen period. 
  
  train<-data
  
  
  train$pango_seg <- paste(train$id,train$Block_ID,sep="_") #If there are multiple individuals
  #head(train)
  
  #Calculate metrics ####
  
  #Calculate pitch & roll
  train$pitch <- atan((train$X/(sqrt((train$Y*train$Y)+(train$Z*train$Z)))))*(180/pi)
  train$roll <- atan((train$Y/(sqrt((train$X*train$X)+(train$Z*train$Z)))))*(180/pi) 
  #look up yaw
  #train$yaw <- atan((train$Z/(sqrt((train$X*train$X)+(train$Y*train$Y)))))*(180/pi) 
  
  
  #Calculate the metrics for each segment
  pango_seg <- unique(train$pango_seg)
  train_metrics <- as.data.frame(pango_seg)
  #head(train_metrics)
  
  
  #Create blank columns so that NA will appear if the calculations fail If this is not done, 
  #the value for the first iteration of the loop will appear when calculation fail, which'll be wrong
  #Loop will calculate:
  train_metrics$ODBA <- NA
  train_metrics$VeDBA <- NA
  
  train_metrics$meanX_surge <- NA
  train_metrics$stdevX_surge <-NA
  train_metrics$minX_surge <- NA
  train_metrics$maxX_surge <- NA
  
  train_metrics$pitch_mean <- NA
  train_metrics$pitch_stdev <- NA
  train_metrics$roll_mean <- NA
  train_metrics$roll_stdev <- NA
  #train_metrics$yaw_mean <- NA
  #train_metrics$yaw_stdev <- NA
  
  
  train_metrics$meanY_sway <- NA
  train_metrics$stdevY_sway <-NA
  train_metrics$minY_sway <- NA
  train_metrics$maxY_sway <- NA
  
  train_metrics$meanZ_heave <- NA
  train_metrics$stdevZ_heave <-NA
  train_metrics$minZ_heave <- NA
  train_metrics$maxZ_heave <- NA
  
  head(train_metrics)
  
  
  #Loop through each segment
  #This may take a few hours
  for (i in 1:nrow(train_metrics)){
    
    #Extract segment
    seg <- subset(train,pango_seg == train_metrics$pango_seg[i])
    
    #####Calculate ODBA and VeDBA####
    
    #First calculate the static g force
    StaticX <- sum(zoo::rollapply(seg$X,numrows,mean,fill=NA),na.rm = T)
    StaticY <- sum(zoo::rollapply(seg$Y,numrows,mean,fill=NA),na.rm = T)
    StaticZ <- sum(zoo::rollapply(seg$Z,numrows,mean,fill=NA),na.rm = T)
    
    #together should = ~1
    #StaticX + StaticY + StaticZ
    
    ####Calculates DBA for each axis. 
    seg$DynamicX <- seg$X-StaticX
    seg$DynamicY <- seg$Y-StaticY
    seg$DynamicZ <- seg$Z-StaticZ
    
    #Combines the DBA into ODBA or VeDBA
    seg$ODBA  <- abs(seg$DynamicX)+abs(seg$DynamicY)+abs(seg$DynamicZ)
    seg$VeDBA <- sqrt((seg$DynamicX^2)+(seg$DynamicY^2)+(seg$DynamicZ^2))
    
    #Store the mean OBDA/VeDBA for the segment
    train_metrics$ODBA[i]  <- mean(seg$ODBA)
    train_metrics$VeDBA[i] <- mean(seg$VeDBA)
    
    #Calculate and store trhe other metrics
    train_metrics$meanX_surge[i] <- mean(seg$X)
    train_metrics$stdevX_surge[i] <- sd(seg$X)
    train_metrics$minX_surge[i] <- min(seg$X)
    train_metrics$maxX_surge[i] <- max(seg$X)
    
    train_metrics$meanY_sway[i] <- mean(seg$Y)
    train_metrics$stdevY_sway[i] <- sd(seg$Y)
    train_metrics$minY_sway[i] <- min(seg$Y)
    train_metrics$maxY_sway[i] <- max(seg$Y)
    
    train_metrics$meanZ_heave[i] <- mean(seg$Z)
    train_metrics$stdevZ_heave[i] <- sd(seg$Z)
    train_metrics$minZ_heave[i] <- min(seg$Z)
    train_metrics$maxZ_heave[i] <- max(seg$Z)
    
    train_metrics$pitch_mean[i] <- mean(seg$pitch)
    train_metrics$pitch_stdev[i] <- sd(seg$pitch)
    train_metrics$roll_mean[i] <- mean(seg$roll)
    train_metrics$roll_stdev[i] <- sd(seg$roll)
    # train_metrics$yaw_mean[i] <- mean(seg$yaw)
    # train_metrics$yaw_stdev[i] <- sd(seg$yaw)
    #  print(nrow(train_metrics)-i)
    
  }
  
  #Remove any NAs
  
  train_metrics <- train_metrics[complete.cases(train_metrics),]
  
  #Match to add the behaviour label
  train_metrics$behaviour <- train$behaviour[match(train_metrics$pango_seg, train$pango_seg)] 
  
  #head(train_metrics)
  
  #write.csv(train_metrics,"trainingmetrics100_seglength.csv", row.names = F)
  write.csv(train_metrics, paste0(files,"training_metrics/",met_df[m,1],"hz/", met_df[m,1], "hztrainingmetrics", met_df[m,2],pos, "seglength.csv"), row.names = F)
  
}


