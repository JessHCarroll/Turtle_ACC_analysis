#for loggerhead turtles, behaviour UTC time is actually noted in CET. 

rm(list=ls())
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)
library(tidyverse)

id<-"T7"
dattime<-"11/03/2023"
day<-"D2"
position<-"P2"
path<-"C:/Users/xcarrj/Desktop/turtleacc_analysis/"

setwd(paste0(path, id,"/bonus_D2"))

#data<-read.csv("C:/Users/xcarrj/Desktop/turtleacc_analysis/T5/acc_data/06_S1.csv",sep = "", header = T)

data<-read.csv(paste0(path, id, "/acc_data/02_S1.csv"),sep = "", header = T)

head(data)
tail(data)

acc_data<-data

acc_data$x <- paste(acc_data$Date,acc_data$Time)

acc_data$time <- as.POSIXct(acc_data$x, "%d/%m/%Y %H:%M:%OS",tz = "UTC")

#append number between 0 and 6 to OS to show millisecond digits
#https://stackoverflow.com/questions/22037657/milliseconds-in-posixct-class

acc_data$time<-format(acc_data$time, "%Y-%m-%d %H:%M:%OS2")
# acc_data$label <- NA

#import behaviour labesl
files<-list.files()
tables <- lapply(files, read.csv, header = TRUE)
labs <- do.call(rbind , tables)
#labs<-read.csv("C:/Users/xcarrj/Desktop/turtleacc_analysis/T5/bonus/T5D1_GX010276.csv")
 #b<-read.csv("C:/Users/xcarrj/Desktop/turtleacc_analysis/T5/bonus/T3D2_GX020229.csv")


#labs<-rbind(labs,b)


labs$timeformat<-as.POSIXct(labs$Time, origin="1970-01-01")

#labs$timeformat<-with_tz(labs$timeformat, tzone="UTC")

labs$timeformat<-format(labs$timeformat,"%Y-%m-%d %H:%M:%OS2")


#subset data for master time synch DF and vidoe batches
labs<-labs %>%
  mutate(input = cumsum(Source != lag(Source) | row_number() == 1)) 

UTClab<-labs %>% filter(Behavior== "UTC time",  )

GPtime1<-labs %>% 
  filter(Behavior == "gopro_time") %>%
  arrange(as.POSIXct(Time, origin="1970-01-01")) %>%
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup()

timeOffsetDF<- rbind(UTClab, GPtime1)

timeOffsetDF<-timeOffsetDF[order(as.POSIXct(timeOffsetDF$Time, origin="1970-01-01")), ] 

#if behaviours in master timesynch file 


if (nrow(labs %>% 
         filter(Behavior == "gopro_time"))==1 | nrow(labs %>% 
                                                     filter(input == "1"))>2){
  labs<-labs %>% 
    filter(!Behavior == "UTC time")
}else{
  
  labs<-labs %>% 
    filter(!Behavior == "UTC time")
  
  labs<-labs %>% 
    filter(!(Behavior == "gopro_time" & input==1))
}




#using multiple video files results in slight time offset between logged behaviour and gopro time. fix this first.

#group each new video
labs<-labs %>%
  mutate(group = cumsum(Behavior=="gopro_time" | row_number() == 1))



# labs<-labs %>%
#   mutate(group = cumsum(Source != lag(Source) | row_number() == 1))

#find time difference

#calculate offset within video
offset_fun<-function(dataframe){
  dataframe$Time<-as.POSIXct(dataframe$Time, origin="1970-01-01")
  dataframe$Time2<-format(dataframe$Time,"%Y-%m-%d %H:%M:%OS4")  
  if (nrow(dataframe[dataframe$Behavior=="gopro_time",]) >1){
    vidtime<-head(dataframe[dataframe$Behavior=="gopro_time",],1)
  }else {
    vidtime<-dataframe[dataframe$Behavior=="gopro_time",] 
  }
  vidtime_extract<-paste(dattime,vidtime$Comment)
  vidtime_extractFor<-as.POSIXct(vidtime_extract, "%d/%m/%Y %H:%M:%OS",tz = "CET")
  timeoffset<-round(difftime(vidtime_extractFor, force_tz(vidtime$Time, "CET"), units="secs"),2) #2 for precision but 1 sec because only rounding to sec.
  
  dataframe$timecorr<- dataframe$Time+ timeoffset
  
  dataframe$timecorr<-as.POSIXct(dataframe$timecorr, origin="1970-01-01")
  #format form CET to UTC
  dataframe$timecorr<-force_tz(dataframe$timecorr, "CET")
  
  dataframe$timecorr_ms<-format(dataframe$timecorr,"%Y-%m-%d %H:%M:%OS4") 
  
  return(dataframe)
  
}

labsCorr<-as.data.frame(labs %>%
                          group_by(group) %>%
                          do(data.frame(offset_fun(.)))%>%
                          ungroup())

labsCorr<-labsCorr[-grep("gopro_time", labsCorr$Behavior), ]


#now each video is corrected, correct master time

##synchronise times on label import
vidtime<-timeOffsetDF

#correct for gopro offset
#vidtimeGP<-offset_fun(vidtime)

vidtimeUTC<-vidtime[vidtime$Behavior == 'UTC time',]
vidtime_extractUTC<-as.character(gsub("CET", "", vidtimeUTC$Comment))
vidtime_extractUTC<-paste(dattime,vidtime_extractUTC)
vidtime_extractForUTC<-as.POSIXct(vidtime_extractUTC, "%d/%m/%Y %H:%M:%OS",tz = "CET")

vidtime$Time<-as.POSIXct(vidtime$Time, origin="1970-01-01")
vidtime$Time2<-format(vidtime$Time,"%Y-%m-%d %H:%M:%OS4") 

#was vidtime_extractForUTC
timeoffsetUTC<-abs(as.numeric(difftime(force_tz(vidtime$Time[which(vidtime$Behavior=="UTC time")],tz = "CET"),force_tz(vidtime$Time[which(vidtime$Behavior=="gopro_time")],tz = "CET"), units="secs")))

# vidtime_UTCTIME<-as.POSIXct(vidtimeUTC$Time, origin="1970-01-01")
# vidtime_UTCTIME<- force_tz(vidtime_UTCTIME, tz="UTC")
# timeoffsetUTC<-as.numeric(difftime(vidtime_extractForUTC,vidtime_UTCTIME, units="secs"))


GPorder<-which(vidtime$Behavior=="gopro_time")
UTCorder<-which(vidtime$Behavior=="UTC time")


#correct for UTC offset
if(GPorder<UTCorder){ #if gp time is ahead of UTC time then - seconds
  #vidtime_UTCTIME<-vidtime_UTCTIME-timeoffsetUTC
  
  labsCorr$timecorrUTC<- labsCorr$timecorr- timeoffsetUTC
  
  labsCorr$timecorrUTC<-as.POSIXct(labsCorr$timecorrUTC, origin="1970-01-01")
  #format form CET to UTC
  labsCorr$timecorrUTC<-force_tz(labsCorr$timecorrUTC, "CET")
  
  labsCorr$timecorr_msUTC<-format(labsCorr$timecorrUTC,"%Y-%m-%d %H:%M:%OS2")
  
  
}else if (GPorder>UTCorder){ #if gp time is behind UTC then + seconds
  # vidtime_UTCTIME<-vidtime_UTCTIME-timeoffsetUTC
  labsCorr$timecorrUTC<- labsCorr$timecorr+ timeoffsetUTC
  
  labsCorr$timecorrUTC<-as.POSIXct(labsCorr$timecorrUTC, origin="1970-01-01")
  #format form CET to UTC
  labsCorr$timecorrUTC<-force_tz(labsCorr$timecorrUTC, "CET")
  
  labsCorr$timecorr_msUTC<-format(labsCorr$timecorrUTC,"%Y-%m-%d %H:%M:%OS2")
} else{
  
  labsCorr$timecorrUTC<-force_tz(labsCorr$timecorr, "CET")
  
  labsCorr$timecorr_msUTC<-format(labsCorr$timecorr,"%Y-%m-%d %H:%M:%OS2")
  
}

############################################
##now convert CET time to UTC for ACC data##
############################################

labsCorr$timecorrUTC<-with_tz(labsCorr$timecorrUTC, tzone="UTC")
labsCorr$timecorr_msUTC<-format(labsCorr$timecorrUTC,"%Y-%m-%d %H:%M:%OS2")




#now format for intervals
#long to wide and format
data_wide <- spread(labsCorr, Behavior.type, timecorrUTC)
data_wide

data_wide$START<-na.locf(data_wide$START,na.rm = FALSE)

data_wide<-data_wide %>% drop_na(STOP)

#final dataframe for time interval labs
labs_finalFormat<-data_wide %>% select('Behavior','START', 'STOP')

#remove milisecond error in label recordings
#round start up to next second
labs_finalFormat$START_ms<-format(labs_finalFormat$START,"%Y-%m-%d %H:%M:%OS2")
labs_finalFormat$START_round <- ceiling_date(labs_finalFormat$START, unit = "1 second")

#round stop down to previous second
labs_finalFormat$STOP_ms<-format(labs_finalFormat$STOP,"%Y-%m-%d %H:%M:%OS2")
labs_finalFormat$STOP_round <- floor_date(labs_finalFormat$STOP, unit = "1 second")

labs_finalCLean<- labs_finalFormat[,c(1,5,7)]

labs_finalCLean$START_round<-format(labs_finalCLean$START_round, "%Y-%m-%d %H:%M:%OS2")
labs_finalCLean$STOP_round<-format(labs_finalCLean$STOP_round, "%Y-%m-%d %H:%M:%OS2")

#df2 should be accelerometer data
#df1 should be intervals
labs_finalCLean <- labs_finalCLean %>% 
  rename("START"="START_round" ,
         "STOP" = "STOP_round")

#remove any observations less than 2 seconds (otherwise nothing left after trimming)
#find time diff in seconds
labs_finalCLean$interval<-as.numeric(difftime(labs_finalCLean$STOP, labs_finalCLean$START))
#remove labels less than 2 seconds
labs_finalCLean2 <-labs_finalCLean[labs_finalCLean$interval > 2, ] 

#cut ends of period. add 1 sec to start, remove 1 sec from end to account for time synch errors
labs_finalCLean2$START<-as.POSIXct(labs_finalCLean2$START, "%Y-%m-%d %H:%M:%OS",tz = "UTC")
labs_finalCLean2$START_buf<-labs_finalCLean2$START + 1
labs_finalCLean2$STOP<-as.POSIXct(labs_finalCLean2$STOP, "%Y-%m-%d %H:%M:%OS",tz = "UTC")
labs_finalCLean2$STOP_buf<-labs_finalCLean2$STOP - 1

##clean dataframe
labs_finalCLean3<- labs_finalCLean2[,c(1,5,6)]

labs_finalCLean3 <- labs_finalCLean3 %>% 
  rename("START"="START_buf" ,
         "STOP" = "STOP_buf")



#now trim acc to start of observations to reduce processing time
#fromat for trimming

acc_data$time<-as.POSIXct(acc_data$time, "%Y-%m-%d %H:%M:%OS",tz = "UTC")
acc_data_trim<-acc_data[acc_data$time > vidtime_extractForUTC,]

#now label accelerometer data!!
df1<-labs_finalCLean3

df2<-acc_data_trim


df1 <- as_tibble(df1) |> 
  mutate(across(c(START, STOP), ymd_hms))

df2 <- as_tibble(df2) |> 
  mutate(x = dmy_hms(x))

newdf<-df2 |> 
  left_join(df1, join_by(between(x, START, STOP))) 
#|> 
# select(x, time, Behavior)

#cleanup output
newdf<-as.data.frame(newdf)

newdf$time_form <- as.POSIXct(newdf$time, "%Y-%m-%d %H:%M:%OS",tz = "UTC")

#options(max.print=500)

colnames(newdf)

newdf_short<-newdf[,c(4,5,6,17,18,19,20,21,24)]

newdf_short<-newdf_short %>% drop_na(Behavior)

head(newdf_short)
tail(newdf_short)

newdf_short$time<-format(newdf_short$time, "%Y-%m-%d %H:%M:%OS2")

newdf_short$id<-paste(id)

acc_file <- paste0(path, "loggers_lab_accdata/", id,day,position, "_bonus.csv")
write.csv(newdf_short, acc_file)


library(plotly)

plot_ly(x = newdf_short$time_form, 
        y = newdf_short$X, 
        mode = 'scatter', 
        color = as.factor(newdf_short$Behavior))


plot_ly(x = newdf_short$time_form, 
        y = newdf_short$Y, 
        mode = 'scatter', 
        color = as.factor(newdf_short$Behavior))


plot_ly(x = newdf_short$time_form, 
        y = newdf_short$Z, 
        mode = 'scatter', 
        color = as.factor(newdf_short$Behavior))







#https://stackoverflow.com/questions/71860379/r-dplyr-join-tables-based-on-overlapping-date-time-intervals

#https://stackoverflow.com/questions/77627981/how-to-label-rows-with-a-specific-value-including-ms-if-time-occurs-in-given-t

