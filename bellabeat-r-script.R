install.packages('tidyverse')

library(tidyverse)

daily_activity<- read.csv("/cloud/project/bella-beat-zip/dailyActivity_merged.csv")
hourly_steps<- read.csv("/cloud/project/bella-beat-zip/hourly-steps.csv")
hourly_calories<- read.csv("/cloud/project/bella-beat-zip/hourly-calories.csv")
minute_steps<- read.csv("/cloud/project/bella-beat-zip/minutes-steps.csv")
minute_sleep<- read.csv("/cloud/project/bella-beat-zip/minute-sleep.csv")
minute_calories<- read.csv("/cloud/project/bella-beat-zip/minute-calories.csv")
daily_sleep<- read.csv("/cloud/project/bella-beat-zip/sleep-day.csv")
weight_log<- read.csv("/cloud/project/bella-beat-zip/weight-log.csv")

glimpse(daily_activity)
glimpse(hourly_calories)
glimpse(hourly_steps)
glimpse(minute_calories)
glimpse(minute_sleep)
tail(minute_sleep)
glimpse(minute_steps)
glimpse(weight_log)
glimpse(daily_sleep)

tail(daily_sleep)
count(daily_sleep)
#delete first row of dataframe for those with title as row
minute_steps <- minute_steps[-1,]
minute_sleep<-minute_sleep[-nrow(minute_sleep),]
daily_sleep<-daily_sleep[-nrow(daily_sleep),]
minute_calories<-minute_calories[-1,]
hourly_steps<-hourly_steps[-1,]
hourly_calories<-hourly_calories[-1,]

#rename date column in dataframe
daily_activity<-rename(daily_activity, activity_date=ActivityDate)
head(daily_activity)


#convert date into datetime object
daily_activity$activity_date=strptime(daily_activity$activity_date,format = "%m/%d/%Y")
hourly_calories$activity_date=strptime(hourly_calories$activity_date,format = "%m/%d/%Y")
hourly_steps$activity_date=strptime(hourly_steps$activity_date,format = "%m/%d/%Y")
minute_calories$activity_date=strptime(minute_calories$activity_date,format = "%m/%d/%Y")
minute_sleep$activity_date=strptime(minute_sleep$activity_date,format = "%m/%d/%Y")
minute_steps$activity_date=strptime(minute_steps$activity_date,format = "%m/%d/%Y")
weight_log$activity_date=strptime(weight_log$activity_date,format = "%m/%d/%Y")
daily_sleep$activity_date=strptime(daily_sleep$activity_date, format = "%m/%d/%Y")

#review dataframes
glimpse(daily_activity)
glimpse(hourly_calories)
glimpse(hourly_steps)
glimpse(minute_calories)
glimpse(minute_sleep)
glimpse(minute_steps)
glimpse(weight_log)
glimpse(daily_sleep)

#find unique ids in each dataframe

daily_activity_id<-unique(daily_activity$Id)
length(unique(daily_activity$Id))
length(unique(minute_sleep$Id))
length(unique(hourly_calories$Id))
length(unique(minute_calories$Id))
length(unique(daily_sleep$Id))
length(unique(weight_log$Id))


#count frequency of unique values
#1. create function to count frequency of unique values and save as dataframe
freqFunc <-function(df){
  freqDF<-as.data.frame(table(df$Id))
}

#2. implement the function
daily_activity_freq<-freqFunc(daily_activity)
daily_sleep_freq<-freqFunc(daily_sleep)
hourly_calories_freq<-freqFunc(hourly_calories)
hourly_steps_freq<-freqFunc(hourly_steps)
minute_calories_freq<-freqFunc(minute_calories)
minute_sleep_freq<-freqFunc(minute_sleep)
minute_steps_freq<-freqFunc(minute_steps)
weight_log_freq<-freqFunc(weight_log)

count(daily_activity_freq)
count(daily_sleep_freq)
count(hourly_calories_freq)
count(hourly_steps_freq)
count(minute_calories_freq)
count(minute_sleep_freq)
count(minute_steps_freq)
count(weight_log_freq)



#rename freq data frame columns
renameFunc<-function(df,new_name){
  old_name<-c("Var1","Freq")
  df%>%
    rename_at(all_of(old_name), ~new_name)
}

anames<-c("Id","daily_activity_freq")
daily_activity_freq<-renameFunc(daily_activity_freq,anames )
bnames<-c("Id","daily_sleep_freq")
daily_sleep_freq<-renameFunc(daily_sleep_freq, bnames)
cnames<-c("Id","hourly_calories_freq")
hourly_calories_freq<-renameFunc(hourly_calories_freq, cnames)
dnames<-c("Id","hourly_steps_freq")
hourly_steps_freq<-renameFunc(hourly_steps_freq, dnames)
enames<-c("Id","minute_calories_freq")
minute_calories_freq<-renameFunc(minute_calories_freq, enames)
fnames<-c("Id","minute_sleep_freq")
minute_sleep_freq<-renameFunc(minute_sleep_freq, fnames)
gnames<-c("Id","minute_steps_freq")
minute_steps_freq<-renameFunc(minute_steps_freq, gnames)
hnames<-c("Id","weight_log_freq" )
weight_log_freq<-renameFunc(weight_log_freq, hnames)

#subset(daily_sleep_freq, !(Var1 %in% minute_sleep_freq$Var1 ))

#confirms daily_sleep and minute_sleep_freq have identical ids
subset(daily_sleep_freq, (Var1 %in% minute_sleep_freq$Var1 ))
#create subset of ids and freq of ids in daily activity but not daily sleep
not_in_sleep_freq<-subset(daily_activity_freq, !(Var1 %in% daily_sleep_freq$Var1))
count(in_sleep_not_weight_freq)

#confirm other subsets match

subset(hourly_calories_freq, !(Var1 %in% hourly_steps_freq$Var1 ))
subset(hourly_calories_freq, !(Var1 %in% minute_steps_freq$Var1 ))

in_weight_not_sleep_freq<-subset(weight_log_freq, !(Var1 %in% daily_sleep_freq$Var1 ))


aggegate_daily_sleep_freq<-daily_sleep_freq%>%
  select(Id,daily_sleep_freq)%>%
  group_by(daily_sleep_freq)%>%
  tally()

daily_sleep_freq_mean<-daily_sleep_freq%>%
  summarise(mean_daily_sleep_freq=mean(daily_sleep_freq))

#combine all frequency data frames
combo_freq<-daily_activity_freq%>%
  full_join(daily_sleep_freq, by="Id")%>%
  full_join(hourly_calories_freq, by="Id")%>%
  full_join(hourly_steps_freq,by="Id")%>%
  full_join(minute_calories_freq, by="Id")%>%
  full_join(minute_sleep_freq, by="Id")%>%
  full_join(minute_steps_freq,by="Id")%>%
  full_join(weight_log_freq,by="Id")

#find means of each column
freq_means<-combo_freq %>%
  dplyr::select(where(is.numeric))%>% 
  summarise_each(funs( mean( .,na.rm = TRUE)))
  

#extract only those members who used all tracking services
all_round_use_freq<-combo_freq[complete.cases(combo_freq),]
count(combo_freq[complete.cases(combo_freq),])
 
  
head (daily_activity)
head(daily_sleep)

#create combined frequency summary
combo_freq_summary<-combo_freq %>%
  dplyr::select(where(is.numeric))%>%
  rowwise() %>%
  mutate(not_present_in=sum(is.na(c_across(where(is.numeric)))))


#create combo freq summary as data frame
freq_df<-as.data.frame(combo_freq_summary)


freq_df%>%
  filter(freq_df$not_present_in==1)%>%
  summarise_all(list(~sum(is.na(.))))


#ensure data formats match
daily_sleep$Id<-as.numeric(daily_sleep$Id)

#create function to add day column to df
dayFunc<-function (df){
  df%>%
    mutate(day=strftime(df$activity_date, "%A"))
  }

#implement functions
daily_activity<-dayFunc(daily_activity)
daily_sleep<-dayFunc(daily_sleep)
weight_log<-dayFunc(weight_log)


#reorganize day column for easier plotting
daily_activity$day <- factor(daily_activity$day, levels= c("Sunday", "Monday", 
                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

daily_sleep$day <- factor(daily_sleep$day, levels= c("Sunday", "Monday", 
                                                           "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

weight_log$day <- factor(weight_log$day, levels= c("Sunday", "Monday", 
                                                     "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
weight_log$IsManualReport<-factor(weight_log$IsManualReport, levels=c("true","false"))


###visualize most frequent day

ggplot(daily_activity, aes(x=day))+ 
  geom_bar(fill="#FF6666")+
  theme_classic()+
  labs(x="Days",y="No. of records",
       title="        Daily Activity Record Counts by Day")


ggplot(daily_sleep, aes(x=day))+ 
  geom_bar(fill="#FF6666")+
  theme_classic()+
  labs(x="Days",y="No. of records",
       title="        Daily Sleep Record Counts by Day")
  

ggplot(weight_log, aes(x=day))+ 
  geom_bar(fill="#FF6666")+
  theme_classic()+
  labs(x="Days",y="No. of records",
       title="        Weight Record Counts by Day")


ggplot(weight_log, aes(x=IsManualReport))+ 
  geom_bar(fill="#FF6666")+
  theme_classic()+
  labs(x="Manual Entry",y="No. of records",
       title="        Weight Log: Manual Entries",
       subtitle="                         True or False")

#review combined df
glimpse(combo_freq)
combo_freq<-combo_freq%>%
  replace(is.na(.), 0)

combo_freq_summary<-combo_freq_summary%>%
  replace(is.na(.), 0)



#create scatter plot comparison
ggplot(data=combo_freq_summary,aes(x=daily_activity_freq, y=daily_sleep_freq))+
  geom_point()+
  theme_light()

glimpse(combo_freq_summary)

#reshape data frame for plotting

df_reshaped<- data.frame(x = as.numeric(row.names(combo_freq_summary)),
                                        y=c(combo_freq_summary$daily_activity_freq,
                                            combo_freq_summary$daily_sleep_freq,
                                            combo_freq_summary$weight_log_freq),
                         group=c(rep("daily_activity_freq",nrow(combo_freq_summary)),
                                 rep("daily_sleep_freq",nrow(combo_freq_summary)),
                                 rep("weight_log_freq",nrow(combo_freq_summary))))

ggplot(df_reshaped, aes(x, y, col = group)) +  
  geom_point()+
  theme_light()+
  labs(x="Record",y="No. of records",
       title="        Comparing Record Frequency",
       subtitle="     Daily Activity vs Daily Sleep vs Weight Log")


