# This is the script I used to merge, clean and analyze the past 12 months of Cyclistic ride data
# I created several smaller .csv files to be used for data visualization in Tableau

# Import libraries that will be used
library('tidyverse')
library('ggplot2')
library('ggthemes') 
library('stringr')
library('timeDate')
library('zoo')
library('geosphere')

#load up data and start merging
curDir<-getwd()
#setwd('C:/Users/Jenny/Documents/DataAnalytics/Cyclistic/WorkingData/')
setwd('C:/Users/S421596/Documents/Personal/DataAnalyticsCourse/Capstone/Cyclistic/Data/')
df_Jul<-read_csv('202107-divvy-tripdata.csv')
#df_Jul<-select(df_Jul,ride_id,started_at,member_casual)
df_Jun<-read_csv('202106-divvy-tripdata.csv')
#df_Jun<-select(df_Jun,ride_id,started_at,member_casual)
df_Aug<-read_csv('202108-divvy-tripdata.csv')
#df_Aug<-select(df_Aug,ride_id,started_at,member_casual)

df_Sum<-full_join(df_Jun,df_Jul)
df_Sum<-full_join(df_Sum,df_Aug)#now you have the dates and ride id's for all of summer
rm(df_Jul)
rm(df_Jun)
rm(df_Aug)#now your workspace and memory are cleaned up

#now do more
df_9<-read_csv('202109-divvy-tripdata.csv')
#df_9<-select(df_9,ride_id,started_at,member_casual)
df_5<-read_csv('202105-divvy-tripdata.csv')
#df_5<-select(df_5,ride_id,started_at,member_casual)
df_4<-read_csv('202104-divvy-tripdata.csv')
#df_4<-select(df_4,ride_id,started_at,member_casual)

df_Other<-full_join(df_9,df_5)
df_Other<-full_join(df_Other,df_4)#now you have the dates and ride id's for all of summer
rm(df_9)
rm(df_5)
rm(df_4)#now your workspace and memory are cleaned up

#next group
df_3<-read_csv('202103-divvy-tripdata.csv')
#df_3<-select(df_3,ride_id,started_at,member_casual)
df_2<-read_csv('202102-divvy-tripdata.csv')
#df_2<-select(df_2,ride_id,started_at,member_casual)
df_1<-read_csv('202101-divvy-tripdata.csv')
#df_1<-select(df_1,ride_id,started_at,member_casual)

df_Other1<-full_join(df_3,df_2)
df_Other1<-full_join(df_Other1,df_1)#now you have the dates and ride id's for all of summer
rm(df_3)
rm(df_2)
rm(df_1)#now your workspace and memory are cleaned up

#now end of 2020
df_3<-read_csv('202012-divvy-tripdata.csv')
#df_3<-select(df_3,ride_id,started_at,member_casual)
df_2<-read_csv('202011-divvy-tripdata.csv')
df_2$start_station_id<-as.character(df_2$start_station_id) #change start and end station id to char
df_2$end_station_id<-as.character(df_2$end_station_id)
#df_2<-select(df_2,ride_id,started_at,member_casual)
df_1<-read_csv('202010-divvy-tripdata.csv')
df_1$start_station_id<-as.character(df_1$start_station_id)
df_1$end_station_id<-as.character(df_1$end_station_id)
#df_1<-select(df_1,ride_id,started_at,member_casual)

df_Other2<-full_join(df_3,df_2)
df_Other2<-full_join(df_Other2,df_1)#now you have the dates and ride id's for all of summer
rm(df_3)
rm(df_2)
rm(df_1)#now your workspace and memory are cleaned up

#now merge the whole year
df_Full<-full_join(df_Sum,df_Other)
df_Full<-full_join(df_Full,df_Other1)#n
df_Full<-full_join(df_Full,df_Other2)#n
rm(df_Sum)
rm(df_Other)
rm(df_Other2)
rm(df_Other1)#now your workspace and memory are cleaned up

##now clean the data and create a few new columns
#make a new column for ride duration and only select rides that had a duration greater than zero and not NA
df_Full<-mutate(df_Full,ride_duration=ended_at-started_at)
df_Full$ride_duration=as.numeric(df_Full$ride_duration, units="mins")
df_Full<-df_Full%>%
        filter(ride_duration>0)%>% #must be greater than zero
        filter(!is.na(ride_duration))%>% #can't be NA
        filter(ride_duration<720) #limiting to rides that were under 12 hours

#see if any ride IDs are duplicated. This returns an empty vector. We are good to go!
weirdIDs<-df_Full[duplicated(df_Full$ride_id),1]
rm(weirdIDs)

#now extract hour from datetime
df_Full<-df_Full%>%
        mutate(hour24=as.numeric(format(started_at,format="%H")))

#now convert the datetime to day of week and month
df_Full<-df_Full%>%
        mutate(weekDay=weekdays(started_at),month=months(started_at))

#Code the weekdays in a way that can be used and interpreted by Tableau
df_Full$weekDay_code=recode(df_Full$weekDay, 'Monday'=2, 'Tuesday'=3, 'Wednesday'=4, 'Thursday'=5, 'Friday'=6, 'Saturday'=7, 'Sunday'=1)
df_Full$month_code=recode(df_Full$month, 'January'=1,'February'=2,'March'=3,'April'=4,'May'=5,'June'=6, 'July'=7, 'August'=8,'September'=9,'October'=10,'November'=11,'December'=12)
df_Full<-df_Full%>%
        mutate(Weekend=isWeekend(started_at))
df_Full$Weekend<-if_else(df_Full$Weekend== 0, "1", "2", "missing")
df_Full$Weekend<-factor(df_Full$Weekend,labels=c("Weekday","Weekend"))
df_Full$dayMonth=format(as.Date(df_Full$started_at), "%d%m%Y")
df_Full$monthYear=format(as.Date(df_Full$started_at), "%Y%m")
#df_Full$monthYear=as.Date(df_Full$monthYear,"%Y%m")
df_Full$season=recode(df_Full$month, 'January'='Winter','February'='Winter','March'='Spring','April'='Spring','May'='Spring','June'='Summer', 'July'='Summer', 'August'='Summer','September'='Fall','October'='Fall','November'='Fall','December'='Winter')

#option to save large dataset for future use, but chose not to becuase it really doesn't take that long to load up
########
#now analyze the data
#make facet labeller for month for plotting in R
monthNames<-list('January','February','March','April','May','June','July','August','September','October','November','December')

monthLabeller<-function(variable,value){
        return(monthNames[value])
}

ridesPerMonth<-df_Full%>%
        select(ride_id,started_at,member_casual,monthYear)%>%
        group_by(member_casual,monthYear) %>%
        summarise(total_rides=n())
ridesPerMonth$monthCode<-recode(ridesPerDay$monthYear,'202010'=1,'202011'=2,'202012'=3,"202101"=4,'202102'=5,'202103'=6,'202104'=7,'202105'=8,"202106"=9,"202107"=10,"202108"=11,"202109"=12)
ridesPerMonth$month<-recode(ridesPerDay$monthYear,'202010'='Oct20','202011'='Nov20','202012'='Dec20',"202101"='Jan21','202102'='Feb21','202103'='Mar21','202104'='Apr21','202105'='May21',"202106"='Jun21',"202107"='Jul21',"202108"='Aug21',"202109"='Sep21')
write_csv(ridesPerMonth,'ridesPerMonth.csv')

## optional plot just to look at data in R
#now plot rides per month for member vs casual
ggplot(ridesPerDay, aes(monthCode,total_rides)) +
        geom_line(aes(color=member_casual),size=2)+
        ggtitle("Rides Over Time")+
        theme(axis.title.y=element_text(face="bold",size=20),
              axis.title.x=element_blank(),
              axis.text.x = element_text(face="bold", size=14,angle=45,hjust=0.95),
              axis.text.y = element_text(face="bold", size=14),
              legend.title=element_text(face="bold",size=12),
              plot.title = element_text(face='bold',size=18)
        )+
        scale_color_discrete("Rider Type")+
        scale_y_continuous(name="Rides per Month",breaks=c(0,100000,200000,300000,400000,500000),labels=c("0","1000k","200k","300k","400k","500k"),limits=c(0,500000))+
        scale_x_continuous(name="Month",breaks=c(1:12),labels=c('Oct20','Nov20','Dec20','Jan21','Feb21','Mar21','Apr21','May21','Jun21','Jul21','Aug21','Sep21'),limits=c(1,12))


#Now compare rides over the course of the week
ridesPerDay<-df_Full%>%
        select(ride_id,started_at,member_casual, weekDay)%>%
        group_by(member_casual,weekDay)%>%
        summarise(total_rides=n())
#now save this
write_csv(ridesPerDay,'ridesPerDay.csv')

        
#Now plot everything.#normalize by the number of days that played into each
theme_set(theme_bw())
ggplot(data=ridesPerDay, aes(x=weekDay_code,y=total_rides)) +
        geom_point(aes(color=member_casual),size=5)+
        geom_line(aes(color = member_casual),size=2)+
        scale_x_continuous(breaks=c(1,2,3,4,5,6,7),labels=c("Monday", "Tuesday","Wednesday", "Thursday","Friday",
                                                            "Saturday","Sunday")) +
        scale_y_continuous(name ="Number of Rides",breaks=c(100000,150000,200000,250000,300000),labels=c("100k","150k","200k","250k","300k"),limits=c(100000,300000)) +
        theme(axis.title.x=element_blank(),
              axis.title.y=element_text(face="bold",size=20),
              axis.text.x = element_text(face="bold", size=14, angle=45,hjust=0.95),
              axis.text.y = element_text(face="bold", size=14),
              legend.title=element_text(face="bold",size=12),
              panel.grid.minor.x=element_blank())+
        scale_color_discrete("Rider Type")

# now look at rides per season
ridesPerSeason<-df_Full%>%
        select(ride_id,started_at,member_casual,hour24,season)%>%
        group_by(member_casual,season,hour24) %>%
        summarise(total_rides=n())
getwd()
write_csv(ridesPerSeason,'ridesPerSeason.csv')

# now look at rides/day on a weekday vs weekend
numDays<-unique(df_Full$dayMonth)
weekendDays<-isWeekend(as.Date(numDays,"%d%m%Y"))
nWE<-sum(weekendDays==TRUE)
nWD<-sum(weekendDays==FALSE)

ridesPerHourWeekend<-df_Full%>%
        select(ride_id, member_casual,hour24,Weekend)%>%
        group_by(member_casual,hour24,Weekend)%>%
        summarise(total_rides=n())%>%
        arrange(Weekend)
ridesPerHourWeekend$normWeek<-ridesPerHourWeekend$total_rides
ridesPerHourWeekend$normWeek[1:48]=ridesPerHourWeekend$normWeek[1:48]/nWD
ridesPerHourWeekend$normWeek[49:96]=ridesPerHourWeekend$normWeek[49:96]/nWE
write_csv(ridesPerHourWeekend,'ridesPerHourWeekend.csv')

ridesPerWeekend<-df_Full%>%
        select(ride_id, member_casual,Weekend)%>%
        group_by(member_casual,Weekend)%>%
        summarise(total_rides=n())%>%
        arrange(Weekend)

#now normalize by total number of weekdays or weekend-days in the year. manually do it bc idk how else
ridesPerWeekend$normWeek<-ridesPerWeekend$total_rides
ridesPerWeekend$normWeek[1:2]=ridesPerWeekend$normWeek[1:2]/nWD
ridesPerWeekend$normWeek[3:4]=ridesPerWeekend$normWeek[3:4]/nWE
write_csv(ridesPerWeekend,'ridesPerWeekend.csv')

ridesPerHourWeekendSeason<-df_Full%>%
        select(ride_id, member_casual,hour24,Weekend,season)%>%
        group_by(member_casual,hour24,Weekend,season)%>%
        summarise(total_rides=n())%>%
        arrange(Weekend,season)
#normalize manually because I couldn't figure out how to do it automatically
ridesPerHourWeekendSeason$normWeek<-ridesPerHourWeekendSeason$total_rides
ridesPerHourWeekendSeason$normWeek[1:192]=ridesPerHourWeekendSeason$normWeek[1:192]/5 #not a great way to do this, I admit. But it does the thing I want
ridesPerHourWeekendSeason$normWeek[193:384]=ridesPerHourWeekendSeason$normWeek[193:384]/2
ridesPerHourWeekendSeason<-select(ridesPerHourWeekendSeason, -total_rides)
write_csv(ridesPerHourWeekendSeason,'ridesPerHourWeekendSeason.csv')

ggplot(ridesPerHourWeekendSeason,aes(hour24,normWeek,color=season))+
        geom_line()+
        facet_grid(rows=vars(member_casual),cols=vars(Weekend))

# Now compare preferences for members and non-members for electric or regular bikes
regElec<-df_Full%>%
        select(member_casual,ride_duration)%>%
        group_by(member_casual)%>%
        summarize(count=n(),mean=mean(ride_duration),sd=sd(ride_duration),median=median(ride_duration),IQR=IQR(ride_duration))

# take a look at the distribution of ride durations. It is not normally distributed so I will use nonparametric stats
df_Full%>%
ggplot(aes(log(ride_duration)))+
geom_histogram(binwidth = .5)+
        facet_grid(cols=vars(member_casual))


# Now compare preferences for members and non-members for electric or regular bikes
bikeType<-df_Full%>%
        select(member_casual,rideable_type)%>%
        group_by(member_casual,rideable_type)%>%
        summarize(count=n())
write_csv(bikeType,'bikeType.csv')

#function to calculate distance between start and stop coordinates
distCalc<-function(twoCoords){
        mTemp<-rbind(c(twoCoords[1:2]),c(twoCoords[3:4]))
        distTemp<-distm(mTemp)
        return(distTemp[2])
}


# Aggregate ride duration and distance by member/casual status.
rDist<-df_Full%>%
        select(member_casual,ride_duration)
rDist$rideDist<-apply(df_Full[,c("start_lng","start_lat","end_lng","end_lat")],MARGIN=1,distCalc)/1000 #convert to meters

#get stats about ride distance and duration, excluding rides that started and ended at same station or for some reason had an NA
rDistSum<-rDist%>%
        group_by(member_casual)%>%
        filter(!is.na(rideDist),rideDist>0)%>%
        summarise(meanDist=mean(rideDist),stdDist=sd(rideDist),median(rideDist),rangeDist=range(rideDist),meanDur=mean(ride_duration),stdDur=sd(ride_duration),medianDur=median(ride_duration),rangeDur=range(ride_duration),count=n())

write_csv(rDist,'ridesDistance.csv')
write_csv(rDistSum,'ridesDistanceSum.csv')

#now look at histograms of duration (adjusted to exclude 0 mins)
rDist%>%
        filter(!is.na(rideDist),rideDist>1)%>%
        ggplot(aes(log(ride_duration),color=member_casual))+
        geom_boxplot(aes(x=member_casual,y=log(ride_duration)))
        #geom_histogram(aes(x=log(ride_duration),y=..density..,color=member_casual),binwidth = .1)
        #facet_grid(cols=vars(member_casual))
rDist$member_casual<-as.factor(rDist$member_casual)
#do stats on duration
wilcox.test(rDist$ride_duration[rDist$member_casual=='casual'],rDist$ride_duration[rDist$member_casual=='member'])

#same as above for distance
rDist%>%
        filter(!is.na(rideDist),rideDist>0)%>%
        ggplot(aes(log(rideDist)))+
        geom_histogram(aes(x=rideDist,y=..density..,color=member_casual),binwidth = .05)+
        facet_grid(cols=vars(member_casual))
wilcox.test(rDist$rideDist[rDist$member_casual=='casual'],rDist$rideDist[rDist$member_casual=='member'])

##
# now count the number of unique docking stations that were used for casual riders and members
#for each station, determine distance from certain points of interest. "the bean", navy pier, etc. map of attractions in link; https://www.touropia.com/tourist-attractions-in-chicago/
nStations<-df_Full%>%
        select(member_casual,start_station_id,end_station_id)%>%
        group_by(member_casual)%>%
        summarise(numStart=n_distinct(start_station_id),numStop=n_distinct(end_station_id))

# Now extract the top 10 most used start stations by casual riders and members
stationRide_c<-df_Full%>%
        select(member_casual,start_station_name)%>%
        filter(member_casual=='casual')%>%
        group_by(member_casual,start_station_name)%>%
        summarise(numRides=n())%>%
        arrange(desc(numRides))
stationRide_c<-stationRide_c[2:11,]
stationRide_c$percentRides<-stationRide_c$numRides/2095444*100
stationRide_m<-df_Full%>%
        select(member_casual,start_station_name)%>%
        filter(member_casual=='member')%>%
        group_by(member_casual,start_station_name)%>%
        summarise(numRides=n())%>%
        arrange(desc(numRides))
stationRide_m<-stationRide_m[2:11,]
stationRide_m$percentRides<-stationRide_m$numRides/2506940*100
stationRide_Count<-rbind(stationRide_m,stationRide_c)
sum(stationRide_c$percentRides)
sum(stationRide_m$percentRides)
write_csv(stationRide_Count,'stationRide_Count.csv')

numNonNA<-df_Full%>%
        select(member_casual,start_station_name)%>%
        filter(!is.na(start_station_name))%>%
        group_by(member_casual)%>%
        summarise(tot_Rides=n())

### The following section extracts the latitude and longitude for each top 10 station for input into google maps
casualTop10_start<-df_Full%>%
        select(member_casual,start_station_name,end_station_name)%>%
        filter(!is.na(start_station_name),member_casual=='casual')%>%
        group_by(start_station_name)%>%
        summarise(numStart=n())%>%
        arrange(desc(numStart))
casualTop10_start<-casualTop10_start[1:10,]
#now member
memberTop10_start<-df_Full%>%
        select(member_casual,start_station_name,end_station_name)%>%
        filter(!is.na(start_station_name),member_casual=='member')%>%
        group_by(start_station_name)%>%
        summarise(numStart=n())%>%
        arrange(desc(numStart))
memberTop10_start<-memberTop10_start[1:10,]

# now get the lat and long of each of the top 10 stations
getCoords<-function(stationName){
        mTemp<-filter(df_Full,start_station_name==stationName)
        return(c(mTemp$start_lat[1],mTemp$start_lng[1]))
}

coordTemps<-apply(memberTop10_start[,1],MARGIN=1,getCoords)
memberTop10_start$start_lat<-coordTemps[1,]
memberTop10_start$start_lng<-coordTemps[2,]

coordTemps<-apply(casualTop10_start[,1],MARGIN=1,getCoords)
casualTop10_start$start_lat<-coordTemps[1,]
casualTop10_start$start_lng<-coordTemps[2,]

### and we're done!




