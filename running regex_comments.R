library(tidyverse)
library(stringr)

# read the text file containing dates and information about the runs, as hand-written.
original<- read.delim(file="trcanje info.txt",header = F, encoding = "UTF-8")

# Experimenting with regex ----
# finding out the patterns suitable for extractions
# 
# first, we remove the rows that have no date - these are the location indicators and other notes
# Variables: date, run time, run distance (in laps)
# date is standard:dd.mm.yy format

str_view_all(original,"kr")


test_vec <- original$V1[c(1:5,250:260,270:272,111)]

str_view_all(test_vec,"\\d\\d\\.\\d\\d\\.\\d\\d") #date

str_view_all(test_vec,"\\d\\d")
#two numbers with zero or more spaces, then "m"
# another extra :dd is possible (with times of over an hour)
##that's an alternate possibility, indicated |
str_view_all(test_vec,"(\\d\\d\\:\\d\\d.*m?)|(\\d+\\s*m(in)*)") #run time

#lap numbers end with "kr"
str_view_all(test_vec, "\\d+(\\.\\d)*\\s*kr")



#work on full base ----
str_view_all(original$V1,"\\d\\d\\.\\d\\d\\.\\d\\d") #date
str_view_all(original$V1,"(\\d\\d\\:\\d\\d.*m?)|(\\d+\\s*m(in)*)") #run time
str_view_all(original$V1,"\\d+(\\.\\d)*\\s*kr") #laps

#extract dates
str_extract(test_vec,"\\d\\d\\.\\d\\d\\.\\d\\d")
str_extract(test_vec,"\\d+\\.\\d+\\.\\d\\d")

dates_1 <- str_extract(original$V1,"\\d+\\.\\d+\\.\\d\\d")
dates_2 <- as.Date(dates_1,format="%d.%m.%y")

#extract time
times_1 <- str_extract(original$V1,"(\\d\\d\\:\\d\\d.*m?)|(\\d+\\s*m(in)*)")
# works well


#extract laps
laps_1 <- str_extract(original$V1,"\\d+([\\.\\,]\\d)*\\s*kr") #samo brojke
laps_2 <- str_extract(laps_1,"^\\d+([\\.\\,]\\d)*")
laps_2 <- as.numeric(laps_2)



# to extract time, we first extract a vector of strings like "mm:ss", then turn that into a number of second in a second vector
#extract time
times_1 <- str_extract(original$V1,"(\\d\\d\\:\\d\\d.*m?)|(\\d+\\s*m(in)*)")
times_mins <- str_extract(times_1,"^\\d+") #100 is from "100m" notes, doesn't represent time
times_mins2 <- times_mins
times_mins2[which(times_mins2=="100")] <- NA
times_mins2 <- as.numeric(times_mins2)
times_secs <- str_extract(times_1,"\\:\\d\\d")
times_secs2 <- str_extract(times_secs,"\\d\\d")
times_secs2 <- as.numeric(times_secs2)


times_hours <- str_extract(original$V1,"\\d\\:\\d\\d:\\d\\d.")
times_hours2 <- str_extract(times_hours,"^\\d") #in those rows that contain a number of hours, the FIRST digit is the number of hours
times_hours2 <- as.numeric(times_hours2)
times_hours_minutespart <- str_extract(times_hours,"\\:\\d\\d:\\d\\d")
times_hours_minutes_sec <- str_extract(times_hours_minutespart,"\\d\\d:\\d\\d")
times_hours_minutes1 <- str_extract(times_hours_minutes_sec,"^\\d+")
times_hours_minutes1 <- as.numeric(times_hours_minutes1)

times_hours_sec1 <- str_extract(times_hours_minutes_sec,":\\d\\d")
times_hours_sec2 <- str_extract(times_hours_sec1,"\\d\\d")
times_hours_sec2 <- as.numeric(times_hours_sec2)

#add hours to minutes
hourtime_mins <- times_hours2*60 + times_hours_minutes1
hourtime_secs <- times_hours_sec2
  
#add the times with hours to the regulars mins and secs
hourtime_which <- which(!is.na(hourtime_mins))

times_mins2[hourtime_which] <- hourtime_mins[hourtime_which]
times_secs2[hourtime_which] <- hourtime_secs[hourtime_which]


# minutes to seconds and add the two
data_times <- data.frame(ortime=times_1,min=times_mins2,secs_only=times_secs2)
data_times <- mutate(data_times,secs_calc=replace_na(secs_only,0),mintosec=min*60+secs_calc)

#round up minutes for seconds>31
data_times$min_round <- data_times$min
data_times$min_round[which(data_times$secs_calc>30)] <- data_times$min[which(data_times$secs_calc>30)] + 1

data_times_to_join <- select(data_times, ortime,min_round,min,secs_only,mintosec)

all_data <- data.frame(original_text=original$V1,date=dates_2,laps=laps_2)
all_data <- cbind(all_data,data_times_to_join)


# remove rows with less than 10 mins and with have no time indicated
all_data[which(all_data$min_round<11),"min_round"] <- NA
all_data <- all_data[which(is.na(all_data$min_round)==F),]

rownames(all_data) <- NULL #ne znam zašto ovo postoji uopće, zbunjujuće je
all_data <- all_data[which(is.na(all_data$date)==F),]

# Extracting location ----
# Runs were recorded in two places, Koprivnica and Zagreb. Laps in Koprivnica (KC) are 300m, in Zagreb they're 400m.
# So we wanted to extract the location data for each run, which will also allow us to calculate run distance accurately.

# How we're going about it:
# locations are named as specific rows in the text. All runs until the next location is mentioned are presumed to be in that location.
# Locate dates when those locations are named, all following runs will have that location applied to them.
# If a further location change exists, it will overwrite the location info in the next pass, keeping it all up to date!

lokacije_text<- read.delim(file="trcanje info.txt",header = F, encoding = "UTF-8")

#everything into uppercase for easier searching
for (i in 1:nrow(lokacije_text)) {
  lokacije_text[i,1] <- str_to_upper(lokacije_text[i,1], locale="hr")
}

# words in text relating to location: KOPRIVNICA , KC , ZAGREB, ZG
# Find the rownumbers of those words. The next runs occurs in the same row, OR the one below, or, at maximum, the one below that.

kc_1 <- str_which(lokacije_text$V1, "KOPRIVNICA")
kc_2 <- str_which(lokacije_text$V1, "KC")
zg_1 <- str_which(lokacije_text$V1, "ZAGREB")
zg_2 <- str_which(lokacije_text$V1, "ZG")

#finding dates - in the same rows, ones below and ones one more below
datumi_kc1 <- dates_2[c(kc_1,kc_2)]
datumi_kc2 <- dates_2[c(kc_1,kc_2)+1]
datumi_kc3 <- dates_2[c(kc_1,kc_2)+2]

df_location_dates <- data.frame(location=rep("KOPRIVNICA", length(datumi_kc1)))

#put the ones found in the same line in a dataframe.
df_location_dates$date <- datumi_kc1

# if it's not in the same row, try the next. if it's still empty, try the next.
 
for (i in 1:nrow( df_location_dates)) {
if (is.na( df_location_dates$date[i])) {
  df_location_dates$date[i] <- datumi_kc2[i]
}  

}

for (i in 1:nrow( df_location_dates)) {
  if (is.na( df_location_dates$date[i])) {
    df_location_dates$date[i] <- datumi_kc3[i]
  }  
  
}

#that's it for Koprivnica, repeat the same for Zagreb.

datumi_zg1 <- dates_2[c(zg_1,zg_2)]
datumi_zg2 <- dates_2[c(zg_1,zg_2)+1]
datumi_zg3 <- dates_2[c(zg_1,zg_2)+2]

df_location_dates_zg <- data.frame(location=rep("ZAGREB", length(datumi_zg1)))

df_location_dates_zg$date <- datumi_zg1

for (i in 1:nrow( df_location_dates_zg)) {
  if (is.na( df_location_dates_zg$date[i])) {
    df_location_dates_zg$date[i] <- datumi_zg2[i]
  }  
  
}

for (i in 1:nrow( df_location_dates_zg)) {
  if (is.na( df_location_dates_zg$date[i])) {
    df_location_dates_zg$date[i] <- datumi_zg3[i]
  }  
  
}

df_location_dates <- rbind(df_location_dates,df_location_dates_zg)
df_location_dates <- arrange(df_location_dates,date)

#and now to apply it to the main database. The first row is in Zagreb, apply that to everything as 
#the default. It will be changed where needed.

all_data$location <- "ZAGREB"

for (i in 1:nrow(df_location_dates)) {
  #This for goes through every date that has a location change
  for (j in 1:nrow(all_data)) {
    #for every row of the main base checks if the date is equal or higher than the current location date
   #if so, it changes the location
     if (all_data$date[j] >= df_location_dates$date[i]) {
       all_data$location[j] <- df_location_dates$location[i]}
    
  }
  
}

#it works! and now the main part... tako easy :P

#calculate distance, and speed in km/h

data_calculations <- mutate(all_data, distance_km=laps*0.4)

#Koprivnica distances are 3/4ths as big
for (i in 1: nrow(data_calculations)) {
  if (data_calculations$location[i] == "KOPRIVNICA") {data_calculations$distance_km[i] = data_calculations$distance_km[i] *3/4}
  
}

data_calculations <- mutate(data_calculations, km_h=distance_km / (mintosec/3600))


#graphs----- 

ggplot(data_calculations, aes(x=date,y=km_h)) + geom_point() + geom_smooth(se=F, color="red")
ggsave(filename = "kmh map.png",width = 20)
ggplot(data_calculations, aes(x=date,y=distance_km)) + geom_point() + geom_smooth(se=F, color="red")
ggsave(filename = "distance map.png",width = 20)
ggplot(data_calculations, aes(x=date,y=min_round)) + geom_point() + geom_smooth(se=F, color="red")
ggsave(filename = "time spent map.png",width = 20)

#data only on runs with number of laps

data_only_with_laps <- filter(data_calculations, is.na(laps)==F)
ggplot(data_only_with_laps, aes(x=date,y=min_round)) + geom_point() + geom_smooth(se=F, color="red")
ggsave(filename = "time spent map_only with laps.png",width = 20)

# calculating a new measure for fun: Running points. Basically, they reward going fast, and going for longer.
# A run with higher speed which went over a longer distance is better exercise, right?
# So far, it's simply the product of km/h and distance, so running 10 km at 10km/h would be worth 100 points.

data_calculations <- mutate(data_calculations, running_points= km_h*distance_km)

ggplot(data_calculations, aes(x=date,y=running_points)) + geom_point() + geom_smooth(se=F, color="red")
ggsave(filename = "running points.png",width = 20)

#save dataframe as excel file, with today's date appended
writexl::write_xlsx(data_calculations, path = paste("Running data",format(Sys.time(), "%Y-%m-%d") ,".xlsx"))

