library(tidyverse)
library(stringr)

# Read the text file containing dates and information about the runs, as hand-written.
original <- read.delim(file = "trcanje info.txt", header = F, encoding = "UTF-8")

# Extracting dates and numerical data with Regex ----
# Finding out the patterns suitable for extractions
# 
# First, we remove the rows that have no date - these are the location indicators and other notes
# Variables: date, run time, run distance (in laps)
# Date is standard:dd.mm.yy format




# Extract dates

regex_date_pattern <- "\\d+\\.\\d+\\.\\d\\d"
extracted_dates <- str_extract(original$V1, regex_date_pattern) %>% as.Date(extracted_dates, format="%d.%m.%y")

# Extract laps

# First we'll extract the whole part describing number of laps ran,
# Then just the number
regex_lap_pattern <- "\\d+([\\.\\,]\\d)*\\s*kr"
regex_lap_pattern_just_number <-  "^\\d+([\\.\\,]\\d)*"


extracted_laps <- str_extract(original$V1, regex_lap_pattern) %>% str_extract(regex_lap_pattern_just_number) %>%
    as.numeric()

# To extract time, we first extract a vector of strings like "mm:ss", 
# Then turn that into a number of seconds in a new vector

# Two numbers with zero or more spaces, then "m"
regex_time_ran_pattern <- "(\\d\\d\\:\\d\\d.*m?)|(\\d+\\s*m(in)*)"

extracted_times <- str_extract(original$V1,regex_time_ran_pattern)

# Extract number of minutes
just_minutes <- str_extract(extracted_times,"^\\d+")
# 100 is from "100m" notes, doesn't represent time - delete
just_minutes[which(just_minutes == "100")] <- NA
just_minutes <- as.numeric(just_minutes)

# Extract number of seconds
just_seconds <- str_extract(extracted_times,"\\:\\d\\d") %>% str_extract("\\d\\d") %>% as.numeric()

# Extract number of hours for runs longer than 59:59m
# In those rows that contain a number of hours, the FIRST digit is the number of hours
regex_time_ran_pattern_more_than_hour <- "\\d\\:\\d\\d:\\d\\d."
just_hours <- str_extract(original$V1, regex_time_ran_pattern_more_than_hour) %>% str_extract("^\\d") %>% as.numeric()


# Get the full text for later display in database, replace those parts in extracted_times
# Because they only caught the minutes and seconds, not the hours

extracted_times_more_than_hour <- str_extract(original$V1, regex_time_ran_pattern_more_than_hour)
extracted_times_more_than_hour_row_number <- which(!is.na(extracted_times_more_than_hour))

# Update the text info for these runs
extracted_times[extracted_times_more_than_hour_row_number] <- extracted_times_more_than_hour[extracted_times_more_than_hour_row_number]

# Get minutes from these parts too 
more_than_hour_text <- str_extract(original$V1, regex_time_ran_pattern_more_than_hour) %>% 
    str_extract("\\:\\d\\d:\\d\\d") %>%  str_extract("\\d\\d:\\d\\d") 
more_than_hour_minutes <- more_than_hour_text %>% str_extract("^\\d+") %>% as.numeric()
more_than_hour_seconds <- more_than_hour_text %>% str_extract(":\\d\\d") %>% str_extract("\\d\\d") %>% as.numeric()

# Add hours to minutes
more_than_hour_minutes <- just_hours * 60 + more_than_hour_minutes

  
# Add the times with hours to the regulars mins and secs
which_times_more_than_hour <- which(!is.na(more_than_hour_minutes))

just_minutes[which_times_more_than_hour] <- more_than_hour_minutes[which_times_more_than_hour]
just_seconds[which_times_more_than_hour] <- more_than_hour_seconds[which_times_more_than_hour]


# Minutes to seconds and add the two
data_times <- data.frame(time_original_text = extracted_times, min = just_minutes, sec_only = just_seconds)
data_times <- mutate(data_times, sec_only = replace_na(sec_only, 0), total_seconds = min * 60 + sec_only)

# Round up minutes for seconds > 31
data_times$min_rounded <- data_times$min
data_times$min_rounded[which(data_times$sec_only > 30)] <- data_times$min[which(data_times$sec_only > 30)] + 1

# Reordering the columns
data_times <- select(data_times, time_original_text, min_rounded, min,sec_only, total_seconds)

# Joining with text, date and lap info
running_data <- data.frame(original_text=original$V1, date = extracted_dates, laps = extracted_laps)
running_data <- cbind(running_data, data_times)

# Remove rows with less than 10 mins and which have no time indicated
running_data[which(running_data$min_rounded < 11), "min_rounded"] <- NA
running_data <- running_data[which(is.na(running_data$min_rounded)==F), ]

running_data <- running_data[which(is.na(running_data$date) == F), ]
rownames(running_data) <- NULL # We don't need the original rowname numbers

# Extracting location ----
# Runs were recorded in two places, Koprivnica and Zagreb. Laps in Koprivnica (KC) are 300m, in Zagreb (ZG) they're 400m.
# So we want to extract the location data for each run, which will also allow us to calculate run distance accurately.

# How we're going about it:
# Locations are named as specific rows in the text. All runs until the next location is mentioned are presumed to be in that location.
# Locate dates when those locations are named, all following runs will have that location applied to them.
# If a further location change exists, it will overwrite the location info in the next pass, keeping it all up to date!

locations_text<- read.delim(file= "trcanje info.txt", header = F, encoding = "UTF-8")

# Everything into uppercase for easier searching
for (i in 1:nrow(locations_text)) {
  locations_text[i, 1] <- str_to_upper(locations_text[i, 1], locale="hr")
}

# Words in the text relating to location: KOPRIVNICA , KC , ZAGREB, ZG
# Find the rownumbers of those words. The next runs occurs in the same row, OR the one below, or, at maximum, the one below that.

kc_1 <- str_which(locations_text$V1, "KOPRIVNICA|KC")
zg_1 <- str_which(locations_text$V1, "ZAGREB|ZG")

# Finding dates - in the same rows, ones below and ones one more below
dates_kc1 <- extracted_dates[kc_1]
dates_kc2 <- extracted_dates[kc_1 + 1]
dates_kc3 <- extracted_dates[kc_1 + 2]

# New dataframe just for Koprivnica locations - we'll do the same for Zagreb, combine them and add it to the main df later
df_location_dates <- data.frame(location = rep("KOPRIVNICA", length(dates_kc1)))

# Put the ones found in the same line in a dataframe.
df_location_dates$date <- dates_kc1

# If it's not in the same row, try the next. if it's still empty, try the next.
 
for (i in 1 : nrow(df_location_dates)) {
  if (is.na(df_location_dates$date[i])) {
    df_location_dates$date[i] <- dates_kc2[i]
}
  # Still empty? Try the n+2 row
  if (is.na(df_location_dates$date[i])) {
    df_location_dates$date[i] <- dates_kc3[i]
  }  
}  

# That's it for Koprivnica, repeat the same for Zagreb.

dates_zg1 <- extracted_dates[zg_1]
dates_zg2 <- extracted_dates[zg_1 + 1]
dates_zg3 <- extracted_dates[zg_1 + 2]

# Dataframe of locations and dates they occur
df_location_dates_zg <- data.frame(location = rep("ZAGREB", length(dates_zg1)))

df_location_dates_zg$date <- dates_zg1

# Same as for Koprivnica
for (i in 1 : nrow(df_location_dates_zg)) {
  if (is.na(df_location_dates_zg$date[i])) {
    df_location_dates_zg$date[i] <- dates_zg2[i]
  }  
  if (is.na(df_location_dates_zg$date[i])) {
    df_location_dates_zg$date[i] <- dates_zg3[i]
  } 
}

# Bind them together, arrange by date
df_location_dates <- rbind(df_location_dates, df_location_dates_zg) %>% arrange(date)
df_location_dates <- arrange(df_location_dates, date)

# And now to apply it to the main database. The first row is in Zagreb,
# Apply that to everything as the default. The df_location_dates marks when locations change -
# From that point to the next, every run is in the location named.

running_data$location <- "ZAGREB"

# This for goes through every date that has a location change
for (i in 1 : nrow(df_location_dates)) {   
 # For every row of the main base checks if the date is equal or higher than the current location date
 # If so, it changes the location
    for (j in 1 : nrow(running_data)) {  
     if (running_data$date[j] >= df_location_dates$date[i]) {
       running_data$location[j] <- df_location_dates$location[i]}
  }
}

# Calculating distance and speed ----

running_data <- mutate(running_data, distance_km = laps * 0.4)

# Koprivnica distances are 3/4ths as big
for (i in 1: nrow(running_data)) {
  if (running_data$location[i] == "KOPRIVNICA") {
    running_data$distance_km[i] = running_data$distance_km[i] * 3/4
    }
}

running_data <- mutate(running_data, km_h = distance_km / (total_seconds / 3600))


# Graphs----- 

ggplot(running_data, aes(x = date, y = km_h)) + geom_point() + geom_smooth(se = F, color = "red")
ggsave(filename = "kmh map.png",width = 20)
ggplot(running_data, aes(x = date, y = distance_km)) + geom_point() + geom_smooth(se = F, color = "red")
ggsave(filename = "distance map.png", width = 20)
ggplot(running_data, aes(x = date, y = min_rounded)) + geom_point() + geom_smooth(se = F, color = "red")
ggsave(filename = "time spent map.png", width = 20)

# Data only for runs with number of laps

data_only_with_laps <- filter(running_data, is.na(laps) == F)
ggplot(data_only_with_laps, aes(x = date, y = min_rounded)) + geom_point() + geom_smooth(se = F, color = "red")
ggsave(filename = "time spent map_only with laps.png", width = 20)

# Save dataframe as excel file, with today's date appended
writexl::write_xlsx(running_data, path = paste("Running data", format(Sys.time(), "%Y-%m-%d"), ".xlsx"))
