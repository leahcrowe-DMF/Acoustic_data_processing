library(dplyr);library(lubridate);library(suncalc)

# manual params ----

drivepath = "P:/" 
site = "CCB07"
deployment_number = "02"
ST_ID = "8852"

# choose one of the below for the timezone the ST files were offloaded in
#ST_TZ = "UTC"
ST_TZ = "America/New_York"

## position of deployment ---- 
lat = 42.05311356
lon = -70.31334876

## detector choice ----
detector = "clnb_gom9"
#detector = "clnb_gomlf_blue"

## deployment start ----

path<-paste0(drivepath,'/',site,'/',site,'_',deployment_number,'/',ST_ID)
path

all_wav<-as.data.frame(list.files(path))%>%
  dplyr::rename(filename = `list.files(path)`)%>%
  filter(grepl('.wav', filename))%>%mutate(basefilename = substr(filename, 6, nchar(filename) - 4))%>%
  mutate(date = ymd_hms(basefilename),
         STID = substr(filename, 1, nchar(filename) - 17))

#check if folder matches the ST ID in the file string
# if it says FALSE, probably because of time change so check unique(all_wave$STD)
identical(unique(all_wav$STID), ST_ID)

### start and end of deployment ----
# date/time of files that include the deployment and recovery (or is the earliest/latest file in the folder) 
start_deploy = force_tz(min(all_wav$date), tz = ST_TZ)
start_deploy
start_deploy_tz<-format(start_deploy, format = "%Z")
start_deploy_tz

end_deploy = force_tz(max(all_wav$date), tz = ST_TZ)
end_deploy
end_deploy_tz<-format(end_deploy, format = "%Z")
end_deploy_tz

## LFDCS output as csv ----
filename = paste0(site,"_",deployment_number,"-",ST_ID,"-all_LFDCS_Mah3")

# read in LFDCS detections ----
all_lines<-read.delim(paste0(drivepath,site,"/",site,"_",deployment_number,"/lfdcs_processed/",filename,".csv"), skip = 14, header = T, sep = ",")
head(all_lines)
tail(all_lines)
all_lines$start.time<-force_tz(mdy_hms(all_lines$start.time), tz = ST_TZ)

## filter out anything before earliest wavefile in folder ----
all_lines<-all_lines%>%filter(start.time > start_deploy & start.time < end_deploy)

head(all_lines)
tail(all_lines)
nrow(all_lines)
unique(all_lines$Call.type)
all_lines%>%filter(Call.type == -1)%>%nrow()

## wrangle into a selection table for Raven ----

all_lines_Raven<-all_lines%>%
  mutate(Selection = 1: n(),
         View = "Spectrogram 1",
         Channel = 1)%>%
  mutate(start_deploy = start_deploy)%>%
  mutate(
    start.time_UTC = with_tz(start.time, tz = "UTC"),
    start.time_ET = with_tz(start.time_UTC, tz = "America/New_York"), # start.time_ET should be the same as start.time
    time_zone_ET = format(start.time_ET, format = "%Z"))%>%
  mutate(`Begin Time (s)` = as.numeric(start.time_ET - start_deploy, units="secs") + start.fractional.second,
         `End Time (s)` = as.numeric(`Begin Time (s)` + Duration, units="secs"))%>%
  dplyr::rename(
    `Low Freq (Hz)` = Min.freq,
    `High Freq (Hz)` = Max.freq)%>%
  dplyr::select(Selection, View, Channel, Call.type, start.time, start_deploy, `Begin Time (s)`, `End Time (s)`, everything())

head(all_lines_Raven)
tail(all_lines_Raven)

if(detector == "clnb_gom9"){  
  all_lines_Raven<-all_lines_Raven%>%
    mutate(Call.type.translation = case_when(
      Call.type == -1 ~ "Unknown",
      Call.type >= 1 & Call.type <= 3 ~ "Sei whale",
      Call.type == 4 ~ "Unknown LF",
      Call.type >= 5 & Call.type <= 9 ~ "Right whale",
      Call.type == 10 ~ "Unknown impulsive",
      (Call.type >= 15 & Call.type <= 20) | (Call.type >= 23 & Call.type <= 25) ~ "Humpback whale",
      Call.type == 29 ~ "Unknown mid-freq"
    ))
} else {
  all_lines_Raven<-all_lines_Raven%>%
    mutate(Call.type.translation = case_when(
      Call.type == -1 ~ "Unknown",
      Call.type == 1 ~ "Fin whale",
      Call.type >= 2 & Call.type <= 4 ~ "Blue whale"
    ))
}

#all_lines_Raven
nrow(all_lines_Raven)
unique(all_lines_Raven$Call.type.translation)

all_whales_Raven<-all_lines_Raven%>%
  filter(grepl("whale", tolower(Call.type.translation)))%>%
  mutate(validation = "",
         dolphins = "",
         comments = "",
         date = as.Date(start.time))
nrow(all_whales_Raven)

nrow(all_whales_Raven)/nrow(all_lines_Raven) 

head(all_whales_Raven)
tail(all_whales_Raven)

### tod_bins ----
#tod = time of day, choices = morning, day, night

dates<-data.frame(date = seq(from = as.Date(start_deploy), to = as.Date(max(all_whales_Raven$start.time)), by = "day"))

Sys.timezone()

# "sunrise" : sunrise (top edge of the sun appears on the horizon) 
# "sunset" : sunset (sun disappears below the horizon, evening civil twilight starts)

# dawn to dusk is essentially civil twilight
# "dawn" : dawn (morning nautical twilight ends, morning civil twilight starts)
# "dusk" : dusk (evening nautical twilight starts)

dawndusk<-dates%>%
  mutate(sunrise = getSunlightTimes(date = date, lat = lat, lon = lon, keep = c("dawn","dusk"), tz = "America/New_York"))

format(dawndusk$sunrise, format = "%Z")

join_sun<-all_whales_Raven%>%
  left_join(dawndusk, by = "date")

format(join_sun$sunrise$dawn, format = "%Z")
format(join_sun$sunrise$dusk, format = "%Z")

head(join_sun)
tail(join_sun)

format(join_sun$start.time_ET, format = "%Z")

# 
all_whales_Raven_sun<-join_sun%>%
  mutate(tod_bin = case_when(
    start.time_ET <= sunrise$dawn ~ "morning",
    start.time_ET >= sunrise$dusk ~ "night",
    start.time_ET > sunrise$dawn & start.time_ET > sunrise$dawn ~ "day"
  ))%>%
  dplyr::select(-date, -sunrise)

head(all_whales_Raven_sun)
tail(all_whales_Raven_sun)

# write file ----
# dawndusk for reference
write.table(dawndusk, paste0(drivepath,site,"/",site,"_",deployment_number,"/lfdcs_processed/",filename,"-dawndusk.txt"), sep = '\t',
            row.names = F, col.names = T, quote = F)
# selection table for Raven 
write.table(all_whales_Raven_sun, paste0(drivepath,site,"/",site,"_",deployment_number,"/lfdcs_processed/",filename,"-RavenST.txt"), sep = '\t',
            row.names = F, col.names = T, quote = F)

# write.csv(all_whales_Raven_sun, paste0(drivepath,site,"/",site,"_",deployment_number,"/lfdcs_processed/",filename,"-RavenST.csv"), 
#             row.names = F)

