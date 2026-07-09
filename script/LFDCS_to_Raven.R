library(dplyr);library(lubridate);library(suncalc)
 
# manual params ----

drivepath = "P:/" 
site = "CCB19"
deployment_number = "01"
ST_ID = "8856"

## position of deployment ---- 
lat = 41.88209865
lon = -70.26950737

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

start_deploy = min(all_wav$date)
start_deploy

end_deploy = max(all_wav$date)
end_deploy

year_spring<-year(end_deploy)

#find spring forward date
get_dst2 <- function(y = year_spring, tz = "America/New_York"){
  start <- paste0(y, '-01-01')
  end <- paste0(y, '-12-31')
  d1 <- seq(
    as.POSIXct(start, tz = tz),
    as.POSIXct(end, tz =tz), 
    by = "hour")
  data.frame(
    year = y,
    spring_shift = range(d1[lubridate::dst(d1)])[1],
    autumn_shift = range(d1[lubridate::dst(d1)])[2],
    stringsAsFactors = FALSE)
}

get_dst2()$spring_shift


#check if folder matches the ST ID in the file string
identical(unique(all_wav$STID), ST_ID)

## LFDCS output as csv ----
filename = paste0(site,"_",deployment_number,"-",ST_ID,"-all_LFDCS_Mah3")
 
# read in LFDCS detections ----
all_lines<-read.delim(paste0(drivepath,site,"/",site,"_",deployment_number,"/lfdcs_processed/",filename,".csv"), skip = 14, header = T, sep = ",")

#filter out anything before data recording start
all_lines<-all_lines%>%filter(mdy_hms(start.time) > start_deploy)

head(all_lines)
nrow(all_lines)
unique(all_lines$Call.type)
all_lines%>%filter(Call.type == -1)
 
# wrangle into a selection table for Raven ----
 
all_lines_Raven<-all_lines%>%
  mutate(Selection = 1: n(),
         View = "Spectrogram 1",
         Channel = 1,
         start.time = mdy_hms(start.time))%>%
  mutate(start_deploy = start_deploy)%>%
  mutate(`Begin Time (s)` = as.numeric(start.time - start_deploy, units="secs") + start.fractional.second,
         `End Time (s)` = as.numeric(`Begin Time (s)` + Duration, units="secs"))%>%
  dplyr::rename(
    `Low Freq (Hz)` = Min.freq,
    `High Freq (Hz)` = Max.freq)%>%
  dplyr::select(Selection, View, Channel, Call.type, start.time, start_deploy, `Begin Time (s)`, `End Time (s)`, everything())
 
head(all_lines_Raven)
 
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
 
all_lines_Raven
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

dates<-data.frame(date = seq(from = as.Date(start_deploy), to = as.Date(max(all_whales_Raven$start.time)), by = "day"))

Sys.timezone()

# "sunrise" : sunrise (top edge of the sun appears on the horizon) 
# "sunset" : sunset (sun disappears below the horizon, evening civil twilight starts)

# dawn to dusk is essentially civil twilight
# "dawn" : dawn (morning nautical twilight ends, morning civil twilight starts)
# "dusk" : dusk (evening nautical twilight starts)

dawndusk<-dates%>%
  mutate(sunrise = getSunlightTimes(date = date, lat = lat, lon = lon, keep = c("dawn","dusk"), tz = "America/New_York"))

join_sun<-all_whales_Raven%>%
  left_join(dawndusk, by = "date")

head(join_sun)
tail(join_sun)
# tod = time of day
all_whales_Raven_sun<-join_sun%>%
  mutate(tod_bin = case_when(
    ymd_hms(start.time) <= ymd_hms(sunrise$dawn) ~ "morning",
    ymd_hms(start.time) >= ymd_hms(sunrise$dusk) ~ "night",
    ymd_hms(start.time) > ymd_hms(sunrise$dawn) & ymd_hms(start.time) > ymd_hms(sunrise$dawn) ~ "day"
  ))%>%
  dplyr::select(-date, -sunrise)

head(all_whales_Raven_sun)
tail(all_whales_Raven_sun)

all_whales_Raven_sun%>%filter(start.time > ymd_hms("2026-03-08 02:50:01"))

#while the data are not really in UTC, this code works without declaring the time zone, the UTC assignment to springshift is just to trick it

if (force_tz(get_dst2()$spring_shift, "UTC") > start_deploy & force_tz(get_dst2()$spring_shift, "UTC") < end_deploy){
  all_whales_Raven_sun <- all_whales_Raven_sun %>%
    mutate(`Begin Time (s)` = case_when(
      start.time > force_tz(get_dst2()$spring_shift, "UTC") ~ `Begin Time (s)` - 3600,
      TRUE ~ `Begin Time (s)`
    )) %>%
    mutate(`End Time (s)` = case_when(
      start.time > force_tz(get_dst2()$spring_shift, "UTC") ~ `End Time (s)` - 3600,
      TRUE ~ `End Time (s)`
    ))
  print(TRUE)
} else {
  print(FALSE)
}

# write file ----
# dawndusk for reference
write.table(dawndusk, paste0(drivepath,site,"/",site,"_",deployment_number,"/lfdcs_processed/",filename,"-dawndusk.txt"), sep = '\t',
            row.names = F, col.names = T, quote = F)
# selection table for Raven 
write.table(all_whales_Raven_sun, paste0(drivepath,site,"/",site,"_",deployment_number,"/lfdcs_processed/",filename,"-RavenST.txt"), sep = '\t',
            row.names = F, col.names = T, quote = F)

# write.csv(all_whales_Raven_sun, paste0(drivepath,site,"/",site,"_",deployment_number,"/lfdcs_processed/",filename,"-RavenST.csv"), 
#             row.names = F)

