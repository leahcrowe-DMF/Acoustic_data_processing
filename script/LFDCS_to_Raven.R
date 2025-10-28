library(dplyr);library(lubridate);library(suncalc)
 
# manual params ----
drivepath = "P:/" 
site = "EOS08"
deployment_number = "01"
ST_ID = "8848"

## detector choice ----
detector = "clnb_gom9"
#detector = "clnb_gomlf_blue"
 
## deployment start ----

#dir(paste0(drivepath,site,"/",site,"_",deployment_number,'/',ST_ID))[1]

start_deploy = ymd_hms("2025-03-27 12:26:26")
 
## LFDCS output as csv ----
filename = paste0(site,"_",deployment_number,"-all_LFDCS_Mah3")

## position of deployment ---- 
lat = 42.057981
lon = -69.989925
 
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

# tod = time of day
all_whales_Raven_sun<-join_sun%>%
  mutate(tod_bin = case_when(
    ymd_hms(start.time) <= ymd_hms(sunrise$dawn) ~ "morning",
    ymd_hms(start.time) >= ymd_hms(sunrise$dusk) ~ "night",
    ymd_hms(start.time) > ymd_hms(sunrise$dawn) & ymd_hms(start.time) > ymd_hms(sunrise$dawn) ~ "day"
  ))%>%
  dplyr::select(-date, -sunrise)

head(all_whales_Raven_sun)

# write file ----
# dawndusk for reference
write.table(dawndusk, paste0(drivepath,site,"/",site,"_",deployment_number,"/lfdcs_processed/",filename,"-dawndusk.txt"), sep = '\t',
            row.names = F, col.names = T, quote = F)
# selection table for Raven 
write.table(all_whales_Raven_sun, paste0(drivepath,site,"/",site,"_",deployment_number,"/lfdcs_processed/",filename,"-RavenST.txt"), sep = '\t',
            row.names = F, col.names = T, quote = F)
