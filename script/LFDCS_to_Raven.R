library(dplyr);library(lubridate);library(suntools)
 
# manual params ----
drivepath = "E:/" 
site = "EOS08"
deployment_number = "01"
#ST_ID = "8854"

## detector choice ----
detector = "clnb_gom9"
#detector = "clnb_gomlf_blue"
 
## deployment start ----
start_deploy = ymd_hms("2025-03-27 12:26:26")
 
## LFDCS output as csv ----
filename = paste0(site,"_",deployment_number,"-all_LFDCS_Mah3")

## position of deployment ---- 
lat = 42.057981
lon = -69.989925
 
# read in LFDCS detections ----
all_lines<-read.delim(paste0(drivepath,site,"/",site,"_",deployment_number,"/lfdcs_processed/",filename,".csv"), skip = 14, header = T, sep = ",")
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
         comments = "")
nrow(all_whales_Raven)

nrow(all_whales_Raven)/nrow(all_lines_Raven) 

head(all_whales_Raven)

sunriset(crds = c(lon, lat), dateTime = date_time, direction = "sunrise")

# write file ----
 
write.table(all_lines_Raven, paste0("E:/EOS08/EOS08_01/lfdcs_processed/",filename,"-RavenST.txt"), sep = '\t',
            row.names = F, col.names = T, quote = F)
