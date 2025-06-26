library(dplyr);library(lubridate)

# detector choice ----
detector = "clnb_gom9"
#detector = "clnb_gomlf_blue"

# deployment start
start_deploy = ymd_hms("2009-03-29 00:00:00")

# LFDCS output as csv ----
filename = "20090329_all"
all_lines<-read.delim(paste0("./data/",filename,".csv"), skip = 14, header = T, sep = ",")
head(all_lines)
nrow(all_lines)

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

# write file ----

write.table(all_lines_Raven, paste0('./data/',filename,"_RavenST.txt"), sep = "\t",
            row.names = F, col.names = TRUE)
