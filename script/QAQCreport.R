# libraries ----

library(PAMmisc);library(PAMscapes);library(dplyr);library(lubridate)

# input variables ----

drive_folder = "P:/"
#drive_folder = "D:/DMF_PAM/"
site = "TIL15"
deployment_number = "02"
ST_ID = "8855"

# path to data ----

path<-paste0(drive_folder,'/',site,'/',site,'_',deployment_number,'/',ST_ID)

# sleuth directory discrepancies ----

dir<-as.data.frame(list.files(path))%>%
  dplyr::rename(filename = `list.files(path)`)
head(dir)
sud <- dir%>%filter(grepl('.sud', filename))%>%mutate(basefilename = substr(filename, 1, nchar(filename) - 4))
nrow(sud)
wav <- dir%>%filter(grepl('.wav', filename))%>%mutate(basefilename = substr(filename, 1, nchar(filename) - 4))
nrow(wav)

wav%>%anti_join(sud, by = 'basefilename')
sud%>%anti_join(wav, by = 'basefilename')

# run report code ----

qaqcData <- evaluateDeployment(dir=path, sensitivity=-172.5, excludeDirs = "Initial offload",
                               outDir=paste0(path,"/QAQC_Output"))


runQAQCReview(data = paste0(path,"/QAQC_Output/",site,"_",deployment_number,"-",ST_ID,"_QAQCData.csv"))

# output for noise floor investigation ----

TOL<-read.csv(paste0(path, "/QAQC_Output/",site,"_",deployment_number,"-",ST_ID,"_QAQCData.csv"), header = T)

noise_floor<-TOL%>%filter(TOL_16000 > TOL_500)%>%
  mutate(TOL_16000_dif = lead(TOL_16000) - TOL_16000)%>%
  dplyr::select(TOL_16000, TOL_16000_dif, UTC)%>%
  mutate(UTC = ymd_hms(UTC, tz = "UTC"))%>%
  mutate(datetime_ET = with_tz(UTC, tz = "America/New_York"))%>%
  group_by(date = as.Date(datetime_ET))%>%
  mutate(num_hours = n(),
         min_hour_ET = min(datetime_ET),
         max_hour_ET = max(datetime_ET))%>%
  distinct(min_hour_ET, max_hour_ET, num_hours)%>%
  mutate(type = "", Comments = "")%>%
  filter(!is.na(date))

write.csv(noise_floor, paste0(path,"/QAQC_Output/",site,"_",deployment_number,"-",ST_ID,"_noisefloor.csv"), row.names = F)
