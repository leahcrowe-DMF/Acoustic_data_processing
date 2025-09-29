# libraries ----

library(PAMmisc);library(PAMscapes);library(dplyr)

# input variables ----

drive_folder = "E:/"
#drive_folder = "D:/DMF_PAM/"
site = "BUZ17"
deployment_number = "01"
ST_ID = "8854"

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

qaqcData <- evaluateDeployment(dir=path, sensitivity=-172.5,
                               outDir=paste0(path,"/QAQC_Output"))


runQAQCReview(data = paste0(path,"/QAQC_Output/",ST_ID,"_QAQCData.csv"))
