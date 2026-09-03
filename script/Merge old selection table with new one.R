library(dplyr);library(lubridate);library(ggplot2)

path<-"C:/Users/Leah.M.Crowe/OneDrive - Commonwealth of Massachusetts/PAM_analysis_backup"

#read all files in the folder
analysis_files<-as.data.frame(list.files(path))%>%
  dplyr::rename(filename = `list.files(path)`)%>%
  mutate(fullpath = paste0(path,"/",filename))

head(analysis_files)

analysis_data_ls<-lapply(analysis_files_ls, function(x){ 
  
  #x<-analysis_files_ls$`BUZ17_01-all_LFDCS_Mah3-RavenST_LMC.txt`
  
  read.table(x$fullpath, header = T, sep = "\t", quote = "")%>%
    mutate(Site = substr(x[[1]],1,5),
           Analyst = "LMC",
           dolphins = as.character(dolphins),
           Call.type = as.integer(Call.type))}
)

old<-analysis_data_ls$`MBW04_02-8859-all_LFDCS_Mah3-RavenST_LMC.txt`

##
drivepath = "P:/" 
site = "MBW04"
deployment_number = "02"
ST_ID = "8859"

new<-read.table(paste0(drivepath,site,"/",site,"_",deployment_number,"/lfdcs_processed/",filename,"-RavenST.txt"), header = T, sep = "\t", quote = "")

#

old%>%filter(validation == "r")%>%dplyr::select(Selection, start.time)

head(old)
head(new)

tail(old)
tail(new)

nrow(old)
nrow(new)

merge<-old%>%left_join(new, by = c("Selection","View","Channel","Low.Freq..Hz.","High.Freq..Hz.", "Begin.Time..s.", "End.Time..s.",
                            "Call.type","start_deploy","start.fractional.second","Duration","Bandwidth","Amplitude",
                            "Mahalanobis.distance","Call.type.translation"))
nrow(merge)

merge%>%filter(validation.x == "r")%>%dplyr::select(Selection, start.time.x, start.time.y)

new_merge<-merge%>%
  mutate(
  validation = validation.x,
  dolphins = dolphins.x,
  comments = comments.x,
  tod_bin = tod_bin.x,
  `Begin Time (s)` = Begin.Time..s.,
  `End Time (s)` = End.Time..s.
)%>%
  dplyr::rename(
  `Low Freq (Hz)` = Low.Freq..Hz.,
  `High Freq (Hz)` = High.Freq..Hz.)%>%
  dplyr::select("Selection","View","Channel",`Begin Time (s)`,`End Time (s)`,`Low Freq (Hz)`,`High Freq (Hz)`,
                "Call.type","start.time","start_deploy","start.fractional.second","Duration","Bandwidth","Amplitude",
                "Mahalanobis.distance","Call.type.translation","validation","dolphins","comments","tod_bin")

nrow(new_merge)
new_merge%>%filter(validation == "r")%>%dplyr::select(Selection, start.time)

head(new_merge)
new_merge[is.na(new_merge)] <- ""

write.table(new_merge, paste0(path,"/",site,"_",deployment_number,"-", ST_ID, "-all_LFDCS_Mah3-RavenST_LMC2.txt"), sep = '\t',
            row.names = F, col.names = T, quote = F)
