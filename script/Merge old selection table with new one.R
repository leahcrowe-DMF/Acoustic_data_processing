library(dplyr);library(lubridate);library(ggplot2)

# manual params of deployment ----

drivepath = "P:/" 
site = "OUT98"
deployment_number = "01"
ST_ID = "9365"
analyst_initials = "JAF"

file<-paste0(site,'_',deployment_number,"-",ST_ID,"-all_LFDCS_Mah3-RavenST")
file

## NAS or local ----
# on NAS
path<-paste0(drivepath,'/',site,'/',site,'_',deployment_number,'/',"lfdcs_processed/")

# local
# path<-"C:/Users/Leah.M.Crowe/OneDrive - Commonwealth of Massachusetts/Desktop/"
# path

# read old file with validation work ----
# need to rename the selection table file where the validation work was logged with the suffix "-old"

old_selectiontable<- read.table(paste0(path,file,"_",analyst_initials,"-old.txt"), header = T, sep = "\t", quote = "")
old_tz = "UTC"

old_selectiontable$start.time<-ymd_hms(old_selectiontable$start.time, force_tz = old_tz)
old_selectiontable$start_deploy<-ymd_hms(old_selectiontable$start_deploy)

head(old_selectiontable)

# read new selection file without validation work

new_selectiontable<-read.table(paste0(path,file,".txt"), header = T, sep = "\t", quote = "")
new_selectiontable$start_deploy<-ymd_hms(new_selectiontable$start_deploy)
new_selectiontable$validation<-as.character(new_selectiontable$validation)
new_selectiontable$dolphins<-as.character(new_selectiontable$dolphins)
new_selectiontable$comments<-as.character(new_selectiontable$comments)

head(new_selectiontable)
#

old_selectiontable%>%filter(validation == "r")%>%dplyr::select(Selection, start.time)
new_selectiontable%>%filter(validation == "r")%>%dplyr::select(Selection, start.time)

head(old_selectiontable)
head(new_selectiontable)

tail(old_selectiontable)
tail(new_selectiontable)

nrow(old_selectiontable)
nrow(new_selectiontable)

str(old_selectiontable)
str(new_selectiontable)

merge<-old_selectiontable%>%left_join(new_selectiontable, by = c("View","Channel","Low.Freq..Hz.","High.Freq..Hz.", "Begin.Time..s.", "End.Time..s.",
                            "Call.type","start.time","start_deploy","start.fractional.second","Duration","Bandwidth","Amplitude",
                            "Mahalanobis.distance","Call.type.translation"))

# below should be 0 or only include manual validation (may also include detection before or after deployment that have a validation value, include " ")

old_selectiontable%>%anti_join(new_selectiontable, by = c("View","Channel","Low.Freq..Hz.","High.Freq..Hz.", "Begin.Time..s.", "End.Time..s.",
                                                          "Call.type","start.time","start_deploy","start.fractional.second","Duration","Bandwidth","Amplitude",
                                                          "Mahalanobis.distance","Call.type.translation"))

# final format of the newly merged file ----

nrow(merge)
head(merge)
merge%>%filter(validation.x == "")%>%dplyr::select(Selection.y, start.time)

new_merge<-merge%>%
  mutate(
  validation = validation.x,
  dolphins = dolphins.x,
  comments = comments.x,
  tod_bin = tod_bin.x,
  `Begin Time (s)` = Begin.Time..s.,
  `End Time (s)` = End.Time..s.,
  Selection = Selection.y,
  tod_bin = tod_bin.y
  #start.time = start.time.x
)%>%
  dplyr::rename(
  `Low Freq (Hz)` = Low.Freq..Hz.,
  `High Freq (Hz)` = High.Freq..Hz.)%>%
  dplyr::select("Selection","View","Channel",`Begin Time (s)`,`End Time (s)`,`Low Freq (Hz)`,`High Freq (Hz)`,
                "Call.type","start.time","start_deploy","start.fractional.second","Duration","Bandwidth","Amplitude",
                "Mahalanobis.distance",start.time_UTC, start.time_ET, time_zone_ET,"Call.type.translation","validation","dolphins","comments","tod_bin")

nrow(new_merge)
nrow(old_selectiontable)
nrow(new_selectiontable)
head(new_selectiontable)
new_merge%>%filter(validation == "r")%>%dplyr::select(Selection, start.time)

new_merge%>%filter(validation == "r" & comments != "")

head(new_merge)
new_merge[is.na(new_merge)] <- ""

write.table(new_merge, paste0(path,file,"_",analyst_initials,".txt"), sep = '\t',
            row.names = F, col.names = T, quote = F)
