library(arcgis);library(arcgisbinding);
library(arcgisutils);library(dplyr);library(lubridate);
library(stringr);library(geosphere);library(sf);library(tidyr)


## check there is ArcGIS on this machine ----
arc.check_product()

## GIS access token to grab program data ----
source('./script/pswd.R', local = TRUE)$value

my_token <- auth_user(
  username = "leah.crowe_DMF",
  password = pswd
)

set_arc_token(my_token)

##

PAM<-arc_read("https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/PAM_Location/FeatureServer/3")
head(PAM)

predeploy<-PAM%>%filter(DEPLOYSTATUS == "Pre-deployment")%>%as.data.frame()%>%
  dplyr::select(SAT_beacon_ID, SoundTrap_ID, VR2_ID, PAMID, "Pre-deploy_notes" = "NOTES")

recover<-PAM%>%filter(DEPLOYSTATUS == "Recovery")%>%as.data.frame()%>%
  dplyr::select("Recover_Datetime_ET" = "CaptureDate", "Recover_LAT" = "LAT_DD", "Recover_LON" = "LONG_DD", PAMID, "Recover_notes" = "NOTES")

all_deploy_data<-PAM%>%filter(DEPLOYSTATUS == "Deployment")%>%dplyr::select(PAMID, "Deploy_Datetime_ET" = "CaptureDate", "Deploy_LAT" = "LAT_DD", "Deploy_LON" = "LONG_DD", DEPTHFT, "Deploy_notes" = "NOTES")%>%
  full_join(predeploy, by = "PAMID")%>%
  full_join(recover, by = "PAMID")%>%
  filter(PAMID != "Accidental deployment")%>%
  filter(!grepl("Never deployed", `Pre-deploy_notes`))%>%
  as.data.frame()%>%
  arrange(Deploy_Datetime_ET)%>%
  dplyr::select(PAMID, SAT_beacon_ID, SoundTrap_ID,VR2_ID, Deploy_Datetime_ET, Deploy_LAT, Deploy_LON, DEPTHFT, `Pre-deploy_notes`, Deploy_notes, Recover_Datetime_ET, Recover_LAT, Recover_LON, Recover_notes)

max_deploy_date<-max(as.Date(all_deploy_data$Deploy_Datetime_ET))

write.csv(all_deploy_data, paste0("./data/MADMF-all_deploy_data-thru", max_deploy_date,".csv"), row.names = F)

