library(arcgis);library(arcgisbinding);
library(arcgisutils);library(dplyr);library(lubridate);
library(stringr);library(geosphere);library(sf)

arc.check_product()

#works
my_token <- auth_user(
  username = "leah.crowe_DMF",
  password = pswd
)

set_arc_token(my_token)

###
trackline<-arc_read("https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Acoustic_Monitor_Deployment_Survey_Track_Points_view/FeatureServer/0")%>%
  mutate(data_file = "tracks")
names(trackline)

sightings<-arc_read("https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Species_Sighting/FeatureServer/4")%>%
              filter(SurveyType == "dedicated")%>%
              dplyr::rename("LAT_DD" = "S_LAT", "LONG_DD" = "S_LONG")%>%
              mutate(data_file = "sigs")%>%
              mutate(SPECCODE = case_when(
                SPECCODE == "OTHER" ~ SPECOTHER,
                TRUE ~ SPECCODE))%>%
              mutate(NOTES = paste0(NOTES, ", RANGE: ", as.character(RANGE), "NM, BEARING: ", as.character(BEARING)))%>%
              mutate(NOTES = str_replace(NOTES, "^, ", ""))
names(sightings)
head(sightings)

conditions<-arc_read("https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Conditions_Observation/FeatureServer/1")%>%
  mutate(data_file = "conditions")
names(conditions)

events<-conditions%>%
  bind_rows(sightings)%>%
  mutate(DATE = as.Date(CaptureDate))%>%
  arrange(CaptureDate)%>% 
  group_by(DATE)%>% # group also be vessel in the future
  mutate(SIGHTNO = 1: n())

survey<-trackline%>%
  bind_rows(events)%>%
  #bind_rows(sightings)%>%
  dplyr::rename("Datetime_UTC" = "CaptureDate",
                "NUMBER" = "NUMANIMAL")%>%
  arrange(Datetime_UTC,data_file)%>%
  mutate(DATE = as.Date(Datetime_UTC),
         TIME = str_replace_all(substr(Datetime_UTC,12,19), ":",""),
         EVENTNO = 1:n())%>%
  mutate(BEAUFORT = as.numeric(BEAUFORT))%>%
  group_by(DATE)%>% # group also by Vessel in the future
  tidyr::fill(VESSEL, SURVEYTYPE, VISIBLTY, BEAUFORT, CLOUD, WX, GLARE, SWELL, QUALITY, 
              .direction = "down")%>%
  ungroup()

survey%>%group_by(Datetime_UTC)%>%mutate(n = n())%>%filter(n>1)%>%dplyr::select(OBJECTID,Datetime_UTC,n,data_file)

#get bearing
HEADING_grab = NULL

for(i in 1:(nrow(survey))){
  
  HEADING_grab[i] = round(bearing(c(survey$LONG_DD[i],survey$LAT_DD[i]), 
                                  c(survey$LONG_DD[i+1],survey$LAT_DD[i+1])),0)
  print(HEADING_grab[i])
}

#get previous bearing when position and time are duplicated (sig or conditions at same time as survey point)
for(i in 1:(nrow(survey)-1)){
  
  if (survey$LONG_DD[i] == survey$LONG_DD[i+1] & survey$LAT_DD[i] == survey$LAT_DD[i+1] & survey$Datetime_UTC[i] == survey$Datetime_UTC[i+1]){
    print(i)
    HEADING_grab[i] = HEADING_grab[i-1]
  } 
  
}

HEADING_grab[nrow(survey)]<-HEADING_grab[nrow(survey)-1]

tail(HEADING_grab)

survey2<-survey%>%
  mutate(
    LEGTYPE = 5,
    HEADING = HEADING_grab)%>%
  mutate(HEADING = case_when(
    HEADING < 0 ~ HEADING + 360,
    TRUE ~ HEADING
  ))%>%
  # mutate(SURVEYTYPE = case.names(
  #   is.na(SURVEYTYPE) &
  # ))
  dplyr::select(-GlobalID, -CreationDate, -Creator, -EditDate, -Editor, -HorizontalAccuracy, -LatestSurveyEffort)%>%
  dplyr::select(OBJECTID, DDSOURCE, VESSEL, Datetime_UTC, DATE, TIME, EVENTNO, LAT_DD, LONG_DD, SURVEYTYPE, LEGTYPE, #LEGSTAGE, 
                HEADING, VISIBLTY, BEAUFORT, CLOUD, WX, SIGHTNO, SPECCODE, NUMBER, NUMCALF, #PHOTOS,
                IDREL, CONFIDNC, ANHEAD, BEHAV1, NOTES)

sigs1<-survey%>%
  filter(is.na(SURVEYTYPE) & !is.na(SPECCODE))%>%
  as.data.frame()


names(survey2)
head(survey2)
nrow(survey2)


## To DO
## move SIGHTNO for first condition to following line, remove that first row
## ask Cara to include vessel in conditions and sightings
## legstage 1, 2, 5, remove? when on acoustic station