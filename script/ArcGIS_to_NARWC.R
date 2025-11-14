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
date_start<-ymd_hms("2025-01-01 00:00:01")
date_end<-ymd_hms(Sys.time())

###
trackline<-arc_read("https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Acoustic_Monitor_Deployment_Survey_Track_Points_view/FeatureServer/0")%>%
  mutate(data_file = "tracks")%>%
  filter(CaptureDate >= date_start & CaptureDate < date_end)
names(trackline)
head(trackline)

sightings<-arc_read("https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Species_Sighting/FeatureServer/4")%>%
              filter(SurveyType == "dedicated")%>%
              dplyr::rename("LAT_DD" = "S_LAT", "LONG_DD" = "S_LONG")%>%
              mutate(data_file = "sigs")%>%
              mutate(SPECCODE = case_when(
                SPECCODE == "OTHER" ~ SPECOTHER,
                TRUE ~ SPECCODE))%>%
              mutate(NOTES = paste0(NOTES, ", RANGE: ", as.character(RANGE), "NM, BEARING: ", as.character(BEARING)))%>%
              mutate(NOTES = str_replace(NOTES, "^, ", ""))%>%
              filter(CaptureDate >= date_start & CaptureDate < date_end)%>%
              mutate(PHOTOS = case_when(
                    grepl("photo", NOTES) ~ 2,
                    TRUE ~ 1
              ))
names(sightings)
head(sightings)

conditions<-arc_read("https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Conditions_Observation/FeatureServer/1")%>%
  mutate(data_file = "conditions")
names(conditions)
head(conditions)

events<-conditions%>%
  bind_rows(sightings)%>%
  mutate(DATE = as.Date(CaptureDate))%>%
  arrange(CaptureDate)%>% 
  group_by(DATE)%>% # group also be vessel in the future
  mutate(SIGHTNO = 1:n())%>%
  filter(CaptureDate >= date_start & CaptureDate < date_end)

survey<-trackline%>%
  bind_rows(events)%>%
  #bind_rows(sightings)%>%
  dplyr::rename("Datetime_UTC" = "CaptureDate",
                "NUMBER" = "NUMANIMAL")%>%
  arrange(Datetime_UTC,data_file)%>%
  mutate(DATE = as.Date(Datetime_UTC),
         TIME = str_replace_all(substr(Datetime_UTC,12,19), ":",""))%>%
  mutate(BEAUFORT = as.numeric(BEAUFORT))%>%
  group_by(DATE)%>% # group also by Vessel in the future
  tidyr::fill(VESSEL, SURVEYTYPE, VISIBLTY, BEAUFORT, CLOUD, WX, GLARE, SWELL, QUALITY, 
              .direction = "down")%>%
  ungroup()%>%
  filter(!is.na(VISIBLTY))

survey%>%group_by(Datetime_UTC)%>%mutate(n = n())%>%filter(n>1)%>%dplyr::select(OBJECTID,Datetime_UTC,n,data_file)
survey%>%filter(is.na(SURVEYTYPE))


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

for(i in 1:(nrow(survey)-1)){

  if(is.na(survey$SIGHTNO[i+1]) & is.na(survey$SURVEYTYPE[i])){
    survey$SIGHTNO[i+1]<-survey$SIGHTNO[i]
    survey$NOTES[i+1]<-survey$NOTES[i]
  }
  
}


tail(HEADING_grab)

survey2<-survey%>%
  mutate(
    LEGTYPE = 5,
    HEADING = HEADING_grab)%>%
  mutate(HEADING = case_when(
    HEADING < 0 ~ HEADING + 360,
    TRUE ~ HEADING
  ))%>%
  group_by(DATE, VESSEL)%>%
  mutate(EVENTNO = 1:n())%>%
  filter(!is.na(SURVEYTYPE))%>%
  mutate(LEGSTAGE = case_when(
     SIGHTNO == 1 & SURVEYTYPE == "Regular survey (looking)" ~ 1,
     SIGHTNO == max(SIGHTNO) & SURVEYTYPE == "Regular survey (looking)" ~ 5,
     TRUE ~ 2
   ))%>%
  as.data.frame()%>%
  dplyr::select(-GlobalID, -CreationDate, -Creator, -EditDate, -Editor, -HorizontalAccuracy, -LatestSurveyEffort, -geometry)%>%
  dplyr::select(OBJECTID, DDSOURCE, VESSEL, Datetime_UTC, DATE, TIME, EVENTNO, LAT_DD, LONG_DD, LEGTYPE, LEGSTAGE, 
                HEADING, VISIBLTY, BEAUFORT, CLOUD, WX, SIGHTNO, SPECCODE, NUMBER, NUMCALF, PHOTOS,
                IDREL, CONFIDNC, ANHEAD, BEHAV1, NOTES, SURVEYTYPE
                )

write.csv(survey2, paste0("./data/MADMF-NARWC_", as.Date(date_start), "-", as.Date(date_end),".csv"), row.names = F)

## below is working to remove acoustic station prep time

sigs1<-survey%>%
  filter(is.na(SURVEYTYPE) & !is.na(SPECCODE))%>%
  as.data.frame()


names(survey2)
head(survey2)
nrow(survey2)


survey3<-survey2%>%
  group_by(DATE, VESSEL)%>%
  mutate(legstage2 = case_when(
    SIGHTNO == 1 ~ 1
  ))%>%ungroup()

for (i in 1:(nrow(survey3)-1)){
 if (survey3$SURVEYTYPE[i+1] != survey3$SURVEYTYPE[i]){
    print(i)
    survey3$legstage2[i+1] = 55 
  }
}


library(ggplot2)

ggplot(survey3)+
  geom_point(aes(x = LONG_DD, LAT_DD), color = "black")+
  geom_point(survey3%>%filter(!is.na(legstage2)), mapping = aes(x = LONG_DD, LAT_DD, color = as.factor(legstage2)))+
  theme(legend.position = "bottom")

## To DO
## legstage 1, 2, 5, remove? when on acoustic station