library(arcgis);library(arcgisbinding);
library(arcgisutils);library(dplyr);library(lubridate);
library(stringr);library(geosphere);library(sf);library(tidyr)

# manual parameters ----

## check there is ArcGIS on this machine ----
arc.check_product()

## GIS access token to grab program data ----
my_token <- auth_user(
  username = "leah.crowe_DMF",
  password = pswd
)

set_arc_token(my_token)

## establish time of interest ----
date_start<-ymd_hms("2025-01-01 00:00:01")
date_end<-ymd_hms(Sys.time())

# pull data ----

## trackline ----
trackline<-arc_read("https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Acoustic_Monitor_Deployment_Survey_Track_Points_view/FeatureServer/0")%>%
  mutate(data_file = "tracks")%>%
  filter(CaptureDate >= date_start & CaptureDate < date_end)
names(trackline)
head(trackline)

## sightings ----
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

## conditions ----
conditions<-arc_read("https://services1.arcgis.com/7iJyYTjCtKsZS1LR/arcgis/rest/services/Conditions_Observation/FeatureServer/1")%>%
  mutate(data_file = "conditions")
names(conditions)
head(conditions)

# combine data ----

## bind conditions + sightings to establish sightno ----
events<-conditions%>%
  bind_rows(sightings)%>%
  mutate(DATE = as.Date(CaptureDate))%>%
  arrange(CaptureDate)%>% 
  group_by(DATE)%>% # group also be vessel in the future
  mutate(SIGHTNO = 1:n())%>%
  filter(CaptureDate >= date_start & CaptureDate < date_end)

CCBMB<-events%>%filter(DATE == "2025-09-13")

## bind all three data sources ----
survey<-trackline%>%
  bind_rows(events)%>%
  dplyr::rename("Datetime_UTC" = "CaptureDate",
                "NUMBER" = "NUMANIMAL")%>%
  arrange(Datetime_UTC,data_file)%>%
  mutate(DATE = as.Date(Datetime_UTC),
         TIME = str_replace_all(substr(Datetime_UTC,12,19), ":",""))%>%
  mutate(BEAUFORT = as.numeric(BEAUFORT))%>%
  group_by(DATE)%>% # group also by Vessel in the future
  tidyr::fill(VESSEL, SURVEYTYPE, VISIBLTY, BEAUFORT, CLOUD, WX, GLARE, SWELL, QUALITY, 
              .direction = "down")%>% # last occasion carried forward (locf)
  mutate(lag_survey = lag(SURVEYTYPE))%>%
  mutate(SIGHTNO = case_when(
    is.na(lag_survey) & is.na(SIGHTNO) ~ lag(SIGHTNO), #move details from first row (which is the metadata collected before the survey starts) to the next line when the survey officially starts
    TRUE ~ SIGHTNO
  ))%>%
  filter(!is.na(SURVEYTYPE))%>%
  ungroup()%>%
  filter(!is.na(VISIBLTY)) # remove rows without any data that occur before surveys starts (note that locf already occurred)

# calculations ----

## vessel heading ----
HEADING_grab = NULL

### based on i and i+1 location ----
for(i in 1:(nrow(survey))){
  HEADING_grab[i] = round(bearing(c(survey$LONG_DD[i],survey$LAT_DD[i]), 
                                  c(survey$LONG_DD[i+1],survey$LAT_DD[i+1])),0)
  print(HEADING_grab[i])
}

### get previous vessel heading when position and time are duplicated (sig or conditions at same time as survey point) ----
for(i in 1:(nrow(survey)-1)){
  if (survey$LONG_DD[i] == survey$LONG_DD[i+1] & survey$LAT_DD[i] == survey$LAT_DD[i+1] & survey$Datetime_UTC[i] == survey$Datetime_UTC[i+1]){
    print(i)
    HEADING_grab[i] = HEADING_grab[i-1]
  }
}

### final vessel heading is same as point before ----
HEADING_grab[nrow(survey)]<-HEADING_grab[nrow(survey)-1]

tail(HEADING_grab)

# tidy for NARWC submission
survey2<-survey%>%
  mutate(
    LEGTYPE = 5, # POP survey
    HEADING = HEADING_grab)%>%
  mutate(HEADING = case_when(
    HEADING < 0 ~ HEADING + 360, # make vessel heading 0–359
    TRUE ~ HEADING
  ))%>%
  group_by(DATE, VESSEL)%>%
  mutate(EVENTNO = 1:n(),
         EDITS = "")%>% # continuous event number per date/vessel
  mutate(LEGSTAGE = case_when( # legstage starts and ends per date/vessel
     SIGHTNO == 1 & SURVEYTYPE == "Regular survey (looking)" ~ 1,
     EVENTNO == max(EVENTNO) ~ 5, #& SURVEYTYPE == "Regular survey (looking)"
     TRUE ~ 2
   ))%>%
  as.data.frame()%>%
  dplyr::select(-GlobalID, -CreationDate, -Creator, -EditDate, -Editor, -HorizontalAccuracy, -LatestSurveyEffort, -geometry)%>%
  dplyr::select(OBJECTID, DDSOURCE, VESSEL, Datetime_UTC, DATE, TIME, EVENTNO, LAT_DD, LONG_DD, LEGTYPE, LEGSTAGE, 
                HEADING, VISIBLTY, BEAUFORT, CLOUD, WX, SIGHTNO, SPECCODE, NUMBER, NUMCALF, PHOTOS,
                IDREL, CONFIDNC, ANHEAD, BEHAV1, NOTES, EDITS, SURVEYTYPE
                )%>%
  replace(is.na(.), "")

write.csv(survey2, paste0("./data/MADMF-NARWC_", as.Date(date_start), "-", as.Date(date_end),"_wAcoustic.csv"), row.names = F)

# cut out survey when focused on Acoustic monitoring ----
## how many times a survey note occurs consecutively ----
survey2%>%
  filter(SURVEYTYPE == "Survey note")%>%
  group_by(DATE)%>%
  mutate(event_diff_lag = lag(EVENTNO) - EVENTNO,
         event_diff_lead = lead(EVENTNO) - EVENTNO)%>%
  filter(event_diff_lag == -1 | event_diff_lead == 1)

## update legstage ----
# should EVENTNO be updated?

survey3<-survey2%>%
  group_by(DATE, VESSEL)%>%
  mutate(SURVEYTYPE = case_when( # need to do this three times because this has occurred three times in a row above. This is not eloquent.
    SURVEYTYPE == "Survey note" ~ lead(SURVEYTYPE),
    TRUE ~ SURVEYTYPE
  ))%>%
  mutate(SURVEYTYPE = case_when(
    SURVEYTYPE == "Survey note" ~ lead(SURVEYTYPE),
    TRUE ~ SURVEYTYPE
  ))%>%
  mutate(SURVEYTYPE = case_when(
    SURVEYTYPE == "Survey note" ~ lead(SURVEYTYPE),
    TRUE ~ SURVEYTYPE
  ))%>%
  mutate(legstage2 = case_when(
    SURVEYTYPE != lag(SURVEYTYPE) ~ 5, # change in survey type
    SIGHTNO == 1 ~ 1,
    grepl("Acoustic", SURVEYTYPE) == T ~ 99,
    TRUE ~ LEGSTAGE
  ))%>%ungroup()%>%
  filter(legstage2 != 99)%>%
  mutate(legstage2 = 
           case_when(
             legstage2 == 5 & lag(SURVEYTYPE) == "Acoustic station (dedicated looking is paused to focus on acoustic equipment recovery and deployment)"
                                  ~ 1,
             TRUE ~ legstage2)
           )%>%
  mutate(LEGSTAGE = legstage2)%>%
  dplyr::select(-SURVEYTYPE, -legstage2)

write.csv(survey3, paste0("./data/MADMF-NARWC_", as.Date(date_start), "-", as.Date(date_end),".csv"), row.names = F)
  
nrow(survey2)
nrow(survey3)

# map of each day ----

library(ggplot2)

ggplot(survey3)+
  geom_point(aes(x = LONG_DD, LAT_DD, color = as.factor(LEGSTAGE)), size = 0.5)+
  #geom_point(aes(x = LONG_DD, LAT_DD), color = "black")+
  geom_point(survey3%>%filter(LEGSTAGE != 2), mapping = aes(x = LONG_DD, LAT_DD, color = as.factor(LEGSTAGE)), size = 3, alpha = 0.4)+
  theme(legend.position = "bottom")+
  facet_wrap(~DATE, scales = "free")

