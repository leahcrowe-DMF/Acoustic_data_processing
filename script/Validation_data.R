library(dplyr);library(lubridate);library(ggplot2)

path<-"C:/Users/Leah.M.Crowe/OneDrive - Commonwealth of Massachusetts/PAM_analysis_backup"

#read all files in the folder
analysis_files<-as.data.frame(list.files(path))%>%
  dplyr::rename(filename = `list.files(path)`)%>%
  mutate(fullpath = paste0(path,"/",filename))

head(analysis_files)

analysis_files_ls<-split(analysis_files, analysis_files$filename)


analysis_data_ls<-lapply(analysis_files_ls, function(x){ 
  
  #x<-analysis_files_ls$`BUZ17_01-all_LFDCS_Mah3-RavenST_LMC.txt`
  
  read.table(x$fullpath, header = T, sep = "\t", quote = "")%>%
         mutate(Deployment = substr(x[[1]],1,5),
                Analyst = substr(x[[1]],38,40))}
  )

analysis_data_df<-bind_rows(analysis_data_ls)%>%
  mutate(validation = case_when(
        validation == "nn" ~ "n",
         validation == "rh" ~ "r?",
         validation == "rh?" ~ "r?",
         validation == "f" ~ "n", # fin whale?
        validation == "b" ~ "n", ## fixing Manali's typo
         TRUE ~ validation))%>%
  mutate(confidence = case_when(
    grepl("?",validation, fixed = TRUE) ~ "Possible",
    validation == "n" ~ "False detection",
    validation != "" ~ "Definite",
    TRUE ~ ""
  ))%>%
  mutate(validated_sp = case_when(
    validation == "n" ~ "False detection",
    grepl("r",validation) ~ "Right whale",
    grepl("h",validation) ~ "Humpback whale",
    grepl("s",validation) ~ "Sei whale",
    grepl("m",validation) ~ "Minke whale",
    TRUE ~ ""
  ))%>%
  mutate(date = as.Date(start.time),
         time = substr(start.time, 12, 20))

analysis_data_df%>%filter(validated_sp == "")

analysis_data_df%>%filter(validated_sp == "Minke whale")

max(nchar(analysis_data_df$comments))

analysis_data_df%>%filter(grepl('?',validation, fixed = TRUE))

analysis_data_df%>%filter(grepl('h',validation, fixed = TRUE))%>%distinct(Deployment, date)

analysis_data_df%>%filter(date == "2025-04-19" & Deployment == "EOS08")

analysis_data_df%>%filter(Deployment == "MBW05" & tod_bin == "")

analysis_data_df%>%filter(Deployment == "MBW05" & 
                            ymd_hms(start.time) < ymd_hms("2025-06-01 00:00:02") & 
                            Call.type.translation == "Right whale" & validation == "")

analysis_data_df%>%filter(Deployment == "CCB06")%>%
  filter(Call.type.translation == "Humpback whale" & validation == "")%>%
  filter(date < "2025-06-01")

analysis_data_df%>%filter(Deployment == "JEF02")%>%
  filter(validation == "r")

# talk to Manali about this, classified right whales without validation
# these probably manual validations that need validated_sp = r
analysis_data_df%>%filter(Deployment == "MBW04")%>%
  filter(validation == "" & Call.type.translation == "Right whale")

unique(analysis_data_df$validation)

analysis_data_df%>%
  filter(validation == "e?")

analysis_data_df%>%filter(Deployment == "CCB06")%>%
  filter(validation == "b")

# megapclicks ----
megapclicks<-analysis_data_df%>%filter(grepl("megap",comments))
unique(megapclicks$Deployment)
# Mn song -----
Mnsong<-analysis_data_df%>%filter(grepl("song",comments) & validated_sp == "Humpback whale" & confidence == "Definite")

ggplot(Mnsong)+
  geom_point(aes(x = date, y = Deployment, color = confidence))+
  ggtitle("Detection of humpback song")

# dolphins  -----
dolphins<-bind_rows(analysis_data_ls)%>%
  filter(dolphins != '')%>%
  mutate(date = as.Date(start.time),
         time = substr(start.time, 12, 20))
unique(dolphins$dolphins)

dolphins%>%filter(dolphins == "vessel")
dolphins%>%filter(dolphins == "other calls")
dolphins%>%filter(dolphins == "pinger")
dolphins%>%filter(dolphins == "r")

ggplot(dolphins)+
  geom_point(aes(x = date, y = Deployment, color = dolphins))+
  ggtitle("Dolphin detections")

# plot false detections and true positives ------


true_positives<-analysis_data_df%>%
  filter(Call.type.translation == validated_sp)

false_sp<-analysis_data_df%>%
  filter(validation != "" & validation != "n")%>%
  filter(Call.type.translation != validated_sp)

false_detections<-analysis_data_df%>%
  filter(confidence == "False detection")

# ggplot(true_positives)+
#   geom_histogram(aes(x = Mahalanobis.distance))+
#   facet_wrap(~Call.type)+
#   ggtitle("True positives")
# 
# ggplot(false_sp)+
#   geom_histogram(aes(x = Mahalanobis.distance))+
#   facet_wrap(~Call.type)+
#   ggtitle("Wrong species")
# 
# ggplot(false_detections)+
#   geom_histogram(aes(x = Mahalanobis.distance))+
#   facet_wrap(~Call.type)+
#   ggtitle("False detections")

right_whale_false_sp<-false_sp%>%
  filter(validated_sp == "Right whale")

# ggplot(right_whale_false_sp%>%filter(confidence != "Possible"))+
#   geom_histogram(aes(x = Mahalanobis.distance), binwidth = 0.5)+
#   facet_wrap(~Call.type)+
#   ggtitle("Wrong species LFDCS, right whale")

## definite detections ----

### by day & tod bins ----
detection_bin<-analysis_data_df%>%
  filter(confidence == "Definite")%>%
  group_by(Deployment, date, tod_bin, validated_sp, confidence)%>%
  mutate(n = n())%>%
  mutate(confidence2 = case_when(
    confidence == "Definite" & n >= 3 ~ "Definite",
    TRUE ~ "Possible"
  ))%>%
  distinct(Deployment, date, tod_bin, validated_sp, confidence2)%>%
  arrange(Deployment, date, tod_bin, validated_sp, confidence2)%>%
  group_by(Deployment, date, tod_bin, validated_sp)%>%
  mutate(con_count = 1:n())%>%
  filter(con_count == 1)
  
detection_bin%>%filter(con_count == 1 & confidence2 == "Possible")

### by date only ----

detection_date<-analysis_data_df%>%
  filter(confidence == "Definite")%>%
  group_by(Deployment, date, validated_sp, confidence)%>%
  mutate(n = n())%>%
  mutate(confidence2 = case_when(
    confidence == "Definite" & n >= 3 ~ "Definite",
    TRUE ~ "Possible"
  ))%>%
  distinct(Deployment, date, validated_sp, confidence2)%>%
  arrange(Deployment, date, validated_sp, confidence2)%>%
  group_by(Deployment, date, validated_sp)%>%
  mutate(con_count = 1:n())%>%
  filter(con_count == 1)

unique(detection_date$validated_sp)

detection_date%>%filter(validated_sp == "")

# species by Deployment

ggplot(detection_date)+
  geom_point(aes(x = date, y = Deployment, color = confidence2))+
  facet_wrap(~validated_sp, ncol = 1)

detection_date%>%filter(Deployment == "JEF02" & validated_sp == "Humpback whale" & confidence2 == "Possible")

detection_date%>%filter(validated_sp == "")

### ----
#Use the below to find days to doublecheck "r?" and look for any unclassified upcalls
detection_date%>%filter(Deployment == "JEF02" & validated_sp == "Right whale" & confidence2 == "Possible")
###
detection_date%>%filter(Deployment == "JEF02" & validated_sp == "Minke whale")

# right whale time of day----
NARW_det<-detection_bin%>%filter(validated_sp == "Right whale")
NARW_det$Deployment<-factor(NARW_det$Deployment, levels = c("EOS08","CCB07","CCB06","MBW05","MBW04","TIL15","JEF03","JEF02","JEF01"))

ggplot(NARW_det)+
  geom_rect(mapping = aes(xmin = ymd("2025-03-27"), xmax = ymd("2025-04-14"), y = Deployment, height = 0.25), fill = "black", alpha = 0.2, data = data.frame(Deployment = c("MBW04","MBW05","CCB06","CCB07")))+
  geom_rect(mapping = aes(xmin = ymd("2025-03-27"), xmax = ymd("2025-04-10"), y = Deployment, height = 0.25), fill = "black", alpha = 0.2, data = data.frame(Deployment = c("JEF01","JEF02","JEF03","TIL15")))+
  geom_rect(mapping = aes(xmin = ymd("2025-03-27"), xmax = ymd("2025-05-01"), y = Deployment, height = 1), fill = "red", alpha = 0.2, data = data.frame(Deployment = unique(NARW_det$Deployment)))+
  geom_rect(mapping = aes(xmin = ymd("2026-02-01"), xmax = ymd("2026-03-01"), y = Deployment, height = 1), fill = "red", alpha = 0.2, data = data.frame(Deployment = unique(NARW_det$Deployment)))+
  geom_rect(mapping = aes(xmin = ymd("2025-05-01"), xmax = ymd("2025-05-14"), y = Deployment, height = 1), fill = "blue", alpha = 0.2, data = data.frame(Deployment =  c("MBW04","CCB06","CCB07","EOS08")))+
  #geom_rect(mapping = aes(xmin = ymd("2026-02-01"), xmax = ymd("2026-03-01"), y = Deployment, height = 1), fill = "red", alpha = 0.2, data = data.frame(Deployment = unique(NARW_det$Deployment)))+
  geom_point(NARW_det%>%filter(confidence2 == "Possible"), mapping = aes(x = date, y = Deployment, color = confidence2))+
  geom_point(NARW_det%>%filter(confidence2 == "Definite"), mapping = aes(x = date, y = Deployment, color = confidence2))+
  facet_wrap(~validated_sp, ncol = 1)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")+
  scale_color_viridis_d(end = 0.8)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank())

ggplot(NARW_det)+
  geom_rect(mapping = aes(xmin = ymd("2025-03-27"), xmax = ymd("2025-04-14"), ymin = "morning", ymax = "night"), fill = "black", alpha = 0.2, data = data.frame(Deployment = c("MBW04","MBW05","CCB06","CCB07")))+
  geom_rect(mapping = aes(xmin = ymd("2025-03-27"), xmax = ymd("2025-04-10"), ymin = "morning", ymax = "night"), fill = "black", alpha = 0.2, data = data.frame(Deployment = c("JEF01","JEF02","JEF03","TIL15")))+
  geom_rect(mapping = aes(xmin = ymd("2025-03-27"), xmax = ymd("2025-05-01"), y = "MRA", height = 0.5), fill = "red", alpha = 0.5, data = data.frame(Deployment = unique(NARW_det$Deployment)))+
  geom_rect(mapping = aes(xmin = ymd("2026-02-01"), xmax = ymd("2026-03-01"), y = "MRA", height = 0.5), fill = "red", alpha = 0.5, data = data.frame(Deployment = unique(NARW_det$Deployment)))+
  geom_rect(mapping = aes(xmin = ymd("2025-05-01"), xmax = ymd("2025-05-14"),  y = "MRA", height = 0.5), fill = "blue", alpha = 0.5, data = data.frame(Deployment =  c("MBW04","CCB06","CCB07","EOS08")))+
  geom_point(detection_bin%>%filter(validated_sp == "Right whale" & confidence2 == "Possible"), mapping = aes(x = date, y = factor(tod_bin, levels = c("morning","day","night")), color = confidence2), alpha = 0.6)+
  geom_point(detection_bin%>%filter(validated_sp == "Right whale" & confidence2 == "Definite"), mapping = aes(x = date, y = factor(tod_bin, levels = c("morning","day","night")), color = confidence2), alpha = 0.6)+
  facet_wrap(~Deployment, ncol = 1)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")+
  scale_color_viridis_d(end = 0.8)+
  theme_bw()+
  ylab("")

detection_bin%>%filter(validated_sp == "Right whale" & confidence2 == "Possible")%>%filter(Deployment == "EOS08")%>%ungroup()%>%distinct(date, tod_bin)

detection_bin%>%filter(tod_bin == "")

# humpback whale ----

ggplot(detection_bin%>%filter(validated_sp == "Humpback whale"))+
  geom_point(aes(x = date, y = factor(tod_bin, levels = c("morning","day","night")), color = confidence2))+
  facet_wrap(~Deployment, ncol = 1)

detection_bin%>%filter(validated_sp == "Humpback whale")%>%filter(Deployment == "BUZ17")

# sei whale ----

ggplot(detection_bin%>%filter(validated_sp == "Sei whale"))+
  geom_point(aes(x = date, y = factor(tod_bin, levels = c("morning","day","night")), color = confidence2))+
  facet_wrap(~Deployment, ncol = 1)

# minke whale ----

ggplot(detection_bin%>%filter(validated_sp == "Minke whale"))+
  geom_point(aes(x = date, y = factor(tod_bin, levels = c("morning","day","night")), color = confidence2))+
  facet_wrap(~Deployment, ncol = 1)
