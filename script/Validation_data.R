library(dplyr);library(lubridate)

path<-"C:/Users/Leah.M.Crowe/OneDrive - Commonwealth of Massachusetts/PAM_analysis_backup"

analysis_files<-as.data.frame(list.files(path))%>%
  dplyr::rename(filename = `list.files(path)`)%>%
  mutate(fullpath = paste0(path,"/",filename))

head(analysis_files)

analysis_files_ls<-split(analysis_files, analysis_files$filename)


analysis_data_ls<-lapply(analysis_files_ls, function(x){ 
  
  #x<-analysis_files_ls$`BUZ17_01-all_LFDCS_Mah3-RavenST_LMC.txt`
  
  read.table(x$fullpath, header = T, sep = "\t", quote = "")%>%
         mutate(Deployment = substr(x[[1]],1,5),
                Analyst = substr(x[[1]],33,35))}
  )

analysis_data_df<-bind_rows(analysis_data_ls)%>%
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


analysis_data_df%>%filter(validated_sp == "Minke whale")

max(nchar(analysis_data_df$comments))

analysis_data_df%>%filter(grepl('?',validation, fixed = TRUE))

analysis_data_df%>%filter(grepl('h',validation, fixed = TRUE))%>%distinct(Deployment, date)

analysis_data_df%>%filter(date == "2025-04-19" & Deployment == "EOS08")

analysis_data_df%>%filter(Deployment == "MBW05" & tod_bin == "")

analysis_data_df%>%filter(Deployment == "MBW05" & 
                            ymd_hms(start.time) > ymd_hms("2025-06-15 00:00:02") & 
                            Call.type.translation == "Right whale" & validation == "")

analysis_data_df%>%filter(Deployment == "CCB07")%>%
  filter(Call.type.translation == "Humpback whale" & validation == "")%>%
  filter(date < "2025-06-01")

#str(analysis_data_df)


library(ggplot2)

true_positives<-analysis_data_df%>%
  filter(Call.type.translation == validated_sp)

false_sp<-analysis_data_df%>%
  filter(validation != "" & validation != "n")%>%
  filter(Call.type.translation != validated_sp)

false_detections<-analysis_data_df%>%
  filter(confidence == "False detection")

ggplot(true_positives)+
  geom_histogram(aes(x = Mahalanobis.distance))+
  facet_wrap(~Call.type)+
  ggtitle("True positives")

ggplot(false_sp)+
  geom_histogram(aes(x = Mahalanobis.distance))+
  facet_wrap(~Call.type)+
  ggtitle("Wrong species")

ggplot(false_detections)+
  geom_histogram(aes(x = Mahalanobis.distance))+
  facet_wrap(~Call.type)+
  ggtitle("False detections")

right_whale_false_sp<-false_sp%>%
  filter(validated_sp == "Right whale")

ggplot(right_whale_false_sp%>%filter(confidence != "Possible"))+
  geom_histogram(aes(x = Mahalanobis.distance), binwidth = 0.5)+
  facet_wrap(~Call.type)+
  ggtitle("Wrong species LFDCS, right whale")

library(ggplot2)


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

# species by Deployment

ggplot(detection_date)+
  geom_point(aes(x = date, y = Deployment, color = confidence2))+
  facet_wrap(~validated_sp, ncol = 1)

detection_date%>%filter(Deployment == "EOS08" & validated_sp == "Humpback whale" & confidence2 == "Possible")
detection_date%>%filter(Deployment == "MBW05" & validated_sp == "Humpback whale")
detection_date%>%filter(Deployment == "EOS08" & validated_sp == "Minke whale")
# right whale ----

ggplot(detection_bin%>%filter(validated_sp == "Right whale"))+
  geom_point(aes(x = date, y = factor(tod_bin, levels = c("morning","day","night")), color = confidence2))+
  facet_wrap(~Deployment, ncol = 1)

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
