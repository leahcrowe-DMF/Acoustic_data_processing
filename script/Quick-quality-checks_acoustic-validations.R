library(dplyr);library(lubridate);library(ggplot2)

# adjust path for your files

path<-"C:/Users/Leah.M.Crowe/OneDrive - Commonwealth of Massachusetts/PAM_analysis_backup"

#read all files in the folder
analysis_files<-as.data.frame(list.files(path))%>%
  dplyr::rename(filename = `list.files(path)`)%>%
  mutate(fullpath = paste0(path,"/",filename))

head(analysis_files)

analysis_files_ls<-split(analysis_files, analysis_files$filename)


analysis_data_ls<-lapply(analysis_files_ls, function(x){ 
  
  read.table(x$fullpath, header = T, sep = "\t", quote = "")%>%
    mutate(Site = substr(x[[1]],1,5),
           Analyst = substr(x[[1]],38,40),
           dolphins = as.character(dolphins),
           Call.type = as.integer(Call.type))}
)

analysis_data_df<-bind_rows(analysis_data_ls)%>% #compresses the list of selection table dataframes
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

# 1. check distinct "validation" and "dolphin" values ----

## validation column ----
unique(analysis_data_df$validation)

analysis_data_df%>%filter(validation == "rh?")%>%dplyr::select(Site, Selection, start.time)

## dolphins column ----

unique(analysis_data_df$dolphins)

analysis_data_df%>%filter(dolphins == "")%>%dplyr::select(Site, Selection, start.time)

# 2. Check when definite right whale detections are < 3 calls in one day ----

detection_date<-analysis_data_df%>%
  filter(confidence == "Definite")%>%
  group_by(Site, date, validated_sp, confidence)%>%
  mutate(n = n())%>%
  mutate(confidence2 = case_when(
    confidence == "Definite" & n >= 3 ~ "3+ calls",
    TRUE ~ "<3 calls"
  ))%>%
  distinct(Site, date, validated_sp, confidence2)%>%
  arrange(Site, date, validated_sp, confidence2)%>%
  group_by(Site, date, validated_sp)%>%
  mutate(con_count = 1:n())%>%
  filter(con_count == 1)

## see all days with < 3 calls ----

detection_date%>%
  filter(validated_sp == "Right whale" & confidence2 == "<3 calls")

## check on specific site ----

detection_date%>%
  filter(Site == "ACK16")%>% #change the site text
  filter(validated_sp == "Right whale" & confidence2 == "<3 calls")#%>%
  #filter(date > ymd("2025-03-01"))

# 3. Check on right whale detections that were skipped

analysis_data_df%>%
  filter(Call.type.translation == "Right whale" & validation == "")
