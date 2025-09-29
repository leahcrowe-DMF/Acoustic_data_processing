library(PAMmisc);library(PAMscapes)

# input variables ----
drive_folder = "E:/"
site = "EOS08"
deployment_number = "01"
ST_ID = "8848"


#qaqcData <- evaluateDeployment(dir="D:/DMF_PAM/GSC12/GSC12_01/8858", sensitivity=-172.5,
 #                              outDir="D:/DMF_PAM/GSC12/GSC12_01/8858/QAQC_Output")

# run report code ----

path<-paste0(drive_folder,'/',site,'/',site,'_',deployment_number,'/',ST_ID)

qaqcData <- evaluateDeployment(dir=path, sensitivity=-172.5,
                               outDir=paste0(path,"/QAQC_Output"))


runQAQCReview(data = paste0(path,"/QAQC_Output/",ST_ID,"_QAQCData.csv"))
