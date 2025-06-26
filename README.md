# Acoustic_data_processing

EXPORT: Autodetections
-	Select index
-	Specify “Output CSV file” 
o	remember to include “.csv” at the end
o	Name file as “deployment ID” + “-all_LFDCS_Mah3”
o	Example: CCB01_01-all_LFDCS_Mah3.csv
-	Specify “Maximum Mahalanobis distance filter” as 3
-	OK
Run “LFDCS_to_Raven.R”
-	Creates selection table of LFDCS detections for Raven
-	Named as “deployment ID” + “-all_LFDCS_Mah3” + “-RavenST.txt”
Load the audio and selection table in Raven
Validate detections
