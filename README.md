# Acoustic_data_processing

1. EXPORT: Autodetections
*	Select index
*	Specify “Output CSV file”
  
    - remember to include “.csv” at the end
    -  Name file as “deployment ID” + “-all_LFDCS_Mah3”
    -	Example: CCB01_01-all_LFDCS_Mah3.csv 
-	Specify “Maximum Mahalanobis distance filter” as 3
- OK
  
2. Run “LFDCS_to_Raven.R”
-	Creates selection table of LFDCS detections for Raven
-	Named as “deployment ID” + “-all_LFDCS_Mah3” + “-RavenST.txt”

3. Load the audio and selection table in Raven

4. Validate detections
