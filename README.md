# Acoustic_data_processing

1. EXPORT: Autodetections
*	Select index
*	Specify “Output CSV file”
    - save inside "lfdsc_processed" folder
    - remember to include “.csv” at the end
    - Name file as “deployment ID” + “-all_LFDCS_Mah3”
    -	Example: CCB01_01-all_LFDCS_Mah3.csv
* For the clnb_gom9
    - Narrowband only = True
    - Broadband only = False 

*	Specify “Maximum Mahalanobis distance filter” as 3
*	Export date/time in text format with fractional seconds = True
* OK
  
2. Run “LFDCS_to_Raven.R”
-	Creates selection table of LFDCS detections for Raven
-	Named as “deployment ID” + “-all_LFDCS_Mah3” + “-RavenST.txt”

3. Load the audio and selection table in Raven

4. Validate detections in Raven
    - load all sound files
    - File/Open Selection Table... navigate to the 'lfdcs_processed' folder to find the “deployment ID” + “-all_LFDCS_Mah3” + “-RavenST.txt”
    - Right click on the "Table 1" tab of the selection table to Save As... “deployment ID” + “-all_LFDCS_Mah3” + “-RavenST_[your initials].txt”
    - validation:
      
          b = blue whale
          h = humpback whale
          f = fin whale
          m = minke whale
          n = not a whale or biotic detection
          r = right whale
          s = sei whale
          ? = unsure what it is, but seems biotic
   - dolphins: opportunistic if you happen to notice them while validating a call. Leave blank otherwise. Can put both (i.e. "we")
     
          w = whistles
          e = echolocation
   - comments: any helpful text to understanding why we would want to revisit this plcae in the recording in the future.
   - Save selection table periodically to protect against data loss from Raven crashes
     
      
