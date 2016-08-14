## Python scripts

### 1_DataMergeScript.py
Standardizes data format between Marin and TEAM data.

reads files:
* data/original/ChedaJewel-Cheda-and-Jewel-GGNRA-Cameras.csv
* data/original/ChedaJewel-Cheda-and-Jewel-GGNRA-Deployments.csv
* data/original/ChedaJewel-Cheda-and-Jewel-GGNRA-Images.csv
* data/original/Terrestrial_Vertebrate.csv

writes files:
* data/intermediate/Terrestrial_Vertebrate_Cols_Edited.csv
* data/intermediate/Merged.csv

### 2_EventBurstScript.py
Detects the maximum number of animals in each photo burst event.

reads files:
* data/original/Terrestrial_Vertebrate.csv
* data/intermediate/Marin_Merged.csv

writes files:
* data/intermediate/Max_Animals_MARIN_data.csv
* data/intermediate/Max_Animals_TEAM_data.csv

### 3_DataPreparation.py
Calculate rate of detection and join with metadata.

reads files:
* data/original/Terrestrial_Vertebrate.csv
* data/intermediate/Max_Animals_TEAM_data.csv
* data/intermediate/team_trap_days.csv

writes files:
* data/processed/rate_of_detection.csv

