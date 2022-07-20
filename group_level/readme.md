# Level1 directory
Ceci Westbrook, 2022

 # Analysis steps # 
Group level analyses in FSL are conducted using higher-level analyses in FEAT. The scripts for those analyses are .fsfs contained in the results directory itself. This directory includes files to help generate those scripts and also scripts for behavioral analyses.

 # Files in this directory and how to use them #
bet_behav_vars.R - this is the main running script used to generate lists of directories and accompanying behavioral or categorical variables to paste into group-level FEAT analyses. The only other thing needed to create the FEATs is the design matrix which can be obtained from the fsf file.

fsfs/ - directory with the fsf files from the results directories, preserving directory structure. Takes account of analyses run and includes the design files (fsfs) so the design matrices can be reproduced.

get_behav_vars.R - script to extract response rates and numbers by subject for further analysis. Writes out task_behav.txt.

Accessory files:
allQC.txt - omnibus QC file created from an R-script and used for data examination and exclusion, as well as some behavioral analyses.

allQC.xlsx - this is the main file I used for data QC.

get_prepost_copes.sh - helper script to identify subjects with prepost cope files.

get_regressor_lengths.R - script to get the timing for different task epochs in a different format. Useful mostly for writing.

get_fsfs.sh - to copy over the fsfs mentioned above.

usable_subjects_pre/posttreatment.txt - subject numbers by timepoint for contenience.

subjects_by_cope/ - more files for convenience/reference.
