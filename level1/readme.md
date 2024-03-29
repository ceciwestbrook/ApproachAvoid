# Level1 directory
Ceci Westbrook, 2022

 # Analysis steps # 
Level 1 analyses were completed using FSL's FEAT. GLMs were performed on the data preprocessed by fmriprep (see the preprocessed/ directory) using 3-column FSL-formatted timing files created from the behavioral data output by the task. (Note that this also requires the motion confound files which were generated in preprocessed/.) Registration and preprocessing was minimized in FEAT and will then be bypassed at level2 using a workaround (see level2/ directory).

Note that analyses were complicated by the fact that not all subjects had all trial types (e.g., some subjects never avoided during approach-avoid conflict). This resulted in empty EV's, which does not cause a problem for the FEATs running, but contrasts involving those EV's will zero out and need to be excluded from further analyses. Thus, this directory also contains code to identify each subject's available runs (pretreatment 1 & 2 and posttreatment 1 and 2) and then which if any conditions were empty for that run.

 # Files in this directory and how to use them #
Behavioral data are excluded from this repository for reasons of confidentiality, but are available from the authors at request.

For analyses, in order of use:
AA_onsets_fromdats.R - this is an R script that reads in the .dat files produced by E-Prime and generates the 3-column FSL timing files. It also creates .tsv files to back-propagate into the BIDS-formatted rawdata directory (move_tsvs_to_BIDS.sh).

find_blank_regressors.R - this file reads in the timing files generated by the prior script and writes out the usable vs. blank regressors by subject (blank_regressors.txt and nonblank_regressors.txt). These files will get used at level2 and the group level to identify contrasts (called 'copes' in FSL parlance) to ignore later. 

make_fsfs_level1.sh - this reads in the template fsf (design_template_level1.fsf) and populates the subject and run numbers using a sed command. Note that this might generate unusable files for subjects missing a run, but I think I just handled that by letting those runs crash out. The design template file has to be created by opening up FEAT and clicking through and then saving, unless you're a better coder than I am :)

run_feat_level1.sh (and _arraybatch.sh) - the script that submits the fsf files to the server to run the glms. This is written using SLURM syntax but can be edited to run locally or whatever.

Accessory files:
allQC.R - the main R script to assist with QC and data exclusion. Reads in data from various sources (including excel database) to create an omnibus database to use for data examination and exclusion, as well as later in other analyses.

check_for_behav_data.sh - checks for existence of the dat files by subject and run and creates a machine-readable text file (behav_data.txt) which I used for QC. Some subjects had to be excluded due to missing behavioral data, and this is how I identified them.

copenames.txt - a list of which cope is which contrast, for convenience :)

subjects_postrun1.txt etc. - lists of subjects per run, since not all subjects had all runs. I believe this was created using a plain text editor and c/p. Will be used in level2.

check_for_cope.sh, check_for_level1_errors.R - a couple helper scripts to quickly identify any feats that didn't run correctly. They just search for cope8 since if that works, the run probably worked!
