# Level1 directory
Ceci Westbrook, 2022

 # Analysis steps # 


 # Files in this directory and how to use them #
Behavioral data are excluded from this repository for reasons of confidentiality, but are available from the authors at request.

For analyses, in order of use:
convert_DICOMS.sh - this script takes the raw DICOM data and converts it into a BIDS-formatted dataset using dcm2niix.

fmriprep_cmd, _batch, _arraybatch etc. - the scripts used to run fmriprep. These are formatted different ways to run locally (cmd), or using SLURM batch and arraybatch functionality. The NONLONGITUDINAL version excludes the longitudinal option for subjects missing either pre- or posttreatment data.

make_motion_confound_files.sh - the next step is to make the confound files that will be needed for level1 FEATs. This script calls the R script (extract_motion_confounds.R) that reads in the fmriprep output, extracts and reformats the standard and extended motion parameters that will be used as confounds in FEAT. Note that this also puts out spike_numbers.txt which is an easy way to scan the number of censored TRs per subject and run, for QC purposes.

Accessory files:
AA_onsets_fromdats.R - this is a copy of a script to generate timing files from dat files. I'm not sure why this is in here as well as level1 but I guess it doesn't hurt to be redundant!

spike_numbers_wide.txt - reformatted spike_numbers.txt for QC purposes.

testfmriprep_batch.sh - an early test script, best ignored.


