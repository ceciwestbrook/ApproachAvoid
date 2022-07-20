# Level1 directory
Ceci Westbrook, 2022

 # Analysis steps # 
This follows the same basic procedure as the level 2 FEATs. However, the prepost level2 is actually a paired t-test between pre-treatment and post-treatment timepoints (which have been co-registered by fmriprep). These FEATs both average runs together and compute the within subjects paired t-test for later analyses. Similarly to level 2, this involves some finesse to get the right subject runs in the right places by each COPE. This directory reuses some of the files generated in level2 for those purposes.

 # Files in this directory and how to use them #
Behavioral data are excluded from this repository for reasons of confidentiality, but are available from the authors at request.

For analyses, in order of use:
make_fsfs/ - directory containing the files with runs by subject, timepoint and cope number. The file make_fsfs_prepost.sh does the heavy lifting of populating the fsfs correctly.

run_feat_prepost_arraybatch.sh - submits the fsfs to run by FEAT. Can be edited to run locally or on other distributed systems aside from SLURM.

Accessory files:





