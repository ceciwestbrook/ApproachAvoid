# ApproachAvoid
This is the accompanying analysis code for the CATS Approach-Avoid Conflict study, PIs Westbrook, Schlund and Ladouceur. Please see our preregistration at https://osf.io/jxske for more information.

All code and associated documents were written by Ceci Westbrook 2021-2022 unless otherwise noted.

This document is a work in progress and will be edited with updates and further information as indicated.

 # Task description: # 
...

 # How to use this repository: # 
This dataset was analyzed primarily using FSL's Feat program, which takes design files (.fsf) as input. These files can be batch-edited and submitted, which was the approach used here, following Jeanette Mumford's approach (please see: https://www.youtube.com/playlist?list=PLB2iAtgpI4YHlH4sno3i3CUjCofI38a-3). Preprocessing was completed using fmriprep version 1.0.12 (https://fmriprep.org/en/stable/).
Code for this project was run on a parallel server using SLURM, and as such, scripts will contain syntax for batch processing using SLURM. These files can be run on local servers with minor editing to remove SLURM syntax, but this is up to the individual user.
The order of use for these scripts are:
 1) Preprocessing with fmriprep using scripts in the preprocessing directory
 2) level 1 analyses conducted by batch-editing the template .fsf scripts found in the level1/ directory and running the resulting files using feat
 2) adjusting the registration in level1 feat directories using Jeanette Mumford's workaround (https://mumfordbrainstats.tumblr.com/post/166054797696/feat-registration-workaround)
 4) level 2 analyses conducted by batch-editing template .fsfs in the level2/ directory and running the resulting files using feat, and finally
 5) group-level analyses using FEAT.

 # Data included: # 
All imaging data was collected on a 3T MRI scanner ...

Anatomical 
...

Functional
...

Fieldmap
This was not collected on all participants and as such was excluded from the current analyses.

 # Preregistration: # 
This dataset corresponds to pre-registered analyses which can be found here: https://osf.io/tsd74 (updates to preregistration at https://osf.io/b8exs).

 # Associated neural data # 
This code is associated with a neural dataset published on openneuro.org. DOI for this dataset will be added when it becomes publicly available.

 # Data collection and acknowledgements: # 
Data were collected ...


 # Authors: # 
...

 # Status of current dataset: # 
Data are currently under submission at .... Publication information will be forthcoming as it becomes available.

