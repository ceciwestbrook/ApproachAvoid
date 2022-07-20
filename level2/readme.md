# Level1 directory
Ceci Westbrook, 2022

 # Analysis steps # 
Level 2, in FSL parlance involves averaging runs at each timepoint together using fixed-effects modeling in FEAT. This is complicated by 2 things in this study: 1) we preprocssed using fmriprep, but FEAT crashes unless it finds its own registration files, which necessitates using a special workaround to get it to ignore its own preprocessing. We use Jeanette Mumford's workaround to handle this, see: https://mumfordbrainstats.tumblr.com/post/166054797696/feat-registration-workaround. This works because registration is done by FEAT at level1 but not applied to data until level2, so you can sneak in there and reset it without FEAT noticing. 2) Some subjects are missing a run at pre/posttreatment. The way this is handled is by averaging the non-missing run with itself, to keep coding consistent. This necessitates some code to find the missing runs and plug them in appropriately. 3) Because subjects are inconsistent between contrasts, this has to be done by individual copes rather than across the whole FEAT directory (which is easier). Things get quite finicky at this level of detail!

 # Files in this directory and how to use them #
Behavioral data are excluded from this repository for reasons of confidentiality, but are available from the authors at request.

For analyses, in order of use:
adj_feat_reg.sh and _arraybatch.sh - Script to implement the workaround. It replaces transformation matrices in the reg/ directory with the identity matrix, and the reg/standard with mean_func. I relabeled the directories to make this clear and avoid errors.

get_level2_runs.R - this fairly complicated script looks through the files in level1/ and creates a whole bunch of files with lists of subject numbers broken down by timepoint, run and cope. These will then be used to generate the level2 fsfs for each cope for each subject.

make_fsfs/ - this directory contains all the files generated above, with lists of subject numbers by timepoint/cope/run. It also contains the template fsf (design_template_level2.fsf), and the make_fsfs_level2.sh file which reads in all of the above file and uses that to correctly populate all the fsf files that need to be generated. Those end up in an fsf directory (not included in this repository).

run_feat_level2.sh, _arraybatch.sh etc. - runs the fsf files using FEAT. This is the easy step once you've made it through all of the above!

Accessory files:
check_for_cope.sh - generic helper script to look for feat output and identify bugs etc.

run_feat_prepost_arraybatch.sh - copy of script to submit prepost feats. There is also one in the prepost/ directory.


