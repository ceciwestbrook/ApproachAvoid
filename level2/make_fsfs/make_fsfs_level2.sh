#!/bin/bash

# Ceci Westbrook May 2022
# This script replaces the subject and run numbers in template fsf files
# based on how many level1 runs they have for pre- and post-treatment timepoints
# as well as excluding individual runs where the EV was blank (no trials of that type)

fsfdir=/ix/cladouceur/westbrook-data/Scripts/level2/level2_fsf_files

for cope in cope1 cope2 cope3 cope4 cope5 cope6 cope7 cope8 ; do
	for session in pre post; do
		for subject in `cat "${cope}${session}run1run2.txt"` ; do
			sed 's/SUBNUM/'${subject}'/g' design_template_level2.fsf > $fsfdir'/sub-'${subject}'_ses-'${session}'treatment_'${cope}'_r1r2_fsf-lv2.fsf'
			sed -i 's/COPENUM/'${cope}'/g' $fsfdir'/sub-'${subject}'_ses-'${session}'treatment_'${cope}'_r1r2_fsf-lv2.fsf'
			sed -i 's/SESNUM/'${session}'/g' $fsfdir'/sub-'${subject}'_ses-'${session}'treatment_'${cope}'_r1r2_fsf-lv2.fsf'
    		sed -i 's/RUNNUM1/run1/g' $fsfdir'/sub-'${subject}'_ses-'${session}'treatment_'${cope}'_r1r2_fsf-lv2.fsf'
    		sed -i 's/RUNNUM2/run2/g' $fsfdir'/sub-'${subject}'_ses-'${session}'treatment_'${cope}'_r1r2_fsf-lv2.fsf'
		done
		for subject in `cat "${cope}${session}run1run1.txt"`; do
			sed 's/SUBNUM/'${subject}'/g' design_template_level2.fsf > $fsfdir'/sub-'${subject}'_ses-'${session}'treatment_'${cope}'_r1r1_fsf-lv2.fsf'
			sed -i 's/COPENUM/'${cope}'/g' $fsfdir'/sub-'${subject}'_ses-'${session}'treatment_'${cope}'_r1r1_fsf-lv2.fsf'
			sed -i 's/SESNUM/'${session}'/g' $fsfdir'/sub-'${subject}'_ses-'${session}'treatment_'${cope}'_r1r1_fsf-lv2.fsf'
    		sed -i 's/RUNNUM1/run1/g' $fsfdir'/sub-'${subject}'_ses-'${session}'treatment_'${cope}'_r1r1_fsf-lv2.fsf'
    		sed -i 's/RUNNUM2/run1/g' $fsfdir'/sub-'${subject}'_ses-'${session}'treatment_'${cope}'_r1r1_fsf-lv2.fsf'
		done
		for subject in `cat "${cope}${session}run2run2.txt"` ; do
			sed 's/SUBNUM/'${subject}'/g' design_template_level2.fsf > $fsfdir'/sub-'${subject}'_ses-'${session}'treatment_'${cope}'_r2r2_fsf-lv2.fsf'
			sed -i 's/COPENUM/'${cope}'/g' $fsfdir'/sub-'${subject}'_ses-'${session}'treatment_'${cope}'_r2r2_fsf-lv2.fsf'
			sed -i 's/SESNUM/'${session}'/g' $fsfdir'/sub-'${subject}'_ses-'${session}'treatment_'${cope}'_r2r2_fsf-lv2.fsf'
    		sed -i 's/RUNNUM1/run2/g' $fsfdir'/sub-'${subject}'_ses-'${session}'treatment_'${cope}'_r2r2_fsf-lv2.fsf'
    		sed -i 's/RUNNUM2/run2/g' $fsfdir'/sub-'${subject}'_ses-'${session}'treatment_'${cope}'_r2r2_fsf-lv2.fsf'
		done
  done
done
