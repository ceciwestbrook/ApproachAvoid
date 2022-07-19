#!/bin/bash
# Ceci Westbrook, April 2022
# little script to make list of subjects from a BIDS directory

# creates an array of all subjects from directories in BIDS dir (rawdata)
subs=()
for dir in `ls -d /ix/cladouceur/westbrook-data/rawdata/*`; do
	rawname=`basename $dir`
	subject=`echo ${rawname} | grep -Eo '[0-9]{4}'`
	subs+=($subject)
done

# remove old file if still there
rm subject_list.txt

# prints to file
uniq_subs=($(printf "%s\n" ${subs[@]} | sort -u))
for sub in ${uniq_subs[@]} ; do
	echo $sub >> subject_list.txt
done
