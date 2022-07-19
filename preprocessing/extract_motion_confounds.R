#!/usr/bin/env Rscript
# script to create motion variables from the fmriprep output, for later use in FEAT

args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else if (length(args)==1) {
  # default output file
  args[4] = "out.txt"
}

# for debugging
# subject='2025'
# session='posttreatment'
# run=1
# setwd(paste('/ix/cladouceur/westbrook-data/preprocessed/sub-',subject,'/ses-',session,'/func',sep=""))
# d <- read.csv(paste('sub-',subject,'_ses-',session,'_task-avoid_run-',run,'_desc-confounds_timeseries.tsv',sep=""),sep="\t",header=TRUE)

 setwd(paste('/ix/cladouceur/westbrook-data/preprocessed/sub-',args[1],'/ses-',args[2],'/func',sep=""))
 d <- read.table(paste('sub-',args[1],'_ses-',args[2],'_task-avoid_run-',args[3],'_desc-confounds_timeseries.tsv',sep=""),sep="\t",header=TRUE)

censors <- d[,c("framewise_displacement","std_dvars")]
censors$framewise_displacement <- as.numeric(as.character(censors$framewise_displacement))
censors$std_dvars <- as.numeric(as.character(censors$std_dvars))
i = 0
for (outliers in as.numeric(row.names(censors[censors$framewise_displacement>=0.9 | censors$std_dvars>=3,]))) {
  if(is.na(censors[outliers,"framewise_displacement"])) next
  i = i + 1
  curvar <- rep(0, nrow(censors))
  censors[ , ncol(censors) + 1] <- curvar
  censors[outliers,ncol(censors)] <- 1
  colnames(censors)[ncol(censors)] <- paste0("motion_outlier", i) 
}

# to write out all motion variables
mot_vars <- d[,c(grep("trans", names(d), value=TRUE), grep("rot", names(d), value=TRUE),grep("motion_outlier", names(d), value=TRUE))]
write.table(mot_vars,paste('sub-',args[1],'_ses-',args[2],'_motion_confound_run-',args[3],'.txt',sep=""), sep=" ", row.names=FALSE)

# to write out the number of spikes per subject
# for debugging
#cat(subject,session,'run',run,ncol(as.data.frame(censors[,grep("motion_outlier", names(censors), value=TRUE)])),'\n')

cat(args[1],args[2],'run',args[3],ncol(as.data.frame(censors[,grep("motion_outlier", names(censors), value=TRUE)])),'\n')
