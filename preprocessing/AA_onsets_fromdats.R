library(gdata)
library(tidyverse)
setwd('~/Box Sync/Paperwork/Research/Ladouceur/Avoid/data_cleaning/')
allsubs <- read.csv('imaging_subjects.txt')
setwd('~/Box Sync/Paperwork/Research/Ladouceur/Avoid/CATS Behavioral Data Files 2022/')

# for testing
#allsubs = data.frame(2270)

for(subject in allsubs[,]){
  
  # make directories for later
  subDir <- paste('~/Box Sync/Paperwork/Research/Ladouceur/Avoid/timing_files_new/sub-',subject,sep="")
  if (!file.exists(subDir)) {
    dir.create(subDir)
  }
  
  for(sesnum in c('02','22')) {
      for(run in c('a','b')) {
        possibleError <- tryCatch(read_csv2(paste(subject,'_',sesnum,run,'.dat',sep=""),skip=1,col_name=FALSE),
               error = function(x) x)
        
        session <- ifelse(sesnum=='02',"pretreatment",
                          ifelse(sesnum=='22',"posttreatment",""))

        runnum <- ifelse(run=='a',"01",
                          ifelse(run=='b',"02",""))
        
        # check to make sure the file exists, otherwise break the loop
        if(!inherits(possibleError, "error")) {
          
          # make directories for later
          sesDir <- paste('~/Box Sync/Paperwork/Research/Ladouceur/Avoid/timing_files_new/sub-',subject,'/ses-',session,sep="")
          if (!file.exists(sesDir)) {
            dir.create(sesDir)
            dir.create(paste(sesDir,'/func',sep=""))
          }
          
          # read in the dat files and convert them into useable tibbles
          d <- read_csv2(paste(subject,'_',sesnum,run,'.dat',sep=""),skip=1,col_name=FALSE)
          d2 <- as_tibble(d[1:nrow(d)-1,])
          d2 <- mutate_if(d2,
                          is.character,
                          str_replace_all, "  "," ")
          curr_sub <- separate(d2,col=X1,into = c("Trial_type","Trial_begin","Num_avoid_resp","Num_approach_resp",
                                                  "Last_response_T","Outcome_T","GainLoss_Magnitude","Cumulative_points"),sep=" ")
          curr_sub <- as_tibble(sapply(curr_sub,as.numeric))
          curr_sub$next_trial_start <- lead(curr_sub$Trial_begin)
          curr_sub$last_trial_points <- lag(curr_sub$Cumulative_points)
          curr_sub$GainLoss <- ifelse(curr_sub$Cumulative_points > curr_sub$last_trial_points,"Gain",
                                      ifelse(curr_sub$Cumulative_points < curr_sub$last_trial_points,"Loss",
                                             ifelse(curr_sub$Cumulative_points == curr_sub$last_trial_points,"NoChange",NA)))
          curr_sub[1,"GainLoss"] <- ifelse(curr_sub[1,"Cumulative_points"] > 0, "Gain",
                                           ifelse(curr_sub[1,"Cumulative_points"] < 0, "Loss",
                                                  ifelse(curr_sub[1,"Cumulative_points"] == 0, "NoChange",NA)))
          curr_sub[nrow(curr_sub),"next_trial_start"] <- 384.10
          curr_sub$Response <- ifelse(curr_sub$Last_response_T > 0, (curr_sub$Last_response_T - curr_sub$Trial_begin),(curr_sub$Outcome_T - curr_sub$Trial_begin))
          curr_sub$PostResponse <- ifelse(curr_sub$Last_response_T > 0,(curr_sub$Outcome_T - curr_sub$Last_response_T),0)
          curr_sub$Feedback <- curr_sub$next_trial_start - curr_sub$Outcome_T
          ResponseTrials <- curr_sub[curr_sub$Last_response_T>0,]
          NoResponseTrials <- curr_sub[curr_sub$Last_response_T==0,]
          
          # Here, we break things down into every possible event type, in case future analyses want to slice it
          # differently. This will get consolidated for current purposes down below.
          
          # NB:
          # Approach/Avoid is defined by which column gets to 6 responses.
          # Final trial type for ambiguous trials (approach/avoid) will be determined by what the subject's response was,
          # NOT what the correct response is. In other words, if they approach and had a loss, this is "AmbigApproachLoss."
          
          AvoidLossResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==1 & ResponseTrials$GainLoss=="Loss",c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          AvoidLossPostResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==1 & ResponseTrials$GainLoss=="Loss",c("Last_response_T","PostResponse")],1,.name_repair = ~ c("onset","duration","modulator"))
          AvoidLossFeedback <- tibble(ResponseTrials[ResponseTrials$Trial_type==1 & ResponseTrials$GainLoss=="Loss",c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          AvoidNoLossResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==1 & ResponseTrials$GainLoss=="NoChange",c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          AvoidNoLossPostResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==1 & ResponseTrials$GainLoss=="NoChange",c("Last_response_T","PostResponse")],1,.name_repair = ~ c("onset","duration","modulator"))
          AvoidNoLossFeedback <- tibble(ResponseTrials[ResponseTrials$Trial_type==1 & ResponseTrials$GainLoss=="NoChange",c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          ApproachGainResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==2 & ResponseTrials$GainLoss=="Gain",c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          ApproachGainPostResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==2 & ResponseTrials$GainLoss=="Gain",c("Last_response_T","PostResponse")],1,.name_repair = ~ c("onset","duration","modulator"))
          ApproachGainFeedback <- tibble(ResponseTrials[ResponseTrials$Trial_type==2 & ResponseTrials$GainLoss=="Gain",c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          ApproachNoGainResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==2 & ResponseTrials$GainLoss=="NoChange",c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          ApproachNoGainPostResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==2 & ResponseTrials$GainLoss=="NoChange",c("Last_response_T","PostResponse")],1,.name_repair = ~ c("onset","duration","modulator"))
          ApproachNoGainFeedback <- tibble(ResponseTrials[ResponseTrials$Trial_type==2 & ResponseTrials$GainLoss=="NoChange",c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          AmbigApproachGainResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==4 & ResponseTrials$Num_approach_resp==6 & ResponseTrials$GainLoss=="Gain",c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          AmbigApproachGainPostResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==4 & ResponseTrials$Num_approach_resp==6 & ResponseTrials$GainLoss=="Gain",c("Last_response_T","PostResponse")],1,.name_repair = ~ c("onset","duration","modulator"))
          AmbigApproachGainFeedback <- tibble(ResponseTrials[ResponseTrials$Trial_type==4 & ResponseTrials$Num_approach_resp==6 & ResponseTrials$GainLoss=="Gain",c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          AmbigApproachLossResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==4 & ResponseTrials$Num_approach_resp==6 & ResponseTrials$GainLoss=="Loss",c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          AmbigApproachLossPostResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==4 & ResponseTrials$Num_approach_resp==6 & ResponseTrials$GainLoss=="Loss",c("Last_response_T","PostResponse")],1,.name_repair = ~ c("onset","duration","modulator"))
          AmbigApproachLossFeedback <- tibble(ResponseTrials[ResponseTrials$Trial_type==4 & ResponseTrials$Num_approach_resp==6 & ResponseTrials$GainLoss=="Loss",c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          AmbigAvoidNoLossResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==4 & ResponseTrials$Num_avoid_resp==6 & ResponseTrials$GainLoss=="NoChange",c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          AmbigAvoidNoLossPostResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==4 & ResponseTrials$Num_avoid_resp==6 & ResponseTrials$GainLoss=="NoChange",c("Last_response_T","PostResponse")],1,.name_repair = ~ c("onset","duration","modulator"))
          AmbigAvoidNoLossFeedback <- tibble(ResponseTrials[ResponseTrials$Trial_type==4 & ResponseTrials$Num_avoid_resp==6 & ResponseTrials$GainLoss=="NoChange",c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          XRespResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==3 ,c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          XRespPostResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==3 ,c("Last_response_T","PostResponse")],1,.name_repair = ~ c("onset","duration","modulator"))
          XRespFeedback <- tibble(ResponseTrials[ResponseTrials$Trial_type==3 ,c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          AvoidNoRespResponse <- tibble(NoResponseTrials[NoResponseTrials$Trial_type==1 ,c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          AvoidNoRespFeedback <- tibble(NoResponseTrials[NoResponseTrials$Trial_type==1 ,c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          ApproachNoRespResponse <- tibble(NoResponseTrials[NoResponseTrials$Trial_type==2 ,c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          ApproachNoRespFeedback <- tibble(NoResponseTrials[NoResponseTrials$Trial_type==2 ,c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          AmbigNoRespResponse <- tibble(NoResponseTrials[NoResponseTrials$Trial_type==4 ,c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          AmbigNoRespFeedback <- tibble(NoResponseTrials[NoResponseTrials$Trial_type==4 ,c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          XNoRespResponse <- tibble(NoResponseTrials[NoResponseTrials$Trial_type==3 ,c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          XNoRespFeedback <- tibble(NoResponseTrials[NoResponseTrials$Trial_type==3 ,c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          # Consolidate:
          # The only conditions we are interested in are AvoidNoLoss, Approach Reward, AmbigApproach, and 
          # AmbigAll(AmbigApproach + AmbigAvoid). Failure and no-response trials can be modeled out.
          # Feedback and post-response periods can all be consolidated.
          # In addition, will make a second version which splits the first 2s of the response period out from
          # the rest of the trial.
          
          # Events of interest
          AvoidNoLossResponse$analysis_vars <- "Avoid"
          ApproachGainResponse$analysis_vars <- "Approach"
          AmbigApproachGainResponse$analysis_vars <- "AmbigApproach"
          AmbigApproachLossResponse$analysis_vars <- "AmbigApproach"
          AmbigAvoidNoLossResponse$analysis_vars <- "AmbigAvoid"
          XRespResponse$analysis_vars <- "XResponse"
          
          # For the 2s version:
          # Events of interest
          Avoid2s <- AvoidNoLossResponse
          Approach2s <- ApproachGainResponse
          AmbigApproach2s <- rbind(AmbigApproachGainResponse,AmbigApproachLossResponse)
          AmbigAvoid2s <- AmbigAvoidNoLossResponse
          XResponse2s <- XRespResponse
          
          # Write
          for(onsetfile in c("AvoidLossResponse","AvoidLossPostResponse","AvoidLossFeedback","AvoidNoLossResponse","AvoidNoLossPostResponse",
          "AvoidNoLossFeedback","ApproachGainResponse","ApproachGainPostResponse","ApproachGainFeedback",
          "ApproachNoGainResponse","ApproachNoGainPostResponse","ApproachNoGainFeedback","AmbigApproachGainResponse",
          "AmbigApproachGainPostResponse","AmbigApproachGainFeedback","AmbigApproachLossResponse",
          "AmbigApproachLossPostResponse","AmbigApproachLossFeedback","AmbigAvoidNoLossResponse","AmbigAvoidNoLossPostResponse",
          "AmbigAvoidNoLossFeedback","XRespResponse","XRespPostResponse","XRespFeedback","AvoidNoRespResponse",
          "AvoidNoRespFeedback","ApproachNoRespResponse","ApproachNoRespFeedback","AmbigNoRespResponse","AmbigNoRespFeedback",
          "XNoRespResponse","XNoRespFeedback")) {
            
            # Events of no interest
            if(onsetfile %in% c("AvoidLossResponse","AvoidLossPostResponse","AvoidLossFeedback","AvoidNoLossPostResponse","AvoidNoLossFeedback",
                               "ApproachGainPostResponse","ApproachGainFeedback","ApproachNoGainResponse","ApproachNoGainPostResponse",
                               "ApproachNoGainFeedback","AmbigApproachGainPostResponse","AmbigApproachGainFeedback","AmbigApproachLossPostResponse",
                               "AmbigApproachLossFeedback","AmbigAvoidNoLossPostResponse","AmbigAvoidNoLossFeedback","XRespPostResponse",
                               "XRespFeedback","AvoidNoRespResponse","AvoidNoRespFeedback","ApproachNoRespResponse","ApproachNoRespFeedback",
                               "AmbigNoRespResponse","AmbigNoRespFeedback","XNoRespResponse","XNoRespFeedback")){
              cur_df <- get(onsetfile)
              cur_df$analysis_vars <- "NoInterest"
              assign(onsetfile,cur_df)
            }
            
            # add extra column for use with BIDS tsvs
            cur_df <- get(onsetfile)
            cur_df$event <- onsetfile
            assign(onsetfile,cur_df)
          }
          
          # make tsv file for BIDS format
          allevents <- Reduce(
            function(x, y) merge (x, y, all=TRUE),
            list(AvoidLossResponse,AvoidLossPostResponse,AvoidLossFeedback,AvoidNoLossResponse,AvoidNoLossPostResponse,
                 AvoidNoLossFeedback,ApproachGainResponse,ApproachGainPostResponse,ApproachGainFeedback,
                 ApproachNoGainResponse,ApproachNoGainPostResponse,ApproachNoGainFeedback,AmbigApproachGainResponse,
                 AmbigApproachGainPostResponse,AmbigApproachGainFeedback,AmbigApproachLossResponse,
                 AmbigApproachLossPostResponse,AmbigApproachLossFeedback,AmbigAvoidNoLossResponse,AmbigAvoidNoLossPostResponse,
                 AmbigAvoidNoLossFeedback,XRespResponse,XRespPostResponse,XRespFeedback,AvoidNoRespResponse,
                 AvoidNoRespFeedback,ApproachNoRespResponse,ApproachNoRespFeedback,AmbigNoRespResponse,AmbigNoRespFeedback,
                 XNoRespResponse,XNoRespFeedback))
          
          allevents <- allevents[is.na(allevents$onset)==FALSE,c("onset","duration","event","analysis_vars","modulator")]
          allevents$onset <- as.numeric(allevents$onset)
          allevents <- allevents[order(allevents$onset),]
          
          for(avars in unique(allevents$analysis_vars)) {
            write.table(allevents[allevents$analysis_vars==avars,c("onset","duration","modulator")],
                        paste('~/Box Sync/Paperwork/Research/Ladouceur/Avoid/timing_files_new/sub-',
                              subject,'/ses-',session,'/sub-',subject,'_ses-',session,'_FSLtiming-',
                              avars,'_run-',runnum,'.txt',sep=""),
                        sep=" ",row.names = FALSE,col.names = FALSE)
          }
          # For the 2s version:
          for(fullist in c("Approach","Avoid","AmbigApproach","AmbigAvoid","XResponse")){
            if(nrow(allevents[allevents$analysis_vars==fullist,]) == 0){
              towrite <- as.data.frame(cbind(0,0,0))
              write.table(towrite,paste('~/Box Sync/Paperwork/Research/Ladouceur/Avoid/timing_files_new/sub-',
                                           subject,'/ses-',session,'/sub-',subject,'_ses-',session,'_FSLtiming-',
                                        fullist,'_run-',runnum,'.txt',sep=""),
                          sep=" ",row.names = FALSE,col.names = FALSE)
              write.table(towrite,paste('~/Box Sync/Paperwork/Research/Ladouceur/Avoid/timing_files_new/sub-',
                                        subject,'/ses-',session,'/sub-',subject,'_ses-',session,'_FSLtiming-',
                                        fullist,'2sResponse_run-',runnum,'.txt',sep=""),
                          sep=" ",row.names = FALSE,col.names = FALSE)
              write.table(towrite,paste('~/Box Sync/Paperwork/Research/Ladouceur/Avoid/timing_files_new/sub-',
                                        subject,'/ses-',session,'/sub-',subject,'_ses-',session,'_FSLtiming-',
                                        fullist,'2sRestOfTrial_run-',runnum,'.txt',sep=""),
                          sep=" ",row.names = FALSE,col.names = FALSE)
            } else {
              resp2s <- paste(fullist,"2s",sep="")
                cur_df2s <- get(resp2s)
                cur_dfRest <- get(resp2s)
                cur_df2s$duration <- 2
                cur_dfRest$onset <- cur_dfRest$onset + 2
                cur_dfRest$duration <- cur_dfRest$duration - 2
                write.table(cur_df2s[,c("onset","duration","modulator")],
                            paste('~/Box Sync/Paperwork/Research/Ladouceur/Avoid/timing_files_new/sub-',
                                  subject,'/ses-',session,'/sub-',subject,'_ses-',session,'_FSLtiming-',
                                  resp2s,'Response_run-',runnum,'.txt',sep=""),
                            sep=" ",row.names = FALSE,col.names = FALSE)
                write.table(cur_dfRest[,c("onset","duration","modulator")],
                            paste('~/Box Sync/Paperwork/Research/Ladouceur/Avoid/timing_files_new/sub-',
                                  subject,'/ses-',session,'/sub-',subject,'_ses-',session,'_FSLtiming-',
                                  resp2s,'RestOfTrial_run-',runnum,'.txt',sep=""),
                            sep=" ",row.names = FALSE,col.names = FALSE)
          }

          # BIDS events.tsv
          write.table(allevents[,c("onset","duration","event","analysis_vars")],
                      paste("~/Box Sync/Paperwork/Research/Ladouceur/Avoid/timing_files_new/sub-",subject,
                                             "/ses-",session,"/func/sub-",subject,"_ses-",session,"_task-avoid_run-",runnum,"_events.tsv", sep=""),sep = "\t",row.names=FALSE,quote = FALSE)
          }
          
        }
      }
    }
  }

