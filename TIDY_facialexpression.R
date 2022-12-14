# SCRIPT TO CALCULATE INTERPERSONAL SYNCHRONY IN ACTION UNITS AS RETRIEVED FROM OPENFACE

#load libraries
install.packages("rMEA")
install.packages("data.table") 
install.packages("tidyverse")
install.packages("tools")
install.packages("moments")

library("rMEA")
library("data.table")
library("tidyverse")
library("tools")
library("moments")

options(datatable.fread.datatable=FALSE)

# clean workspace
rm(list=ls())

### NAMING OF OPENFACE .CSV OUTPUT FILES: id_group_task_seatingposition


# 1. READ IN DATA ---------------------------------------------------------

# set path to csv files as retrieved from OpenFace
path <- "" 

# lists and reads in all files on path (CAVE: long processing time depending on number of files) 
# skips first 10 seconds (b/c of MEA artefacts), preserves columns for frame number, 
# timestamp, confidence, success, head pose (pitch, yaw, roll), intensity and occurrence of different AUs
files <- list.files(path=path, recursive = T, pattern = "ML_.*\\.csv", full.names = T, include.dirs = F) 
myfilelist <- lapply(files, fread, header = F, skip=301, drop = c(2,6:296,300:679)) 
names(myfilelist) <- file_path_sans_ext(basename(files)) 


# read in header separately 
header <- fread(files[1], header = F, nrows = 1, stringsAsFactors=F,  drop = c(2,6:296,300:679))
for (i in 1:length(myfilelist)){
  colnames(myfilelist[[i]]) <- unlist(header)
}


# 2. CLEAN AND FILTER DATA ------------------------------------------------

# check for NANs 
for (i in 1:length(myfilelist)){
  if (anyNA(myfilelist[[i]])==T){ 
    print('NaN')
  }
}

# check if every dataframe has 18,000 lines
for (i in 1:length(myfilelist)){
  if (nrow(myfilelist[[i]])!=18000){ 
    print('incomplete')
  }
}

# filter data based on mean confidence and successfully tracked frames 
outlier <- c()
for (i in 1:length(myfilelist)) {
  # logical: is mean confidence of tracked frames under 75%?
  if (mean(myfilelist[[i]]$confidence) < 0.75 
      # OR
      || 
      # logical: is number of successfully tracked frames under 90%?
      sum(myfilelist[[i]]$success == 1) / nrow(myfilelist[[i]]) < 0.9) { 
    print(paste0("face tracking not reliable enough for ", names(myfilelist[i])))
    outlier <- c(outlier, names(myfilelist[i])) #keep vector for documentation purposes
  }
}

# exclude outlier and respective interaction partners
myfilelist_cleaned <- myfilelist
outlier <- lapply(strsplit(as.character(outlier), split="_"),"[",c(2,4)) #grab dyad number and task out of outlier vector

for (i in names(myfilelist_cleaned)){ 
  for (j in outlier){
    if (all(str_detect(i,j))==TRUE){ #if ID contains both elements (dyad number and task)
      myfilelist_cleaned[[i]] = NULL
  }
  }
}

# sanity check
for (i in 1:length(myfilelist_cleaned)) {
  # logical: is mean confidence of tracked frames under 75%?
  if (mean(myfilelist_cleaned[[i]]$confidence) >= 0.75 
      # AND
      && 
      # logical: is number of successfully tracked frames under 90%?
      sum(myfilelist_cleaned[[i]]$success == 1) / nrow(myfilelist_cleaned[[i]]) >= 0.9) { 
    print("TRUE")
  }
  else {
    print(paste0("FALSE", names(myfilelist_cleaned[i])))
  }
}

# data wrangling: ID-dyad-task-dx-position
alldata <- bind_rows(myfilelist_cleaned, .id = "ID")
dyad <- sapply(strsplit(as.character(alldata$ID), split="_"),"[",2) # create dyad number column
task <- sapply(strsplit(as.character(alldata$ID), split = "_"),"[",4) # create task column
dx <- sapply(strsplit(as.character(alldata$ID), split = "_"),"[",3) # create diagnosis column
position <- sapply(strsplit(as.character(alldata$ID), split = "_"),"[",5) # create seating position column (L/R)
alldata <- cbind(dyad, task, dx, position, alldata)


# 3. SYNCHRONIZATION OF FACIAL EXPRESSION TIME SERIES ---------------------
# Steps: 
# 1) create fake MEA object
# 2) calculate ccf according to rMEA

# 1) initialize function to create a fake MEA object out of two vectors
# Input: 
#     * s1, s2: numeric vectors containing the values to be correlated
#     * sampRate: sampling rate per second
#     * s1Name, s2Name: name for the values to be correlated, default is "s1Name" and "s2Name"
# Output:
#     * fake MEA object that pretends to be a MEA object
#
# (c) Irene Sophia Plank, 10plank@gmail.com
fakeMEA <- function(s1,s2,sampRate,s1Name = "s1Name",s2Name = "s2Name") {
  mea = structure(list(all_01_01 = structure(list(MEA = structure(list(
    s1Name = s1, s2Name = s2), row.names = c(NA, -length(s1)), class = "data.frame"), 
    ccf = NULL, ccfRes = NULL), id = "01", session = "01", group = "all", sampRate = sampRate, 
    filter = "raw", ccf = "", s1Name = s1Name, s2Name = s2Name, uid = "all_01_01", 
    class = c("MEA","list"))), class = "MEAlist", nId = 1L, n = 1L, groups = "all", sampRate = sampRate, 
    filter = "raw", s1Name = s1Name, s2Name = s2Name, ccf = "")
  return(mea)
}

# 2) calculate ccf according to rMEA
# prepare dyads and AUs to loop over
dyad_no <- as.data.frame(unique(alldata$dyad))
names(dyad_no) <- "dyad"
rownames(dyad_no) <- dyad_no$dyad # rownames = dyad ID's
AU <- c("pose_Rx",
        "pose_Ry", 
        "pose_Rz", 
        "AU01_r",  
        "AU02_r", 
        "AU04_r", 
        "AU05_r", 
        "AU06_r", 
        "AU07_r", 
        "AU09_r",  
        "AU10_r",  
        "AU12_r", 
        "AU14_r",  
        "AU15_r",
        "AU17_r",  
        "AU20_r", 
        "AU23_r",  
        "AU25_r",  
        "AU26_r", 
        "AU45_r")

# loop over dyads for hobbies task
entrain_hobbies_all <- data.frame()
for (i in dyad_no$dyad){ 
  # initialize heatmaps
  pdf(paste0(i, "_hobbies",".pdf")) 
  
  # grab only hobbies portion from big df
  cur_df = alldata[alldata$dyad == i & alldata$task == "hobbies",1:29] 
  
  # check if data frame is present
  if (nrow(cur_df) > 0) { 
    
    # loop over AUs
    for (j in AU){ 
      
      # prepare fake MEA components
      s1 = cur_df[cur_df$position == "L", j] # AU for left participant
      s2 = cur_df[cur_df$position == "R", j] # AU for right participant
      sampRate = 30
      s1Name = as.character(cur_df[cur_df$position == "L","ID"])[1] 
      s2Name = as.character(cur_df[cur_df$position == "R","ID"])[1]
      # create fake MEA object
      mea = fakeMEA(s1,s2,sampRate,s1Name,s2Name) 
      
      # time lagged windowed cross-correlations
      mea = MEAccf(mea,lagSec = 2, winSec = 7, incSec = 4, r2Z=T, ABS=T) 
      # extract matrix with all ccf values over all lags and windows 
      ccfvalues <- mea[["all_01_01"]][["ccf"]] 
     
      # configure heatmap
      par(col.main='white') # set plot title to white
      heatmap <- MEAheatmap(mea[["all_01_01"]])
      par(col.main='black') # set plot title back to black
      title(main=paste0(j)) # alternative title
      
      # peak picking
      # CAUTION: set NAN removal to true to ignore nans in the calculation of the maximum
      L <- apply(ccfvalues[,c(1:60)], 1, max, na.rm =T ) 
      R <- apply(ccfvalues[,c(62:121)], 1, max, na.rm =T )
      entrain_df <- as.data.frame(t(cbind(L,R))) 
      entrain_df <- cbind(rownames(entrain_df), data.frame(entrain_df, row.names=NULL))
      colnames(entrain_df)[1] <- "position"
      
      # set any infinite values to NA
      entrain_df <- do.call(data.frame,lapply(entrain_df, function(x) replace(x, is.infinite(x),NA)))
      
      # calculate summary statistics
      entrain_df$min <- apply(entrain_df[,grep(pattern="w",names(entrain_df))],1,min,na.rm=T)
      entrain_df$max <- apply(entrain_df[,grep(pattern="w",names(entrain_df))],1,max,na.rm=T)
      entrain_df$sd <- apply(entrain_df[,grep(pattern="w",names(entrain_df))],1,sd,na.rm=T)
      entrain_df$mean <- apply(entrain_df[,grep(pattern="w",names(entrain_df))],1,mean,na.rm=T)
      entrain_df$md <- apply(entrain_df[,grep(pattern="w",names(entrain_df))],1,median,na.rm=T)
      entrain_df$kurtosis <- apply(entrain_df[,grep(pattern="w",names(entrain_df))],1,kurtosis,na.rm=T)
      entrain_df$skew <- apply(entrain_df[,grep(pattern="w",names(entrain_df))],1,skewness,na.rm=T)
      
      # configure data frame 
      entrain_df$ID[entrain_df$position=="L"] <- paste(gsub("_hobbies","",s1Name))
      entrain_df$ID[entrain_df$position=="R"] <- paste(gsub("_hobbies","",s2Name))
      entrain_df$AU <- paste(j)
      entrain_df$task <- "hobbies"
      entrain_hobbies_all <- rbind(entrain_hobbies_all,entrain_df)
      
      # show progress
      print(paste(j,"for",i, "done"))
    }
  }
  dev.off()
}

# loop over dyads for mealplanning task
entrain_mealplanning_all <- data.frame()
for (i in dyad_no$dyad){ 
  # initialize heatmaps
  pdf(paste0(i, "_mealplanning",".pdf")) 
  
  # grab only mealplanning portion from big df
  cur_df = alldata[alldata$dyad == i & alldata$task == "mealplanning",1:29] 
  
  # check if data frame is present
  if (nrow(cur_df) > 0) { 
    
    # loop over AUs
    for (j in AU){ 
      
      # prepare fake MEA components
      s1 = cur_df[cur_df$position == "L", j] # AU for left participant
      s2 = cur_df[cur_df$position == "R", j] # AU for right participant
      sampRate = 30
      s1Name = as.character(cur_df[cur_df$position == "L","ID"])[1] 
      s2Name = as.character(cur_df[cur_df$position == "R","ID"])[1]
      # create fake MEA object
      mea = fakeMEA(s1,s2,sampRate,s1Name,s2Name) 
      
      # time lagged windowed cross-correlations
      mea = MEAccf(mea,lagSec = 2, winSec = 7, incSec = 4, r2Z=T, ABS=T) 
      # extract matrix with all ccf values over all lags and windows 
      ccfvalues <- mea[["all_01_01"]][["ccf"]] 
      
      # configure heatmap
      par(col.main='white') # set plot title to white
      heatmap <- MEAheatmap(mea[["all_01_01"]])
      par(col.main='black') # set plot title back to black
      title(main=paste0(j)) # alternative title
      
      # peak picking
      # CAUTION: set NAN removal to true to ignore nans in the calculation of the maximum
      L <- apply(ccfvalues[,c(1:60)], 1, max, na.rm =T ) 
      R <- apply(ccfvalues[,c(62:121)], 1, max, na.rm =T )
      entrain_df <- as.data.frame(t(cbind(L,R))) 
      entrain_df <- cbind(rownames(entrain_df), data.frame(entrain_df, row.names=NULL))
      colnames(entrain_df)[1] <- "position"
      
      # set any infinite values to NA
      entrain_df <- do.call(data.frame,lapply(entrain_df, function(x) replace(x, is.infinite(x),NA)))
      
      # calculate summary statistics
      entrain_df$min <- apply(entrain_df[,grep(pattern="w",names(entrain_df))],1,min,na.rm=T)
      entrain_df$max <- apply(entrain_df[,grep(pattern="w",names(entrain_df))],1,max,na.rm=T)
      entrain_df$sd <- apply(entrain_df[,grep(pattern="w",names(entrain_df))],1,sd,na.rm=T)
      entrain_df$mean <- apply(entrain_df[,grep(pattern="w",names(entrain_df))],1,mean,na.rm=T)
      entrain_df$md <- apply(entrain_df[,grep(pattern="w",names(entrain_df))],1,median,na.rm=T)
      entrain_df$kurtosis <- apply(entrain_df[,grep(pattern="w",names(entrain_df))],1,kurtosis,na.rm=T)
      entrain_df$skew <- apply(entrain_df[,grep(pattern="w",names(entrain_df))],1,skewness,na.rm=T)
      
      # configure data frame 
      entrain_df$ID[entrain_df$position=="L"] <- paste(gsub("_mealplanning","",s1Name))
      entrain_df$ID[entrain_df$position=="R"] <- paste(gsub("_mealplanning","",s2Name))
      entrain_df$AU <- paste(j)
      entrain_df$task <- "mealplanning"
      entrain_mealplanning_all <- rbind(entrain_mealplanning_all,entrain_df)
      
      # show progress
      print(paste(j,"for",i, "done"))
    }
  }
  dev.off()
}


# 4. CHECK MISSING VALUES -------------------------------------------------

# nanchecking per AU
entrain_hobbies_all$perc_na <- rowSums(is.na(entrain_hobbies_all[,grep("w[123456789]",names(entrain_hobbies_all))]))/ncol(entrain_hobbies_all[,grep("w[123456789]",names(entrain_hobbies_all))])*100 
hobbies_missings <- psych::describeBy(entrain_hobbies_all$perc_na,group=entrain_hobbies_all$AU)

entrain_mealplanning_all$perc_na <- rowSums(is.na(entrain_mealplanning_all[,grep("w[123456789]",names(entrain_mealplanning_all))]))/ncol(entrain_mealplanning_all[,grep("w[123456789]",names(entrain_mealplanning_all))])*100 
mealplanning_missings <- psych::describeBy(entrain_mealplanning_all$perc_na,group=entrain_mealplanning_all$AU)

# remove every AU where max > 50% 
hobbies_exclusion <- c()
for (i in 1:length(hobbies_missings)){
  if (hobbies_missings[[i]][["max"]] >= 50){
    hobbies_exclusion <- c(hobbies_exclusion, names(hobbies_missings)[[i]])
  }
}

entrain_hobbies_all <- entrain_hobbies_all[!entrain_hobbies_all$AU %in% hobbies_exclusion,]

mealplanning_exclusion <- c()
for (i in 1:length(mealplanning_missings)){
  if (mealplanning_missings[[i]][["max"]] >= 50){
    mealplanning_exclusion <- c(mealplanning_exclusion, names(mealplanning_missings)[[i]])
  }
}

entrain_mealplanning_all <- entrain_mealplanning_all[!entrain_mealplanning_all$AU %in% mealplanning_exclusion,]

# merge together
entrain_all <- bind_rows(entrain_hobbies_all, entrain_mealplanning_all)


# 6. FACIAL EXPRESSIVENESS ------------------------------------------------
# expressiveness of face as operationalized in mean intensity of all AUs (excl. AUs 4,5,10,12,14,23) per task

# hobbies
AU_incl <- c("ID","task",unique(entrain_hobbies_all$AU)[grep("AU",unique(entrain_hobbies_all$AU))])
hobbies_intensity <- alldata[alldata$task=="hobbies",AU_incl]
hobbies_intensity$ID <- stringr::str_replace(hobbies_intensity$ID, "_hobbies", "") # remove task from ID

mean_intensity_h <- data.frame()
for (i in unique(hobbies_intensity$ID)){
    mean_intensity <- c(i,mean(as.matrix(hobbies_intensity[hobbies_intensity$ID==i,c(3:ncol(hobbies_intensity))]),na.rm =T ))
    mean_intensity_h <- rbind(mean_intensity_h,mean_intensity)
  }
colnames(mean_intensity_h)<-c("ID", "mean_intensity_h")

# mealplanning
AU_incl <- c("ID","task",unique(entrain_mealplanning_all$AU)[grep("AU",unique(entrain_mealplanning_all$AU))])
mealplanning_intensity <- alldata[alldata$task=="mealplanning",AU_incl]
mealplanning_intensity$ID <- stringr::str_replace(mealplanning_intensity$ID, "_mealplanning", "") # remove task from ID

mean_intensity_mp <- data.frame()
for (i in unique(mealplanning_intensity$ID)){
  mean_intensity <- c(i,mean(as.matrix(mealplanning_intensity[mealplanning_intensity$ID==i,c(3:ncol(mealplanning_intensity))]),na.rm =T ))
  mean_intensity_mp <- rbind(mean_intensity_mp,mean_intensity)
}
colnames(mean_intensity_mp)<-c("ID", "mean_intensity_mp")

mean_intensity_all <- merge(mean_intensity_mp,mean_intensity_h,by="ID",all=T)

# 7. EXPORT ---------------------------------------------------------------

# facial expression
write.csv(entrain_all, "FE_syncentrain.csv")

# intensity
write.csv(mean_intensity_all, "FE_intensity.csv")

# save workspace
save.image(file="ccfdata_NANsremoved_entrainment_incsec4.Rdata")