# SCRIPT TO CALCULATE INTRAPERSONAL SYNCHRONY BETWEEN HEAD AND UPPER BODY MOVEMENT 

# load libraries
install.packages("rMEA")
install.packages("data.table") 
install.packages("tidyverse")
install.package("tools")
install.packages("moments")

library("rMEA")
library("data.table")
library("tidyverse")
library("tools")
library("moments")

options(datatable.fread.datatable=FALSE)

# clean workspace
rm(list=ls())


# 1. READ IN HEAD MOVEMENT DATA FROM OPENFACE -----------------------------

# set path to OpenFace CSV files
path <- ""

# read in head pose parameters
files <- list.files(path=path, recursive = T, pattern = "ML_.*\\.csv", full.names = T, include.dirs = F)
myfilelist <- lapply(files, fread, header = F, skip=300, select = c(1,3:5,294:296)) # read in 18,001 lines to calculate distances
header <- fread(files[1], header = F, nrows = 1, stringsAsFactors=F,  select = c(1,3:5,294:296))
names(myfilelist) <- file_path_sans_ext(basename(files)) 

# add header 
for (i in 1:length(myfilelist)){
  colnames(myfilelist[[i]]) <- unlist(header)
}

# 2. CLEAN AND FILTER DATA ------------------------------------------------

# check if there are non missing values and every dataframe has 18,001 lines
for (i in 1:length(myfilelist)){
  if (anyNA(myfilelist[[i]])==T){ 
    print('NaN')
  }
}

for (i in 1:length(myfilelist)){
  if (nrow(myfilelist[[i]])!=18001){ 
    print('incomplete')
  }
}

# filter data based on mean confidence and successfully tracked frames and exclude cases
outlier <- c()
for (i in 1:length(myfilelist)) {
  # logical: is mean confidence of tracked frames under 75%?
  if (mean(myfilelist[[i]]$confidence) < 0.75 
      # OR
      || 
      # logical: is number of successfully tracked frames under 90%?
      sum(myfilelist[[i]]$success == 1) / nrow(myfilelist[[i]]) < 0.9) { 
    print(paste0("face tracking not reliable enough for ", names(myfilelist[i])))
    # keep vector for documentation purposes
    outlier <- c(outlier, names(myfilelist[i])) 
  }
}

# exclude outliers and respective interaction partners
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


# 3. CREATE HEAD MOVEMENT VECTOR ------------------------------------------

# initialize function
vlength <- function(x) {
  length <- sqrt((x[1])^2+(x[2])^2+(x[3])^2)
  return(length)
}

# loop through list of dataframes 
for (i in 1:length(myfilelist_cleaned)){
  # calculate frame-to-frame differences
  myfilelist_cleaned[[i]]$diff_Tx <- c(NA,diff(myfilelist_cleaned[[i]]$pose_Tx)) 
  myfilelist_cleaned[[i]]$diff_Ty <- c(NA,diff(myfilelist_cleaned[[i]]$pose_Ty))
  myfilelist_cleaned[[i]]$diff_Tz <- c(NA,diff(myfilelist_cleaned[[i]]$pose_Tz))
  # delete first row of NA
  myfilelist_cleaned[[i]] <- myfilelist_cleaned[[i]][-1,]  
  # calculate vector length
  myfilelist_cleaned[[i]]$vec_length <- apply(myfilelist_cleaned[[i]][,c("diff_Tx","diff_Ty","diff_Tz")],1,vlength)
}

# data wrangling: ID-dyad-task-dx-head movement vector length 
alldata <- bind_rows(myfilelist_cleaned, .id = "ID")
dyad <- sapply(strsplit(as.character(alldata$ID), split="_"),"[",2) # create dyad number column
task <- sapply(strsplit(as.character(alldata$ID), split = "_"),"[",4) # create task column
dx <- sapply(strsplit(as.character(alldata$ID), split = "_"),"[",3) # create diagnosis column
alldata$ROI <- "OF"
alldata <- cbind(dyad, task, dx, alldata)


# 4. READ IN BODY MOVEMENT DATA FROM MOTION ENERGY ANALYSIS ---------------
# CAVE: ROI assignment must be consistent (Control: ROI 2, Patient: ROI 4 OR Control L: ROI 2, Control R: ROI 4)

# mixed dyads
path_mea_mixed <- ""

files_mea_mixed <- list.files(path=path_mea_mixed, recursive = F, pattern = "ML_.*\\.txt", full.names = T, include.dirs = F) 
myfilelist_mea_pat <- lapply(files_mea_mixed, fread, header = F, skip=300, nrow = 18000, select = 4, col.names="mea_body") # patient
myfilelist_mea_td <- lapply(files_mea_mixed, fread, header = F, skip = 300, nrow = 18000, select = 2, col.names="mea_body") # control

# control dyads 
path_mea_td <- ""

files_mea_td <- list.files(path=path_mea_td, recursive = F, pattern = "ML_.*\\.txt", full.names = T, include.dirs = F) 
myfilelist_mea_td_L <- lapply(files_mea_td, fread, header = F, skip=300, nrow = 18000,select = 2, col.names="mea_body") 
myfilelist_mea_td_R <- lapply(files_mea_td, fread, header = F, skip =300, nrow = 18000,select = 4, col.names="mea_body")

# merge all lists 
list_all <- c(myfilelist_mea_pat,myfilelist_mea_td,myfilelist_mea_td_L,myfilelist_mea_td_R)

# convert to dataframe                                                        
alldata_mea <- bind_rows(list_all, .id = "ID")

# data wrangling: ID-dyad-task-dx-head movement vector length                                                      
dyad <- sapply(strsplit(as.character(alldata_mea$ID), split="_"),"[",2) # create dyad number column
task <- sapply(strsplit(as.character(alldata_mea$ID), split = "_"),"[",4) # create task column
dx <- sapply(strsplit(as.character(alldata_mea$ID), split = "_"),"[",3) # create diagnosis column
alldata_mea$ROI <- "mea_body"
alldata_mea <- cbind(dyad, task, dx, alldata_mea)
alldata_mea <- alldata_mea[,c(4,1,2,3,6,5)] 
                                                        

# 5. SYNCHRONIZATION OF OF AND MEA TIME SERIES ----------------------------
# Steps: 
# 1) create fake MEA object
# 2) calculate ccf according to rMEA

# merge OpenFace and MEA data to one dataframe
names(alldata_mea)[names(alldata_mea) == "mea_body"] <- "value" 
names(alldata)[names(alldata) == "vec_length"] <- "value" 

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
# prepare dyads to loop over
ids <- as.data.frame(unique(alldata$ID)) 
names(ids) <- "ID"
rownames(ids) <- ids$ID # rownames = individual IDs

# loop over dyads
peaks_all <- data.frame()
for (i in ids$ID){ 
  cur_df_openface = alldata[alldata$ID == i,] 
  cur_df_mea = alldata_mea[alldata_mea$ID == i,] 
  # initialize heatmaps
  pdf(paste0(i,".pdf")) 

  # check if data frame is present
  if (nrow(cur_df_openface) && nrow(cur_df_mea) > 0) { 
      
      # prepare fakeMEA components
      s1 = cur_df_openface[cur_df_openface$ROI == "OF", "value"] 
      s2 = cur_df_mea[cur_df_mea$ROI == "mea_body", "value"] 
      sampRate = 30
      s1Name = as.character(paste0("fe_",cur_df_openface[cur_df_openface$ROI == "OF","ID"][1])) 
      s2Name = as.character(paste0("mea_",cur_df_mea[cur_df_mea$ROI == "mea_body","ID"][1]))
      # create fakeMEA object
      mea = fakeMEA(s1,s2,sampRate,s1Name,s2Name) 
      
      # time-lagged windowed cross-correlation
      mea = MEAccf(mea,lagSec = 5, winSec = 30, incSec = 15, r2Z=T, ABS=T) 
      # extracts matrix with all ccf values over all lags and windows 
      ccfvalues <- mea[["all_01_01"]][["ccf"]] 
      
      # configure heatmap
      par(col.main='white') # set plot title to white
      heatmap <- MEAheatmap(mea[["all_01_01"]])
      par(col.main='black') # set plot title back to black
      title(main=paste0(i)) # alternative title
      dev.off()
      
      # peak picking
      # CAUTION: set NAN removal to true to ignore nans in the calculation of the maximum
      peaks <- apply(ccfvalues, 1, max, na.rm = T)
      
      # set any infinite values to NA
      peaks <- do.call(data.frame,lapply(peaks, function(x) replace(x, is.infinite(x),NA)))
      
      # calculate summary statistics
      peaks$min <- apply(peaks[,grep(pattern="w",names(peaks))],1,min,na.rm=T)
      peaks$max <- apply(peaks[,grep(pattern="w",names(peaks))],1,max,na.rm=T)
      peaks$sd <- apply(peaks[,grep(pattern="w",names(peaks))],1,sd,na.rm=T)
      peaks$mean <- apply(peaks[,grep(pattern="w",names(peaks))],1,mean,na.rm=T)
      peaks$md <- apply(peaks[,grep(pattern="w",names(peaks))],1,median,na.rm=T)
      peaks$kurtosis <- apply(peaks[,grep(pattern="w",names(peaks))],1,kurtosis,na.rm=T)
      peaks$skew <- apply(peaks[,grep(pattern="w",names(peaks))],1,skewness,na.rm=T)
      
      # configure data frame
      peaks$ID <- paste(gsub("_hobbies||_mealplanning||fe_","",s1Name))
      peaks$task <- unique(cur_df_mea$task)
      peaks_all <- rbind(peaks_all,peaks) 

      # show progress
      print(paste(i, "done")) 
    }
  }


# 6. EXPORT ---------------------------------------------------------------

# save peak file
write.csv(peaks_all,"peaks_intra.csv")

# save workspace
save.image("intra.RData")