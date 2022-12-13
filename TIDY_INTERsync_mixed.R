# SCRIPT TO CALCULATE INTERPERSONAL SYNCHRONY FROM HEAD AND BODY MOTION ENERGY TXT OUTPUT FILES IN MIXED DYADS

# load libraries
install.packages('rMEA')
install.packages('writexl')
install.packages('readxl')
install.packages('data.table')
install.packages('dplyr')
install.packages('stringr')
install.packages('moments')

library('rMEA')
library('writexl')
library('readxl')
library('data.table')
library('dplyr')
library('stringr')
library('moments')

# clean workspace
rm(list=ls())

### ROI ASSIGNMENT: ROI 1 and 2 --> TD head and body; ROI 3 and 4 --> ASD head and body
### NAMING OF MEA .TXT OUTPUT FILES: id_group_task


# 1. READ IN DATA ---------------------------------------------------

# import id vector (format: ML_dyadnumber_diagnosis_seatingposition)
id_vector <- as.data.frame(read_xlsx(""))

# set path to MEA txt files
path <- ""

## hobbies task
# read in head ROI data of mixed dyads
mea_mixed_head_hobbies <- readMEA(path,
                                  sampRate = 30, #frame rate of videos
                                  skip = 300, #skips the first 10s to remove motion artefacts due to switching on wristband
                                  s1Col = c(1), s2Col = c(3), #set columns according to original MEA ROI assignment
                                  s1Name = "TD", s2Name = "ASD", #names columns according to original MEA ROI assignment
                                  header = FALSE,
                                  namefilt = "hobbies", 
                                  idOrder = c("x","id","x","group","session","x"), 
                                  idSep = "_",
                                  sep = "")

mea_mixed_head_hobbies <- setGroup(mea_mixed_head_hobbies, "head") #add to filename to distinguish between different ROIs


# read in body ROI data of mixed dyads
mea_mixed_body_hobbies <- readMEA(path,
                                  sampRate = 30, #frame rate of videos
                                  skip = 300, #skips the first 10s to remove motion artefacts due to switching on wristband
                                  s1Col = c(2), s2Col = c(4), #set columns according to original MEA ROI assignment
                                  s1Name = "TD", s2Name = "ASD", #names columns according to original MEA ROI assignment
                                  header = FALSE,
                                  namefilt = "hobbies", 
                                  idOrder = c("x","id","x","group","session","x"), 
                                  idSep = "_",
                                  sep = "")

mea_mixed_body_hobbies <- setGroup(mea_mixed_body_hobbies, "body") #add to filename to distinguish between different ROIs

## mealplanning task
# read in head ROI data of mixed dyads
mea_mixed_head_mealplanning <- readMEA(path,
                                       sampRate = 30, #frame rate of videos
                                       skip = 300, #skips the first 10s to remove motion artefacts due to switching on wristband
                                       s1Col = c(1), s2Col = c(3), #set columns according to original MEA ROI assignment
                                       s1Name = "TD", s2Name = "ASD", #names columns according to original MEA ROI assignment
                                       header = FALSE,
                                       namefilt = "mealplanning", 
                                       idOrder = c("x","id","x","group","session","x"), 
                                       idSep = "_",
                                       sep = "")

mea_mixed_head_mealplanning <- setGroup(mea_mixed_head_mealplanning, "head") #add to filename to distinguish between different ROIs


# read in ROI data of mixed dyads
mea_mixed_body_mealplanning <- readMEA(path,
                                       sampRate = 30, #frame rate of videos
                                       skip = 300, #skips the first 10s to remove motion artefacts due to switching on wristband
                                       s1Col = c(2), s2Col = c(4), #set columns according to original MEA ROI assignment
                                       s1Name = "TD", s2Name = "ASD", #names columns according to original MEA ROI assignment
                                       header = FALSE,
                                       namefilt = "mealplanning", 
                                       idOrder = c("x","id","x","group","session","x"), 
                                       idSep = "_",
                                       sep = "")

mea_mixed_body_mealplanning <- setGroup(mea_mixed_body_mealplanning, "body") #add to filename to distinguish between different ROIs

# combine lists according to ROI
head_mixed <- c(mea_mixed_head_hobbies, mea_mixed_head_mealplanning)
body_mixed <- c(mea_mixed_body_hobbies, mea_mixed_body_mealplanning)


# 2. VISUAL INSPECTION OF RAW DATA -------------------------------------------------------
# saves histograms of raw data according to ROI to wd

pdf(file="mixed_raw_histograms_head.pdf")  
for (i in 1:length(head_mixed)){
  plot(head_mixed[[i]], from=0, to=600, rescale = FALSE) 
}
dev.off()

pdf(file="mixed_raw_histograms_body.pdf") 
for (i in 1:length(body_mixed)){
  plot(body_mixed[[i]], from=0, to=600, rescale = FALSE)
}
dev.off()


# 3. SCALING -----------------------------------------------------------------
# scales motion energy time series by standard deviation

scaled_head_mixed <- MEAscale(head_mixed)
scaled_body_mixed <- MEAscale(body_mixed)


# 4. TIME SERIES SYNCHRONIZATION ---------------------------------------------
# calculate interpersonal movement synchrony of head and body ROI between interaction partners

# Run CCF analysis on scaled time series
ccf_head_mixed <- MEAccf(scaled_head_mixed,
                               lagSec= 5, 
                               winSec = 30, 
                               incSec=15, 
                               r2Z = TRUE, 
                               ABS = TRUE)

ccf_body_mixed <- MEAccf(scaled_body_mixed,
                                    lagSec= 5, 
                                    winSec = 30, 
                                    incSec=15, 
                                    r2Z = TRUE, 
                                    ABS = TRUE)

# save for later pseudosynchrony analysis
saveRDS(ccf_head_mixed, file="ccf_head_mixed.RData") 
saveRDS(ccf_body_mixed, file="ccf_body_mixed.RData")


# 5. VISUAL INSPECTION OF SYNCHRONY ANALYSIS -----------------------------------------------------
# saves heatmaps of ccf matrices to disk 

# headsynchrony
pdf(file="heatmaps_IPS_head_mixed.pdf") 
for (i in 1:length(ccf_head_mixed)){
  MEAheatmap(ccf_head_mixed[[i]], legendSteps = 20, rescale = T)
}
dev.off()

# bodysynchrony
pdf(file="heatmaps_IPS_body_mixed.pdf") 
for (i in 1:length(ccf_body_mixed)){
  MEAheatmap(ccf_body_mixed[[i]], legendSteps = 20, rescale = T)
}
dev.off()


# 6. PEAK PICKING --------------------------------------------------

# combine lists
mea_ccf_all <- c(getCCF(ccf_head_mixed, type = "fullMatrix"),
                 getCCF(ccf_body_mixed, type = "fullMatrix"))

# peak picking
for (i in 1:length(mea_ccf_all)){
  # append maximum of positive lag (s1 movement happening before s2 movement)
  mea_ccf_all[[i]]$ASD <- apply(mea_ccf_all[[i]][,152:301], 1, max, na.rm = T) 
  # append maximum of negative lag (s2 movement happening before s1 movement)
  mea_ccf_all[[i]]$TD <- apply(mea_ccf_all[[i]][,1:150], 1, max, na.rm = T) 
  # keep only relevant columns
  mea_ccf_all[[i]] <- mea_ccf_all[[i]][,c("TD","ASD")]
  # transpose all df in list
  mea_ccf_all[[i]] <- as.data.frame(t(mea_ccf_all[[i]]))
  # set rownames as first column
  setDT(mea_ccf_all[[i]], keep.rownames = TRUE)
  colnames(mea_ccf_all[[i]])[1] <- "dx"
}

# data wrangling: create one overall dataframe in the format ID-peaks
mea_ccf_all_df <- bind_rows(mea_ccf_all, .id="id")
mea_ccf_all_df$dyad <- paste0("ML_",str_split_fixed(mea_ccf_all_df$id, "_", n=3)[,2])
mea_ccf_all_df$task <- str_split_fixed(mea_ccf_all_df$id, "_", n=3)[,3]
mea_ccf_all_df$roi <- str_split_fixed(mea_ccf_all_df$id, "_", n=3)[,1]
 
# decompose id vector
id_vector$dx <- str_split_fixed(id_vector$ID, "_", n=4)[,3]
id_vector$dyad <- paste0(str_split_fixed(id_vector$ID, "_", n=3)[,1],"_",str_split_fixed(id_vector$ID, "_", n=3)[,2])

# merge id vector with dataset
mea_ccf_all_df <- merge(id_vector, mea_ccf_all_df, by=c("dyad", "dx"))


# 7. FEATURE EXTRACTION HEAD AND BODY SYNCHRONY ---------------------------

#convert inf to NA - important because at peak picking a full row of NAs is returned as infinite 
mea_ccf_all_df <- do.call(data.frame,lapply(mea_ccf_all_df, function(x) replace(x, is.infinite(x),NA)))

# calculate summary statistics of all window peaks
mea_ccf_all_df$min <- apply(mea_ccf_all_df[,grep(pattern="w",names(mea_ccf_all_df))],1,min,na.rm=T)
mea_ccf_all_df$max <- apply(mea_ccf_all_df[,grep(pattern="w",names(mea_ccf_all_df))],1,max,na.rm=T)
mea_ccf_all_df$sd <- apply(mea_ccf_all_df[,grep(pattern="w",names(mea_ccf_all_df))],1,sd,na.rm=T)
mea_ccf_all_df$mean <- apply(mea_ccf_all_df[,grep(pattern="w",names(mea_ccf_all_df))],1,mean,na.rm=T)
mea_ccf_all_df$md <- apply(mea_ccf_all_df[,grep(pattern="w",names(mea_ccf_all_df))],1,median,na.rm=T)
mea_ccf_all_df$kurtosis <- apply(mea_ccf_all_df[,grep(pattern="w",names(mea_ccf_all_df))],1,kurtosis,na.rm=T)
mea_ccf_all_df$skew <- apply(mea_ccf_all_df[,grep(pattern="w",names(mea_ccf_all_df))],1,skewness,na.rm=T)

# split into head and body sync
mea_ccf_head <- mea_ccf_all_df[mea_ccf_all_df$roi=="head",]
mea_ccf_body <- mea_ccf_all_df[mea_ccf_all_df$roi=="body",]

  
# 8. FEATURE EXTRACTION MOVEMENT QUANTITY ----------------------------------------------------

# extract movement variables from MEA summary
movementquantity_head <- summary(ccf_head_mixed)[,4:5]
movementquantity_body <- summary(ccf_body_mixed)[,4:5]

# set ID as column
movementquantity_head <- setDT(movementquantity_head, keep.rownames = TRUE)[]
movementquantity_body <- setDT(movementquantity_body, keep.rownames = TRUE)[]

# change column names
colnames(movementquantity_head) <- c("ID", "TD", "ASD")
colnames(movementquantity_body) <- c("ID", "TD", "ASD")

# combine in single dataframe
movementquantity_all <- rbind(movementquantity_head,movementquantity_body)

# add dyad, task and ROI column
dyad <- paste0("ML_",sapply(strsplit(as.character(movementquantity_all$ID), split="_"),"[",2)) #create dyad number column
task <- sapply(strsplit(as.character(movementquantity_all$ID), split = "_"),"[",3) #create task column
roi <- sapply(strsplit(as.character(movementquantity_all$ID), split = "_"),"[",1) #create roi column
movementquantity_all <- cbind(dyad, task, roi, movementquantity_all[,2:3]) #merge together

# melt dataframe
movementquantity_all <- melt(movementquantity_all, id.vars = c("dyad","task","roi"),
            measure.vars = c("TD","ASD"), variable.name = "dx", value.name = "movement")

# merge with ID vector
movementquantity_all <- merge(id_vector, movementquantity_all, by = c("dyad","dx"))


# 9. EXPORT DATA TO WD ----------------------------------------------------------

# movement sync
write.csv(mea_ccf_head, "mea_ccf_head_mixed.csv")
write.csv(mea_ccf_body, "mea_ccf_body_mixed.csv")

# movement quantity
write.csv(movementquantity_all, "movementquantity_mixed.csv")

# save workspace
save.image("workspace_INTERsync_mixed.RData")