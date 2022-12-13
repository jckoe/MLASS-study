# SCRIPT TO VALIDATE SYNCHRONY VALUES AS EXTRACTED FROM CCF ANALYSIS COMPARED TO RANDOM SCORES

# load libraries
install.packages('rMEA')
install.packages('data.table')
install.packages('psych')

library('rMEA')
library('data.table')
library('psych')

# clean workspace
rm(list=ls())



# 1. READ IN DATA ---------------------------------------------------------

# mixed dyads
ccf_head_mixed <- readRDS("ccf_head_mixed.RData")
ccf_body_mixed <- readRDS("ccf_body_mixed.RData")

# TD dyads
ccf_head_TD <- readRDS("ccf_head_TD.RData")
ccf_body_TD <- readRDS("ccf_body_TD.RData")


## combine datasets (cannot be done right away because of different s1 and s2 names)
new_s1 <-  "x"
new_s2 <-  "y"

# head
for (i in 1:length(ccf_head_mixed)){ 
  attr(ccf_head_mixed[[i]],"s1Name") <- new_s1
  attr(ccf_head_mixed[[i]],"s2Name") <- new_s2
}

for (i in 1:length(ccf_head_TD)){ 
  attr(ccf_head_TD[[i]],"s1Name") <- new_s1
  attr(ccf_head_TD[[i]],"s2Name") <- new_s2
}

# body
for (i in 1:length(ccf_body_mixed)){ 
  attr(ccf_body_mixed[[i]],"s1Name") <- new_s1
  attr(ccf_body_mixed[[i]],"s2Name") <- new_s2
}

for (i in 1:length(ccf_body_TD)){ 
  attr(ccf_body_TD[[i]],"s1Name") <- new_s1
  attr(ccf_body_TD[[i]],"s2Name") <- new_s2
}

# merge by ROI
headsynchrony <- c(ccf_head_mixed, ccf_head_TD) 
bodysynchrony <- c(ccf_body_mixed, ccf_body_TD)



# 2. PSEUDOSYNC CALCULATION --------------------------------------------------

# shuffle datasets, create 500 random pairings per ROI
random_head <- shuffle(headsynchrony, size = 500)
random_body <- shuffle(bodysynchrony, size = 500)

# calculate ccfs
pseudo_head <- MEAccf(random_head, 
                          lagSec= 5, 
                          winSec = 30, 
                          incSec=15, 
                          r2Z = T, 
                          ABS = T)

pseudo_body <- MEAccf(random_body, 
                          lagSec= 5, 
                          winSec = 30, 
                          incSec=15, 
                          r2Z = T, 
                          ABS = T)



# 3. VISUAL INSPECTION AND DESCRIPTIVES -----------------------------------

# boxplots
boxplot(getCCF(headsynchrony, type="grandAver"), getCCF(pseudo_head, type="grandAver"))
boxplot(getCCF(bodysynchrony, type="grandAver"), getCCF(pseudo_body, type="grandAver"))

# distribution of the ccf calculations against random matched dyads
MEAdistplot(headsynchrony, contrast = pseudo_head)
MEAlagplot(headsynchrony, contrast = pseudo_head)

MEAdistplot(bodysynchrony, contrast = pseudo_body)
MEAlagplot(bodysynchrony, contrast=pseudo_body)

# compute descriptive statistics
describe(getCCF(headsynchrony, type="grandAver"))
describe(getCCF(pseudo_head, type="grandAver"))

describe(getCCF(bodysynchrony, type="grandAver"))
describe(getCCF(pseudo_body, type="grandAver"))



# 4. COMPARISON SYNCHRONY VS. PSEUDOSYNCHRONY -------------------------------

# check for normal distribution
shapiro.test(getCCF(headsynchrony, type="grandAver")) 
shapiro.test(getCCF(pseudo_head, type="grandAver"))
shapiro.test(getCCF(bodysynchrony, type="grandAver")) 
shapiro.test(getCCF(pseudo_body, type="grandAver"))

# check for unequal variances
var.test(getCCF(headsynchrony, type="grandAver"), getCCF(pseudo_head, type="grandAver")) 
var.test(getCCF(bodysynchrony, type="grandAver"), getCCF(pseudo_body, type="grandAver"))  

# compute t-test
t.test(getCCF(headsynchrony, type="grandAver"), getCCF(pseudo_head, type="grandAver"), var.equal = F)
t.test(getCCF(bodysynchrony, type="grandAver"), getCCF(pseudo_body, type="grandAver"), var.equal = T)

# effect size
cohens_d(getCCF(headsynchrony, type="grandAver"), getCCF(pseudo_head, type="grandAver"))
cohens_d(getCCF(bodysynchrony, type="grandAver"), getCCF(pseudo_body, type="grandAver"))



# 5. SAVE WORKSPACE -------------------------------------------------------
# for documentation of random values

save.image("workspace_pseudosync.RData")

