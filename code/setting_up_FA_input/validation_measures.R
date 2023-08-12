
### Setting up imaging, cognition, suicidal info for regression B-ESEM models
library(tidyverse)
library(broom)


# cognitive data
# Measures included in NIH TB Summary Scores: NIH TBX Picture Vocabulary; NIH Tbx Flanker Inhibitory Control and Attention; NIH Tbx List Sorting Working Memory; NIH Tbx Dimensional Change Card Sort; NIH Tbx Pattern Comparison Processing Speed;NIH Tbx Picture Sequence Memory; NIH Tbx Oral Reading Recognition
NIH <- read.table("../data/Cognition/abcd_tbss01.txt", sep="\t", header=TRUE)
NIH <- NIH[which(NIH$eventname == "baseline_year_1_arm_1"), ]
NIH <- NIH[, c("subjectkey", "nihtbx_cryst_fc", "nihtbx_fluidcomp_fc")]
NIH$nihtbx_cryst_fc <- as.numeric(NIH$nihtbx_cryst_fc)

# imaging data
source("setting_up_data/imaging_data.R") #8772
t1_dk <- t1_dk[ , c("subjectkey", "smri_vol_scs_subcorticalgv", "smri_area_cdk_total")]

ct_dk <- read.table("../data/Imaging_Quality_Control/abcd_smrip101.txt", sep="\t", header=TRUE)
ct_dk <- ct_dk[which(ct_dk$eventname == "baseline_year_1_arm_1"), ]
ct_dk <- ct_dk %>% select(subjectkey, smri_vol_cdk_total)

t1_dk <- merge(t1_dk, ct_dk, by="subjectkey")

# smri_area_cdk_total - total whole brain cortical area
# smri_vol_cdk_total - total brain cortical volume
# smri_vol_scs_wholeb - volume of whole brain
# smri_vol_scs_subcorticalgv - subcortical gray matter volume
# smri_vol_scs_crbcortexlh - left cerebellum cortex volume
# smri_vol_scs_crbwmatterlh - left cerebellum white matter

# medical service utilization
medhist <- read.table("../data/Clinical/abcd_mx01.txt", sep="\t", header=TRUE)
medhist <- medhist[which(medhist$eventname == "baseline_year_1_arm_1"), ]

medhist$medhx_1a <- as.numeric(medhist$medhx_1a)
medhist$medhx_1b <- as.numeric(medhist$medhx_1b)
medhist$medhx_1a <- ifelse(medhist$medhx_1a == "6", NA, medhist$medhx_1a)
medhist$medhx_1b <- ifelse(medhist$medhx_1b == "6", NA, medhist$medhx_1b)

medhist$med_service_usage <- rowSums(medhist[ , c("medhx_1a", "medhx_1b")], na.rm=TRUE)

table(medhist$med_service_usage, exclude=NULL)

medhist <- medhist[ , c("subjectkey", "med_service_usage")]


validation <- merge(NIH, t1_dk, by="subjectkey", all=TRUE)
validation <- merge(validation, medhist, by="subjectkey", all=TRUE)

ct_dk <- NULL
vol_dk <- NULL
NIH <- NULL
grades <- NULL
medhist <- NULL
resting_state <- NULL
t1_dk <- NULL



