###Organizing Imaging ABCD data and qcing it based off of recommendations from ABCD including Damien Fair's presentation
#https://abcd-repronim.github.io/materials/week-4/ 

##### Loading all necessary data

# MRI Raw Quality Control
mriqc_1 <- read.table("../data/Imaging_Quality_Control/mriqcrp102.txt", sep="\t", header=TRUE)
mriqc_1 <- mriqc_1[which(mriqc_1$eventname == "baseline_year_1_arm_1"), ]
mriqc_1 <- mriqc_1[ , c("subjectkey", "iqc_t1_ok_ser", "iqc_rsfmri_ok_ser")] 

## Visual qc of freesurfer - consensus from two trained raters
freesurfer <- read.table("../data/Imaging_Quality_Control/freesqc01.txt", sep="\t", header=TRUE)
freesurfer <- freesurfer[which(freesurfer$eventname == "baseline_year_1_arm_1"), ]
freesurfer <- freesurfer[ , c("subjectkey", "fsqc_qc")]

## Auto postqc
auto_postpro_qc <- read.table("../data/Imaging_Quality_Control/abcd_auto_postqc01.txt", sep="\t", header=TRUE)
auto_postpro_qc <- auto_postpro_qc[which(auto_postpro_qc$eventname == "baseline_year_1_arm_1"), ]
auto_postpro_qc <- auto_postpro_qc[ , c("subjectkey", "apqc_fmri_bounwarp_flag", "apqc_fmri_regt1_rigid", "apqc_fmri_fov_cutoff_dorsal", "apqc_fmri_fov_cutoff_ventral")]
auto_postpro_qc[, 2:5] <- apply(auto_postpro_qc[, 2:5], 2, function(x) as.numeric(x))


## Manual post-processing QC 
manual_qc <- read.table("../data/Imaging_Quality_Control/fmriqc01.txt", sep="\t", header=TRUE)
manual_qc <- manual_qc[which(manual_qc$eventname == "baseline_year_1_arm_1"), ]
manual_qc <- manual_qc[ ,c("subjectkey", "fmri_postqc_qc")]

## Imaging inclusion summary - won't seem to load for some strange reason
inclusion_summary <- read.table("../data/Imaging_Quality_Control/abcd_imgincl01.txt", sep="\t", header=TRUE)
inclusion_summary <- inclusion_summary[which(inclusion_summary$eventname == "baseline_year_1_arm_1"), ]
inclusion_summary <- inclusion_summary[ ,c("subjectkey", "imgincl_rsfmri_include")]
inclusion_summary <- inclusion_summary[which(inclusion_summary$imgincl_rsfmri_include == "1"), ]

# Gordon Network network correlations
# QCing based on Hagler et al, NeuroImage, 2019
rs_net <- read.table("../data/Imaging_Quality_Control/abcd_betnet02.txt", sep="\t", header=TRUE)
rs_net <- rs_net[which(rs_net$eventname == "baseline_year_1_arm_1"), ]

# Gordon Network to subcortical ROI correlations
# QCing based on Hagler et al, NeuroImage, 2019
rs_sub <- read.table("../data/Imaging_Quality_Control/mrirscor02.txt", sep="\t", header=TRUE)
rs_sub <- rs_sub[which(rs_sub$eventname == "baseline_year_1_arm_1"), ]
rs_sub <- rs_sub[, c(4, 23:269)]

# # cortical thickness & surface area
ct_dk <- read.table("../data/Imaging_Quality_Control/abcd_smrip101.txt", sep="\t", header=TRUE)
ct_dk <- ct_dk[which(ct_dk$eventname == "baseline_year_1_arm_1"), ]
ct_dk <- ct_dk %>% select(starts_with("smri_thick_cdk"), starts_with("smri_area_cdk"), subjectkey)

# subcortical volume
vol_dk <- read.table("../data/Imaging_Quality_Control/abcd_smrip201.txt", sep="\t", header=TRUE)
vol_dk <- vol_dk[which(vol_dk$eventname == "baseline_year_1_arm_1"), ]
vol_dk <- vol_dk %>% select(starts_with("smri_vol_scs"), subjectkey)

###############################################
# Merging quality control data for structure
mriqc_1$iqc_t1_ok_ser <- as.numeric(mriqc_1$iqc_t1_ok_ser) # Number of series that are complete and passed QC
mriqc_1$iqc_rsfmri_ok_ser <- as.numeric(mriqc_1$iqc_rsfmri_ok_ser) # Number of series that are complete and passed QC
freesurfer <- merge(freesurfer, mriqc_1, by="subjectkey", all.x = TRUE)

# structural T1 weighted measures
t1_dk <- merge(ct_dk, vol_dk, by="subjectkey")
t1_dk <- merge(t1_dk, freesurfer, by="subjectkey") # 11533
t1_dk <- t1_dk[which(t1_dk$fsqc_qc != "0"), ] # passed freesurfer quality control 11003
t1_dk <- t1_dk[which(t1_dk$iqc_t1_ok_ser > 0), ] # 11003

# Quality control for resting-state fmri
rs_net <- merge(rs_net, freesurfer, by="subjectkey", all.x = TRUE) # 
rs_net <- merge(rs_net, auto_postpro_qc, by="subjectkey", all.x = TRUE) #
rs_net <- merge(rs_net, manual_qc, by="subjectkey", all.x = TRUE) # 11309

rs_net <- rs_net[which(rs_net$iqc_t1_ok_ser > 0), ] #11278
rs_net <- rs_net[which(rs_net$iqc_rsfmri_ok_ser > 0), ] #11278
rs_net <- rs_net[which(rs_net$fsqc_qc != "0"), ] #10855
rs_net <- rs_net[which(rs_net$fmri_postqc_qc != "0"), ] #10771
# Number of frames after excluding outlier frames (based on standard deviation across ROIs)
rs_net$rsfmri_c_ngd_ntpoints <- as.numeric(rs_net$rsfmri_c_ngd_ntpoints) 
rs_net <- rs_net[which(rs_net$rsfmri_c_ngd_ntpoints > 375), ] #9398
rs_net <- rs_net[which(rs_net$apqc_fmri_bounwarp_flag == "1"), ] #9393
rs_net <- rs_net[which(rs_net$apqc_fmri_regt1_rigid < 19), ] #9392
rs_net <- rs_net[which(rs_net$apqc_fmri_fov_cutoff_dorsal < 65), ] #9392
rs_net <- rs_net[which(rs_net$apqc_fmri_fov_cutoff_ventral < 60), ] #9387

# Number of frames after excluding outlier frames (based on standard deviation across ROIs)
#Time points with FD greater than 0.2 mm are excluded from thevariance and correlation calculations. \
#Note that this is a slightly moreconservative threshold than that used for the regression step. 
#Time pe-riods with fewer thanfive contiguous, sub-threshold time points are alsoexcluded. 
#The effects of head motion can potentially linger for severalseconds after an abrupt head motion, for example
#due to spin-history orT1relaxation effects (Friston et al., 1996), so an additional round ofcensoring is applied 
#based on detecting time points that are outliers withrespect to spatial variation across the brain. 
#SD across ROIs is calculatedfor each time point, and outlier time points, defined as having an SDvalue more than 
#three times the median absolute deviation (MAD) belowor above the median SD value, are excluded from variance and correla-tion calculations.

rs_net <- rs_net[ , c(1, 23:191)]
resting_state <- merge(rs_net, rs_sub, by="subjectkey")

freesurfer <- NULL
relationship <- NULL
mriqc_1 <- NULL
rs_net <- NULL
rs_sub <- NULL
#vol_dk <- NULL
#ct_dk <- NULL
auto_postpro_qc <- NULL
manual_qc <- NULL
inclusion_summary <- NULL

