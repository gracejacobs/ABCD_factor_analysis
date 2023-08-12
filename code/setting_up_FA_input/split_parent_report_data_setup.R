## setting up data for parent report model


library(ggplot2)
library(plyr)
library(dplyr)
library(knitr)
library(tidyr)
library(psych)
library(polycor)
library(splitstackshape)


############################################################################################################
############################################################################################################
################## Load family, demo, and site info
# family ID
relationship <- read.table("../data/Demographics/acspsw03.txt", sep="\t", header=TRUE)
relationship <- relationship[which(relationship$eventname == "baseline_year_1_arm_1"), ] #11875
relationship <- relationship[ , c("subjectkey", "rel_family_id", "sex", "interview_age")]
relationship <- na.omit(relationship)

## site data 
site <- read.table("../data/Demographics/abcd_lt01.txt", sep="\t", header=TRUE)
site <- site[which(site$eventname == "baseline_year_1_arm_1"), ]
site <- site[ , c("subjectkey", "site_id_l")]
site <- na.omit(site)

############################################################################################################
############################################################################################################
################## Need to determine who has data and then split that subsample

#Child behaviour checklist (CBCL), parent report - 113 questions, 8 syndrome scales
# 3 point scale
cbcl <- read.table("../data/Clinical/abcd_cbcl01.txt", sep="\t", header=TRUE)
cbcl <- cbcl[which(cbcl$eventname == "baseline_year_1_arm_1"), ]
cbcl[, 10:128] <- apply(cbcl[, 10:128], 2, function(x) as.numeric(x))
cbcl <- merge(cbcl, relationship, by="subjectkey")
#cbcl <- na.omit(cbcl) #removes 8

cbcl_endorse <- cbcl %>% select(subjectkey, starts_with("cbcl_q"))
cbcl_endorse <- na.omit(cbcl_endorse)

#Mania questions, parent report - 10 questions
#
mania <- read.table("../data/Clinical/abcd_pgbi01.txt", sep="\t", header=TRUE)
mania <- mania[which(mania$eventname == "baseline_year_1_arm_1"), ]
mania[, 11:20] <- apply(mania[, 11:20], 2, function(x) as.numeric(x))
mania <- merge(mania, relationship, by="subjectkey")

mania_variance <- mania %>% select(subjectkey, starts_with("gen_child_behav")) 
mania_variance <- na.omit(mania_variance)

# Social Responsiveness Scale
social <- read.table("../data/Clinical/abcd_pssrs01.txt", sep="\t", header=TRUE)
social <- social[which(social$eventname == "1_year_follow_up_y_arm_1"), ]
social[, 11:21] <- apply(social[, 11:21], 2, function(x) as.numeric(x))
social <- merge(social, relationship, by="subjectkey")

social_variance <- social %>% select(subjectkey, starts_with("ssrs_")) 
social_variance <- na.omit(social_variance)

participants <- merge(cbcl_endorse, mania_variance, by="subjectkey")
participants <- merge(participants, social_variance, by="subjectkey")

participants <- participants %>% select(subjectkey) 


##########################################################################################
##########################################################################################

## SPLIT sample - randomly within site
all <- merge(relationship, site, by="subjectkey")
all <- merge(all, participants, by="subjectkey")
all$age <- ifelse(all$interview_age > 120, 10, 9)

set.seed(1234)
out <- stratified(all, c("site_id_l", "sex", "age"), 0.5) #takes 50% based on site and sex
all$Sample <- ifelse(all$subjectkey %in% out$subjectkey, 1, 2)
table(all$Sample, all$sex)
table(all$Sample, all$site_id_l)
table(all$Sample, all$age)

all <- all %>% select(subjectkey, Sample) 
write.csv(all, file="../output/Parentreport_split_sample.csv")

##########################################################################################
##########################################################################################
##########################################################################################
## checking polychortic correlations and endorsment levels
cbcl <- merge(all, cbcl)
cbcl_sample <- cbcl %>% filter(Sample == "1")

cbcl_endorse <- cbcl_sample %>% select(starts_with("cbcl_q"))
cbcl_endorse <- na.omit(cbcl_endorse)
cbcl_endorse <- cbcl_endorse %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

cbcl_endorse$Percent <- (cbcl_endorse$Count/5590)*100
# determining which items have too small variance - matches Michelini or Moore et al
low_0 <- cbcl_endorse[which(cbcl_endorse$Percent > 99.49), ]
#low_0 <- cbcl_endorse[which(cbcl_endorse$Percent > 99.06), ]

# calculating the polychoric r, is for ordinal factors fo 
cbcl_items <- cbcl %>% select(starts_with("cbcl_q"))

t <- polychoric(cbcl_items) # psych package, get 22
corr_matrix <- as.data.frame(t[["rho"]])
check <- corr_matrix %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

check <- check[which(check$Value > 0.85 & check$Value != 1), ]
combined <- unique(check$Measure)

cbcl_included <- cbcl %>% filter(Sample == "1") %>% select(subjectkey, starts_with("cbcl_q"))
cbcl_included[, 2:120] <- apply(cbcl_included[, 2:120], 2, function(x) as.numeric(x))
### creating composites
cbcl_included$distracted <- round((cbcl_included$cbcl_q08_p + cbcl_included$cbcl_q78_p)/2)
cbcl_included$destroys <- round((cbcl_included$cbcl_q21_p + cbcl_included$cbcl_q20_p)/2)

# also remove ones that were too correlated in the second half of the sample
cbcl_included <- subset(cbcl_included, select=-c(cbcl_q02_p, cbcl_q101_p, cbcl_q105_p, cbcl_q73_p,cbcl_q99_p, cbcl_q67_p, cbcl_q72_p))
#cbcl_included <- subset(cbcl_included, select=-c(cbcl_q110_p, cbcl_q18_p, cbcl_q59_p, cbcl_q67_p, cbcl_q72_p, cbcl_q96_p))
cbcl_included <- cbcl_included[which(!(colnames(cbcl_included) %in% combined))]

# setting up the second half
names_cbcl <- names(cbcl_included)

cbcl_secondhalf <- cbcl %>% filter(Sample == "2")
cbcl_secondhalf[, 2:120] <- apply(cbcl_secondhalf[, 2:120], 2, function(x) as.numeric(x))

cbcl_endorse <- cbcl_secondhalf %>% select(starts_with("cbcl_q"))
cbcl_endorse <- na.omit(cbcl_endorse)
cbcl_endorse <- cbcl_endorse %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

cbcl_endorse$Percent <- (cbcl_endorse$Count/5595)*100
# determining which items have too small variance - matches Michelini or Moore et al
low_0 <- cbcl_endorse[which(cbcl_endorse$Percent > 99.49), ]

# calculating the polychoric r, is for ordinal factors fo 
cbcl_items <- cbcl_secondhalf %>% select(starts_with("cbcl_q"))

t <- polychoric(cbcl_items) # psych package, get 22
corr_matrix <- as.data.frame(t[["rho"]])
check <- corr_matrix %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value > 0.85 & check$Value != 1), ]

cbcl_secondhalf$distracted <- round((cbcl_secondhalf$cbcl_q08_p + cbcl_secondhalf$cbcl_q78_p)/2)
cbcl_secondhalf$destroys <- round((cbcl_secondhalf$cbcl_q21_p + cbcl_secondhalf$cbcl_q20_p)/2)

cbcl_secondhalf <- cbcl_secondhalf[which((colnames(cbcl_secondhalf) %in% names_cbcl))]


################################################################################################
mania <- merge(all, mania)
mania_sample <- mania %>% filter(Sample == "1")

mania_variance <- mania_sample %>% select(starts_with("gen_child_behav")) 
mania_variance <- na.omit(mania_variance)
mania_variance <- mania_variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

mania_variance$Percent <- (mania_variance$Count/5590)*100

# no low variance items
low_0 <- mania_variance[which(mania_variance$Percent > 99.49), ]

# polychoric
man_items <- mania %>% select(starts_with("gen_child_behav"))
r <- polychoric(man_items) # psych package

corr_matrix <- as.data.frame(r[["rho"]])
check <- corr_matrix %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

# don't need to combine any unless I drop it to 0.75 - this way I'm copying Moore's cut off
# instead of Michelini's
check <- check[which(check$Value > 0.85 & check$Value != 1), ]
combined <- unique(check$Measure)

man_included <- mania %>% filter(Sample == "1") %>% select(subjectkey, starts_with("gen_child_behav"))

# setting up the second half
names_mania <- names(man_included)
man_secondhalf <- mania %>% filter(Sample == "2")

mania_variance <- man_secondhalf %>% select(starts_with("gen_child_behav")) 
mania_variance <- na.omit(mania_variance)
mania_variance <- mania_variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

mania_variance$Percent <- (mania_variance$Count/5595)*100
# no low variance items
low_0 <- mania_variance[which(mania_variance$Percent > 99.49), ]

# polychoric
man_items <- man_secondhalf %>% select(starts_with("gen_child_behav"))
r <- polychoric(man_items) # psych package

corr_matrix <- as.data.frame(r[["rho"]])
check <- corr_matrix %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value > 0.85 & check$Value != 1), ]


man_secondhalf <- man_secondhalf[which((colnames(man_secondhalf) %in% names_mania))]

################################################################################################
social <- merge(all, social)
social_sample <- social %>% filter(Sample == "1")

social_variance <- social_sample %>% select(starts_with("ssrs_") & ends_with("_p")) 
social_variance <- na.omit(social_variance)
social_variance <- social_variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

social_variance$Percent <- (social_variance$Count/5590)*100

# no low variance items
low_0 <- social_variance[which(social_variance$Percent > 99.49), ]

# polychoric
social_items <- social %>% select(starts_with("ssrs_") & ends_with("_p"))

r <- polychoric(social_items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- corr_matrix %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

# don't need to combine any unless I drop it to 0.80
check <- check[which(check$Value > 0.85 & check$Value != 1), ]
combined <- unique(check$Measure)

social_included <- social %>% filter(Sample == "1") %>% select(subjectkey, starts_with("ssrs_") & ends_with("_p"))

# setting up the second half
names_social <- names(social_included)
social_secondhalf <-  social %>% filter(Sample == "2")

social_variance <- social_secondhalf %>% select(starts_with("ssrs_") & ends_with("_p")) 
social_variance <- na.omit(social_variance)
social_variance <- social_variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

social_variance$Percent <- (social_variance$Count/5595)*100
# no low variance items
low_0 <- social_variance[which(social_variance$Percent > 99.49), ]

# polychoric
social_items <- social_secondhalf %>% select(starts_with("ssrs_") & ends_with("_p"))

r <- polychoric(social_items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- corr_matrix %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

# don't need to combine any unless I drop it to 0.80
check <- check[which(check$Value > 0.85 & check$Value != 1), ]
combined <- unique(check$Measure)

social_secondhalf <- social_secondhalf[which((colnames(social_secondhalf) %in% names_social))]

################################################################################################

cbcl <- NULL
check <- NULL
social <- NULL
low_0 <- NULL
combined <- NULL
mania <- NULL
corr_matrix <- NULL
relationship <- NULL
social_items <- NULL
social_variance <- NULL
t <- NULL
cbcl_endorse <- NULL
cbcl_items <- NULL
man_items <- NULL
mania_variance <- NULL
r <- NULL
all <- NULL
out <- NULL
site <- NULL
participants <- NULL
cbcl_sample <- NULL
mania_sample <- NULL
social_sample <- NULL
names_cbcl <- NULL
