
#### Data set up for child report factor analysis
library(plyr)
library(dplyr)
library(knitr)
library(tidyr)
library(psych)
library(polycor)
library(splitstackshape)

############################################################################################################
############################################################################################################
################## Loading family, demo and site info
# family ID
relationship <- read.table("../data/Demographics/acspsw03.txt", sep="\t", header=TRUE)
relationship <- relationship[which(relationship$eventname == "baseline_year_1_arm_1"), ] #11875
relationship <- relationship[ , c("subjectkey", "rel_family_id", "sex", "interview_age")]
relationship <- na.omit(relationship)

## site ../../data 
site <- read.table("../data/Demographics/abcd_lt01.txt", sep="\t", header=TRUE)
site <- site[which(site$eventname == "baseline_year_1_arm_1"), ]
site <- site[ , c("subjectkey", "site_id_l")]
site <- na.omit(site)

## Need to determine who has data and then split that subsample
# 1) Prodromal Questionnaire Brief, child report - 21 questions
psychosis <- read.table("../data/Clinical/pps01.txt", sep="\t", header=TRUE)
psychosis <- psychosis[which(psychosis$visit == "baseline_year_1_arm_1"), ]
psychosis[, 10:72] <- apply(psychosis[, 10:72], 2, function(x) as.numeric(x))

psychosis_variance <- psychosis %>% select(subjectkey, starts_with("prodromal_") & !ends_with("b_y")) 
psychosis_variance <- na.omit(psychosis_variance) #11864


# 2) Modified UPPS-P for Children from PhenX, child report - 20 questions, 4 factors of impulsivity
upps <- read.table("../data/Clinical/abcd_upps01.txt", sep="\t", header=TRUE)
upps <- upps[which(upps$eventname == "baseline_year_1_arm_1"), ]
upps[, 10:29] <- apply(upps[, 10:29], 2, function(x) as.numeric(x))

upps_variance <- upps %>% select(subjectkey, starts_with("upps")) 
upps_variance <- na.omit(upps_variance) #11845

# 3) Youth Behavioral Inhibition/Behavioral Approach System Scales Modified from PhenX (BIS/BAS), child report - 24 questions
bisbas <- read.table("../data/Clinical/abcd_bisbas01.txt", sep="\t", header=TRUE)
bisbas <- bisbas[which(bisbas$eventname == "baseline_year_1_arm_1"), ]
bisbas[, 10:30] <- apply(bisbas[, 10:30], 2, function(x) as.numeric(x))
bisbas$bisbas5_y <- NULL # this is all Nas because there is a 5r_y question as well

bisbas_variance <- bisbas %>% select(subjectkey, starts_with("bisbas")) 
bisbas_variance <- na.omit(bisbas_variance) #11854

# 4) Brief Problem Monitor, teacher or youth report (n~9000) - 18 questions
# anxiety, inattention, aggression, externalizing and internalizing behavior

problem <- read.table("../data/Clinical/abcd_bpm01.txt", sep="\t", header=TRUE) #youth n=9000
problem <- problem[which(problem$eventname == "1_year_follow_up_y_arm_1"), ]
problem$abcd_bpm01_id <- NULL
problem$dataset_id <- NULL
problem <- problem[!duplicated(problem), ]
problem[, 8:26] <- apply(problem[, 8:26], 2, function(x) as.numeric(x))
problem[problem == 999] <- NA
problem[problem == 777] <- NA

bpm_variance <- problem %>% select(subjectkey, starts_with("bpm_"))
bpm_variance <- na.omit(bpm_variance) #9563

# 5) Positive Affective Items from the NIH Toolbox Battery, youth report - 9 questions, n~9000 participants
pos_aff <- read.table("../data/Clinical/abcd_ytbpai01.txt", sep="\t", header=TRUE)
pos_aff <- pos_aff[which(pos_aff$eventname == "1_year_follow_up_y_arm_1"), ]
pos_aff[, 10:18] <- apply(pos_aff[, 10:18], 2, function(x) as.numeric(x))
pos_aff[pos_aff == 999] <- NA
pos_aff[pos_aff == 777] <- NA

pos_aff_variance <- pos_aff %>% select(subjectkey, starts_with("poa_nihtb"))
pos_aff_variance <- na.omit(pos_aff_variance) #10127

# 6) Mania 7 Up - 11,400 participants
# abcd_y7mi01
mania <- read.table("../data/Clinical/abcd_y7mi01.txt", sep="\t", header=TRUE)
mania <- mania[which(mania$eventname == "1_year_follow_up_y_arm_1"), ]
mania[, 10:16] <- apply(mania[, 10:16], 2, function(x) as.numeric(x))

mania_variance <- mania %>% select(subjectkey, starts_with("sup"))
mania_variance <- na.omit(mania_variance) #11196

# 7) delinquincy
delinq <- read.table("../data/Clinical/abcd_y10ids01.txt", sep="\t", header=TRUE)
delinq <- delinq[which(delinq$eventname == "1_year_follow_up_y_arm_1"), ]
delinq[, 10:49] <- apply(delinq[, 10:49], 2, function(x) as.numeric(x))
delinq[delinq == 999] <- NA
delinq$abcd_y10ids01_id <- NULL
delinq$dataset_id <- NULL
delinq <- delinq[!duplicated(delinq), ]

del_variance <- delinq %>% select(subjectkey, starts_with("delq") & !ends_with("a_y") & !ends_with("dk"))
del_variance <- na.omit(del_variance) #10865


##########################################################################################
##########################################################################################
## Narrowing down who has information
participants <- merge(psychosis_variance, upps_variance, by="subjectkey")
participants <- merge(participants, bisbas_variance, by="subjectkey")
participants <- merge(participants, bpm_variance, by="subjectkey")
participants <- merge(participants, pos_aff_variance, by="subjectkey")
participants <- merge(participants, mania_variance, by="subjectkey")
participants <- merge(participants, del_variance, by="subjectkey") #8839
participants[participants == 999] <- NA
participants[participants == 777] <- NA
participants <- na.omit(participants) #8839


participants <- participants %>% select(subjectkey) 

psychosis_variance <- NULL
upps_variance <- NULL
bisbas_variance <- NULL
bpm_variance <- NULL
pos_aff_variance <- NULL
mania_variance <- NULL
del_variance <- NULL
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
write.csv(all, file="../output/Childreport_split_sample.csv")


##########################################################################################
##########################################################################################
################################################################################
# Prodromal Questionnaire Brief, child report - 21 questions
# 6 point scale if weighted by distress

psychosis <- merge(all, psychosis)
psychosis_sample <- psychosis %>% filter(Sample == "1")


# If there is no distress score, value is made 0 or 1 and if there is a distress score that score is added to 1 indicating that that score is experienced
for (i in 1:21){
  psychosis_sample[[paste0("prodromal_",i, "_y")]] <- ifelse(is.na(psychosis_sample[[paste0("prodromal_",i, "b_y")]]), psychosis_sample[[paste0("prodromal_",i, "_y")]], psychosis_sample[[paste0("prodromal_",i, "b_y")]]+psychosis_sample[[paste0("prodromal_",i, "_y")]])
}
variance <- psychosis_sample %>% select(starts_with("prodromal_")) %>% select(!ends_with("b_y")) 
variance <- na.omit(variance) #11864
variance <- variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

variance$Percent <- (variance$Count/4420)*100

## No questions have too low of a frequncy 
low_0 <- variance[which(variance$Percent > 99.49), ]

# polychoric
items <- psychosis_sample %>% select(starts_with("prodromal_")) %>% select(!ends_with("b_y"))
r <- polychoric(items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- as.data.frame(corr_matrix>0.85)

check <- check %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value == "TRUE"), ]
check <- check[which(check$Count > 1), ]

psychosis_sample <- psychosis_sample %>% select(subjectkey, starts_with("prodromal_") & !ends_with("b_y")) 

# setting up the second half
names_psychosis <- names(psychosis_sample)
psychosis_secondhalf <- psychosis %>% filter(Sample == "2")

for (i in 1:21){
  psychosis_secondhalf[[paste0("prodromal_",i, "_y")]] <- ifelse(is.na(psychosis_secondhalf[[paste0("prodromal_",i, "b_y")]]), psychosis_secondhalf[[paste0("prodromal_",i, "_y")]], psychosis_secondhalf[[paste0("prodromal_",i, "b_y")]]+psychosis_secondhalf[[paste0("prodromal_",i, "_y")]])
}

variance <- psychosis_secondhalf %>% select(starts_with("prodromal_")) %>% select(!ends_with("b_y")) 
variance <- na.omit(variance) #11864
variance <- variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

variance$Percent <- (variance$Count/4419)*100

## No questions have too low of a frequncy 
low_0 <- variance[which(variance$Percent > 99.49), ]

# polychoric
items <- psychosis_secondhalf %>% select(starts_with("prodromal_")) %>% select(!ends_with("b_y"))
r <- polychoric(items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- as.data.frame(corr_matrix>0.85)

check <- check %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value == "TRUE"), ]
check <- check[which(check$Count > 1), ]

psychosis_secondhalf <- psychosis_secondhalf %>% select(subjectkey, starts_with("prodromal_")) %>% select(!ends_with("b_y"))

################################################################################################
# Modified UPPS-P for Children from PhenX, child report - 20 questions, 4 factors of impulsivity
# NO low variability questions and NO high correlations
upps <- merge(all, upps)
upps_sample <- upps %>% filter(Sample == "1")

variance <- upps_sample %>% select(starts_with("upps")) 
variance <- na.omit(variance) #11845
variance <- variance%>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

variance$Percent <- (variance$Count/4420)*100
# no items with too low variance
low_0 <- variance[which(variance$Percent > 99.49), ]

# polychoric
items <- upps_sample %>% select(starts_with("upps"))
r <- polychoric(items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- as.data.frame(corr_matrix>0.85)

check <- check %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value == "TRUE"), ]
check <- check[which(check$Count > 1), ] # no questions are too highly correlated

upps_sample <- upps_sample %>% select(subjectkey, starts_with("upps")) 

# setting up the second half
names_upps <- names(upps_sample)
upps_secondhalf <- upps %>% filter(Sample == "2")

variance <- upps_secondhalf %>% select(starts_with("upps")) 
variance <- na.omit(variance) #11845
variance <- variance%>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

variance$Percent <- (variance$Count/4419)*100
# no items with too low variance
low_0 <- variance[which(variance$Percent > 99.49), ]

# polychoric
items <- upps_secondhalf %>% select(starts_with("upps"))
r <- polychoric(items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- as.data.frame(corr_matrix>0.85)

check <- check %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value == "TRUE"), ]
check <- check[which(check$Count > 1), ] # no questions are too highly correlated

upps_secondhalf <- upps_secondhalf[which((colnames(upps_secondhalf) %in% names_upps))]


################################################################################################
# Youth Behavioral Inhibition/Behavioral Approach System Scales Modified from PhenX (BIS/BAS), child report - 24 questions
bisbas <- merge(all, bisbas)
bisbas_sample <-bisbas %>% filter(Sample == "1")

variance <- bisbas_sample %>% select(starts_with("bisbas")) 
variance$bisbas5_y <- NULL
variance <- na.omit(variance) #4420
variance <- variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

variance$Percent <- (variance$Count/4420)*100
# no items with too small variance
low_0 <- variance[which(variance$Percent > 99.49), ]

# polychoric
items <- bisbas_sample %>% select(starts_with("bisbas"))
r <- polychoric(items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- as.data.frame(corr_matrix>0.85)

check <- check %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value == "TRUE"), ]
check <- check[which(check$Count > 1), ] # no questions are too highly correlated

bisbas_sample <- bisbas_sample %>% select(starts_with("bisbas"), subjectkey) 
bisbas_sample <- na.omit(bisbas_sample) #4420

# setting up the second half
names_bisbas <- names(bisbas_sample)
bisbas_secondhalf <- bisbas %>% filter(Sample == "2")

variance <- bisbas_secondhalf %>% select(starts_with("bisbas")) 
variance$bisbas5_y <- NULL
variance <- na.omit(variance) #4420
variance <- variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

variance$Percent <- (variance$Count/4419)*100
# no items with too small variance
low_0 <- variance[which(variance$Percent > 99.49), ]

# polychoric
items <- bisbas_secondhalf %>% select(starts_with("bisbas"))
r <- polychoric(items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- as.data.frame(corr_matrix>0.85)

check <- check %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value == "TRUE"), ]
check <- check[which(check$Count > 1), ] # no questions are too highly correlated


bisbas_secondhalf <- bisbas_secondhalf[which((colnames(bisbas_secondhalf) %in% names_bisbas))]


################################################################################################
# Brief Problem Monitor, teacher or youth report (n~9000) - 18 questions
# anxiety, inattention, aggression, externalizing and internalizing behavior
problem <- merge(all, problem)
problem_sample <- problem %>% filter(Sample == "1")

variance <- problem_sample %>% select(starts_with("bpm_"))
variance <- na.omit(variance) #9563
variance <- variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

variance$Percent <- (variance$Count/4420)*100
# no items with too small variance
low_0 <- variance[which(variance$Percent > 99.49), ]

# polychoric
items <- problem_sample %>% select(starts_with("bpm_"))
r <- polychoric(items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- as.data.frame(corr_matrix>0.85)

check <- check %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value == "TRUE"), ]
check <- check[which(check$Count > 1), ]

problem_sample <- problem_sample %>% select(starts_with("bpm_"), subjectkey)

# setting up the second half
names_problem <- names(problem_sample)
problem_secondhalf <- problem %>% filter(Sample == "2")

variance <- problem_secondhalf %>% select(starts_with("bpm_"))
variance <- na.omit(variance) #9563
variance <- variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

variance$Percent <- (variance$Count/4419)*100
# no items with too small variance
low_0 <- variance[which(variance$Percent > 99.49), ]

# polychoric
items <- problem_secondhalf %>% select(starts_with("bpm_"))
r <- polychoric(items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- as.data.frame(corr_matrix>0.85)

check <- check %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value == "TRUE"), ]
check <- check[which(check$Count > 1), ]


problem_secondhalf <- problem_secondhalf[which((colnames(problem_secondhalf) %in% names_problem))]

################################################################################################
# Positive Affective Items from the NIH Toolbox Battery, youth report - 9 questions, n~9000 participants
#
pos_aff <- merge(all, pos_aff)
pos_aff_sample <- pos_aff %>% filter(Sample == "1")

variance <- pos_aff_sample %>% select(starts_with("poa_nihtb"))
variance <- na.omit(variance) #10127
variance <- variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

variance$Percent <- (variance$Count/4420)*100
# no items with too small variance
low_0 <- variance[which(variance$Percent > 99.49), ]

# polychoric
items <- pos_aff_sample %>% select(starts_with("poa_nihtb"))
r <- polychoric(items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- as.data.frame(corr_matrix>0.85)

check <- check %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value == "TRUE"), ]
check <- check[which(check$Count > 1), ]

pos_aff_sample <- pos_aff_sample %>% select(starts_with("poa_nihtb"), subjectkey)
pos_aff_sample <- na.omit(pos_aff_sample) #10127

# setting up the second half
names_pos_aff <- names(pos_aff_sample)
pos_aff_secondhalf <- pos_aff %>% filter(Sample == "2")

variance <- pos_aff_secondhalf %>% select(starts_with("poa_nihtb"))
variance <- na.omit(variance) #10127
variance <- variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

variance$Percent <- (variance$Count/4419)*100
# no items with too small variance
low_0 <- variance[which(variance$Percent > 99.49), ]

# polychoric
items <- pos_aff_secondhalf %>% select(starts_with("poa_nihtb"))
r <- polychoric(items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- as.data.frame(corr_matrix>0.85)

check <- check %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value == "TRUE"), ]
check <- check[which(check$Count > 1), ]

pos_aff_secondhalf <- pos_aff_secondhalf[which((colnames(pos_aff_secondhalf) %in% names_pos_aff))]



################################################################################################
# Mania 7 Up - 11,400 participants
# abcd_y7mi01
mania <- merge(all, mania)
mania_sample <- mania %>% filter(Sample == "1")

variance <- mania_sample %>% select(starts_with("sup"))
variance <- na.omit(variance) #11196
variance <- variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

variance$Percent <- (variance$Count/4420)*100
# no items with too small variance
low_0 <- variance[which(variance$Percent > 99.49), ]

# polychoric
items <- mania_sample %>% select(starts_with("sup"))
r <- polychoric(items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- as.data.frame(corr_matrix>0.85)

check <- check %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value == "TRUE"), ]
check <- check[which(check$Count > 1), ]

mania_sample <- mania_sample %>% select(starts_with("sup"), subjectkey)
mania_sample <- na.omit(mania_sample) #11196

# setting up the second half
names_mania <- names(mania_sample)
mania_secondhalf <- mania %>% filter(Sample == "2")

variance <- mania_secondhalf %>% select(starts_with("sup"))
variance <- na.omit(variance) #11196
variance <- variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

variance$Percent <- (variance$Count/4419)*100
# no items with too small variance
low_0 <- variance[which(variance$Percent > 99.49), ]

# polychoric
items <- mania_secondhalf %>% select(starts_with("sup"))
r <- polychoric(items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- as.data.frame(corr_matrix>0.85)

check <- check %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value == "TRUE"), ]
check <- check[which(check$Count > 1), ]

mania_secondhalf <- mania_secondhalf[which((colnames(mania_secondhalf) %in% names_mania))]



################################################################################################
# Youth Delinquincy 10 Item scale
# These questions are binary though so not sure if I can use them
delinq <- merge(all, delinq)
delinq_sample <- delinq %>% filter(Sample == "1")

variance <- delinq_sample %>% select(starts_with("delq") & !ends_with("a_y") & !ends_with("dk"))
variance <- na.omit(variance) #10865
variance <- variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

variance$Percent <- (variance$Count/4420)*100
# no items with too small variance
low_0 <- variance[which(variance$Percent > 99.49), ]

# polychoric
items <- delinq_sample %>% select(starts_with("delq") & !ends_with("a_y") & !ends_with("dk"))
items <- na.omit(items) #10865
r <- polychoric(items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- as.data.frame(corr_matrix>0.85)

check <- check %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value == "TRUE"), ]
check <- check[which(check$Count > 1), ]

delinq_sample <- delinq_sample %>% select(subjectkey, starts_with("delq") & !ends_with("a_y") & !ends_with("dk"))
delinq_sample <- na.omit(delinq_sample) #10865
delinq_sample$delq_8_y <- NULL
delinq_sample$delq_10_y <- NULL
#based off the second half of the sample
delinq_sample$delq_5_6_y <- ifelse(delinq_sample$delq_5_y == "1" | delinq_sample$delq_6_y == "1", 1, 0)
delinq_sample$delq_5_y <- NULL
delinq_sample$delq_6_y <- NULL

# setting up the second half
names_delinq <- names(delinq_sample)
delinq_secondhalf <- delinq %>% filter(Sample == "2")

variance <- delinq_secondhalf %>% select(starts_with("delq") & !ends_with("a_y") & !ends_with("dk"))
variance <- na.omit(variance) #10865
variance <- variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

variance$Percent <- (variance$Count/4419)*100
# no items with too small variance
low_0 <- variance[which(variance$Percent > 99.49), ]

# polychoric
items <- delinq_secondhalf %>% select(starts_with("delq") & !ends_with("a_y") & !ends_with("dk"))
items <- na.omit(items) #10865
r <- polychoric(items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- as.data.frame(corr_matrix>0.85)

check <- check %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value == "TRUE"), ]
check <- check[which(check$Count > 1), ]

delinq_secondhalf$delq_5_6_y <- ifelse(delinq_secondhalf$delq_5_y == "1" | delinq_secondhalf$delq_6_y == "1", 1, 0)
delinq_secondhalf <- delinq_secondhalf[which((colnames(delinq_secondhalf) %in% names_delinq))]


low_0 <- NULL
r <- NULL
corr_matrix <- NULL
items <- NULL
variance <- NULL
check <- NULL
out <- NULL
participants <- NULL
relationship <- NULL
site <- NULL
all <- NULL
bisbas<- NULL
delinq<- NULL
mania<- NULL
pos_aff<- NULL
psychosis<- NULL
upps<- NULL
problem <- NULL




