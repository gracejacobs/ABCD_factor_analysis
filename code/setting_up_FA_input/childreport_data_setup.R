
#### Data set up for child report factor analysis
library(tidyverse)
library(psych)

################################################################################
# Prodromal Questionnaire Brief, child report - 21 questions
# 6 point scale if weighted by distress
psychosis <- read.table("../data/Clinical/pps01.txt", sep="\t", header=TRUE)
psychosis <- psychosis[which(psychosis$visit == "baseline_year_1_arm_1"), ]
psychosis[, 10:72] <- apply(psychosis[, 10:72], 2, function(x) as.numeric(x))

# If there is no distress score, value is made 0 or 1 and if there is a distress score that score is added to 1 indicating that that score is experienced
for (i in 1:21){
  psychosis[[paste0("prodromal_",i, "_y")]] <- ifelse(is.na(psychosis[[paste0("prodromal_",i, "b_y")]]), psychosis[[paste0("prodromal_",i, "_y")]], psychosis[[paste0("prodromal_",i, "b_y")]]+psychosis[[paste0("prodromal_",i, "_y")]])
}

variance <- psychosis %>% select(starts_with("prodromal_")) %>% select(!ends_with("b_y")) 
variance <- na.omit(variance) #11864
variance <- variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

variance$Percent <- (variance$Count/11864)*100

## No questions have too low of a frequncy 
low_0 <- variance[which(variance$Percent > 99.49), ]

# polychoric
items <- psychosis %>% select(starts_with("prodromal_")) %>% select(!ends_with("b_y"))
items <- na.omit(items)
r <- polychoric(items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- as.data.frame(corr_matrix>0.85)

check <- check %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value == "TRUE"), ]
check <- check[which(check$Count > 1), ]

psychosis <- psychosis %>% select(starts_with("prodromal_") & !ends_with("b_y") | starts_with("subjectkey")) 
psychosis <- na.omit(psychosis) #11864

################################################################################################
# Modified UPPS-P for Children from PhenX, child report - 20 questions, 4 factors of impulsivity
# NO low variability questions and NO high correlations
upps <- read.table("../data/Clinical/abcd_upps01.txt", sep="\t", header=TRUE)
upps <- upps[which(upps$eventname == "baseline_year_1_arm_1"), ]
upps[, 10:29] <- apply(upps[, 10:29], 2, function(x) as.numeric(x))

variance <- upps %>% select(starts_with("upps")) 
variance <- na.omit(variance) #11845
variance <- variance%>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

variance$Percent <- (variance$Count/11845)*100
# no items with too low variance
low_0 <- variance[which(variance$Percent > 99.49), ]

# polychoric
items <- upps %>% select(starts_with("upps"))
r <- polychoric(items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- as.data.frame(corr_matrix>0.85)

check <- check %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value == "TRUE"), ]
check <- check[which(check$Count > 1), ] # no questions are too highly correlated

upps <- upps %>% select(starts_with("upps") | starts_with("subjectkey")) 
upps <- na.omit(upps) #11845

################################################################################################
# Youth Behavioral Inhibition/Behavioral Approach System Scales Modified from PhenX (BIS/BAS), child report - 24 questions
## NO low variability questions and NO high correlations
bisbas <- read.table("../data/Clinical/abcd_bisbas01.txt", sep="\t", header=TRUE)
bisbas <- bisbas[which(bisbas$eventname == "baseline_year_1_arm_1"), ]
bisbas[, 10:30] <- apply(bisbas[, 10:30], 2, function(x) as.numeric(x))
bisbas$bisbas5_y <- NULL # this is all Nas because there is a 5r_y question as well
bisbas <- na.omit(bisbas) #11854

variance <- bisbas %>% select(starts_with("bisbas")) 
variance$bisbas5_y <- NULL
variance <- na.omit(variance) #11854
variance <- variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

variance$Percent <- (variance$Count/11854)*100
# no items with too small variance
low_0 <- variance[which(variance$Percent > 99.49), ]

# polychoric
items <- bisbas %>% select(starts_with("bisbas"))
r <- polychoric(items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- as.data.frame(corr_matrix>0.85)

check <- check %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value == "TRUE"), ]
check <- check[which(check$Count > 1), ] # no questions are too highly correlated

bisbas <- bisbas %>% select(starts_with("bisbas") | starts_with("subjectkey")) 


################################################################################################
# Brief Problem Monitor, teacher or youth report (n~9000) - 18 questions
# anxiety, inattention, aggression, externalizing and internalizing behavior

problem <- read.table("../data/Clinical/abcd_bpm01.txt", sep="\t", header=TRUE) #youth n=9000
problem <- problem[which(problem$eventname == "1_year_follow_up_y_arm_1"), ]
problem$abcd_bpm01_id <- NULL
problem$dataset_id <- NULL
problem <- problem[!duplicated(problem), ]
problem[, 8:26] <- apply(problem[, 8:26], 2, function(x) as.numeric(x))
problem[problem == 999] <- NA
problem[problem == 777] <- NA

variance <- problem %>% select(starts_with("bpm_"))
variance <- na.omit(variance) #9563
variance <- variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

variance$Percent <- (variance$Count/9563)*100
# no items with too small variance
low_0 <- variance[which(variance$Percent > 99.49), ]

# polychoric
items <- problem %>% select(starts_with("bpm_"))
r <- polychoric(items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- as.data.frame(corr_matrix>0.85)

check <- check %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value == "TRUE"), ]
check <- check[which(check$Count > 1), ]

problem <- problem %>% select(starts_with("bpm_") | starts_with("subjectkey"))
problem <- na.omit(problem) #9563

################################################################################################
# Positive Affective Items from the NIH Toolbox Battery, youth report - 9 questions, n~9000 participants
#
pos_aff <- read.table("../data/Clinical/abcd_ytbpai01.txt", sep="\t", header=TRUE)
pos_aff <- pos_aff[which(pos_aff$eventname == "1_year_follow_up_y_arm_1"), ]
pos_aff[, 10:18] <- apply(pos_aff[, 10:18], 2, function(x) as.numeric(x))
pos_aff[pos_aff == 999] <- NA
pos_aff[pos_aff == 777] <- NA

variance <- pos_aff %>% select(starts_with("poa_nihtb"))
variance <- na.omit(variance) #10127
variance <- variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

variance$Percent <- (variance$Count/10127)*100
# no items with too small variance
low_0 <- variance[which(variance$Percent > 99.49), ]

# polychoric
items <- pos_aff %>% select(starts_with("poa_nihtb"))
r <- polychoric(items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- as.data.frame(corr_matrix>0.85)

check <- check %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value == "TRUE"), ]
check <- check[which(check$Count > 1), ]

pos_aff <- pos_aff %>% select(starts_with("poa_nihtb") | starts_with("subjectkey"))
pos_aff <- na.omit(pos_aff) #10127

################################################################################################
# Mania 7 Up - 11,400 participants
# abcd_y7mi01
mania <- read.table("../data/Clinical/abcd_y7mi01.txt", sep="\t", header=TRUE)
mania <- mania[which(mania$eventname == "1_year_follow_up_y_arm_1"), ]
mania[, 10:16] <- apply(mania[, 10:16], 2, function(x) as.numeric(x))

variance <- mania %>% select(starts_with("sup"))
variance <- na.omit(variance) #11196
variance <- variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

variance$Percent <- (variance$Count/11196)*100
# no items with too small variance
low_0 <- variance[which(variance$Percent > 99.49), ]

# polychoric
items <- mania %>% select(starts_with("sup"))
r <- polychoric(items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- as.data.frame(corr_matrix>0.85)

check <- check %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())
check <- check[which(check$Value == "TRUE"), ]
check <- check[which(check$Count > 1), ]

mania <- mania %>% select(starts_with("sup") | starts_with("subjectkey"))
mania <- na.omit(mania) #11196

################################################################################################
# Youth Delinquincy 10 Item scale
# These questions are binary though so not sure if I can use them

delinq <- read.table("../data/Clinical/abcd_y10ids01.txt", sep="\t", header=TRUE)
delinq <- delinq[which(delinq$eventname == "1_year_follow_up_y_arm_1"), ]
delinq[, 10:49] <- apply(delinq[, 10:49], 2, function(x) as.numeric(x))
delinq[delinq == 999] <- NA
delinq$abcd_y10ids01_id <- NULL
delinq$dataset_id <- NULL
delinq <- delinq[!duplicated(delinq), ]

variance <- delinq %>% select(starts_with("delq") & !ends_with("a_y") & !ends_with("dk"))
variance <- na.omit(variance) #10865
variance <- variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

variance$Percent <- (variance$Count/10865)*100
# no items with too small variance
low_0 <- variance[which(variance$Percent > 99.49), ]

# polychoric
items <- delinq %>% select(starts_with("delq") & !ends_with("a_y") & !ends_with("dk"))
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

delinq <- delinq %>% select(starts_with("subjectkey") | starts_with("delq") & !ends_with("a_y") & !ends_with("dk"))
delinq <- na.omit(delinq) #10865
#delinq$delq_8_y <- NULL
#delinq$delq_10_y <- NULL
#delinq$delq_5_6_y <- ifelse(delinq$delq_5_y == "1" | delinq$delq_6_y == "1", 1, 0)
#delinq$delq_5_y <- NULL
#delinq$delq_6_y <- NULL


low_0 <- NULL
r <- NULL
corr_matrix <- NULL
items <- NULL
variance <- NULL
check <- NULL
