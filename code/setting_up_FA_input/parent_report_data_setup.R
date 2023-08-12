## setting up data for parent report model


library(ggplot2)
library(plyr)
library(dplyr)
library(knitr)
library(tidyr)
library(psych)
library(polycor)

# demographics to remove siblings
relationship <- read.table("../data/Demographics/acspsw03.txt", sep="\t", header=TRUE)
relationship <- relationship[which(relationship$eventname == "baseline_year_1_arm_1"), ] #11875
relationship <- relationship[ , c("subjectkey", "rel_family_id")]


#Child behaviour checklist (CBCL), parent report - 113 questions, 8 syndrome scales
# 3 point scale
cbcl <- read.table("../data/Clinical/abcd_cbcl01.txt", sep="\t", header=TRUE)
cbcl <- cbcl[which(cbcl$eventname == "baseline_year_1_arm_1"), ]
cbcl[, 10:128] <- apply(cbcl[, 10:128], 2, function(x) as.numeric(x))
cbcl <- merge(cbcl, relationship, by="subjectkey")
#cbcl <- na.omit(cbcl) #removes 8

cbcl_endorse <- cbcl %>% select(starts_with("cbcl_q"))
cbcl_endorse <- na.omit(cbcl_endorse)
cbcl_endorse <- cbcl_endorse %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

cbcl_endorse$Percent <- (cbcl_endorse$Count/11870)*100
# determining which items have too small variance - matches Michelini or Moore et al
low_0 <- cbcl_endorse[which(cbcl_endorse$Percent > 99.5), ]

# calculating the polychoric r, is for ordinal factors fo 
cbcl_items <- cbcl %>% select(starts_with("cbcl_q"))
# removing iteems with low variancee - same as Michelini at this point
cbcl_items <- subset(cbcl_items, select=-c(cbcl_q02_p, cbcl_q101_p, cbcl_q105_p, cbcl_q73_p,cbcl_q99_p))

t <- polychoric(cbcl_items) # psych package, get 22


corr_matrix <- as.data.frame(t[["rho"]])
check <- corr_matrix %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

check <- check[which(check$Value > 0.85 & check$Value != 1), ]
combined <- unique(check$Measure)


cbcl_included <- cbcl %>% select(subjectkey, starts_with("cbcl_q"))
cbcl_included[, 2:120] <- apply(cbcl_included[, 2:120], 2, function(x) as.numeric(x))
cbcl_included <- na.omit(cbcl_included)

### creating composites
cbcl_included$distracted <- round((cbcl_included$cbcl_q08_p + cbcl_included$cbcl_q78_p)/2)
cbcl_included$destroys <- round((cbcl_included$cbcl_q21_p + cbcl_included$cbcl_q20_p)/2)


cbcl_included <- subset(cbcl_included, select=-c(cbcl_q02_p, cbcl_q101_p, cbcl_q105_p, cbcl_q73_p,cbcl_q99_p))
cbcl_included <- cbcl_included[which(!(colnames(cbcl_included) %in% combined))]


################################################################################################
#Mania questions, parent report - 10 questions
#
mania <- read.table("../data/Clinical/abcd_pgbi01.txt", sep="\t", header=TRUE)
mania <- mania[which(mania$eventname == "baseline_year_1_arm_1"), ]
mania[, 11:20] <- apply(mania[, 11:20], 2, function(x) as.numeric(x))
mania <- merge(mania, relationship, by="subjectkey")


mania_variance <- mania %>% select(starts_with("gen_child_behav")) 
mania_variance <- na.omit(mania_variance)
mania_variance <- mania_variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

mania_variance$Percent <- (mania_variance$Count/11870)*100

low_0 <- mania_variance[which(mania_variance$Percent > 99.49), ]

# polychoric
man_items <- mania %>% select(starts_with("gen_child_behav"))
man_items <- na.omit(man_items)

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

man_included <- mania %>% select(subjectkey, starts_with("gen_child_behav"))
man_included <- na.omit(man_included)

################################################################################################
# Social Responsiveness Scale


social <- read.table("../data/Clinical/abcd_pssrs01.txt", sep="\t", header=TRUE)
social <- social[which(social$eventname == "1_year_follow_up_y_arm_1"), ]
social[, 11:21] <- apply(social[, 11:21], 2, function(x) as.numeric(x))
social <- merge(social, relationship, by="subjectkey")


social_variance <- social %>% select(starts_with("ssrs_"), -ssrs_select_language___1)
social_variance <- na.omit(social_variance)
social_variance <- social_variance %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

social_variance$Percent <- (social_variance$Count/11194)*100

# no low variance items
low_0 <- social_variance[which(social_variance$Percent > 99.49), ]

# polychoric
social_items <- social %>% select(starts_with("ssrs_"), -ssrs_select_language___1)
social_items <- na.omit(social_items)

r <- polychoric(social_items) # psych package
corr_matrix <- as.data.frame(r[["rho"]])
check <- corr_matrix %>%
  gather(Measure, Value) %>% 
  dplyr::group_by(Measure, Value) %>%         
  dplyr::summarise(Count = n())

# don't need to combine any unless I drop it to 0.80
check <- check[which(check$Value > 0.85 & check$Value != 1), ]
combined <- unique(check$Measure)

social_included <- social %>% select(subjectkey, starts_with("ssrs_"), -ssrs_select_language___1)
social_included <- na.omit(social_included)

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

