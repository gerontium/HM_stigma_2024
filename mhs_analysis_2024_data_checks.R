# install packages ----
packages <- c("WRS2", "tidyr", "ggplot2", "haven", "plyr", "dplyr", "reshape", "ez", "lme4", "stringr", "cowplot", "tidyverse", "RColorBrewer")
sapply(packages, require, character.only = TRUE, quietly = TRUE)

#############################################################################
# FIRST PART IS DATA CHECK, SECOND PART IS TO SAVE MAYBE
#############################################################################

# load in data ----
setwd("C:/Users/geron/Dropbox/Research/Mental Health Stigma/New Work")

path = file.path("C:/", "Users/geron/Dropbox/Research/Mental Health Stigma/New Work/New data", 
                 "Study+on+Mental+and+neurological+conditions+in+Ireland-+Final_December+2,+2022_05.55 - recoded.sav")

#############################################################################
############################# LOAD ORIGINAL DATA ############################
#############################################################################

df <- data.frame(read_sav(path))
#sapply(df, class)
#detach("package:haven", unload=TRUE)

#############################################################################
############################# CHECK NAs #####################################
#############################################################################

# investigate data, first count NAs in each variable, and get complete rows
dim(df) # get size of dataframe = 1232
colSums(is.na(df)) # count NAs in each column
sum(complete.cases(df)) # get complete rows in column

df_na <- df[!complete.cases(df), ]
row_na <- rowSums(is.na(df_na))
windows();
res <- hist(row_na)
res

df_na$na_count <- apply(df_na, 1, function(x) sum(is.na(x)))
df_na <- df_na[df_na$na_count < dim(df_na)[2]-round(dim(df_na)[2]/2,0), ] # get rid of ps with all NAs
df_na <- df_na[order(df_na$na_count,decreasing=TRUE),]
row_na <- rowSums(is.na(df_na))
windows();
res <- hist(row_na)
res
colSums(is.na(df_na)) # count NAs in each column again

# check how many left in full dataset once main ones left.
df2 <- df
df2$na_count <- apply(df2, 1, function(x) sum(is.na(x)))
df2 <- df2[order(df2$na_count,decreasing=TRUE),]
view(df2)

df3 <- df2[df2$na_count < dim(df2)[2]-round(dim(df_na)[2]/2,0), ] # get rid of ps with more than half questions NA.
dim(df3)

#############################################################################
################################ CONCLUSIONS ################################
#############################################################################

# A few ps with many NAs = remove
# A few ps with approx. 9 to 12 NAs, never completed Knowledge/Attitude/Behaviour questions = remove
# Remainder missed a couple of questions here and there. Could technically keep these and just remove as desired for each analysis
# Also, if someone has NA in a given LoCR but then answers 1 in LoCR afterwards, then technically can still include them
# Most people answer demographics

# So to clean dataset:
# Will need to load PEPC_1 and PEPC_2, but not include them in any NA count! Perhaps make all NA for PEPC_1 and 2 999s and then go over later
# 1. Remove all ps with all NAs
# 2. Remove all ps with 4 NAs and above
# 3. Remove all ps with any NAs in demographics
# 4. Go through LoCR in other code. Does row contain NA and no 1 after? If so, delete. Per row, find last NA of LoCR. Any 1 after? If yes, make any NAs in that row a 2. Then delete all NA rows.

# for first pass basics, go for dim[2]-18. eliminating all participants who completed less than half the questions.
# However, this depends on what questions we include.

#############################################################################
################################ CLEAN/CALC VARS/SAVE #################################
#############################################################################

df <- data.frame(read_sav(path))
colnames(df)

# get rid of bad subjects
df$na_count <- apply(df, 1, function(x) sum(is.na(x)))
df <- df[order(df$na_count,decreasing=TRUE),]
df <- df[df$na_count < dim(df)[2]-round(dim(df)[2]/2,0), ] # get rid of ps with more than half questions NA.    

dim(df) # get size of dataframe
colSums(is.na(df)) # count NAs in each column again
sapply(df, class)

# recalculate Knowledge score using only 1, 4, 6, 9
colnms=c("QAMD_Schizo_1_recoded", "QAMD_Schizo_4_recoded","QAMD_Schizo_6_recoded", "QAMD_Schizo_9_recoded")
df$Total_knowledge_schizophrenia <- rowSums(df[,colnms])
colnms=c("QAMD_Bipolar_1_recoded", "QAMD_Bipolar_4_recoded","QAMD_Bipolar_6_recoded", "QAMD_Bipolar_9_recoded")
df$Total_knowledge_bipolar <- rowSums(df[,colnms])
colnms=c("QAMD_Autism_1_recoded", "QAMD_Autism_4_recoded","QAMD_Autism_6_recoded", "QAMD_Autism_9_recoded")
df$Total_knowledge_autism <- rowSums(df[,colnms])

# calculate Epilepsy Attitude variable
df$A5_QSI_CS_Epilepsy_1 <- as.numeric(df$A5_QSI_CS_Epilepsy_1)
df <- df %>% mutate(A5_QSI_CS_Epilepsy_1=as.numeric(recode(A5_QSI_CS_Epilepsy_1,'1'='0','2'='1','3'='2','4'='0')))

df$A5_QSI_CS_Epilepsy_2 <- as.numeric(df$A5_QSI_CS_Epilepsy_2)
df <- df %>% mutate(A5_QSI_CS_Epilepsy_2=as.numeric(recode(A5_QSI_CS_Epilepsy_2,'1'='0','2'='1','3'='2','4'='0')))

df$A5_QSI_CS_Epilepsy_3 <- as.numeric(df$A5_QSI_CS_Epilepsy_3)
df <- df %>% mutate(A5_QSI_CS_Epilepsy_3=as.numeric(recode(A5_QSI_CS_Epilepsy_3,'1'='0','2'='1','3'='2','4'='0')))

df$A5_QSI_CS_Epilepsy_4 <- as.numeric(df$A5_QSI_CS_Epilepsy_4)
df <- df %>% mutate(A5_QSI_CS_Epilepsy_4=as.numeric(recode(A5_QSI_CS_Epilepsy_4,'1'='0','2'='1','3'='2','4'='0')))

df$A5_QSI_CS_Epilepsy_5 <- as.numeric(df$A5_QSI_CS_Epilepsy_5)
df <- df %>% mutate(A5_QSI_CS_Epilepsy_5=as.numeric(recode(A5_QSI_CS_Epilepsy_5,'1'='0','2'='1','3'='2','4'='0')))

# calculate Epilepsy Behaviour variable
df$PALoCEp_1_4 <- as.numeric(df$PALoC_1_4)
df <- df %>% mutate(PALoCEp_1_4=as.numeric(recode(PALoCEp_1_4,'1'='3','2'='2','3'='1')))

df$PALoCEp_2_4 <- as.numeric(df$PALoC_2_4)
df <- df %>% mutate(PALoCEp_2_4=as.numeric(recode(PALoCEp_2_4,'1'='3','2'='2','3'='1')))

df$PALoCEp_3_4 <- as.numeric(df$PALoC_3_4)
df <- df %>% mutate(PALoCEp_3_4=as.numeric(recode(PALoCEp_3_4,'1'='3','2'='2','3'='1')))

# calculate MHKQ score
MHKQ_correct <- c(1,2,1,2,1,2,1,1,2,2,
                  1,1,2,2,1,1,1,1,1,1)

df['MHKQ_score'] <- NA
df['MHKQ_score_NA_count'] <- NA

for (x in 1:dim(df)[1]) {
  ans <- as.vector(unlist(df[x,match("MHKQ_1", colnames(df)):match("MHKQ_20", colnames(df))]))
  df[x,'MHKQ_score_NA_count'] <- sum(is.na(ans))
  correct <- ans==MHKQ_correct
  correct[is.na(correct)] <- FALSE
  total_correct <- sum(correct, na.rm=TRUE)
  df[x,'MHKQ_score'] <- total_correct
}
df[df[,'MHKQ_score_NA_count']==20,'MHKQ_score'] <- NA

# go through LoCR, check for NAs
for (x in 1:dim(df)[1]) {
  thisfar <- c()
  for (xx in grep("^LoCR_1$", colnames(df)):grep("^LoCR_12$", colnames(df))) {
    if (is.na(df[x,xx])) {
      thisfar <- c(thisfar,xx)
    }
  }
  
  # thisfar is array of indx of NaN across df. Check for any 1 after last index
  thisfar2 <- 0
  if (length(thisfar) > 0) {
    if (tail(thisfar, n=1) != 22) {
      for (xx in tail(thisfar, n=1):grep("^LoCR_12$", colnames(df))) {
        if (df[x,xx] == 1 & !is.na(df[x,xx])) {
          thisfar2 <- 1
        }
      }
    }
  }
  
  # if there is a 1 after last NA, change any NaN in that row to be 999 instead of NA
  if (thisfar2 == 1) {
    df[x,thisfar] <- 999
  }
}

# calculate familiarity from LoCR score, bear in mind we still have NAs!
df['Familiarity'] <- NA
df['Familiarity_sum'] <- NA
df['Familiarity_recoded'] <- NA

start_indx <- match("LoCR_1", colnames(df))-1

for (x in 1:dim(df)[1]) {
  thisfar <- 0
  sumfar <- 0
  # take care of NAs first
  if (rowSums(is.na(df[x,grep("^LoCR_1$", colnames(df)):grep("^LoCR_12$", colnames(df))]))==0) {
    for (xx in grep("^LoCR_1$", colnames(df)):grep("^LoCR_12$", colnames(df))) {
      if (df[x,xx] == 1) {
        thisfar <- xx-start_indx
        sumfar <- sumfar+1
      }
    }
    
    df[x,'Familiarity'] <- thisfar
    df[x,'Familiarity_sum'] <- sumfar
    
    if (thisfar <= 6) {
      df[x,'Familiarity_recoded'] <- 1
    } else if (thisfar >= 9 & thisfar <= 11) {
      df[x,'Familiarity_recoded'] <- 2
    } else if (thisfar == 12) {
      df[x,'Familiarity_recoded'] <- 3
    }
  }
}
# extra code if needed for 4 groups, currently leaving out services groups
#else if (thisfar >= 7 & thisfar <= 8) {
#  df[x,'Familiarity_recoded'] <- 2
#}

#sapply(df, class)
colSums(is.na(df)) # count NAs in each column again

df <- df %>%
  mutate(Total_positive_behaviour_epilepsy = rowSums(select(., contains("PALoCEp")))) %>%
  mutate(A5_QSI_CS_Epilepsy_total = rowSums(select(., contains("A5_QSI_CS_Epilepsy")))) %>%
  dplyr::select(ResponseId, Total_knowledge_schizophrenia, Total_knowledge_bipolar, Total_knowledge_autism, 
                A5_QSI_CS_Schizo_total, A5_QSI_CS_Bipolar_total, A5_QSI_CS_Autism_total, A5_QSI_CS_Epilepsy_total,
                Total_positive_behaviour_schizophrenia, Total_positive_behaviour_bipolar, Total_positive_behaviour_autism, Total_positive_behaviour_epilepsy,
                EOPD_1, EOPD_2, EOPD_3, EOPD_4, EOPD_5, EOPD_6, EOPD_7, EOPD_8, Q4, Q6, Q8, Q9, PEPC_1, PEPC_2, MHKQ_score,Familiarity_recoded)

colSums(is.na(df)) # count NAs in each column again
dim(df) # get size of dataframe
sum(complete.cases(df)) # get complete rows in column
df$na_count <- apply(df, 1, function(x) sum(is.na(x)))
df <- df[order(df$na_count,decreasing=TRUE),]

# save final version
path2 = file.path("C:/", "Users/geron/Dropbox/Research/Mental Health Stigma/New Work/New data", "MHS_final.csv")
write.csv(df, path2, row.names=FALSE)
# save as dataframe
path3 = file.path("C:/", "Users/geron/Dropbox/Research/Mental Health Stigma/New Work/New data", "MHS_final.Rda")
save(df,file=path3)
