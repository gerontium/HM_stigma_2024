# install packages ----
packages <- c("WRS2", "tidyr", "ggplot2", "haven", "plyr", "dplyr", "reshape", "ez", "lme4", "stringr", "cowplot", "tidyverse", "RColorBrewer", 
              "lmerTest", "ordinal")
sapply(packages, require, character.only = TRUE, quietly = TRUE)

# load in data ----
setwd("C:/Users/geron/Dropbox/Research/Mental Health Stigma/New Work")
source("summarySE.R")

path3 = file.path("C:/", "Users/geron/Dropbox/Research/Mental Health Stigma/New Work/New data", "MHS_final.Rda")
load(path3)

# get subset of dataframe ----
df <- df %>% dplyr::select(ResponseId, Total_knowledge_schizophrenia, Total_knowledge_bipolar, Total_knowledge_autism, 
                           A5_QSI_CS_Schizo_total, A5_QSI_CS_Bipolar_total, A5_QSI_CS_Autism_total, A5_QSI_CS_Epilepsy_total,
                           Total_positive_behaviour_schizophrenia, Total_positive_behaviour_bipolar, Total_positive_behaviour_autism, Total_positive_behaviour_epilepsy,
                           EOPD_1, EOPD_2, EOPD_3, EOPD_4, EOPD_5, EOPD_6, EOPD_7, EOPD_8, Q4, Q6, Q8, Q9, PEPC_1, PEPC_2, MHKQ_score,Familiarity_recoded)

# investigate data, first count NAs in each variable, and get complete rows
dim(df) # get size of dataframe = 1232
colSums(is.na(df)) # count NAs in each column
sum(complete.cases(df)) # get complete rows in column

# get MHKQ quantiles
df$MHKQ_quantile <- ntile(df$MHKQ_score, n=4)

df <- df %>% dplyr::select(ResponseId, EOPD_1, EOPD_2, EOPD_3, EOPD_4, EOPD_5, EOPD_6, EOPD_7, EOPD_8, 
                           Familiarity_recoded, MHKQ_quantile)

# rename variables
colnames(df) <- c("PATID","Danger_Themselves_SZ","Danger_Others_SZ","Danger_Themselves_BD",
                  "Danger_Others_BD","Danger_Themselves_ASD","Danger_Others_ASD","Danger_Themselves_EP","Danger_Others_EP",
                  "Familiarity_Recode","MHKQ_quantile")

fam_tags <- c("1 - No Exp", "2 - Family Member", "3 - Oneself") # "2 - Provide Services", 
MHKQ_tags <- c("1 - Low MHKQ Score", "2", "3", "4 - High MHKQ Score") 
level_order <- c('EP', 'ASD', 'BD', 'SZ')

################################################################
###################### FAMILIARITY ################################
################################################################

print('**********************Danger Themselves************************')

# Conduct regular and trimmed mean ANOVAs, with line plots ----
# Danger Themselves ANOVA
longdf <- df %>% select(PATID, Danger_Themselves_SZ, Danger_Themselves_BD, Danger_Themselves_ASD, Danger_Themselves_EP, Familiarity_Recode) 
longdf <- longdf[complete.cases(longdf),] %>% 
  melt(id = c('PATID', 'Familiarity_Recode'), measured = c('Danger_Themselves_SZ', 'Danger_Themselves_BD', 
                                                           'Danger_Themselves_ASD', 'Danger_Themselves_EP'))
names(longdf) <- c('PATID', 'Affected', 'Disorder', 'Danger_Themselves')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
#longdf$Knowledge <- factor(longdf$Knowledge) only use this if doing clm
longdf$Affected <- factor(longdf$Affected)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_k <- longdf # ugly code!

# Trimmed mean ANOVA, with 20% trimmed means
bwtrim(formula = Danger_Themselves ~ Affected * Disorder, id = PATID, data = longdf_k)

# cumulative linked model
# make dv factor and ordered
longdf_k$Danger_Themselves <- factor(longdf_k$Danger_Themselves, ordered = TRUE)
longdf_k$Affected <- factor(longdf_k$Affected, ordered = TRUE)
model <- clmm(Danger_Themselves ~ Affected*Disorder + (1 | PATID), data = longdf_k)
summary(model)
#anova_result <- drop1(model, test = "Chisq"), print(anova_result)
model_no_interaction <- clmm(Danger_Themselves ~ Affected + Disorder + (1 | PATID), data = longdf_k)
model_no_disorder <- clmm(Danger_Themselves ~ Affected + (1 | PATID), data = longdf_k)
model_no_affected <- clmm(Danger_Themselves ~ Disorder + (1 | PATID), data = longdf_k)
# test disorder
anova(model_no_disorder,model)
# test affected
anova(model_no_affected,model)
# test interaction
anova(model_no_interaction,model)

df_se <- summarySE(longdf, measurevar="Danger_Themselves", groupvars=c("Disorder","Affected"))
(df_se2 <- summarySE(longdf, measurevar="Danger_Themselves", groupvars=c("Disorder")))

pd <- position_dodge(0.1) # move them .05 to the left and right
# Line plot
#scale_color_manual(labels = c("Not Affected", "Affected"), values = c("blue", "red"))
#facet_wrap(~Disorder,dir="v")
windows();
g1 <- ggplot(df_se, aes(x=factor(Disorder, level=level_order), y=Danger_Themselves, group=Affected, colour=Affected))  + 
  scale_color_hue(labels = fam_tags) + 
  geom_errorbar(aes(ymin=Danger_Themselves-se, ymax=Danger_Themselves+se), colour="black", width=.1, position=pd, size=1) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=6) +
  theme(text = element_text(size = 16)) + 
  labs(title="Danger to Themselves Scores Across Diagnosis", subtitle="Scores out of 4, lower score means more danger") + 
  xlab("Disorder") +
  ylab("Danger to Themselves Score")
g1
#ggsave("danger_themselves_affected.png")

print('**********************Danger Others************************')

# Danger Others ANOVA
longdf <- df %>% select(PATID, Danger_Others_SZ, Danger_Others_BD, Danger_Others_ASD, Danger_Others_EP, Familiarity_Recode) 
longdf <- longdf[complete.cases(longdf),] %>% 
  melt(id = c('PATID', 'Familiarity_Recode'), measured = c('Danger_Others_SZ', 'Danger_Others_BD', 'Danger_Others_ASD', 'Danger_Others_EP'))
names(longdf) <- c('PATID', 'Affected', 'Disorder', 'Danger_Others')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
#longdf$Knowledge <- factor(longdf$Knowledge) only use this if doing clm
longdf$Affected <- factor(longdf$Affected)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_k <- longdf # ugly code!

# Trimmed mean ANOVA, with 20% trimmed means
bwtrim(formula = Danger_Others ~ Affected * Disorder, id = PATID, data = longdf_k)

# cumulative linked model
# make dv factor and ordered
longdf_k$Danger_Others <- factor(longdf_k$Danger_Others, ordered = TRUE)
longdf_k$Affected <- factor(longdf_k$Affected, ordered = TRUE)
model <- clmm(Danger_Others ~ Affected*Disorder + (1 | PATID), data = longdf_k)
summary(model)
#anova_result <- drop1(model, test = "Chisq"), print(anova_result)
model_no_interaction <- clmm(Danger_Others ~ Affected + Disorder + (1 | PATID), data = longdf_k)
model_no_disorder <- clmm(Danger_Others ~ Affected + (1 | PATID), data = longdf_k)
model_no_affected <- clmm(Danger_Others ~ Disorder + (1 | PATID), data = longdf_k)
# test disorder
anova(model_no_disorder,model)
# test affected
anova(model_no_affected,model)
# test interaction
anova(model_no_interaction,model)

df_se <- summarySE(longdf, measurevar="Danger_Others", groupvars=c("Disorder","Affected"))
(df_se2 <- summarySE(longdf, measurevar="Danger_Others", groupvars=c("Disorder")))

pd <- position_dodge(0.1) # move them .05 to the left and right
# Line plot
#scale_color_manual(labels = c("Not Affected", "Affected"), values = c("blue", "red"))
#facet_wrap(~Disorder,dir="v")
windows();
g2 <- ggplot(df_se, aes(x=factor(Disorder, level=level_order), y=Danger_Others, group=Affected, colour=Affected))  + 
  scale_color_hue(labels = fam_tags) + 
  geom_errorbar(aes(ymin=Danger_Others-se, ymax=Danger_Others+se), colour="black", width=.1, position=pd, size=1) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=6) +
  theme(text = element_text(size = 16)) + 
  labs(title="Danger to Others Scores Across Diagnosis", subtitle="Scores out of 4, lower score means more danger") + 
  xlab("Disorder") +
  ylab("Danger to Others Score")
g2
#ggsave("danger_others_affected.png")

#p <- plot_grid(g1, g2, labels = "AUTO", label_size=12)
#save_plot("danger_familiarity.jpg", p, ncol = 2, nrow = 1, dpi = 300)

#windows();
#p

################################################################
###################### MHKQ ################################
################################################################

print('**********************Danger Themselves************************')

# Conduct regular and trimmed mean ANOVAs, with line plots ----
# Danger Themselves ANOVA
longdf <- df %>% select(PATID, Danger_Themselves_SZ, Danger_Themselves_BD, Danger_Themselves_ASD, Danger_Themselves_EP, MHKQ_quantile) 
longdf <- longdf[complete.cases(longdf),] %>% 
  melt(id = c('PATID', 'MHKQ_quantile'), measured = c('Danger_Themselves_SZ', 'Danger_Themselves_BD', 
                                                           'Danger_Themselves_ASD', 'Danger_Themselves_EP'))
names(longdf) <- c('PATID', 'MHKQ', 'Disorder', 'Danger_Themselves')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
#longdf$Knowledge <- factor(longdf$Knowledge) only use this if doing clm
longdf$MHKQ <- factor(longdf$MHKQ)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_k <- longdf # ugly code!

df_se <- summarySE(longdf, measurevar="Danger_Themselves", groupvars=c("Disorder","MHKQ"))
(df_se2 <- summarySE(longdf, measurevar="Danger_Themselves", groupvars=c("Disorder")))

pd <- position_dodge(0.1) # move them .05 to the left and right
# Line plot
#scale_color_manual(labels = c("Not Affected", "Affected"), values = c("blue", "red"))
#facet_wrap(~Disorder,dir="v")
windows();
g3 <- ggplot(df_se, aes(x=factor(Disorder, level=level_order), y=Danger_Themselves, group=MHKQ, colour=MHKQ))  + 
  scale_color_hue(labels = MHKQ_tags) + 
  geom_errorbar(aes(ymin=Danger_Themselves-se, ymax=Danger_Themselves+se), colour="black", width=.1, position=pd, size=1) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=6) +
  theme(text = element_text(size = 16)) + 
  labs(title="Danger to Themselves Scores Across Diagnosis", subtitle="Scores out of 4, lower score means more danger") + 
  xlab("Disorder") +
  ylab("Danger to Themselves Score")
g3

print('**********************Danger Others************************')

# Danger Others ANOVA
longdf <- df %>% select(PATID, Danger_Others_SZ, Danger_Others_BD, Danger_Others_ASD, Danger_Others_EP, MHKQ_quantile) 
longdf <- longdf[complete.cases(longdf),] %>% 
  melt(id = c('PATID', 'MHKQ_quantile'), measured = c('Danger_Others_SZ', 'Danger_Others_BD', 'Danger_Others_ASD', 'Danger_Others_EP'))
names(longdf) <- c('PATID', 'MHKQ', 'Disorder', 'Danger_Others')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
#longdf$Knowledge <- factor(longdf$Knowledge) only use this if doing clm
longdf$MHKQ <- factor(longdf$MHKQ)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_k <- longdf # ugly code!

df_se <- summarySE(longdf, measurevar="Danger_Others", groupvars=c("Disorder","MHKQ"))
(df_se2 <- summarySE(longdf, measurevar="Danger_Others", groupvars=c("Disorder")))

pd <- position_dodge(0.1) # move them .05 to the left and right
# Line plot
#scale_color_manual(labels = c("Not Affected", "Affected"), values = c("blue", "red"))
#facet_wrap(~Disorder,dir="v")
windows();
g4 <- ggplot(df_se, aes(x=factor(Disorder, level=level_order), y=Danger_Others, group=MHKQ, colour=MHKQ))  + 
  scale_color_hue(labels = MHKQ_tags) + 
  geom_errorbar(aes(ymin=Danger_Others-se, ymax=Danger_Others+se), colour="black", width=.1, position=pd, size=1) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=6) +
  theme(text = element_text(size = 16)) + 
  labs(title="Danger to Others Scores Across Diagnosis", subtitle="Scores out of 4, lower score means more danger") + 
  xlab("Disorder") +
  ylab("Danger to Others Score")
g4

p <- plot_grid(g1, g2, g3, g4, labels = "AUTO", label_size=12)
save_plot("danger_familiarity_MHKQ.jpg", p, ncol = 2, nrow = 2, dpi = 300)

windows();
p
