#install.packages("ordinal")
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

# get rid of all NaNs? No, go by blocks of analyses
#df <- na.omit(df)
#sum(complete.cases(df)) # get complete rows in column

# get MHKQ quantiles
df$MHKQ_quantile <- ntile(df$MHKQ_score, n=4)

# only select columns relevant to the analysis
df <- df %>% dplyr::select(ResponseId, Total_knowledge_schizophrenia, Total_knowledge_bipolar, Total_knowledge_autism,
                           A5_QSI_CS_Schizo_total, A5_QSI_CS_Bipolar_total, A5_QSI_CS_Autism_total, A5_QSI_CS_Epilepsy_total,
                           Total_positive_behaviour_schizophrenia, Total_positive_behaviour_bipolar, Total_positive_behaviour_autism, Total_positive_behaviour_epilepsy,
                           Familiarity_recoded,MHKQ_quantile)

# rename variables
colnames(df) <- c("PATID","Knowledge_SZ","Knowledge_BD","Knowledge_ASD",
                  "Attitude_SZ","Attitude_BD","Attitude_ASD", "Attitude_EP",
                  "Behaviour_SZ","Behaviour_BD","Behaviour_ASD","Behaviour_EP",
                  "Familiarity_Recode","MHKQ_quantile")

fam_tags <- c("1 - No Exp", "2 - Family Member", "3 - Oneself") # "2 - Provide Services", 
#MHKQ_tags <- c("1 - Low MHKQ Score", "2", "3 - High MHKQ Score") 
MHKQ_tags <- c("1 - Low MHKQ Score", "2", "3", "4 - High MHKQ Score") 
level_order_no_ep <- c('ASD', 'BD', 'SZ')
level_order <- c('EP', 'ASD', 'BD', 'SZ')

################################################################
########################## PLOTS ###############################
################################################################

########################## KNOWLEDGE ###########################

print('**********************Knowledge************************')

# set up df
longdf <- df %>% select(PATID, Knowledge_SZ, Knowledge_BD, Knowledge_ASD, Familiarity_Recode) 
longdf <- longdf[complete.cases(longdf),] %>% 
  melt(id = c('PATID', 'Familiarity_Recode'), measured = c('Knowledge_SZ', 'Knowledge_BD', 'Knowledge_ASD'))
names(longdf) <- c('PATID', 'Affected', 'Disorder', 'Knowledge')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
longdf$Affected <- factor(longdf$Affected)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_k <- longdf

# Trimmed mean ANOVA, with 20% trimmed means
bwtrim(formula = Knowledge ~ Affected * Disorder, id = PATID, data = longdf_k)
# linear mixed effects model
summary(lme1 <-lmer(Knowledge ~ Affected*Disorder + (1|PATID), data = longdf_k, REML=FALSE, na.action = na.omit))
anova(lme1, type = 3)
# extract coefficients
coefs <- data.frame(coef(summary(lme1)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
# cumulative linked model
# make dv factor and ordered
longdf_k$Knowledge <- factor(longdf_k$Knowledge, ordered = TRUE)
longdf_k$Affected <- factor(longdf_k$Affected, ordered = TRUE)
model <- clmm(Knowledge ~ Affected*Disorder + (1 | PATID), data = longdf_k)
summary(model)
#anova_result <- drop1(model, test = "Chisq"), print(anova_result)
model_no_interaction <- clmm(Knowledge ~ Affected + Disorder + (1 | PATID), data = longdf_k)
model_no_disorder <- clmm(Knowledge ~ Affected + (1 | PATID), data = longdf_k)
model_no_affected <- clmm(Knowledge ~ Disorder + (1 | PATID), data = longdf_k)
# test disorder
anova(model_no_disorder,model)
# test affected
anova(model_no_affected,model)
# test interaction
anova(model_no_interaction,model)

df_se <- summarySE(longdf, measurevar="Knowledge", groupvars=c("Disorder","Affected"))
(df_se2 <- summarySE(longdf, measurevar="Knowledge", groupvars=c("Disorder")))

pd <- position_dodge(0.1) # move them .05 to the left and right
# Line plot
#scale_color_manual(labels = c("Not Affected", "Affected"), values = c("blue", "red"))
#facet_wrap(~Disorder,dir="v")
windows();
g1 <- ggplot(df_se, aes(x=Disorder, y=Knowledge, group=Affected, colour=Affected))  + 
  scale_color_hue(labels = fam_tags) + 
  geom_errorbar(aes(ymin=Knowledge-se, ymax=Knowledge+se), colour="black", width=.1, position=pd, size=1) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=6) +
  theme(text = element_text(size = 16)) + 
  labs(title="Knowledge Scores Across Diagnosis", subtitle="Scores out of 5")
g1
#ggsave("knowledge_affected_grad.png")

########################## ATTITUDE ###########################

print('**********************Attitude************************')

longdf <- df %>% select(PATID, Attitude_SZ, Attitude_BD, Attitude_ASD, Attitude_EP, Familiarity_Recode) 
longdf <- longdf[complete.cases(longdf),] %>% 
  melt(id = c('PATID', 'Familiarity_Recode'), measured = c('Attitude_SZ', 'Attitude_BD', 'Attitude_ASD', 'Attitude_EP'))
names(longdf) <- c('PATID', 'Affected', 'Disorder', 'Attitude')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
#longdf$Attitude <- factor(longdf$Attitude)
longdf$Affected <- factor(longdf$Affected)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_a <- longdf # ugly code!

# Trimmed mean ANOVA
bwtrim(formula = Attitude ~ Affected * Disorder, id = PATID, data = longdf_a)
# linear mixed effects model
summary(lme1 <- lmer(Attitude ~ Affected*Disorder + (1|PATID), data = longdf_a, REML=FALSE, na.action = na.omit))
anova(lme1, type = 3)
# extract coefficients
coefs <- data.frame(coef(summary(lme1)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
# cumulative linked model
# make dv factor and ordered
longdf_a$Attitude <- factor(longdf_a$Attitude, ordered = TRUE)
longdf_a$Affected <- factor(longdf_a$Affected, ordered = TRUE)
model <- clmm(Attitude ~ Affected*Disorder + (1 | PATID), data = longdf_a)
summary(model)
#anova_result <- drop1(model, test = "Chisq"), print(anova_result)
model_no_interaction <- clmm(Attitude ~ Affected + Disorder + (1 | PATID), data = longdf_a)
model_no_disorder <- clmm(Attitude ~ Affected + (1 | PATID), data = longdf_a)
model_no_affected <- clmm(Attitude ~ Disorder + (1 | PATID), data = longdf_a)
# test disorder
anova(model_no_disorder,model)
# test affected
anova(model_no_affected,model)
# test interaction
anova(model_no_interaction,model)

df_se <- summarySE(longdf, measurevar="Attitude", groupvars=c("Disorder","Affected"))
(df_se2 <- summarySE(longdf, measurevar="Attitude", groupvars=c("Disorder")))

pd <- position_dodge(0.1) # move them .05 to the left and right
level_order <- c('EP', 'ASD', 'BD', 'SZ')
# Line plot
windows();
g2 <- ggplot(df_se, aes(x=factor(Disorder, level=level_order), y=Attitude, group=Affected, colour=Affected))  + 
  scale_color_hue(labels = fam_tags) + 
  geom_errorbar(aes(ymin=Attitude-se, ymax=Attitude+se), colour="black", width=.1, position=pd, size=1) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=6) +
  theme(text = element_text(size = 16)) + 
  labs(title="Attitude Scores Across Diagnosis", subtitle="Scores out of 10") + 
  xlab("Disorder") +
  ylab("Attitude Score")
g2
#ggsave("attitude_affected_grad_ep.png")

########################## BEHAVIOUR ###########################

print('**********************Behaviour************************')

longdf <- df %>% select(PATID, Behaviour_SZ, Behaviour_BD, Behaviour_ASD, Behaviour_EP, Familiarity_Recode) 
longdf <- longdf[complete.cases(longdf),] %>% 
  melt(id = c('PATID', 'Familiarity_Recode'), measured = c('Behaviour_SZ', 'Behaviour_BD', 'Behaviour_ASD', 'Behaviour_EP'))
names(longdf) <- c('PATID', 'Affected', 'Disorder', 'Behaviour')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
longdf$Affected <- factor(longdf$Affected)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_b <- longdf # ugly code!

# Trimmed mean ANOVA
bwtrim(formula = Behaviour ~ Affected * Disorder, id = PATID, data = longdf_b)
# linear mixed effects model
summary(lme1 <- lmer(Behaviour ~ Affected*Disorder + (1|PATID), data = longdf_b, REML=FALSE, na.action = na.omit))
anova(lme1, type = 3)
# extract coefficients
coefs <- data.frame(coef(summary(lme1)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
# cumulative linked model
# make dv factor and ordered
longdf_b$Behaviour <- factor(longdf_b$Behaviour, ordered = TRUE)
longdf_b$Affected <- factor(longdf_b$Affected, ordered = TRUE)
model <- clmm(Behaviour ~ Affected*Disorder + (1 | PATID), data = longdf_b)
summary(model)
#anova_result <- drop1(model, test = "Chisq"), print(anova_result)
model_no_interaction <- clmm(Behaviour ~ Affected + Disorder + (1 | PATID), data = longdf_b)
model_no_disorder <- clmm(Behaviour ~ Affected + (1 | PATID), data = longdf_b)
model_no_affected <- clmm(Behaviour ~ Disorder + (1 | PATID), data = longdf_b)
# test disorder
anova(model_no_disorder,model)
# test affected
anova(model_no_affected,model)
# test interaction
anova(model_no_interaction,model)

df_se <- summarySE(longdf, measurevar="Behaviour", groupvars=c("Disorder","Affected"))
(df_se2 <- summarySE(longdf, measurevar="Behaviour", groupvars=c("Disorder")))

pd <- position_dodge(0.1) # move them .05 to the left and right
level_order <- c('EP', 'ASD', 'BD', 'SZ')
# Line plot
windows();
g3 <- ggplot(df_se, aes(x=factor(Disorder, level=level_order), y=Behaviour, group=Affected, colour=Affected))  + 
  scale_color_hue(labels = fam_tags) + 
  geom_errorbar(aes(ymin=Behaviour-se, ymax=Behaviour+se), colour="black", width=.1, position=pd, size=1) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=6) +
  theme(text = element_text(size = 16)) + 
  labs(title="Behaviour Scores Across Diagnosis", subtitle="Scores out of 9 (baseline = 3)") + 
  xlab("Disorder")
g3
#ggsave("behaviour_affected_grad_ep.png")

# turn off graphics
#graphics.off()

p <- plot_grid(g1, g2, g3, labels = "AUTO", label_size=12)
save_plot("vars_familiarity.jpg", p, ncol = 2, nrow = 2, dpi = 300)

windows();
p

# turn off graphics
graphics.off()







################################################################
########################## MHKQ ################################
################################################################

print('**********************Knowledge************************')

# Conduct regular and trimmed mean ANOVAs, with line plots ----
# Knowledge ANOVA
longdf <- df %>% select(PATID, Knowledge_SZ, Knowledge_BD, Knowledge_ASD, MHKQ_quantile) 
longdf <- longdf[complete.cases(longdf),] %>% 
  melt(id = c('PATID', 'MHKQ_quantile'), measured = c('Knowledge_SZ', 'Knowledge_BD', 'Knowledge_ASD'))
names(longdf) <- c('PATID', 'MHKQ_quantile', 'Disorder', 'Knowledge')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
#longdf$Knowledge <- factor(longdf$Knowledge) only use this if doing clm
longdf$MHKQ_quantile <- factor(longdf$MHKQ_quantile)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_k <- longdf # ugly code!

# Trimmed mean ANOVA, with 20% trimmed means
bwtrim(formula = Knowledge ~ MHKQ_quantile * Disorder, id = PATID, data = longdf_k)

# cumulative linked model
# make dv factor and ordered
longdf_k$Knowledge <- factor(longdf_k$Knowledge, ordered = TRUE)
longdf_k$MHKQ_quantile <- factor(longdf_k$MHKQ_quantile, ordered = TRUE)
model <- clmm(Knowledge ~ MHKQ_quantile*Disorder + (1 | PATID), data = longdf_k)
summary(model)
#anova_result <- drop1(model, test = "Chisq"), print(anova_result)
model_no_interaction <- clmm(Knowledge ~ MHKQ_quantile + Disorder + (1 | PATID), data = longdf_k)
model_no_disorder <- clmm(Knowledge ~ MHKQ_quantile + (1 | PATID), data = longdf_k)
model_no_MHKQ <- clmm(Knowledge ~ Disorder + (1 | PATID), data = longdf_k)
# test disorder
anova(model_no_disorder,model)
# test affected
anova(model_no_MHKQ,model)
# test interaction
anova(model_no_interaction,model)

df_se <- summarySE(longdf, measurevar="Knowledge", groupvars=c("Disorder","MHKQ_quantile"))
(df_se2 <- summarySE(longdf, measurevar="Knowledge", groupvars=c("Disorder")))

pd <- position_dodge(0.1) # move them .05 to the left and right
# Line plot
#scale_color_manual(labels = c("Not Affected", "Affected"), values = c("blue", "red"))
#facet_wrap(~Disorder,dir="v")
windows();
g1 <- ggplot(df_se, aes(x=factor(Disorder, level=level_order_no_ep), y=Knowledge, group=MHKQ_quantile, colour=MHKQ_quantile))  + 
  scale_color_hue(labels = MHKQ_tags) + 
  geom_errorbar(aes(ymin=Knowledge-se, ymax=Knowledge+se), colour="black", width=.1, position=pd, size=1) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=6) +
  theme(text = element_text(size = 16)) + 
  labs(title="Knowledge Scores Across Diagnosis", subtitle="Scores out of 5") +
  xlab("Disorder")
g1

print('**********************Attitude************************')

# Attitude ANOVA
longdf <- df %>% select(PATID, Attitude_SZ, Attitude_BD, Attitude_ASD, Attitude_EP, MHKQ_quantile) 
longdf <- longdf[complete.cases(longdf),] %>% 
  melt(id = c('PATID', 'MHKQ_quantile'), measured = c('Attitude_SZ', 'Attitude_BD', 'Attitude_ASD', 'Attitude_EP'))
names(longdf) <- c('PATID', 'MHKQ_quantile', 'Disorder', 'Attitude')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
#longdf$Attitude <- factor(longdf$Attitude)
longdf$MHKQ_quantile <- factor(longdf$MHKQ_quantile) # MAY NOT BE CORRECT TO MAKE THIS A FACTOR! IS ORDINAL
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_a <- longdf # ugly code!

# Trimmed mean ANOVA
bwtrim(formula = Attitude ~ MHKQ_quantile * Disorder, id = PATID, data = longdf_a)

# linear mixed effects model
summary(lme1 <- lmer(Attitude ~ MHKQ_quantile*Disorder + (1|PATID), data = longdf_a, REML=FALSE, na.action = na.omit))
anova(lme1, type = 3)
# cumulative linked model
# make dv factor and ordered
longdf_a$Attitude <- factor(longdf_a$Attitude, ordered = TRUE)
longdf_a$MHKQ_quantile <- factor(longdf_a$MHKQ_quantile, ordered = TRUE)
model <- clmm(Attitude ~ MHKQ_quantile*Disorder + (1 | PATID), data = longdf_a)
summary(model)
#anova_result <- drop1(model, test = "Chisq"), print(anova_result)
model_no_interaction <- clmm(Attitude ~ MHKQ_quantile + Disorder + (1 | PATID), data = longdf_a)
model_no_disorder <- clmm(Attitude ~ MHKQ_quantile + (1 | PATID), data = longdf_a)
model_no_MHKQ <- clmm(Attitude ~ Disorder + (1 | PATID), data = longdf_a)
# test disorder
anova(model_no_disorder,model)
# test affected
anova(model_no_MHKQ,model)
# test interaction
anova(model_no_interaction,model)

df_se <- summarySE(longdf, measurevar="Attitude", groupvars=c("Disorder","MHKQ_quantile"))
(df_se2 <- summarySE(longdf, measurevar="Attitude", groupvars=c("Disorder")))

pd <- position_dodge(0.1) # move them .05 to the left and right
# Line plot
windows();
g2 <- ggplot(df_se, aes(x=factor(Disorder, level=level_order), y=Attitude, group=MHKQ_quantile, colour=MHKQ_quantile))  + 
  scale_color_hue(labels = MHKQ_tags) + 
  geom_errorbar(aes(ymin=Attitude-se, ymax=Attitude+se), colour="black", width=.1, position=pd, size=1) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=6) +
  theme(text = element_text(size = 16)) + 
  labs(title="Attitude Scores Across Diagnosis", subtitle="Scores out of 10") + 
  xlab("Disorder")
g2

print('**********************Behaviour************************')

# Behaviour ANOVA
longdf <- df %>% select(PATID, Behaviour_SZ, Behaviour_BD, Behaviour_ASD, Behaviour_EP, MHKQ_quantile) 
longdf <- longdf[complete.cases(longdf),] %>% 
  melt(id = c('PATID', 'MHKQ_quantile'), measured = c('Behaviour_SZ', 'Behaviour_BD', 'Behaviour_ASD', 'Behaviour_EP'))
names(longdf) <- c('PATID', 'MHKQ_quantile', 'Disorder', 'Behaviour')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
longdf$MHKQ_quantile <- factor(longdf$MHKQ_quantile)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_b <- longdf # ugly code!

# Trimmed mean ANOVA
bwtrim(formula = Behaviour ~ MHKQ_quantile * Disorder, id = PATID, data = longdf_b)

# cumulative linked model
# make dv factor and ordered
longdf_b$Behaviour <- factor(longdf_b$Behaviour, ordered = TRUE)
longdf_b$MHKQ_quantile <- factor(longdf_b$MHKQ_quantile, ordered = TRUE)
model <- clmm(Behaviour ~ MHKQ_quantile*Disorder + (1 | PATID), data = longdf_b)
summary(model)
#anova_result <- drop1(model, test = "Chisq"), print(anova_result)
model_no_interaction <- clmm(Behaviour ~ MHKQ_quantile + Disorder + (1 | PATID), data = longdf_b)
model_no_disorder <- clmm(Behaviour ~ MHKQ_quantile + (1 | PATID), data = longdf_b)
model_no_MHKQ <- clmm(Behaviour ~ Disorder + (1 | PATID), data = longdf_b)
# test disorder
anova(model_no_disorder,model)
# test MHKQ
anova(model_no_MHKQ,model)
# test interaction
anova(model_no_interaction,model)

df_se <- summarySE(longdf, measurevar="Behaviour", groupvars=c("Disorder","MHKQ_quantile"))
(df_se2 <- summarySE(longdf, measurevar="Behaviour", groupvars=c("Disorder")))

pd <- position_dodge(0.1) # move them .05 to the left and right
# Line plot
windows();
g3 <- ggplot(df_se, aes(x=factor(Disorder, level=level_order), y=Behaviour, group=MHKQ_quantile, colour=MHKQ_quantile))  + 
  scale_color_hue(labels = MHKQ_tags) + 
  geom_errorbar(aes(ymin=Behaviour-se, ymax=Behaviour+se), colour="black", width=.1, position=pd, size=1) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=6) +
  theme(text = element_text(size = 16)) + 
  labs(title="Behaviour Scores Across Diagnosis", subtitle="Scores out of 9 (baseline = 3)") + 
  xlab("Disorder")
g3
#ggsave("behaviour_affected_MHKQ.png")

p <- plot_grid(g1, g2, g3, labels = "AUTO", label_size=12)
save_plot("vars_MHKQ.jpg", p, ncol = 2, nrow = 2, dpi = 300)

windows();
p















#################################################################################
############################ linear regressions #################################
#################################################################################

longdfk <- df %>% select(PATID, Knowledge_SZ, Knowledge_BD, Knowledge_ASD, Familiarity_Recode) 
longdfk <- melt(longdfk, id = c('PATID', 'Familiarity_Recode'), measured = c('Knowledge_SZ', 'Knowledge_BD', 'Knowledge_ASD'))
names(longdfk) <- c('PATID', 'Affected', 'Disorder', 'Knowledge')
longdfk$Disorder <- gsub("^.*_", "", longdfk$Disorder)
longdfk$Affected <- factor(longdfk$Affected)
longdfk$Disorder <- factor(longdfk$Disorder)

longdfa <- df %>% select(PATID, Attitude_SZ, Attitude_BD, Attitude_ASD) 
longdfa <- melt(longdfa, id = c('PATID'), measured = c('Attitude_SZ', 'Attitude_BD', 'Attitude_ASD'))
names(longdfa) <- c('PATID', 'Disorder', 'Attitude')
longdfa$Disorder <- gsub("^.*_", "", longdfa$Disorder)
longdfa$Disorder <- factor(longdfa$Disorder)

longdfb <- df %>% select(PATID, Behaviour_SZ, Behaviour_BD, Behaviour_ASD) 
longdfb <- melt(longdfb, id = c('PATID'), measured = c('Behaviour_SZ', 'Behaviour_BD', 'Behaviour_ASD'))
names(longdfb) <- c('PATID', 'Disorder', 'Behaviour')
longdfb$Disorder <- gsub("^.*_", "", longdfb$Disorder)
longdfb$Disorder <- factor(longdfb$Disorder)

longdf_all <- merge(longdfk, longdfa, by = c("PATID", "Disorder"))
longdf_all <- merge(longdf_all, longdfb, by = c("PATID", "Disorder"))
longdf_all <- longdf_all[order(longdf_all$PATID), ]
# get rid of all NaNs?
dim(longdf_all)
#longdf_all <- na.omit(longdf_all)
dim(longdf_all)

# Knowledge vs Attitude
summary(lme1 <-lmer(Attitude ~ Knowledge*Affected + (1|Disorder), data = longdf_all, REML=FALSE, na.action = na.omit)) #  + (1|PATID)
# extract coefficients
coefs <- data.frame(coef(summary(lme1)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
# Bootstrapping
FUN <- function(fit) {
  #return(coef(summary(fit))[, "t value"]) # fixef(fit)
  return(fixef(fit))
}
#summary(result <- bootMer(lme1, FUN, use.u=TRUE, type = c("parametric"), nsim = 2000))
#df_boot <- result[2]$t[, 2]
#hist(df_boot)

#(ci <- confint(lme1, level = 0.95, method = "boot", nsim = 2000, parm = 'beta_'))

# Knowledge vs Behaviour
summary(lme2 <-lmer(Behaviour ~ Knowledge*Affected + (1|Disorder), data = longdf_all, REML=FALSE, na.action = na.omit))
# extract coefficients
coefs <- data.frame(coef(summary(lme2)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

#(ci <- confint(lme2, level = 0.95, method = "boot", nsim = 2000, parm = 'beta_'))

# Behaviour vs Attitude
summary(lme3 <-lmer(Behaviour ~ Attitude*Affected + (1|Disorder), data = longdf_all, REML=FALSE, na.action = na.omit))
# extract coefficients
coefs <- data.frame(coef(summary(lme3)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

#(ci <- confint(lme3, level = 0.95, method = "boot", nsim = 2000, parm = 'beta_'))

#summary(lme1 <-lmer(Knowledge ~ Disorder + (1|PATID), data = longdfk, REML=FALSE, na.action = na.omit))
#summary(lme2 <-lmer(Attitude ~ Disorder + (1|PATID), data = longdfa, REML=FALSE, na.action = na.omit))
#summary(lme3 <-lmer(Behaviour ~ Disorder + (1|PATID), data = longdfb, REML=FALSE, na.action = na.omit))

# Heatmaps!
# Get color maps
#brewer.pal(n = 3, name = "YlOrRd")
brewer.pal(n = 5, name = "RdYlBu")

#ggplot(df_ka, aes(Knowledge, Attitude)) +
#  geom_tile(aes(fill = Freq), colour = "black") +
#  scale_fill_gradient(low = "white", high = "steelblue") # , limits = c(0,90)

# Knowledge vs Attitude
df_short <- longdf_all %>% select(Knowledge, Attitude)
df_ka <- as.data.frame(table(df_short))

theme_nogrid <- function (base_size = 12, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(panel.grid = element_blank())   
}

# do with tiles
windows();
g1 <- ggplot(df_ka, aes(Knowledge, Attitude)) +
  geom_tile(aes(fill = Freq), colour = "transparent") + 
  #scale_fill_gradientn(colours = brewer.pal(4,"YlGnBu"), values = scales::rescale(c(0, 0.25, 0.75, 1))) + 
  scale_fill_gradientn(colours = c("#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61"), values = scales::rescale(c(0, 0.25, 0.75, 2))) +
  labs(title="Knowledge vs Attitude") + 
  theme_nogrid() + theme(text = element_text(size = 16))
g1
#ggsave("knowledge_vs_attitude.png")

# Knowledge vs Behaviour
df_short <- longdf_all %>% select(Knowledge, Behaviour)
df_kb <- as.data.frame(table(df_short))

#windows();
g2 <- ggplot(df_kb, aes(Knowledge, Behaviour)) +
  geom_tile(aes(fill = Freq), colour = "transparent") + 
  scale_fill_gradientn(colours = c("#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61"), values = scales::rescale(c(0, 0.25, 0.75, 2))) +
  labs(title="Knowledge vs Behaviour") + 
  theme_nogrid() + theme(text = element_text(size = 16))
#ggsave("knowledge_vs_behaviour.png")

# Attitude vs Behaviour
df_short <- longdf_all %>% select(Attitude, Behaviour)
df_ab <- as.data.frame(table(df_short))

#windows();
g3 <- ggplot(df_ab, aes(Attitude, Behaviour)) +
  geom_tile(aes(fill = Freq), colour = "transparent") + 
  scale_fill_gradientn(colours = c("#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61"), values = scales::rescale(c(0, 0.25, 0.75, 2))) +
  labs(title="Behaviour vs Attitude") + 
  theme_nogrid() + theme(text = element_text(size = 16))
#ggsave("attitude_vs_behaviour.png")

p <- plot_grid(g1, g2, g3, labels = "AUTO", label_size=12)
#save_plot("scatter_across_diag2.jpg", p, ncol = 2, nrow = 2, dpi = 300)

windows();
p

# turn off graphics
graphics.off()


