# install packages ----
packages <- c("WRS2", "tidyr", "ggplot2", "haven", "plyr", "dplyr", "reshape", "ez", "lme4", "stringr", "cowplot", "tidyverse", "RColorBrewer", 
              "mediation", "ordinal", "boot")
#"lmerTest", "ordinal",
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

df <- df %>% dplyr::select(ResponseId, Total_knowledge_schizophrenia, Total_knowledge_bipolar, Total_knowledge_autism,
                           A5_QSI_CS_Schizo_total, A5_QSI_CS_Bipolar_total, A5_QSI_CS_Autism_total, A5_QSI_CS_Epilepsy_total,
                           Total_positive_behaviour_schizophrenia, Total_positive_behaviour_bipolar, Total_positive_behaviour_autism, Total_positive_behaviour_epilepsy,
                           EOPD_1, EOPD_2, EOPD_3, EOPD_4, EOPD_5, EOPD_6, EOPD_7, EOPD_8, Familiarity_recoded, MHKQ_quantile)

# rename variables
colnames(df) <- c("PATID","Knowledge_SZ","Knowledge_BD","Knowledge_ASD",
                  "Attitude_SZ","Attitude_BD","Attitude_ASD","Attitude_EP",
                  "Behaviour_SZ","Behaviour_BD","Behaviour_ASD","Behaviour_EP",
                  "Danger_Themselves_SZ","Danger_Others_SZ","Danger_Themselves_BD",
                  "Danger_Others_BD","Danger_Themselves_ASD","Danger_Others_ASD","Danger_Themselves_EP","Danger_Others_EP",
                  "Familiarity_Recode","MHKQ_quantile")

fam_tags <- c("1 - No Exp", "2 - Family Member", "3 - Oneself") # "2 - Provide Services", 
MHKQ_tags <- c("1 - Low MHKQ Score", "2", "3", "4 - High MHKQ Score") 

print('******************************************************')
print('********************Behaviour Moderated Mediations*********************')
print('******************************************************')

longdf <- df %>% dplyr::select(PATID, Behaviour_SZ, Behaviour_BD, Behaviour_ASD, Behaviour_EP)
longdf <- melt(id = c('PATID'), measured = c('Behaviour_SZ', 'Behaviour_BD', 'Behaviour_ASD', 'Behaviour_EP'), data = longdf)
names(longdf) <- c('PATID', 'Disorder', 'Behaviour')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_b <- longdf

longdf <- df %>% dplyr::select(PATID, Danger_Themselves_SZ, Danger_Themselves_BD, Danger_Themselves_ASD, Danger_Themselves_EP)
longdf <- melt(id = c('PATID'), measured = c('Danger_Themselves_SZ', 'Danger_Themselves_BD', 'Danger_Themselves_ASD', 'Danger_Themselves_EP'), data = longdf)
names(longdf) <- c('PATID', 'Disorder', 'Danger_Themselves')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_dt <- longdf

longdf <- df %>% dplyr::select(PATID, Danger_Others_SZ, Danger_Others_BD, Danger_Others_ASD, Danger_Others_EP)
longdf <- melt(id = c('PATID'), measured = c('Danger_Others_SZ', 'Danger_Others_BD', 'Danger_Others_ASD', 'Danger_Others_EP'), data = longdf)
names(longdf) <- c('PATID', 'Disorder', 'Danger_Others')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_do <- longdf

longdf_mhkq <- df %>% dplyr::select(PATID, MHKQ_quantile)
longdf_fam <- df %>% dplyr::select(PATID, Familiarity_Recode)

#############################################################
######################## MHKQ ###############################
#############################################################

# define mediation: the effect of disorder on Behaviour is mediated by Danger_Others, moderated by MHKQ
alldf <- merge(x = longdf_b, y = longdf_do, by = c("PATID", "Disorder"), all = TRUE)
alldf <- merge(x = alldf, y = longdf_mhkq, by = c("PATID"), all = TRUE)
alldf <- alldf[complete.cases(alldf),] 

# do real mediation first, across all levels of MHKQ

# mediator model
mediator_model <- lmer(Danger_Others ~ Disorder * MHKQ_quantile + (1 | PATID), data = alldf)
# outcome_model
outcome_model <- lmer(Behaviour ~ Danger_Others * MHKQ_quantile + Disorder * MHKQ_quantile + (1 | PATID), data = alldf)

med_results_bymod <- list()
for (x in 1:4) {
  # Mediation analysis with moderation for each level of MHKQ
  print(x)
  med_results <- mediate(
    model.m = mediator_model,
    model.y = outcome_model,
    treat = "Disorder",
    mediator = "Danger_Others",
    covariates = list(MHKQ_quantile = x), # Specify the level of the moderator
    control.value = "ASD", 
    treat.value = "SZ"
  )
  summary(med_results)
  med_results_bymod[[x]] <- med_results
}

acme_values_real <- numeric(4)
acme_ci_values <- list(4)
acme_ci <- numeric(4)
for (x in 1:4) {
  print(summary(med_results_bymod[[x]]))
  acme_values_real[x] <- med_results_bymod[[x]]$d1
  acme_ci_values[[x]] <- med_results_bymod[[x]]$d1.ci
  acme_ci[x] <- unname(abs(acme_ci_values[[x]][1]-acme_ci_values[[x]][2]))/2
}

df_plot <- data.frame(MHKQ_tags, acme_values_real, acme_ci)

pd <- position_dodge(0.1) # move them .05 to the left and right
windows();
g1 <- ggplot(df_plot, aes(x=MHKQ_tags, y=acme_values_real))  + 
  geom_errorbar(aes(ymin=acme_values_real-acme_ci, ymax=acme_values_real+acme_ci), colour="black", width=.1, position=pd, size=1) +
  geom_point(position=pd, size=6) +
  theme(text = element_text(size = 16)) + 
  labs(title="Mediation Estimate Across MHKQ Score", subtitle="More negative mediation effect = stronger") +
  xlab("MHKQ Score") +
  ylab("Mediation Estimate")
g1
#ggsave("danger_themselves_affected.png")

# Create a function to calculate the moderated mediation effect
moderated_mediation <- function(data, indices, levels_W) {
  # Resample the data
  d <- data[indices, ]
  
  # Initialize a vector to store ACMEs
  acme_values <- numeric(length(levels_W))
  
  # Loop through levels of the moderator
  for (i in 1:length(levels_W)) {
    current_level <- levels_W[i]
    
    # Fit models
    # mediator model
    mediator_model <- lmer(Danger_Others ~ Disorder * MHKQ_quantile + (1 | PATID), data = d)
    # outcome_model
    outcome_model <- lmer(Behaviour ~ Danger_Others * MHKQ_quantile + Disorder * MHKQ_quantile + (1 | PATID), data = d)
    
    #mediator_model <- lmer(M ~ X * W + (1 | RandomEffect), data = d)
    #outcome_model <- lmer(Y ~ M * W + X * W + (1 | RandomEffect), data = d)
    
    # Perform mediation analysis
    med_results <- {
      mediate(
        model.m = mediator_model,
        model.y = outcome_model,
        treat = "Disorder",
        mediator = "Danger_Others",
        covariates = list(MHKQ_quantile = current_level),  # Ensure the covariate is correctly specified
        control.value = "ASD", 
        treat.value = "SZ",
        sims = 100  # Increase the number of simulations for better estimates
      )
    }
    
    # Store ACME value
    acme_values[i] <- med_results$d1
  }
  # print(acme_values)
  # Calculate difference in ACME across levels (moderated mediation effect)
  # You could calculate any ANOVA statistic here, e.g., sum of squares between or F-statistic
  return(var(acme_values))  # Variance across levels as a simple example
  #return(acme_values[4]-acme_values[1]) # top minus bottom
}

# Levels of the moderator
levels_W <- sort(unique(alldf$MHKQ_quantile), decreasing = FALSE)

# Perform bootstrapping
set.seed(123)
boot_results <- boot(data = alldf, statistic = moderated_mediation, R = 50, levels_W = levels_W)

# Summary of bootstrapped results
boot_results

# You can also derive p-values based on the bootstrap results
windows();
res <- hist(boot_results$t)
var(acme_values_real)

#############################################################
######################## FAMILIARITY ###############################
#############################################################

# Familiarity
alldf <- merge(x = longdf_b, y = longdf_do, by = c("PATID", "Disorder"), all = TRUE)
alldf <- merge(x = alldf, y = longdf_fam, by = c("PATID"), all = TRUE)
alldf <- alldf[complete.cases(alldf),] 

# mediator model
mediator_model <- lmer(Danger_Others ~ Disorder * Familiarity_Recode + (1 | PATID), data = alldf)
# outcome_model
outcome_model <- lmer(Behaviour ~ Danger_Others * Familiarity_Recode + Disorder * Familiarity_Recode + (1 | PATID), data = alldf)

med_results_bymod2 <- list()
for (x in 1:3) {
  # Mediation analysis with moderation for each level of MHKQ
  print(x)
  med_results <- mediate(
    model.m = mediator_model,
    model.y = outcome_model,
    treat = "Disorder",
    mediator = "Danger_Others",
    covariates = list(Familiarity_Recode = x), # Specify the level of the moderator
    control.value = "ASD", 
    treat.value = "SZ"
  )
  summary(med_results)
  med_results_bymod2[[x]] <- med_results
}

acme_values_real2 <- numeric(3)
acme_ci_values2 <- list(3)
acme_ci2 <- numeric(3)
for (x in 1:3) {
  print(summary(med_results_bymod2[[x]]))
  acme_values_real2[x] <- med_results_bymod2[[x]]$d1
  acme_ci_values2[[x]] <- med_results_bymod2[[x]]$d1.ci
  acme_ci2[x] <- unname(abs(acme_ci_values2[[x]][1]-acme_ci_values2[[x]][2]))/2
}

df_plot <- data.frame(fam_tags, acme_values_real2, acme_ci2)

pd <- position_dodge(0.1) # move them .05 to the left and right
windows();
g2 <- ggplot(df_plot, aes(x=fam_tags, y=acme_values_real2))  + 
  geom_errorbar(aes(ymin=acme_values_real2-acme_ci2, ymax=acme_values_real2+acme_ci2), colour="black", width=.1, position=pd, size=1) +
  geom_point(position=pd, size=6) +
  theme(text = element_text(size = 16)) + 
  labs(title="Mediation Estimate Across Familiarity", subtitle="More negative mediation effect = stronger") +
  xlab("Familiarity") +
  ylab("Mediation Estimate")
g2
#ggsave("danger_themselves_affected.png")




# Create a function to calculate the moderated mediation effect
moderated_mediation <- function(data, indices, levels_W) {
  # Resample the data
  d <- data[indices, ]
  
  # Initialize a vector to store ACMEs
  acme_values <- numeric(length(levels_W))
  
  # Loop through levels of the moderator
  for (i in 1:length(levels_W)) {
    current_level <- levels_W[i]
    
    # Fit models
    # mediator model
    mediator_model <- lmer(Danger_Others ~ Disorder * Familiarity_Recode + (1 | PATID), data = d)
    # outcome_model
    outcome_model <- lmer(Behaviour ~ Danger_Others * Familiarity_Recode + Disorder * Familiarity_Recode + (1 | PATID), data = d)
    
    #mediator_model <- lmer(M ~ X * W + (1 | RandomEffect), data = d)
    #outcome_model <- lmer(Y ~ M * W + X * W + (1 | RandomEffect), data = d)
    
    # Perform mediation analysis
    med_results <- {
      mediate(
        model.m = mediator_model,
        model.y = outcome_model,
        treat = "Disorder",
        mediator = "Danger_Others",
        covariates = list(Familiarity_Recode = current_level),  # Ensure the covariate is correctly specified
        control.value = "ASD", 
        treat.value = "SZ",
        sims = 100  # Increase the number of simulations for better estimates
      )
    }
    
    # Store ACME value
    acme_values[i] <- med_results$d1
  }
  # print(acme_values)
  # Calculate difference in ACME across levels (moderated mediation effect)
  # You could calculate any ANOVA statistic here, e.g., sum of squares between or F-statistic
  return(var(acme_values))  # Variance across levels as a simple example
  #return(acme_values[4]-acme_values[1]) # top minus bottom
}

# Levels of the moderator
levels_W <- sort(unique(alldf$Familiarity_Recode), decreasing = FALSE)

# Perform bootstrapping
set.seed(123)
boot_results2 <- boot(data = alldf, statistic = moderated_mediation, R = 50, levels_W = levels_W)

# Summary of bootstrapped results
boot_results2

# You can also derive p-values based on the bootstrap results
windows();
res <- hist(boot_results2$t)
var(acme_values_real2)

save(boot_results, boot_results2, file = "bootstrap_results.Rdata")

p <- plot_grid(g1, g2, labels = "AUTO", label_size=12)
#save_plot("danger_mediations.jpg", p, ncol = 2, nrow = 1, dpi = 300)

windows();
p





































################################################################
###################### ANOVAs ##################################
################################################################

# Conduct regular and trimmed mean ANOVAs, with line plots ----
# Danger Themselves ANOVA
longdf <- df %>% dplyr::select(PATID, Danger_Themselves_SZ, Danger_Themselves_BD, Danger_Themselves_ASD, Familiarity_Recode) 
longdf <- longdf[complete.cases(longdf),] %>% 
  melt(id = c('PATID', 'Familiarity_Recode'), measured = c('Danger_Themselves_SZ', 'Danger_Themselves_BD', 'Danger_Themselves_ASD'))
names(longdf) <- c('PATID', 'Affected', 'Disorder', 'Danger_Themselves')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
#longdf$Knowledge <- factor(longdf$Knowledge) only use this if doing clm
longdf$Affected <- factor(longdf$Affected)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_k <- longdf # ugly code!

print('**********************Danger Themselves************************')
# Trimmed mean ANOVA, with 20% trimmed means
bwtrim(formula = Danger_Themselves ~ Affected * Disorder, id = PATID, data = longdf_k)

df_se <- summarySE(longdf_k, measurevar="Danger_Themselves", groupvars=c("Disorder","Affected"))
(df_se2 <- summarySE(longdf, measurevar="Danger_Themselves", groupvars=c("Disorder")))

pd <- position_dodge(0.1) # move them .05 to the left and right
# Line plot
#scale_color_manual(labels = c("Not Affected", "Affected"), values = c("blue", "red"))
#facet_wrap(~Disorder,dir="v")
windows();
g1 <- ggplot(df_se, aes(x=Disorder, y=Danger_Themselves, group=Affected, colour=Affected))  + 
  scale_color_hue(labels = fam_tags) + 
  geom_errorbar(aes(ymin=Danger_Themselves-se, ymax=Danger_Themselves+se), colour="black", width=.1, position=pd, size=1) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=6) +
  theme(text = element_text(size = 16)) + 
  labs(title="Danger to Themselves Scores Across Diagnosis", subtitle="Scores out of 4, lower score means more danger")
g1
#ggsave("danger_themselves_affected.png")

# Danger Others ANOVA
longdf <- df %>% dplyr::select(PATID, Danger_Others_SZ, Danger_Others_BD, Danger_Others_ASD, Familiarity_Recode) 
longdf <- longdf[complete.cases(longdf),] %>% 
  melt(id = c('PATID', 'Familiarity_Recode'), measured = c('Danger_Others_SZ', 'Danger_Others_BD', 'Danger_Others_ASD'))
names(longdf) <- c('PATID', 'Affected', 'Disorder', 'Danger_Others')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
#longdf$Knowledge <- factor(longdf$Knowledge) only use this if doing clm
longdf$Affected <- factor(longdf$Affected)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_k <- longdf # ugly code!

print('**********************Danger Others************************')
# Trimmed mean ANOVA, with 20% trimmed means
bwtrim(formula = Danger_Others ~ Affected * Disorder, id = PATID, data = longdf_k)

df_se <- summarySE(longdf_k, measurevar="Danger_Others", groupvars=c("Disorder","Affected"))
(df_se2 <- summarySE(longdf, measurevar="Danger_Others", groupvars=c("Disorder")))

pd <- position_dodge(0.1) # move them .05 to the left and right
# Line plot
#scale_color_manual(labels = c("Not Affected", "Affected"), values = c("blue", "red"))
#facet_wrap(~Disorder,dir="v")
windows();
g2 <- ggplot(df_se, aes(x=Disorder, y=Danger_Others, group=Affected, colour=Affected))  + 
  scale_color_hue(labels = fam_tags) + 
  geom_errorbar(aes(ymin=Danger_Others-se, ymax=Danger_Others+se), colour="black", width=.1, position=pd, size=1) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=6) +
  theme(text = element_text(size = 16)) + 
  labs(title="Danger to Others Scores Across Diagnosis", subtitle="Scores out of 4, lower score means more danger")
g2
#ggsave("danger_others_affected.png")

#df <- df[df$Familiarity_Recode==3,]

print('******************************************************')
print('********************Behaviour Mediations*********************')
print('******************************************************')

longdf <- df %>% dplyr::select(PATID, Behaviour_SZ, Behaviour_BD, Behaviour_ASD, Behaviour_EP)
longdf <- melt(id = c('PATID'), measured = c('Behaviour_SZ', 'Behaviour_BD', 'Behaviour_ASD', 'Behaviour_EP'), data = longdf)
names(longdf) <- c('PATID', 'Disorder', 'Behaviour')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_b <- longdf

longdf <- df %>% dplyr::select(PATID, Danger_Themselves_SZ, Danger_Themselves_BD, Danger_Themselves_ASD, Danger_Themselves_EP)
longdf <- melt(id = c('PATID'), measured = c('Danger_Themselves_SZ', 'Danger_Themselves_BD', 'Danger_Themselves_ASD', 'Danger_Themselves_EP'), data = longdf)
names(longdf) <- c('PATID', 'Disorder', 'Danger_Themselves')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_dt <- longdf

longdf <- df %>% dplyr::select(PATID, Danger_Others_SZ, Danger_Others_BD, Danger_Others_ASD, Danger_Others_EP)
longdf <- melt(id = c('PATID'), measured = c('Danger_Others_SZ', 'Danger_Others_BD', 'Danger_Others_ASD', 'Danger_Others_EP'), data = longdf)
names(longdf) <- c('PATID', 'Disorder', 'Danger_Others')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_do <- longdf

print('******************************************************')
print('********************Danger Themselves Mediation*********************')
print('******************************************************')

# define mediation: the effect of disorder on Behaviour is mediated by Danger_Themselves
alldf <- merge(x = longdf_b, y = longdf_dt, by = c("PATID", "Disorder"), all = TRUE)
alldf <- alldf[complete.cases(alldf),] 

# Example:
model.fit <-lmer(Danger_Themselves ~ Disorder + (1|PATID), 
                 data = alldf, REML=FALSE, na.action = na.omit)
anova(model.fit)
# extract coefficients
coefs <- data.frame(coef(summary(model.fit)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

############## Disorder -> Danger_Themselves -> Behaviour ####################
# fit total effect, X -> Y
print('Total Effect')
total.fit <- lmer(Behaviour ~ Disorder + (1|PATID), data = alldf, REML=FALSE, na.action = na.omit)
summary(total.fit)
anova(total.fit)

# fit mediation effect, X -> M
print('A path')
med.fit <- lmer(Danger_Themselves ~ Disorder + (1|PATID), data = alldf, REML=FALSE, na.action = na.omit)
summary(med.fit)
anova(med.fit)

# fit outcome effect, M -> Y
print('B path')
out.fit <- lmer(Behaviour ~ Danger_Themselves + Disorder + (1|PATID), data = alldf, REML=FALSE, na.action = na.omit)
summary(out.fit)
anova(out.fit)

# These fitted objects can then be fed into the mediate function in the usual manner.
print('Mediation')
medmod <- mediate(med.fit, out.fit, treat = "Disorder", mediator = "Danger_Themselves", control.value = "EP", treat.value = "SZ")
summary(medmod)

print('******************************************************')
print('********************Danger Others Mediation*********************')
print('******************************************************')

# define mediation: the effect of disorder on Behaviour is mediated by Danger_Themselves
alldf <- merge(x = longdf_b, y = longdf_do, by = c("PATID", "Disorder"), all = TRUE)
alldf <- alldf[complete.cases(alldf),] 

# Example:
model.fit <-lmer(Danger_Others ~ Disorder + (1|PATID), 
                 data = alldf, REML=FALSE, na.action = na.omit)
anova(model.fit)
# extract coefficients
coefs <- data.frame(coef(summary(model.fit)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

############## Disorder -> Danger_Others -> Behaviour ####################
# fit total effect, X -> Y
print('Total Effect')
total.fit <- lmer(Behaviour ~ Disorder + (1|PATID), data = alldf, REML=FALSE, na.action = na.omit)
summary(total.fit)
anova(total.fit)

# fit mediation effect, X -> M
print('A path')
med.fit <- lmer(Danger_Others ~ Disorder + (1|PATID), data = alldf, REML=FALSE, na.action = na.omit)
summary(med.fit)
anova(med.fit)

# fit outcome effect, M -> Y
print('B path')
out.fit <- lmer(Behaviour ~ Danger_Others + Disorder + (1|PATID), data = alldf, REML=FALSE, na.action = na.omit)
summary(out.fit)
anova(out.fit)

# These fitted objects can then be fed into the mediate function in the usual manner.
#print('Mediation')
#medmod <- mediate(med.fit, out.fit, treat = "Disorder", mediator = "Danger_Others", control.value = "EP", treat.value = "SZ")
#summary(medmod)

# try ASD as control contrast, perhaps too little variability in EP
print('Mediation')
medmod <- mediate(med.fit, out.fit, treat = "Disorder", mediator = "Danger_Others", control.value = "ASD", treat.value = "SZ")
summary(medmod)

print('******************************************************')
print('********************Behaviour Moderated Mediations*********************')
print('******************************************************')

longdf <- df %>% dplyr::select(PATID, Behaviour_SZ, Behaviour_BD, Behaviour_ASD, Behaviour_EP)
longdf <- melt(id = c('PATID'), measured = c('Behaviour_SZ', 'Behaviour_BD', 'Behaviour_ASD', 'Behaviour_EP'), data = longdf)
names(longdf) <- c('PATID', 'Disorder', 'Behaviour')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_b <- longdf

longdf <- df %>% dplyr::select(PATID, Danger_Themselves_SZ, Danger_Themselves_BD, Danger_Themselves_ASD, Danger_Themselves_EP)
longdf <- melt(id = c('PATID'), measured = c('Danger_Themselves_SZ', 'Danger_Themselves_BD', 'Danger_Themselves_ASD', 'Danger_Themselves_EP'), data = longdf)
names(longdf) <- c('PATID', 'Disorder', 'Danger_Themselves')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_dt <- longdf

longdf <- df %>% dplyr::select(PATID, Danger_Others_SZ, Danger_Others_BD, Danger_Others_ASD, Danger_Others_EP)
longdf <- melt(id = c('PATID'), measured = c('Danger_Others_SZ', 'Danger_Others_BD', 'Danger_Others_ASD', 'Danger_Others_EP'), data = longdf)
names(longdf) <- c('PATID', 'Disorder', 'Danger_Others')
longdf$Disorder <- gsub("^.*_", "", longdf$Disorder)
longdf$Disorder <- factor(longdf$Disorder)
longdf <- longdf[order(longdf$PATID), ]
longdf_do <- longdf

longdf_mhkq <- df %>% dplyr::select(PATID, MHKQ_quantile)
longdf_fam <- df %>% dplyr::select(PATID, Familiarity_Recode)

# define mediation: the effect of disorder on Behaviour is mediated by Danger_Others, moderated by MHKQ
alldf <- merge(x = longdf_b, y = longdf_do, by = c("PATID", "Disorder"), all = TRUE)
alldf <- merge(x = alldf, y = longdf_mhkq, by = c("PATID"), all = TRUE)
alldf <- alldf[complete.cases(alldf),] 

# mediator model
mediator_model <- lmer(Danger_Others ~ Disorder * MHKQ_quantile + (1 | PATID), data = alldf)

# outcome_model
outcome_model <- lmer(Behaviour ~ Danger_Others * MHKQ_quantile + Disorder * MHKQ_quantile + (1 | PATID), data = alldf)

# Mediation analysis with moderation
med_results <- mediate(
  model.m = mediator_model,
  model.y = outcome_model,
  treat = "Disorder",
  mediator = "Danger_Others",
  covariates = list(MHKQ_quantile = 2), # Specify the level of the moderator
  control.value = "ASD", 
  treat.value = "SZ"
)
summary(med_results)

# define mediation: the effect of disorder on Behaviour is mediated by Danger_Others, moderated by familiarity
alldf <- merge(x = longdf_b, y = longdf_do, by = c("PATID", "Disorder"), all = TRUE)
alldf <- merge(x = alldf, y = longdf_fam, by = c("PATID"), all = TRUE)
alldf <- alldf[complete.cases(alldf),] 

# mediator model
mediator_model <- lmer(Danger_Others ~ Disorder * Familiarity_Recode + (1 | PATID), data = alldf)

# outcome_model
outcome_model <- lmer(Behaviour ~ Danger_Others * Familiarity_Recode + Disorder * Familiarity_Recode + (1 | PATID), data = alldf)

# Mediation analysis with moderation
med_results <- mediate(
  model.m = mediator_model,
  model.y = outcome_model,
  treat = "Disorder",
  mediator = "Danger_Others",
  covariates = list(Familiarity_Recode = 2), # Specify the level of the moderator
  control.value = "ASD", 
  treat.value = "SZ"
)
summary(med_results)

print('******************************************************')
print('******************** Full Behaviour Moderated Mediations*********************')
print('******************************************************')

# MHKQ first
alldf <- merge(x = longdf_b, y = longdf_do, by = c("PATID", "Disorder"), all = TRUE)
alldf <- merge(x = alldf, y = longdf_mhkq, by = c("PATID"), all = TRUE)
alldf <- alldf[complete.cases(alldf),] 

# mediator model
mediator_model <- lmer(Danger_Others ~ Disorder * MHKQ_quantile + (1 | PATID), data = alldf)

# outcome_model
outcome_model <- lmer(Behaviour ~ Danger_Others * MHKQ_quantile + Disorder * MHKQ_quantile + (1 | PATID), data = alldf)

med_results_bymod <- list()
for (x in 1:4) {
  # Mediation analysis with moderation for each level of MHKQ
  print(x)
  med_results <- mediate(
  model.m = mediator_model,
  model.y = outcome_model,
  treat = "Disorder",
  mediator = "Danger_Others",
  covariates = list(MHKQ_quantile = x), # Specify the level of the moderator
  control.value = "ASD", 
  treat.value = "SZ"
  )
  summary(med_results)
  med_results_bymod[[x]] <- med_results
}

acme_values_real <- numeric(4)
for (x in 1:4) {
  print(summary(med_results_bymod[[x]]))
  acme_values_real[x] <- med_results_bymod[[x]]$d1
}

library(boot)

# Create a function to calculate the moderated mediation effect
moderated_mediation <- function(data, indices, levels_W) {
  # Resample the data
  d <- data[indices, ]
  
  # Initialize a vector to store ACMEs
  acme_values <- numeric(length(levels_W))
  
  # Loop through levels of the moderator
  for (i in 1:length(levels_W)) {
    current_level <- levels_W[i]
    
    # Fit models
    # mediator model
    mediator_model <- lmer(Danger_Others ~ Disorder * MHKQ_quantile + (1 | PATID), data = d)
    # outcome_model
    outcome_model <- lmer(Behaviour ~ Danger_Others * MHKQ_quantile + Disorder * MHKQ_quantile + (1 | PATID), data = d)
    
    #mediator_model <- lmer(M ~ X * W + (1 | RandomEffect), data = d)
    #outcome_model <- lmer(Y ~ M * W + X * W + (1 | RandomEffect), data = d)
    
    # Perform mediation analysis
    med_results <- {
      mediate(
        model.m = mediator_model,
        model.y = outcome_model,
        treat = "Disorder",
        mediator = "Danger_Others",
        covariates = list(MHKQ_quantile = current_level),  # Ensure the covariate is correctly specified
        control.value = "ASD", 
        treat.value = "SZ",
        sims = 100  # Increase the number of simulations for better estimates
      )
    }
    
    # Store ACME value
    acme_values[i] <- med_results$d1
  }
  print(acme_values)
  # Calculate difference in ACME across levels (moderated mediation effect)
  # You could calculate any ANOVA statistic here, e.g., sum of squares between or F-statistic
  return(var(acme_values))  # Variance across levels as a simple example
  #return(acme_values[4]-acme_values[1]) # top minus bottom
}

# Levels of the moderator
levels_W <- unique(alldf$MHKQ_quantile)

# Perform bootstrapping
set.seed(123)
boot_results <- boot(data = alldf, statistic = moderated_mediation, R = 50, levels_W = levels_W)

# Summary of bootstrapped results
boot_results

# You can also derive p-values based on the bootstrap results
windows();
res <- hist(boot_results$t)
var(acme_values_real)

acme_values_real[4]-acme_values_real[1]


p_value <- mean(boot_results$t > 0) * 2  # For a two-tailed test
p_value
p_value <- mean(boot_results$t > 0, na.rm = TRUE)  # Adjust based on the hypothesis test you are performing
print(p_value)






# Familiarity
alldf <- merge(x = longdf_b, y = longdf_do, by = c("PATID", "Disorder"), all = TRUE)
alldf <- merge(x = alldf, y = longdf_fam, by = c("PATID"), all = TRUE)
alldf <- alldf[complete.cases(alldf),] 

# mediator model
mediator_model <- lmer(Danger_Others ~ Disorder * Familiarity_Recode + (1 | PATID), data = alldf)

# outcome_model
outcome_model <- lmer(Behaviour ~ Danger_Others * Familiarity_Recode + Disorder * Familiarity_Recode + (1 | PATID), data = alldf)

med_results_bymod2 <- list()
for (x in 1:3) {
  # Mediation analysis with moderation for each level of MHKQ
  print(x)
  med_results <- mediate(
    model.m = mediator_model,
    model.y = outcome_model,
    treat = "Disorder",
    mediator = "Danger_Others",
    covariates = list(Familiarity_Recode = x), # Specify the level of the moderator
    control.value = "ASD", 
    treat.value = "SZ"
  )
  summary(med_results)
  med_results_bymod2[[x]] <- med_results
}

for (x in 1:3) {
  print(summary(med_results_bymod2[[x]]))
}















############## Disorder -> Danger_Others -> Behaviour ####################
############## USING CLMMs #################
# DOESN'T WORK WITH CLMMS, TRY MANUAL VERSION USING CHATGPT SUGGESTION

alldf_fac <- alldf
alldf_fac$Danger_Others <- factor(alldf_fac$Danger_Others, ordered = TRUE)
alldf_fac$Behaviour <- factor(alldf_fac$Behaviour, ordered = TRUE)

# fit total effect, X -> Y
#print('Total Effect')
#total.fit <- lmer(Behaviour ~ Disorder + (1|PATID), data = alldf, REML=FALSE, na.action = na.omit)
#summary(total.fit)
#anova(total.fit)

# fit mediation effect, X -> M
print('A path')
med.fit <- clmm(Danger_Others ~ Disorder + (1|PATID), data = alldf_fac, na.action = na.omit)
summary(med.fit)
#anova(med.fit)

# fit outcome effect, M -> Y
print('B path')
out.fit <- clmm(Behaviour ~ Danger_Others + Disorder + (1|PATID), data = alldf_fac, na.action = na.omit)
summary(out.fit)
#anova(out.fit)

# try ASD as control contrast, perhaps too little variability in EP
print('Mediation')
medmod <- mediate(model.m = med.fit, model.y = out.fit, treat = "Disorder", mediator = "Danger_Others", control.value = "ASD", treat.value = "SZ")
summary(medmod)







# CODE GRAVEYARD

print('Regression model')
model.fit <- lm(Behaviour ~ Disorder, data=longdf_b)
#model.fit <- lm(Behaviour ~ Disorder*Familiarity + Disorder*Income + Disorder*Age + Disorder*Gender+ Disorder*Education, data=longdf_b)
summary(model.fit)
anova(model.fit)