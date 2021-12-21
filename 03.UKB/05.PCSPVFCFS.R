setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/01.UKBB/01.GWAS/20210816/")

data1 <- readRDS("LongCOVID2.rds")

library(data.table)
infile <- "/home/richards/tomoko.nakanishi/09.COVID19/scratch/01.UKBB/02.LongCOVID/ukb27449_20688_Sx.tab.gz"
t <- fread(infile, sep="\t")

#4717 SOB
tmp <- t %>% select(f.eid, f.4717.0.0, f.22502.0.0)
colnames(tmp) <- c("ID", "SOB", "cough")

data1 <- data1 %>% mutate(age_range = case_when(AGE < 70 & AGE >= 60 ~ "0Age60-70",
                                                AGE >= 70 ~ ">70",
                                                AGE < 60 ~ "<60"),
                          age_range1 = case_when(newage < 60 ~ "<60",
                                                newage < 70 & newage >= 60 ~ "0newage60-70",
                                                newage >= 70 ~ ">70"))

data1 <- data1 %>% mutate(obesity = case_when(BMI >= 30 ~ 1,
                                              BMI < 30 ~ 0),
                          sex = case_when(SEX == 1 ~ "0M",
                                          TRUE ~ "F"))

data1 <- data1 %>% rename(com_hypertension = hypertension,
                          com_diabetes = diabetes,
                          com_autoimmune = autoimmune,
                          com_copd = COPD,
                          com_asthma = asthma,
                          com_cancer = cancer,
                          com_dementia = dementia
)

data1 <- data1 %>% merge(tmp, by="ID", all.x=T)
#data1 <- data1 %>% drop_na(CFS, PVF, cough, SOB)

LM1 <- glm(PCS ~ hospital + age_range1 + sex +  obesity + smoking + com_hypertension +
             com_diabetes + com_autoimmune + com_copd + com_asthma + com_cancer + com_dementia, data = data1, family="binomial")

out <- data.frame(summary(LM1)$coefficients[-1,])
out1 <- out %>% mutate(group = "PCS",
                       covariates = rownames(out))

LM1 <- glm(CFS ~ age_range + sex +  obesity + smoking + com_hypertension +
             com_diabetes + com_autoimmune + com_copd + com_asthma + com_cancer + com_dementia, data = data1, family="binomial")

out <- data.frame(summary(LM1)$coefficients[-1,])
out2 <- out %>% mutate(group = "CFS",
                       covariates = rownames(out))

LM1 <- glm(PVF ~ age_range + sex +  obesity + smoking + com_hypertension +
             com_diabetes + com_autoimmune + com_copd + com_asthma + com_cancer + com_dementia, data = data1, family="binomial")

out <- data.frame(summary(LM1)$coefficients[-1,])
out3 <- out %>% mutate(group = "PVF",
                       covariates = rownames(out))

summary <- bind_rows(out1, out2, out3)
write.table(summary, file="UKB_PCSPVFCFS.summary.tsv", sep="\t", quote=F, col.names = T, row.names = F)

summary <- fread("UKB_PCSPVFCFS.summary.tsv")

summary <- summary %>% mutate(OR = exp(Estimate),
                              LL = exp(Estimate - qnorm(0.975)*Std..Error),
                              UL = exp(Estimate + qnorm(0.975)*Std..Error))

summary <- summary %>% mutate(sig = case_when(as.numeric(Pr...z..) <= 0.05 ~ TRUE,
                                              TRUE ~ FALSE),
                              shape = case_when(as.numeric(Pr...z..) > 0.05 ~ 21,
                                                as.numeric(Pr...z..) <= 0.05 & OR > 1 ~ 24,
                                                as.numeric(Pr...z..) <= 0.05 & OR < 1 ~ 25))
summary <- summary %>% mutate(covariates = gsub("age_range1", "age_range", covariates))

data1 <- summary
data1 <- data1 %>% filter(group %in% c("PVF", "CFS"))

data1$covariates <- rep(c("<60", ">70", "Female", "BMI>30", 
                          "Ever smoker", "Asthma", "COPD", "Hypertension", "Diabetes", "Autoimmune",
                          "Cancer", "Dementia"), 2)

tmp <- data1[1:4,]
tmp$covariates <- rep(c("60-70 (ref)", "Never smoker (ref)"), 2)
tmp$Estimate <- 0
tmp$Std..Error <- 0
tmp$Pr...z.. <- 1
tmp$group <- c(rep("CFS", 2), rep("PVF", 2))
tmp$OR <- 1
tmp$LL <- 1
tmp$UL <- 1

data1 <- bind_rows(data1, tmp)

data1 <- data1 %>% mutate(OR = as.numeric(OR),
                          LL = as.numeric(LL),
                          UL = as.numeric(UL)) 

data1$covariates <- factor(data1$covariates, levels = c("<60", "60-70 (ref)", ">70", "Female", "BMI>30", 
                                                        "Never smoker (ref)", "Ever smoker", "Asthma", "COPD", "Hypertension", "Diabetes", "Autoimmune",
                                                        "Cancer", "Dementia"))

data1 <- data1 %>% mutate(sig = case_when(as.numeric(Pr...z..) <= 0.05 ~ TRUE,
                                          TRUE ~ FALSE),
                          shape = case_when(as.numeric(Pr...z..) > 0.05 ~ 21,
                                            as.numeric(Pr...z..) <= 0.05 & OR > 1 ~ 24,
                                            as.numeric(Pr...z..) <= 0.05 & OR < 1 ~ 25))

data1$group <- factor(data1$group, levels = c("PVF", "CFS"))

#data1 <- data1 %>% filter(!(covariates %in% c("<60", "60-70 (ref)", ">70")))

png("PCSCFSPVF.png", width = 700, height = 500)
data1 %>%
  ggplot(aes(y=OR, ymin=LL, ymax=UL, x=group, color=group)) + geom_pointrange(aes(col=group, fill=group, alpha=sig, shape=shape), lwd=0.8) + geom_hline(aes(), yintercept = 1, linetype=2) +
  theme_minimal() + facet_wrap(~covariates,strip.position="left",nrow=20,scales = "free_y") +
  scale_shape_identity() +
  geom_errorbar(aes(ymin=LL, ymax=UL, col=group, alpha=sig), width=0.1, cex=1) + xlab("")  + ylab("Odds ratio") + 
  scale_color_manual(values=c("#17A91B", "#FFCB1C"), labels = c("Post-viral fatigue", "ME/CFS")) + 
  scale_fill_manual(values=c("#17A91B", "#FFCB1C")) + scale_y_log10(breaks=c(0.1, 0.3, 0.5, 1, 2, 6, 10)) + scale_alpha_discrete(range = c(0.5, 1)) + 
  theme(axis.text.x = element_text(size=20,face="bold"),
        axis.text.y=element_blank(),
        axis.title=element_text(size=20,face="bold"), 
        strip.text.y.left = element_text(hjust=0.5,vjust = 0.5,angle=0,size=20,face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(size=15)) +coord_flip() + guides(col = guide_legend(reverse = TRUE),
                                                                    fill = FALSE,
                                                                    shape = FALSE,
                                                                    alpha = FALSE)
dev.off()

data1 <- readRDS("LongCOVID2.rds")

library(data.table)
infile <- "/home/richards/tomoko.nakanishi/09.COVID19/scratch/01.UKBB/02.LongCOVID/ukb27449_20688_Sx.tab.gz"
t <- fread(infile, sep="\t")

#4717 SOB
tmp <- t %>% select(f.eid, f.4717.0.0, f.22502.0.0)
colnames(tmp) <- c("ID", "SOB", "cough")

data1 <- data1 %>% mutate(age_range = case_when(AGE < 70 & AGE >= 60 ~ "0Age60-70",
                                                AGE >= 70 ~ ">70",
                                                AGE < 60 & AGE >= 50 ~ "50-60",
                                                AGE < 50 ~ "<50"))

data1 <- data1 %>% mutate(obesity = case_when(BMI > 30 ~ 1,
                                              BMI <= 30 ~ 0),
                          sex = case_when(SEX == 1 ~ "0M",
                                          TRUE ~ "F"))

data1 <- data1 %>% rename(com_hypertension = hypertension,
                          com_diabetes = diabetes,
                          com_autoimmune = autoimmune,
                          com_copd = COPD,
                          com_asthma = asthma,
                          com_cancer = cancer,
                          com_dementia = dementia
)

data1 <- data1 %>% merge(tmp, by="ID", all.x=T)

data1 <- data1 %>% filter(cough >= 0 & SOB >= 0)

LM1 <- glm(cough ~ age_range + sex +  obesity + smoking + com_hypertension +
             com_diabetes + com_autoimmune + com_copd + com_asthma + com_cancer + com_dementia, data = data1, family="binomial")

out <- data.frame(summary(LM1)$coefficients[-1,])
out3 <- out %>% mutate(group = "cough",
                       covariates = rownames(out))

LM1 <- glm(SOB ~ age_range + sex +  obesity + smoking + com_hypertension +
             com_diabetes + com_autoimmune + com_copd + com_asthma + com_cancer + com_dementia, data = data1, family="binomial")

out <- data.frame(summary(LM1)$coefficients[-1,])
out4 <- out %>% mutate(group = "SOB",
                       covariates = rownames(out))


summary <- bind_rows(out3, out4)
write.table(summary, file="UKB_coughSOB.summary.tsv", sep="\t", quote=F, col.names = T, row.names = F)

summary <- summary %>% mutate(OR = exp(Estimate),
                              LL = exp(Estimate - qnorm(0.975)*Std..Error),
                              UL = exp(Estimate + qnorm(0.975)*Std..Error))

summary <- summary %>% mutate(sig = case_when(as.numeric(Pr...z..) <= 0.05 ~ TRUE,
                                              TRUE ~ FALSE),
                              shape = case_when(as.numeric(Pr...z..) > 0.05 ~ 21,
                                                as.numeric(Pr...z..) <= 0.05 & OR > 1 ~ 24,
                                                as.numeric(Pr...z..) <= 0.05 & OR < 1 ~ 25))
summary <- summary %>% mutate(covariates = gsub("age_range1", "age_range", covariates))

data1 <- summary

data1$covariates <- rep(c("50-60", "<50",">70", "Female", "BMI>30", 
                          "Ever smoker", "Asthma", "COPD", "Hypertension", "Diabetes", "Autoimmune",
                          "Cancer", "Dementia"), 2)

tmp <- data1[1:4,]
tmp$covariates <- rep(c("60-70 (ref)", "Never smoker (ref)"), 2)
tmp$Estimate <- 0
tmp$Std..Error <- 0
tmp$Pr...z.. <- 1
tmp$group <- c(rep("cough", 2), rep("SOB", 2))
tmp$OR <- 1
tmp$LL <- 1
tmp$UL <- 1

data1 <- bind_rows(data1, tmp)

data1 <- data1 %>% mutate(OR = as.numeric(OR),
                          LL = as.numeric(LL),
                          UL = as.numeric(UL)) 

data1$covariates <- factor(data1$covariates, levels = c("<50", "50-60", "60-70 (ref)", ">70", "Female", "BMI>30", 
                                                        "Never smoker (ref)", "Ever smoker", "Asthma", "COPD", "Hypertension", "Diabetes", "Autoimmune",
                                                        "Cancer", "Dementia"))

data1 <- data1 %>% mutate(sig = case_when(as.numeric(Pr...z..) <= 0.05 ~ TRUE,
                                          TRUE ~ FALSE),
                          shape = case_when(as.numeric(Pr...z..) > 0.05 ~ 21,
                                            as.numeric(Pr...z..) <= 0.05 & OR > 1 ~ 24,
                                            as.numeric(Pr...z..) <= 0.05 & OR < 1 ~ 25))

data1 %>%
  ggplot(aes(y=OR, ymin=LL, ymax=UL, x=group, color=group)) + geom_pointrange(aes(col=group, fill=group, alpha=sig, shape=shape), lwd=0.8) + geom_hline(aes(), yintercept = 1, linetype=2) +
  theme_minimal() + facet_wrap(~covariates,strip.position="left",nrow=20,scales = "free_y") +
  scale_shape_identity() +
  geom_errorbar(aes(ymin=LL, ymax=UL, col=group, alpha=sig), width=0.1, cex=1) + xlab("")  + ylab("Odds ratio") + 
  scale_color_manual(values=c("#00AFBB", "#E7B800")) + 
  scale_fill_manual(values=c("#00AFBB", "#E7B800")) + scale_y_log10(breaks=c(0.1, 0.3, 0.5, 1, 2, 6, 10)) + scale_alpha_discrete(range = c(0.5, 1)) + 
  theme(axis.text.x = element_text(size=15,face="bold"),
        axis.text.y=element_blank(),
        axis.title=element_text(size=15,face="bold"), 
        strip.text.y.left = element_text(hjust=0.5,vjust = 0.5,angle=0,size=15,face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(size=15)) +coord_flip() + guides(col = guide_legend(reverse = TRUE),
                                                                    fill = FALSE,
                                                                    shape = FALSE,
                                                                    alpha = FALSE)



