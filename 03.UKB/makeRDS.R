setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/01.UKBB/01.GWAS/20210816/")

data1 <- readRDS("LongCOVID2.rds")
data1 <- data1 %>% mutate(age_range = case_when(newage < 60 & newage >= 50 ~ "50-60",
                                                newage < 70 & newage >= 60 ~ "0Age60-70",
                                                newage >= 70 & newage < 80 ~ "70-80",
                                                newage >= 80 ~ ">80"))

data1 <- data1 %>% mutate(obesity = case_when(BMI >= 30 ~ 1,
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

data2 <- data1 %>% filter(covid19_test == 0 | (covid19_test == 1 & COVIDdate < "2021-07-25"))
death <- fread("/home/richards/tomoko.nakanishi/09.COVID19/src/01.UKBB/01.GWAS/data/death_20210408.txt.gz")

data2 <- data2 %>% filter(PCS == 1 | (PCS == 0 & !(ID %in% death$eid)))


LM1 <- glm(PCS ~ age_range + hospital + sex +  obesity + smoking + com_hypertension +
            com_diabetes + com_autoimmune + com_copd + com_asthma + com_cancer + com_dementia, data = data2, family="binomial")

out <- data.frame(summary(LM1)$coefficients[-1,])
out1 <- out %>% mutate(group = "UKB_C2",
                       covariates = rownames(out))

cases <- data2 %>% filter(covid19_test == 1)

LM1 <- glm(PCS ~ age_range + hospital + sex +  obesity + smoking + com_hypertension +
             com_diabetes + com_autoimmune + com_copd + com_asthma + com_cancer + com_dementia, data = cases, family="binomial")

out <- data.frame(summary(LM1)$coefficients[-1,])
out2 <- out %>% mutate(group = "UKB_C1",
                       covariates = rownames(out))

out <- bind_rows(out1, out2)
write.table(out, file="UKB_PCS.summary.tsv", sep="\t", quote=F, col.names = T, row.names = F)

#################
data1 <- data1 %>% mutate(PVF = case_when(CFS == 1 ~ 1,
                                          PVF == 1 ~ 1,
                                          TRUE ~ 0))
data1 <- data1 %>% mutate(age_range = case_when(AGE < 60 & AGE >= 50 ~ "50-60",
                                                AGE < 70 & AGE >= 60 ~ "0Age60-70",
                                                AGE >= 70 & AGE < 80 ~ "70-80",
                                                AGE >= 80 ~ ">80",
                                                AGE < 50 ~ "<50"))

LM1 <- glm(PVF ~ age_range + sex +  obesity + smoking + com_hypertension +
             com_diabetes + com_autoimmune + com_copd + com_asthma + com_cancer + com_dementia, data = data1, family="binomial")

out <- data.frame(summary(LM1)$coefficients[-1,])
out <- out %>% mutate(OR = exp(Estimate),
                              LL = exp(Estimate - qnorm(0.975)*Std..Error),
                              UL = exp(Estimate + qnorm(0.975)*Std..Error))

summary <- summary %>% mutate(sig = case_when(as.numeric(Pr...z..) <= 0.05 ~ TRUE,
                                          TRUE ~ FALSE),
                          shape = case_when(as.numeric(Pr...z..) > 0.05 ~ 21,
                                            as.numeric(Pr...z..) <= 0.05 & OR > 1 ~ 24,
                                            as.numeric(Pr...z..) <= 0.05 & OR < 1 ~ 25))


label = c("vs COVID-19 patients\nwithout long COVID","vs population control")


summary %>%
  ggplot(aes(y=OR, ymin=LL, ymax=UL, x=group, color=group)) + geom_pointrange(aes(col=group, fill=group, alpha=sig, shape=shape), lwd=0.8) + geom_hline(aes(), yintercept = 1, linetype=2) +
  theme_minimal() + facet_wrap(~covariates,strip.position="left",nrow=20,scales = "free_y") +
  scale_shape_identity() +
  geom_errorbar(aes(ymin=LL, ymax=UL, col=group, alpha=sig), width=0.1, cex=1) + xlab("")  + ylab("Odds ratio") + 
  scale_color_manual(values=c("#00AFBB", "#E7B800", "#FC4E07")) + 
  scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07")) + scale_y_log10(breaks=c(0.1, 0.3, 0.5, 1, 2, 6, 10)) + scale_alpha_discrete(range = c(0.5, 1)) + 
  theme(axis.text.x = element_text(size=15,face="bold"),
        axis.text.y=element_blank(),
        axis.title=element_text(size=15,face="bold"), 
        strip.text.y.left = element_text(hjust=0.5,vjust = 0.5,angle=0,size=15,face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(size=15)) +coord_flip() + guides(col = guide_legend(reverse = TRUE),
                                                                    fill = FALSE,
                                                                    shape = FALSE,
                                                                    alpha = FALSE)


summary %>%
  ggplot(aes(y=OR, ymin=LL, ymax=UL, x=group, color=group)) + geom_pointrange(aes(col=group), lwd=0.8) + geom_hline(aes(fill=group), yintercept =1, linetype=2) +
  theme_minimal() + facet_wrap(~covariates,strip.position="left",nrow=20,scales = "free_y") +
  geom_errorbar(aes(ymin=LL, ymax=UL, col=group), width=0.1, cex=1) +  xlab("Odds ratio") + 
  theme(axis.text.x = element_text(size=15,face="bold"),
        axis.text.y=element_blank(),
        axis.title=element_text(size=15,face="bold"), 
        legend.text = element_text(size=15), 
        strip.text.y.left = element_text(hjust=0.5,vjust = 0.5,angle=0,size=15,face="bold"),
        legend.title = element_text(size=15,face="bold")) +coord_flip() +guides(col = guide_legend(reverse = TRUE))

