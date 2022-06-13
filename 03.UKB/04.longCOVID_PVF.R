setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/01.UKBB/01.GWAS/20210816/")

data1 <- readRDS("LongCOVID2.rds")
data1 <- data1 %>% mutate(age_range = case_when(newage < 60 & newage >= 50 ~ "50-60",
                                                newage < 70 & newage >= 60 ~ "0Age60-70",
                                                newage >= 70 & newage < 80 ~ "70-80",
                                                newage >= 80 ~ ">80"))


data2 <- data1 %>% filter(covid_date <= as.Date("2021-09-30") - 28*2 & covid19_test == 1)

death <- fread("/home/richards/tomoko.nakanishi/scratch/DATA/UKB/death_20210930.txt.gz")

data2 <- data2 %>% filter(PCS == 1 | (PCS == 0 & !(ID %in% death$eid)))


LM1 <- glm(PCS ~ age_range + hospital + sex +  obesity + smoking + com_hypertension +
            com_diabetes + com_autoimmune + com_copd + com_asthma + com_cancer + com_dementia, data = data2, family="binomial")


out <- data.frame(summary(LM1)$coefficients[-1,])
tmp <- data2[,c("PCS", "age_range", "hospital", "sex", "obesity", "smoking", "com_hypertension", 
                  "com_diabetes", "com_autoimmune", "com_copd", "com_asthma", "com_cancer", "com_dementia")]
tmp <- tmp[complete.cases(tmp),]
out <- out %>% mutate(group = "UKB_C1",
                       covariates = rownames(out),
                       Ncase = sum(tmp[,"PCS"]),
                       Ncontrol = dim(tmp)[1] - sum(tmp[,"PCS"]))

write.table(out, file="UKB_PCS.summary.tsv", sep="\t", quote=F, col.names = T, row.names = F)

out %>% filter(group == "UKB_C1") %>% write.xlsx("UKB_longCOVID.xlsx")
 
data2 <- data2 %>% filter(hospital == 0)


LM1 <- glm(PCS ~ age_range + sex +  obesity + smoking + com_hypertension +
             com_diabetes + com_autoimmune + com_copd + com_asthma + com_cancer + com_dementia, data = data2, family="binomial")


out <- data.frame(summary(LM1)$coefficients[-1,])
tmp <- data2[,c("PCS", "age_range",  "sex", "obesity", "smoking", "com_hypertension", 
                "com_diabetes", "com_autoimmune", "com_copd", "com_asthma", "com_cancer", "com_dementia")]
tmp <- tmp[complete.cases(tmp),]
out <- out %>% mutate(group = "UKB_C1",
                      covariates = rownames(out),
                      Ncase = sum(tmp[,"PCS"]),
                      Ncontrol = dim(tmp)[1] - sum(tmp[,"PCS"]))

write.table(out, file="UKB_PCS.summary.mild.tsv", sep="\t", quote=F, col.names = T, row.names = F)


setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/01.UKBB/01.GWAS/20210816/")

data1 <- readRDS("LongCOVID2.rds")
data1 <- data1 %>% mutate(age_range = case_when(newage < 60 & newage >= 50 ~ "50-60",
                                                newage < 70 & newage >= 60 ~ "0Age60-70",
                                                newage >= 70 & newage < 80 ~ "70-80",
                                                newage >= 80 ~ ">80"))


data2 <- data1 %>% filter(covid_date <= as.Date("2021-09-30") - 28*2 & covid19_test == 1)

death <- fread("/home/richards/tomoko.nakanishi/scratch/DATA/UKB/death_20210930.txt.gz")

data2 <- data2 %>% filter(PCS == 1 | (PCS == 0 & !(ID %in% death$eid)))

LM1 <- glm(PCS ~ prevaccination + age_range + hospital + sex +  obesity + smoking + com_hypertension +
             com_diabetes + com_autoimmune + com_copd + com_asthma + com_cancer + com_dementia, data = data2, family="binomial")


out <- data.frame(summary(LM1)$coefficients[-1,])
tmp <- data2[,c("prevaccination","PCS", "age_range", "hospital", "sex", "obesity", "smoking", "com_hypertension", 
                "com_diabetes", "com_autoimmune", "com_copd", "com_asthma", "com_cancer", "com_dementia")]
tmp <- tmp[complete.cases(tmp),]
out <- out %>% mutate(group = "UKB_C1",
                      covariates = rownames(out),
                      Ncase = sum(tmp[,"PCS"]),
                      Ncontrol = dim(tmp)[1] - sum(tmp[,"PCS"]))

write.table(out, file="UKB_PCS.summary.vaccination.tsv", sep="\t", quote=F, col.names = T, row.names = F)

data2 <- data2 %>% filter(hospital == 0)

LM1 <- glm(PCS ~ prevaccination + age_range + sex +  obesity + smoking + com_hypertension +
             com_diabetes + com_autoimmune + com_copd + com_asthma + com_cancer + com_dementia, data = data2, family="binomial")


out <- data.frame(summary(LM1)$coefficients[-1,])
tmp <- data2[,c("prevaccination","PCS", "age_range", "sex", "obesity", "smoking", "com_hypertension", 
                "com_diabetes", "com_autoimmune", "com_copd", "com_asthma", "com_cancer", "com_dementia")]
tmp <- tmp[complete.cases(tmp),]
out <- out %>% mutate(group = "UKB_C1",
                      covariates = rownames(out),
                      Ncase = sum(tmp[,"PCS"]),
                      Ncontrol = dim(tmp)[1] - sum(tmp[,"PCS"]))

write.table(out, file="UKB_PCS.summary.vaccination.mild.tsv", sep="\t", quote=F, col.names = T, row.names = F)


#################
data1 <- data1 %>% mutate(age_range = case_when(AGE < 60 ~ "<60",
                                                AGE < 70 & AGE >= 60 ~ "0Age60-70",
                                                AGE >= 70 & AGE < 80 ~ "70-80",
                                                AGE >= 80 ~ ">80"))

LM1 <- glm(PVF ~ age_range + sex +  obesity + smoking + com_hypertension +
             com_diabetes + com_autoimmune + com_copd + com_asthma + com_cancer + com_dementia, data = data1, family="binomial")

tmp <- data1[,c("PVF", "age_range", "hospital", "sex", "obesity", "smoking", "com_hypertension", 
                "com_diabetes", "com_autoimmune", "com_copd", "com_asthma", "com_cancer", "com_dementia")]
tmp <- tmp[complete.cases(tmp),]

out <- data.frame(summary(LM1)$coefficients[-1,])
out <- out %>% mutate(OR = exp(Estimate),
                              LL = exp(Estimate - qnorm(0.975)*Std..Error),
                              UL = exp(Estimate + qnorm(0.975)*Std..Error))

out <- out %>% mutate(Ncase = sum(tmp[,"PVF"]),
                      Ncontrol = dim(tmp)[1] - sum(tmp[,"PVF"]))

out <- out %>% mutate(covariates = rownames(out)) %>% 
  rename(Factors = covariates,
         Pvalue = Pr...z..) %>%
  select(Factors, OR, LL, UL, Pvalue, Ncase, Ncontrol)
write.xlsx(out, "/home/richards/tomoko.nakanishi/my_project/repo/COVID19-LongCOVID-epidemiology/results/UKB.PVF.xlsx")

