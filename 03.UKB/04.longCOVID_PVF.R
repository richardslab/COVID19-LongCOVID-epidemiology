setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/01.UKBB/01.GWAS/20210816/")

data1 <- readRDS("LongCOVID2.rds")
data1 <- data1 %>% mutate(age_range = case_when(newage < 60 & newage >= 50 ~ "50-60",
                                                newage < 70 & newage >= 60 ~ "0Age60-70",
                                                newage >= 70 & newage < 80 ~ "70-80",
                                                newage >= 80 ~ ">80"))

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

data2 <- data1 %>% filter(covid19_test == 0 | (covid19_test == 1 & COVIDdate < "2021-07-25"))
death <- fread("/home/richards/tomoko.nakanishi/09.COVID19/src/01.UKBB/01.GWAS/data/death_20210408.txt.gz")

data2 <- data2 %>% filter(PCS == 1 | (PCS == 0 & !(ID %in% death$eid)))


LM1 <- glm(PCS ~ age_range + hospital + sex +  obesity + smoking + com_hypertension +
            com_diabetes + com_autoimmune + com_copd + com_asthma + com_cancer + com_dementia, data = data2, family="binomial")


out <- data.frame(summary(LM1)$coefficients[-1,])
tmp <- data2[,c("PCS", "age_range", "hospital", "sex", "obesity", "smoking", "com_hypertension", 
                  "com_diabetes", "com_autoimmune", "com_copd", "com_asthma", "com_cancer", "com_dementia")]
tmp <- tmp[complete.cases(tmp),]
out1 <- out %>% mutate(group = "UKB_C2",
                       covariates = rownames(out),
                       Ncase = sum(tmp[,"PCS"]),
                       Ncontrol = dim(tmp)[1] - sum(tmp[,"PCS"]))

cases <- data2 %>% filter(covid19_test == 1)

LM1 <- glm(PCS ~ age_range + hospital + sex +  obesity + smoking + com_hypertension +
             com_diabetes + com_autoimmune + com_copd + com_asthma + com_cancer + com_dementia, data = cases, family="binomial")
tmp <- cases[,c("PCS", "age_range", "hospital", "sex", "obesity", "smoking", "com_hypertension", 
                "com_diabetes", "com_autoimmune", "com_copd", "com_asthma", "com_cancer", "com_dementia")]
tmp <- tmp[complete.cases(tmp),]
out <- data.frame(summary(LM1)$coefficients[-1,])
out2 <- out %>% mutate(group = "UKB_C1",
                       covariates = rownames(out),
                       Ncase = sum(tmp[,"PCS"]),
                       Ncontrol = dim(tmp)[1] - sum(tmp[,"PCS"]))

out <- bind_rows(out1, out2)
write.table(out, file="UKB_PCS.summary.tsv", sep="\t", quote=F, col.names = T, row.names = F)

out %>% filter(group == "UKB_C1") %>% write.xlsx("UKB_longCOVID.xlsx")
  
#################
data1 <- data1 %>% mutate(age_range = case_when(AGE < 60 ~ "<60",
                                                AGE < 70 & AGE >= 60 ~ "0Age60-70",
                                                AGE >= 70 & AGE < 80 ~ "70-80",
                                                AGE >= 80 ~ ">80"))

LM1 <- glm(PVF ~ age_range + sex +  obesity + smoking + com_hypertension +
             com_diabetes + com_autoimmune + com_copd + com_asthma + com_cancer + com_dementia, data = data1, family="binomial")

out <- data.frame(summary(LM1)$coefficients[-1,])
out <- out %>% mutate(OR = exp(Estimate),
                              LL = exp(Estimate - qnorm(0.975)*Std..Error),
                              UL = exp(Estimate + qnorm(0.975)*Std..Error))

out <- out %>% mutate(covariates = rownames(out)) %>% 
  rename(Factors = covariates,
         Pvalue = Pr...z..) %>%
  select(Factors, OR, LL, UL, Pvalue)
write.xlsx(out, "/home/richards/tomoko.nakanishi/my_project/repo/COVID19-LongCOVID-epidemiology/results/UKB.PVF.xlsx")


