setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/01.UKBB/01.GWAS/20210816/")

data1 <- readRDS("LongCOVID2.rds")
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

library(tableone)

Vars <- c("newage", "sex", "obesity", "smoking", colnames(data1)[grepl("^com_", colnames(data1))], "hospital", "PCS")
catVars <- c("sex", "obesity", "smoking", colnames(data1)[grepl("^com_", colnames(data1))], "hospital", "PCS")

tableOne <- CreateTableOne(vars = Vars , strata = c("covid19_test"), data = data1,
                           factorVars = catVars )

tab3Mat <- print(tableOne, quote=F, noSpaces=TRUE)
summary(tableOne)
write.csv(tab3Mat, file = "myTable1.csv")
