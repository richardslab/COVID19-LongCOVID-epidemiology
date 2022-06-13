setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/01.UKBB/01.GWAS/20210816/")


data1 <- readRDS("LongCOVID2.rds")
data1 <- data1 %>% filter(covid_date <= as.Date("2021-09-30") - 28*2 & covid19_test == 1)
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

##vaccination
vaccination <- fread("/home/richards/tomoko.nakanishi/09.COVID19/scratch/01.UKBB/02.LongCOVID/ukb27449_50908_vaccineation.tab.gz")
vaccination <- vaccination %>% mutate(vaccine_date1 = case_when(!is.na(f.27984.0.0) ~ as.Date(f.27984.0.0),
                                                                    !is.na(f.27984.1.0) ~ as.Date(f.27984.1.0)),
                                      vaccine_date2 = case_when(!is.na(f.27986.0.0) ~ as.Date(f.27986.0.0),
                                                                    !is.na(f.27986.1.0) ~ as.Date(f.27986.1.0)),
                                      vaccination = case_when(f.27983.1.0 == 0 ~ 0,
                                                              f.27983.0.0 == 0 ~ 0,
                                                              f.27983.1.0 == 1 ~ 1,
                                                              f.27983.0.0 == 1 ~ 1
                                                              ))
vaccination <- vaccination %>% select(f.eid, vaccination, vaccine_date1, vaccine_date2)

data1 <- data1 %>% left_join(vaccination, by=c("ID"="f.eid"))

data1 <- data1 %>% mutate(prevaccination = case_when(as.Date(covid_date) - as.Date(vaccine_date2) >= 14 ~ 2,
                                                     as.Date(covid_date) - as.Date(vaccine_date1) >= 14 ~ 1,
                                                     as.Date(covid_date) - as.Date(vaccine_date1) < 14 ~ 0,
                                                     vaccination == 0 ~ 0))

data1 %>% saveRDS("LongCOVID2.rds")

library(tableone)

Vars <- c("newage", "sex", "obesity", "smoking", colnames(data1)[grepl("^com_", colnames(data1))], "hospital", "PCS", "prevaccination")
catVars <- c("sex", "obesity", "smoking", colnames(data1)[grepl("^com_", colnames(data1))], "hospital", "PCS", "prevaccination")

tableOne <- CreateTableOne(vars = Vars , data = data1,
                           factorVars = catVars )

tab3Mat <- print(tableOne, quote=F, noSpaces=TRUE)
summary(tableOne)
write.csv(tab3Mat, file = "myTable1.csv")
