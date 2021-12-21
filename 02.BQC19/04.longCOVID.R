setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID")

final <- readRDS("clinical_bqc19.rds")
symptoms <- colnames(final)[grepl("^Sx_", colnames(final))]
final <- final %>% mutate_at(.vars=vars(c(symptoms,"hospital", "age_range", "sex", "obesity", "smoking", 
               "com_asthma", "com_COPD", "com_hypertension", "com_diabetes", "com_autoimmune", 
               "com_cancer", "com_dementia")), .funs=funs(ifelse(.==-1,NA,.)))
final <- final %>% mutate_at(.vars=vars(c(symptoms,"hospital", "age_range", "sex", "obesity", "smoking", 
                                          "com_asthma", "com_COPD", "com_hypertension", "com_diabetes", "com_autoimmune", 
                                          "com_cancer", "com_dementia")), .funs=funs(ifelse(is.infinite(.),NA,.)))

data <- final %>% filter(covid19_test == 1)
data <- data %>% mutate(smoking = case_when(smoking == "Current" ~ "Ever",
                                            smoking == "Past" ~ "Ever",
                                            TRUE ~ smoking))  

symptoms <- colnames(data)[grepl("^Sx_", colnames(data))]
symptoms <- symptoms[grepl("_PCS$", symptoms)]

data <- data %>% select(-Sx_any_PCS) %>% rename(Sx_any_PCS = longCOVID)

for(i in seq(1, length(symptoms))){
  LM1 <- glm(paste0(symptoms[i]," ~ hospital + age_range + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=data, family="binomial")
  tmp <- data[,c(symptoms[i], "hospital", "age_range", "sex", "obesity", "smoking", 
               "com_asthma", "com_COPD", "com_hypertension", "com_diabetes", "com_autoimmune",
               "com_cancer", "com_dementia")]
  tmp <- tmp[complete.cases(tmp),]
  out <- data.frame(summary(LM1)$coefficients[-1,])
  out1 <- out %>% mutate(group = "BQC19",
                         Sx = symptoms[i],
                         casecontrol = "case",
                         covariates = rownames(out),
                         Ncase = sum(tmp[,symptoms[i]]),
                         Ncontrol = dim(tmp)[1] - sum(tmp[,symptoms[i]]))
  write.table(out1, file="BQC19_summary.longCOVID.tsv", sep="\t", quote=F, col.names = F, row.names = F, append=T)
}

library(data.table)
out <- fread("BQC19_summary.longCOVID.tsv")
colnames(out) <- colnames(out1)
write.table(out, file="BQC19_summary.longCOVID.tsv", sep="\t", quote=F, col.names = T, row.names = F, append=F)

library(openxlsx)
out %>% filter(Sx == "Sx_any_PCS") %>% select(-Sx) %>% write.xlsx("BQC19.longCOVID.xlsx")

