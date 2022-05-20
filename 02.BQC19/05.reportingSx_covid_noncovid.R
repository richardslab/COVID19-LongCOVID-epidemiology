setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID")

TMP <- readRDS("clinical_bqc19.rds")

cases <- TMP %>% filter(covid19_test_result == "Positive")

symptoms <- colnames(cases)[grepl("^sx_", colnames(cases)) & !grepl("sx_date", colnames(cases))]
symptoms <- symptoms[!grepl("_PCS$", symptoms)]

for(i in seq(1, length(symptoms))){
  LM1 <- glm(paste0(symptoms[i]," ~ hospitalization + age_range + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=cases, family="binomial")
  tmp <- cases[,c(symptoms[i], "age_range", "hospitalization", "sex", "obesity", "smoking", "com_hypertension", 
                  "com_diabetes", "com_autoimmune", "com_COPD", "com_asthma", "com_cancer", "com_dementia")]
  tmp <- tmp[complete.cases(tmp),]
  out <- data.frame(summary(LM1)$coefficients[-1,])
  out1 <- out %>% mutate(group = "BQC19",
                         Sx = symptoms[i],
                         casecontrol = "case",
                         covariates = rownames(out),
                         covariates = rownames(out),
                         Ncase = sum(tmp[,symptoms[i]]),
                         Ncontrol = dim(tmp)[1] - sum(tmp[,symptoms[i]]))
  write.table(out1, file="BQC19_summary.logreg.tsv", sep="\t", quote=F, col.names = F, row.names = F, append=T)
}

controls <- TMP %>% filter(covid19_test_result == "Negative")

for(i in seq(1, length(symptoms))){
  LM1 <- glm(paste0(symptoms[i]," ~ hospitalization + age_range + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=controls, family="binomial")
  tmp <- controls[,c(symptoms[i], "age_range", "hospitalization", "sex", "obesity", "smoking", "com_hypertension", 
                  "com_diabetes", "com_autoimmune", "com_COPD", "com_asthma", "com_cancer", "com_dementia")]
  tmp <- tmp[complete.cases(tmp),]
  out <- data.frame(summary(LM1)$coefficients[-1,])
  out1 <- out %>% mutate(group = "BQC19",
                         Sx = symptoms[i],
                         casecontrol = "control",
                         covariates = rownames(out),
                         Ncase = sum(tmp[,symptoms[i]]),
                         Ncontrol = dim(tmp)[1] - sum(tmp[,symptoms[i]]))
  write.table(out1, file="BQC19_summary.logreg.tsv", sep="\t", quote=F, col.names = F, row.names = F, append=T)
}

out <- fread("BQC19_summary.logreg.tsv")
colnames(out) <- colnames(out1)
write.table(out, file="BQC19_summary.logreg.tsv", sep="\t", quote=F, col.names = T, row.names = F, append=F)

##interaction
covid <- readRDS("clinical_bqc19.rds")

LM1 <- glm(paste0("sx_any ~ covid19_test_result*(hospitalization + age_range + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia)"), data=covid, family="binomial")

tmp <- covid[,c("sx_any", "covid19_test_result","age_range", "hospitalization", "sex", "obesity", "smoking", "com_hypertension", 
                "com_diabetes", "com_autoimmune", "com_COPD", "com_asthma", "com_cancer", "com_dementia")]
tmp <- tmp[complete.cases(tmp),]
out <- data.frame(summary(LM1)$coefficients[-1,])
out <- data.frame(summary(LM1)$coefficients[-1,])
out1 <- out %>% mutate(group = "BQC19",
                       Sx = "sx_any",
                       casecontrol = "covid",
                       covariates = rownames(out),
                       Ncase = sum(tmp[,"sx_any"]),
                       Ncontrol = dim(tmp)[1] - sum(tmp[,"sx_any"]))

write.table(out1, file="BQC19_summary.interaction.tsv", sep="\t", quote=F, col.names = T, row.names = F, append=F)

out1 <- out1 %>% mutate(OR = exp(Estimate),
                        LL = exp(Estimate - qnorm(0.975)*Std..Error),
                        UL = exp(Estimate + qnorm(0.975)*Std..Error))

out1 <- out1 %>% rename(Factors = covariates,
                        Pvalue = Pr...z..,
                        Cohort = group) %>%
  select(Factors, OR, LL, UL, Pvalue, Ncase, Ncontrol)
write.xlsx(out1, "BQC19_summary.interaction.xlsx")


