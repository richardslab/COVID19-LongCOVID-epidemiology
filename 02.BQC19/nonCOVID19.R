setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID")

tmp <- readRDS("clinical_bqc19.rds")

cases <- tmp %>% filter(Final.COVID.status. == "Positive")
cases <- cases %>% filter(!is.infinite(Sx_any))
cases <- cases %>% mutate_at(.vars=vars(colnames(cases)[grepl("^Sx", colnames(cases))]), .funs=funs(ifelse(is.infinite(.), NA, .)))
cases <- cases %>% mutate_at(.vars=vars(colnames(cases)[grepl("^Sx", colnames(cases))]), .funs=funs(ifelse(Sx_any == 0 ,0, .)))

symptoms <- colnames(cases)[grepl("^Sx_", colnames(cases))]
symptoms <- symptoms[!grepl("_PCS$", symptoms)]

for(i in seq(1, length(symptoms))){
  LM1 <- glm(paste0(symptoms[i]," ~ age_range + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=cases, family="binomial")
  
  out <- data.frame(summary(LM1)$coefficients[-1,])
  out1 <- out %>% mutate(group = "BQC19",
                         Sx = symptoms[i],
                         casecontrol = "case",
                         covariates = rownames(out))
  write.table(out1, file="BQC19_summary.logreg.tsv", sep="\t", quote=F, col.names = F, row.names = F, append=T)
}

controls <- tmp %>% filter(Final.COVID.status. == "Negative")
controls <- controls %>% filter(!is.infinite(Sx_any))
controls <- controls %>% mutate_at(.vars=vars(colnames(controls)[grepl("^Sx", colnames(controls))]), .funs=funs(ifelse(is.infinite(.), NA, .)))
controls <- controls %>% mutate_at(.vars=vars(colnames(controls)[grepl("^Sx", colnames(controls))]), .funs=funs(ifelse(Sx_any == 0 ,0, .)))

for(i in seq(1, length(symptoms))){
  LM1 <- glm(paste0(symptoms[i]," ~ age_range + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=controls, family="binomial")
  
  out <- data.frame(summary(LM1)$coefficients[-1,])
  out1 <- out %>% mutate(group = "BQC19",
                         Sx = symptoms[i],
                         casecontrol = "control",
                         covariates = rownames(out))
  write.table(out1, file="BQC19_summary.logreg.tsv", sep="\t", quote=F, col.names = F, row.names = F, append=T)
}

out <- fread("BQC19_summary.logreg.tsv")
colnames(out) <- colnames(out1)
write.table(out, file="BQC19_summary.logreg.tsv", sep="\t", quote=F, col.names = T, row.names = F, append=F)
