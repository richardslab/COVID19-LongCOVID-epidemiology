setwd("/home/richards/tomoko.nakanishi/scratch/09.COVID19/06.CLSA")
##longCOVID
###any Sx
tmp <- readRDS("CLSA_data.rds")
cases <- tmp %>% filter(C2 == 1)

symptoms <- colnames(tmp)[grepl("^Sx_", colnames(tmp))]
cases <- cases %>% mutate_at(.vars=vars(symptoms), .funs=funs(ifelse(.==-1, NA, .)))
symptoms <- symptoms[c(-1)]
symptoms <- symptoms[grepl("_PCS$", symptoms)]
cases <- cases %>% mutate(smoking = case_when(smoking == "current" ~ "Ever",
                                              smoking == "past" ~ "Ever",
                                              TRUE ~ smoking))


for(i in seq(1, length(symptoms))){
  LM1 <- glm(paste0(symptoms[i]," ~ hospital + age_range + sex + obesity + smoking + 
               com_asthma + com_copd + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=cases, family="binomial")
  tmp <- cases[,c(symptoms[i], "hospital", "age_range", "sex", "obesity", "smoking", 
                 "com_asthma", "com_copd", "com_hypertension", "com_diabetes", "com_autoimmune",
                 "com_cancer", "com_dementia")]
  tmp <- tmp[complete.cases(tmp),]
  out <- data.frame(summary(LM1)$coefficients[-1,])
  out1 <- out %>% mutate(group = "CLSA",
                         Sx = symptoms[i],
                         casecontrol = "case",
                         covariates = rownames(out),
                         Ncase = sum(tmp[,symptoms[i]]),
                         Ncontrol = dim(tmp)[1] - sum(tmp[,symptoms[i]]))
  write.table(out1, file="CLSA_summary.longCOVID.tsv", sep="\t", quote=F, col.names = F, row.names = F, append=T)
}


out <- fread("CLSA_summary.longCOVID.tsv")
colnames(out) <- colnames(out1)
write.table(out, file="CLSA_summary.longCOVID.tsv", sep="\t", quote=F, col.names = T, row.names = F, append=F)

out %>% filter(Sx == "Sx_any_PCS") %>% select(-Sx) %>% write.xlsx("CLSA.longCOVID.xlsx")




