setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID")

data <- readRDS("clinical_bqc19.rds")

#data <- data %>% mutate(smoking = case_when(smoking == "Current" ~ "Ever",
#                                            smoking == "Past" ~ "Ever",
#                                            TRUE ~ smoking))  

symptoms <- colnames(data)[grepl("^Sx_", colnames(data))]
symptoms <- symptoms[grepl("_PCS$", symptoms)]

library(Hmisc)
##install.packages("PredictABEL")
library(PredictABEL)
library(pROC)
out <- data.frame(matrix(0, 5, 7))
colnames(out) <- c("outcome","model","AUC","AUC.LL","AUC.UL", "AIC", "BIC")

#cases <- data[,c("Sx_any_PCS","covid19_test","hospital","age", "sex", "obesity", "com_asthma")]
cases <- data[,c("Sx_any_PCS","covid19_test","hospital","age", "sex", "obesity", "smoking", "com_asthma", "com_COPD", "com_hypertension", 
                  "com_diabetes", "com_autoimmune", "com_cancer", "com_dementia")]
#cases <- cases %>% mutate_all(.funs=funs(ifelse(.==-1, 0, .)))
cases <- cases %>% mutate_all(.funs=funs(ifelse(.==-1, NA, .)))
cases <- cases %>% mutate_all(.funs=funs(ifelse(is.infinite(.), NA, .)))
cases <- cases[complete.cases(cases),]

LM1 <- glm(paste0("Sx_any_PCS ~ covid19_test"), data=cases, family=binomial(link="logit"))
LM2 <- glm(paste0("Sx_any_PCS ~ hospital"), data=cases, family=binomial(link="logit"))
LM3 <- glm(paste0("Sx_any_PCS ~ age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=cases, family=binomial(link="logit"))
LM4 <- glm(paste0("Sx_any_PCS ~ hospital +  age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=cases, family=binomial(link="logit"))
LM5 <- glm(paste0("Sx_any_PCS ~ covid19_test +hospital+ age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=cases, family=binomial(link="logit"))
cases$pred1 <- predict(LM1, cases)
cases$pred2 <- predict(LM2, cases)
cases$pred3 <- predict(LM3, cases)
cases$pred4 <- predict(LM4, cases)
cases$pred5 <- predict(LM5, cases)
cases <- data.frame(cases)
response <- as.numeric(unlist(cases[,"Sx_any_PCS"]))
ROC1 <- roc(response=response, predictor=cases$pred1) 
ROC2 <- roc(response=response, predictor=cases$pred2)
ROC3 <- roc(response=response, predictor=cases$pred3)
ROC4 <- roc(response=response, predictor=cases$pred4) 
ROC5 <- roc(response=response, predictor=cases$pred5)

out[,1] <- "Persistent symptoms"
out[1,2] <- "SARS-CoV-2 test positivity"
out[1,3] <- ROC1$auc
out[1,4] <- ci.auc(ROC1, conf.level=0.95)[1]
out[1,5] <- ci.auc(ROC1, conf.level=0.95)[3]
out[1,6] <- AIC(LM1)
out[1,7] <- BIC(LM1)
out[2,2] <- "Hospitalization"
out[2,3] <- ROC2$auc
out[2,4] <- ci.auc(ROC2, conf.level=0.95)[1]
out[2,5] <- ci.auc(ROC2, conf.level=0.95)[3]
out[2,6] <- AIC(LM2)
out[2,7] <- BIC(LM2)
out[3,2] <- "Demographics + comorbidities"
out[3,3] <- ROC3$auc
out[3,4] <- ci.auc(ROC3, conf.level=0.95)[1]
out[3,5] <- ci.auc(ROC3, conf.level=0.95)[3]
out[3,6] <- AIC(LM3)
out[3,7] <- BIC(LM3)
out[4,2] <- "Hospitalization\n+ demographics + comorbidities"
out[4,3] <- ROC4$auc
out[4,4] <- ci.auc(ROC4, conf.level=0.95)[1]
out[4,5] <- ci.auc(ROC4, conf.level=0.95)[3]
out[4,6] <- AIC(LM4)
out[4,7] <- BIC(LM4)
out[5,2] <- "SARS-CoV-2 test positivity + hospitalization\n+ demographics + comorbidities"
out[5,3] <- ROC5$auc
out[5,4] <- ci.auc(ROC5, conf.level=0.95)[1]
out[5,5] <- ci.auc(ROC5, conf.level=0.95)[3]
out[5,6] <- AIC(LM5)
out[5,7] <- BIC(LM5)

plot.roc(ROC1)
plot.roc(ROC5, add=T)
roc.test(ROC1, ROC3, method="delong", alternative="two.sided")$p
roc.test(ROC1, ROC4, method="delong", alternative="two.sided")$p
roc.test(ROC4, ROC5, method="delong", alternative="two.sided")$p

out <- out %>% mutate(AUC = as.numeric(AUC),
                      AUC.LL = as.numeric(AUC.LL),
                      AUC.UL = as.numeric(AUC.UL)
                      )
write.xlsx(out, file="longCOVID_AUC.xlsx")

out$model <- factor(out$model, levels = out$model)

png("AUC.png",width=500, height=600)
p1 <- ggplot(out, aes(x=model, y=AUC, ymin=AUC.LL, ymax=AUC.UL, fill=model)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_errorbar(width=.2,
                position=position_dodge(.9)) + coord_cartesian(ylim=c(0.5,0.75)) + 
  xlab("") + ylab("AUC (95% Confidence Interval)") + geom_text(aes(y=AUC.UL+0.03, label=round(AUC,2), hjust = 0.5,vjust=0),size=6, position=position_dodge(.9))+
  theme_classic() + ggtitle("") +
  scale_fill_manual(values=c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E"))  +
  theme(text = element_text(size=15, face="bold"),legend.position = "none",
        axis.text.x = element_text(angle = 70,hjust = 1, size=15)) + guides(color = guide_legend(legend.position = "None"))

p1
dev.off()

#missingness pattern
library(finalfit) 
data <- readRDS("clinical_bqc19.rds")
cases <- data[,c("Sx_any_PCS","covid19_test","hospital","age", "sex", "obesity", "smoking", "com_asthma", "com_COPD", "com_hypertension", 
                 "com_diabetes", "com_autoimmune", "com_cancer", "com_dementia")]
cases <- cases %>% mutate_all(.funs=funs(ifelse(.==-1, NA, .)))
cases <- cases %>% mutate_all(.funs=funs(ifelse(is.infinite(.), NA, .)))
cases <- cases %>% mutate_at(.vars=vars(c("Sx_any_PCS", "covid19_test", "hospital", "obesity", colnames(cases)[grepl("^com_", colnames(cases))])),
                             .funs=funs(as.factor(.)))
explanatory = c("covid19_test","hospital","age", "sex", "obesity", "smoking", "com_asthma", "com_COPD", "com_hypertension", 
                "com_diabetes", "com_autoimmune", "com_cancer", "com_dementia")
dependent = "Sx_any_PCS"

cases %>% 
  missing_pattern(dependent, explanatory)
cases %>% 
  ff_glimpse(dependent, explanatory)
cases %>% 
  missing_plot()
cases %>%  
  missing_pairs(dependent, explanatory, position = "fill")
cases %>%  
  missing_compare(dependent, explanatory)
library(dplyr)

explanatory_factor <- c("covid19_test","hospital","sex", "obesity", "smoking", "com_asthma", "com_COPD", "com_hypertension", 
                        "com_diabetes", "com_autoimmune", "com_cancer", "com_dementia")
cases_new <- cases %>% 
  mutate_at(.vars=vars(c(explanatory_factor)),
            .funs=funs(forcats::fct_explicit_na(.))) %>%
  mutate(age = ifelse(is.na(age), mean(age, na.rm=T), age))
cases_new <- cases_new[complete.cases(cases_new),]

out <- data.frame(matrix(0, 5, 7))
colnames(out) <- c("outcome","model","AUC","AUC.LL","AUC.UL", "AIC", "BIC")

LM1 <- glm(paste0("Sx_any_PCS ~ covid19_test"), data=cases_new, family=binomial(link="logit"))
LM2 <- glm(paste0("Sx_any_PCS ~ hospital"), data=cases_new, family=binomial(link="logit"))
LM3 <- glm(paste0("Sx_any_PCS ~ age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=cases_new, family=binomial(link="logit"))
LM4 <- glm(paste0("Sx_any_PCS ~ hospital +  age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=cases_new, family=binomial(link="logit"))
LM5 <- glm(paste0("Sx_any_PCS ~ covid19_test +hospital+ age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=cases_new, family=binomial(link="logit"))
cases_new$pred1 <- predict(LM1, cases_new)
cases_new$pred2 <- predict(LM2, cases_new)
cases_new$pred3 <- predict(LM3, cases_new)
cases_new$pred4 <- predict(LM4, cases_new)
cases_new$pred5 <- predict(LM5, cases_new)
cases_new <- data.frame(cases_new)
response <- as.numeric(unlist(cases_new[,"Sx_any_PCS"]))
ROC1 <- roc(response=response, predictor=cases_new$pred1) 
ROC2 <- roc(response=response, predictor=cases_new$pred2)
ROC3 <- roc(response=response, predictor=cases_new$pred3)
ROC4 <- roc(response=response, predictor=cases_new$pred4) 
ROC5 <- roc(response=response, predictor=cases_new$pred5)

out[,1] <- "Persistent symptoms"
out[1,2] <- "SARS-CoV-2 test positivity"
out[1,3] <- ROC1$auc
out[1,4] <- ci.auc(ROC1, conf.level=0.95)[1]
out[1,5] <- ci.auc(ROC1, conf.level=0.95)[3]
out[1,6] <- AIC(LM1)
out[1,7] <- BIC(LM1)
out[2,2] <- "Hospitalization"
out[2,3] <- ROC2$auc
out[2,4] <- ci.auc(ROC2, conf.level=0.95)[1]
out[2,5] <- ci.auc(ROC2, conf.level=0.95)[3]
out[2,6] <- AIC(LM2)
out[2,7] <- BIC(LM2)
out[3,2] <- "Demographics + comorbidities"
out[3,3] <- ROC3$auc
out[3,4] <- ci.auc(ROC3, conf.level=0.95)[1]
out[3,5] <- ci.auc(ROC3, conf.level=0.95)[3]
out[3,6] <- AIC(LM3)
out[3,7] <- BIC(LM3)
out[4,2] <- "Hospitalization\n+ demographics + comorbidities"
out[4,3] <- ROC4$auc
out[4,4] <- ci.auc(ROC4, conf.level=0.95)[1]
out[4,5] <- ci.auc(ROC4, conf.level=0.95)[3]
out[4,6] <- AIC(LM4)
out[4,7] <- BIC(LM4)
out[5,2] <- "SARS-CoV-2 test positivity + hospitalization\n+ demographics + comorbidities"
out[5,3] <- ROC5$auc
out[5,4] <- ci.auc(ROC5, conf.level=0.95)[1]
out[5,5] <- ci.auc(ROC5, conf.level=0.95)[3]
out[5,6] <- AIC(LM5)
out[5,7] <- BIC(LM5)

plot.roc(ROC1)
plot.roc(ROC5, add=T)
roc.test(ROC1, ROC3, method="delong", alternative="two.sided")$p
roc.test(ROC1, ROC4, method="delong", alternative="two.sided")$p
roc.test(ROC4, ROC5, method="delong", alternative="two.sided")$p

write.xlsx(out, file="longCOVID.missing.xlsx")

  
out <- data.frame(matrix(0, 5, 7))
colnames(out) <- c("outcome","model","AUC","AUC.LL","AUC.UL", "AIC", "BIC")

#cases <- data[,c("Sx_any_PCS","covid19_test","hospital","age", "sex", "obesity", "com_asthma")]
cases <- data[,c("Sx_any_PCS","covid19_test","hospital","age", "sex", "obesity", "smoking", "com_asthma", "com_COPD", "com_hypertension", 
                 "com_diabetes", "com_autoimmune", "com_cancer", "com_dementia")]
#cases <- cases %>% mutate_all(.funs=funs(ifelse(.==-1, 0, .)))
cases <- cases %>% mutate_all(.funs=funs(ifelse(.==-1, NA, .)))
cases <- cases %>% mutate_all(.funs=funs(ifelse(is.infinite(.), NA, .)))
library(mice)

#cases <- cases %>% mutate(sex = ifelse(sex == "F", 1, 0),
#                          Current = case_when(smoking == "Current" ~ 1, 
#                                              smoking == "Past" ~ 0, 
#                                              smoking == "0Never" ~ 0),
#                          Ever = case_when(smoking == "Current" ~ 1, 
#                                           smoking == "Past" ~ 1, 
#                                           smoking == "0Never" ~ 0)) %>% select(-smoking)
  
library("tidyverse")

cases <- cases %>% mutate_at(.vars=vars(c("Sx_any_PCS", "covid19_test", "hospital", "obesity", colnames(cases)[grepl("^com_", colnames(cases))])),
                             .funs=funs(as.factor(.)))
imp1 <- mice(cases, m = 20)
imp_tot2 <- complete(imp1, "long", inc = TRUE)
for(i in seq(1,20)){
  out <- data.frame(matrix(0, 5, 6))
  colnames(out) <- c("imp","model","AUC","AUC.se", "AIC", "BIC")
  tmp <- imp_tot2 %>% filter(.imp == i)
  LM1 <- glm(paste0("Sx_any_PCS ~ covid19_test"), data=tmp, family=binomial(link="logit"))
  LM2 <- glm(paste0("Sx_any_PCS ~ hospital"), data=tmp, family=binomial(link="logit"))
  LM3 <- glm(paste0("Sx_any_PCS ~ age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=tmp, family=binomial(link="logit"))
  LM4 <- glm(paste0("Sx_any_PCS ~ hospital +  age + sex + obesity + smoking + 
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=tmp, family=binomial(link="logit"))
  LM5 <- glm(paste0("Sx_any_PCS ~ covid19_test +hospital+ age + sex + obesity + smoking +  
               com_asthma + com_COPD + com_hypertension + com_diabetes + com_autoimmune +
               com_cancer + com_dementia"), data=tmp, family=binomial(link="logit"))
  tmp$pred1 <- predict(LM1, tmp)
  tmp$pred2 <- predict(LM2, tmp)
  tmp$pred3 <- predict(LM3, tmp)
  tmp$pred4 <- predict(LM4, tmp)
  tmp$pred5 <- predict(LM5, tmp)
  tmp <- data.frame(tmp)
  response <- as.numeric(unlist(tmp[,"Sx_any_PCS"]))
  ROC1 <- roc(response=response, predictor=tmp$pred1) 
  ROC2 <- roc(response=response, predictor=tmp$pred2)
  ROC3 <- roc(response=response, predictor=tmp$pred3)
  ROC4 <- roc(response=response, predictor=tmp$pred4) 
  ROC5 <- roc(response=response, predictor=tmp$pred5)
  
  out[,1] <- i
  out[1,2] <- "SARS-CoV-2 test positivity"
  out[1,3] <- ROC1$auc
  out[1,4] <- (ci.auc(ROC1, conf.level=0.95)[3] - ci.auc(ROC1, conf.level=0.95)[1])/(2*qnorm(0.975))
  out[1,5] <- AIC(LM1)
  out[1,6] <- BIC(LM1)
  out[2,2] <- "Hospitalization"
  out[2,3] <- ROC2$auc
  out[2,4] <- (ci.auc(ROC2, conf.level=0.95)[3] - ci.auc(ROC2, conf.level=0.95)[1])/(2*qnorm(0.975))
  out[2,5] <- AIC(LM2)
  out[2,6] <- BIC(LM2)
  out[3,2] <- "Demographics + comorbidities"
  out[3,3] <- ROC3$auc
  out[3,4] <- (ci.auc(ROC3, conf.level=0.95)[3] - ci.auc(ROC3, conf.level=0.95)[1])/(2*qnorm(0.975))
  out[3,5] <- AIC(LM3)
  out[3,6] <- BIC(LM3)
  out[4,2] <- "Hospitalization + demographics + comorbidities"
  out[4,3] <- ROC4$auc
  out[4,4] <- (ci.auc(ROC4, conf.level=0.95)[3] - ci.auc(ROC4, conf.level=0.95)[1])/(2*qnorm(0.975))
  out[4,5] <- AIC(LM4)
  out[4,6] <- BIC(LM4)
  out[5,2] <- "SARS-CoV-2 test positivity + hospitalization + demographics + comorbidities"
  out[5,3] <- ROC5$auc
  out[5,4] <- (ci.auc(ROC5, conf.level=0.95)[3] - ci.auc(ROC5, conf.level=0.95)[1])/(2*qnorm(0.975))
  out[5,5] <- AIC(LM5)
  out[5,6] <- BIC(LM5)
  write.table(out, file="AUC.imp.tsv", quote=F, col.names = F, row.names = F, append = T, sep="\t")
}
out <- fread("AUC.imp.tsv")
colnames(out) <- c("imp","model","AUC","AUC.se", "AIC", "BIC")
write.table(out, file="AUC.imp.tsv", quote=F, col.names = T, row.names = F, append = F, sep="\t")
library(psfmi)
out <- data.frame(matrix(0, 5, 4))
colnames(out) <- c("model","AUC","AUC.LL", "AUC.UL")

out[1:5,1] <- c("SARS-CoV-2 test positivity", "Hospitalization", "Demographics + comorbidities", 
                "Hospitalization + demographics + comorbidities", 
                "SARS-CoV-2 test positivity + hospitalization + demographics + comorbidities")
data <- fread("AUC.imp.tsv")
modellist <- c("SARS-CoV-2 test positivity", "Hospitalization", "Demographics + comorbidities", 
               "Hospitalization + demographics + comorbidities", 
               "SARS-CoV-2 test positivity + hospitalization + demographics + comorbidities")
for(i in seq(1,5)){
  tmp <- data %>% filter(model == modellist[i])
  out[i,2] <- pool_auc(tmp$AUC, tmp$AUC.se, nimp = 20, log_auc = TRUE)[2]
  out[i,3] <- pool_auc(tmp$AUC, tmp$AUC.se, nimp = 20, log_auc = TRUE)[1]
  out[i,4] <- pool_auc(tmp$AUC, tmp$AUC.se, nimp = 20, log_auc = TRUE)[3]
}

out %>% write.xlsx("imputation.AUC.xlsx")


