setwd("/home/richards/tomoko.nakanishi/scratch/09.COVID19/08.LongCOVID_epi/")

out1 <- fread("/home/richards/tomoko.nakanishi/scratch/09.COVID19/06.CLSA/CLSA_any.cases.tsv")
out2 <- fread("/home/richards/tomoko.nakanishi/scratch/09.COVID19/06.CLSA/CLSA_any.controls.tsv")
out3 <- fread("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID/BQC19_any.cases.tsv")
out4 <- fread("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID/BQC19_any.controls.tsv")

OUT <- bind_rows(out1, out2, out3, out4)
OUT <- OUT %>% mutate(cohort = case_when(grepl("UKB", group) ~ "UKB",
                                         grepl("CLSA", group) ~ "CLSA",
                                         grepl("BQC19", group) ~ "BQC19"),
                      phenotype = case_when(grepl("Any", group) ~ "Any"),
                      casecontrol = case_when(grepl("case", group) ~ "cases",
                                              grepl("control", group) ~ "controls"))

OUT$covariates[OUT$covariates == "smokingCurrent"] <- "smokingcurrent"
OUT$covariates[OUT$covariates == "smokingPast"] <- "smokingpast"
OUT$covariates[OUT$covariates == "com_COPD"] <- "com_copd"

data <- data.frame(matrix(0, 30, 6))
colnames(data) <- c("covariates","beta", "se", "pval", "meta", "phenotype")
data$group <- "cases"
data$phenotype <- "Any"
library(meta)
tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "cases" & covariates == "hospital")
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")

data$covariates[1:2] <- "hospital"
data[1,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[2,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[1:2,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "cases" & covariates %in% c("age_range50-60"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")

data$covariates[3:4] <- "age_range50-60"
data[3,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[4,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[3:4,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "cases" & covariates %in% c("age_range70-80"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[5:6] <- "age_range70-80"
data[5,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[6,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[5:6,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "cases" & covariates %in% c("age_range>80"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[7:8] <- "age_range>80"
data[7,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[8,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[7:8,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "cases" & covariates %in% c("sexF"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[9:10] <- "sexF"
data[9,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[10,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[9:10,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "cases" & covariates %in% c("obesity"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[11:12] <- "obesity"
data[11,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[12,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[11:12,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "cases"  & covariates %in% c("smokingcurrent"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[13:14] <- "smokingcurrent"
data[13,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[14,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[13:14,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "cases" & covariates %in% c("smokingpast"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[15:16] <- "smokingpast"
data[15,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[16,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[15:16,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "cases" & covariates %in% c("com_asthma"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[17:18] <- "com_asthma"
data[17,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[18,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[17:18,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "cases" & covariates %in% c("com_copd"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[19:20] <- "com_copd"
data[19,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[20,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[19:20,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "cases" & covariates %in% c("com_hypertension"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[21:22] <- "com_hypertension"
data[21,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[22,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[21:22,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "cases" & covariates %in% c("com_diabetes"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[23:24] <- "com_diabetes"
data[23,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[24,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[23:24,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "cases" & covariates %in% c("com_autoimmune"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[25:26] <- "com_autoimmune"
data[25,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[26,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[25:26,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "cases" & covariates %in% c("com_cancer"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[27:28] <- "com_cancer"
data[27,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[28,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[27:28,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "cases" & covariates %in% c("com_dementia"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[29:30] <- "com_dimentia"
data[29,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[30,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[29:30,5] <- c("fixed", "random")

write.table(data, file="/home/richards/tomoko.nakanishi/scratch/09.COVID19/08.LongCOVID_epi/anySx.cases.tsv", sep="\t", quote=F, col.names = T, row.names = F)


data <- data.frame(matrix(0, 30, 6))
colnames(data) <- c("covariates","beta", "se", "pval", "meta", "phenotype")
data$group <- "controls"
data$phenotype <- "Any"
library(meta)
tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "controls" & covariates == "hospital")
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")

data$covariates[1:2] <- "hospital"
data[1,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[2,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[1:2,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "controls" & covariates %in% c("age_range50-60"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")

data$covariates[3:4] <- "age_range50-60"
data[3,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[4,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[3:4,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "controls" & covariates %in% c("age_range70-80"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[5:6] <- "age_range70-80"
data[5,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[6,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[5:6,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "controls" & covariates %in% c("age_range>80"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[7:8] <- "age_range>80"
data[7,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[8,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[7:8,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "controls" & covariates %in% c("sexF"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[9:10] <- "sexF"
data[9,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[10,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[9:10,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "controls" & covariates %in% c("obesity"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[11:12] <- "obesity"
data[11,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[12,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[11:12,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "controls"  & covariates %in% c("smokingcurrent"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[13:14] <- "smokingcurrent"
data[13,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[14,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[13:14,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "controls" & covariates %in% c("smokingpast"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[15:16] <- "smokingpast"
data[15,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[16,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[15:16,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "controls" & covariates %in% c("com_asthma"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[17:18] <- "com_asthma"
data[17,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[18,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[17:18,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "controls" & covariates %in% c("com_copd"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[19:20] <- "com_copd"
data[19,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[20,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[19:20,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "controls" & covariates %in% c("com_hypertension"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[21:22] <- "com_hypertension"
data[21,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[22,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[21:22,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "controls" & covariates %in% c("com_diabetes"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[23:24] <- "com_diabetes"
data[23,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[24,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[23:24,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "controls" & covariates %in% c("com_autoimmune"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[25:26] <- "com_autoimmune"
data[25,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[26,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[25:26,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "controls" & covariates %in% c("com_cancer"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[27:28] <- "com_cancer"
data[27,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[28,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[27:28,5] <- c("fixed", "random")

tmp <- OUT %>% filter(phenotype == "Any" & casecontrol == "controls" & covariates %in% c("com_dementia"))
m1 <- metagen(Estimate,
              Std..Error,
              data=tmp,
              studlab=paste(cohort),
              comb.fixed = TRUE,
              comb.random = TRUE,
              prediction=FALSE,
              sm="OR")
m1

data$covariates[29:30] <- "com_dimentia"
data[29,2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
data[30,2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
data[29:30,5] <- c("fixed", "random")

write.table(data, file="/home/richards/tomoko.nakanishi/scratch/09.COVID19/08.LongCOVID_epi/anySx.controls.tsv", sep="\t", quote=F, col.names = T, row.names = F)


out1 <- fread("/home/richards/tomoko.nakanishi/scratch/09.COVID19/08.LongCOVID_epi/anySx.cases.tsv")
out2 <- fread("/home/richards/tomoko.nakanishi/scratch/09.COVID19/08.LongCOVID_epi/anySx.controls.tsv")

data <- bind_rows(out1, out2)

data <- data %>% mutate(OR = exp(beta),
                        LL = exp(beta - qnorm(0.975)*se),
                        UL = exp(beta + qnorm(0.975)*se))

data1 <- data %>% filter(meta == "fixed")

data1$covariates <- rep(c("Hospitalization", "50-60", "70-80", ">80", "Female", "BMI>30", 
                          "Current smoker", "Past smoker", "Asthma", "COPD", "Hypertension", "Diabetes", "Autoimmune",
                          "Cancer", "Dementia"), 2)

tmp <- data1[1:4,]
tmp$covariates <- rep(c("60-70 (ref)", "Never smoker (ref)"), 2)
tmp$beta <- 0
tmp$se <- 0
tmp$pval <- 1
tmp$meta <- "fixed"
tmp$group <- rep(c("cases", "controls"), 2)
tmp$OR <- 1
tmp$LL <- 1
tmp$UL <- 1

data1 <- bind_rows(data1, tmp)

data1 <- data1 %>% mutate(OR = as.numeric(OR),
                          LL = as.numeric(LL),
                          UL = as.numeric(UL)) 

data1$covariates <- factor(data1$covariates, levels = c("Hospitalization", "50-60", "60-70 (ref)",  "70-80",">80",
                                                        "Female", "BMI>30", "Never smoker (ref)",
                                                        "Current smoker", "Past smoker", "Asthma", "COPD", "Autoimmune","Hypertension", "Diabetes", 
                                                        "Cancer", "Dementia"))

data1 <- data1 %>% mutate(sig = case_when(as.numeric(pval) <= 0.05 ~ TRUE,
                                          TRUE ~ FALSE),
                          shape = case_when(as.numeric(pval) > 0.05 ~ 21,
                                            as.numeric(pval) <= 0.05 & OR > 1 ~ 24,
                                            as.numeric(pval) <= 0.05 & OR < 1 ~ 25))

png("case_control_anySx.png", width = 700, height = 700)
data1 %>%
  ggplot(aes(y=OR, ymin=LL, ymax=UL, x=group, color=group)) + geom_pointrange(aes(col=group, fill=group, alpha=sig, shape=shape), lwd=0.8) + geom_hline(aes(), yintercept = 1, linetype=2) +
  theme_minimal() + facet_wrap(~covariates,strip.position="left",nrow=20,scales = "free_y") +
  scale_shape_identity() + ggtitle("Any symptoms") + 
  geom_errorbar(aes(ymin=LL, ymax=UL, col=group, alpha=sig), width=0.1, cex=1) + xlab("")  + ylab("Odds ratio") + 
  scale_color_manual(values=c("#008fd5","#de6b35"), label = c("non-COVID","COVID")) + 
  scale_fill_manual(values=c("#008fd5","#de6b35")) + scale_y_log10(limits=c(0.4, 9.5), breaks=c(0.1, 0.4, 0.5, 1, 2, 3, 4, 5)) + scale_alpha_discrete(range = c(0.5, 1)) + 
  theme(axis.text.x = element_text(size=15,face="bold"),
        axis.text.y=element_blank(),
        title = element_text(size=15, face="bold"),
        axis.title=element_text(size=15,face="bold"), 
        strip.text.y.left = element_text(hjust=0.5,vjust = 0.5,angle=0,size=15,face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(size=15)) +coord_flip() + guides(col = guide_legend(reverse = TRUE),
                                                                    fill = FALSE,
                                                                    shape = FALSE,
                                                                    alpha = FALSE)
dev.off()

