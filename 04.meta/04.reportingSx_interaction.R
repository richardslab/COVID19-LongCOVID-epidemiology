setwd("/home/richards/tomoko.nakanishi/scratch/09.COVID19/08.LongCOVID_epi/")

out1 <- fread("/home/richards/tomoko.nakanishi/scratch/09.COVID19/06.CLSA/CLSA_summary.interaction.tsv")
colnames(out1) <- c("beta", "se", "z", "pval", "cohort", "Sx", "covid", "covariates", "Ncase", "Ncontrol")

out2 <- fread("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID/BQC19_summary.interaction.tsv")
colnames(out2) <- c("beta", "se", "z", "pval", "cohort", "Sx", "covid", "covariates", "Ncase", "Ncontrol")

OUT <- bind_rows(out1, out2)

OUT <- OUT %>% mutate(covariates = case_when(grepl("C2", covariates) ~ gsub("C2", "covid19_test_resultPositive", covariates),
                                             grepl("smokingCurrent", covariates) ~ gsub("smokingCurrent", "smokingcurrent", covariates),
                                             grepl("smokingPast", covariates) ~ gsub("smokingPast", "smokingpast", covariates),
                                             grepl("com_COPD", covariates) ~ gsub("com_COPD", "com_copd", covariates),
                                             grepl("hospitalization", covariates) ~ gsub("hospitalization", "Hospitalization", covariates),
                                             grepl("hospital", covariates) ~ gsub("hospital", "Hospitalization", covariates),
                                             TRUE ~ covariates
                                            ))

OUT <- OUT %>% mutate(covariates = case_when(covariates == "covid19_test_resultPositive:hospital" ~ "covid19_test_resultPositive:Hospitalization",
                                             TRUE ~ covariates
))

table(OUT[,c("covariates", "cohort")])
library(meta)

cov <- unique(OUT$covariates)

data <- data.frame(matrix(0, length(cov)*2, 5))
colnames(data) <- c("covariates","beta", "se", "pval", "meta")
for(j in seq(1, length(cov))){
  tmp <- OUT %>% filter(covariates == cov[j])
      m1 <- metagen(beta,
                    se,
                    data=tmp,
                    studlab=paste(cohort),
                    comb.fixed = TRUE,
                    comb.random = TRUE,
                    prediction=FALSE,
                    sm="OR")
      data$covariates[(2*j-1):(2*j)] <- cov[j]
      data[(2*j-1),2:4] <- m1[c("TE.fixed", "seTE.fixed", "pval.fixed")]
      data[(2*j),2:4] <- m1[c("TE.random", "seTE.random", "pval.random")]
      data[(2*j-1):(2*j),5] <- c("fixed", "random")
}

data

write.table(data, file="meta_summary.interact.tsv", sep="\t", quote=F, col.names = F, row.names = F, append=F)


data <- fread("meta_summary.interact.tsv")
colnames(data) <-  c("covariates","beta", "se", "pval", "meta")

data <- data %>% mutate(OR = exp(beta),
                        LL = exp(beta - qnorm(0.975)*se),
                        UL = exp(beta + qnorm(0.975)*se))

data %>% filter(pval < 0.05)
write.xlsx(data, file="/home/richards/tomoko.nakanishi/my_project/repo/COVID19-LongCOVID-epidemiology/results/meta.reportingSx.interaction.xlsx")
