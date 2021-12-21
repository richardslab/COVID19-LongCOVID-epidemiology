setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID")

final <- readRDS("clinical_bqc19.rds")

library(tableone)

final <- final %>% mutate_at(.vars=c("covid19_test", "Sx_any", "Sx_any_PCS", "hospital"), .funs=funs(ifelse(.==-1, NA, .)))
Vars <- c("age", "sex", "obesity", "smoking", "Sx_any", "Sx_any_PCS", "hospital", colnames(final)[grepl("^com_", colnames(final))])
catVars <- c("sex", "obesity", "smoking", "Sx_any", "Sx_any_PCS", "hospital", colnames(final)[grepl("^com_", colnames(final))])
tableOne <- CreateTableOne(vars = Vars , strata = c("covid19_test"), data = final,
                           factorVars = catVars )

tab3Mat <- print(tableOne, quote=F, noSpaces=TRUE)
summary(tableOne)
write.csv(tab3Mat, file = "myTable1.csv")

