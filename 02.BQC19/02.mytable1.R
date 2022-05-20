setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID")

final <- readRDS("clinical_bqc19.rds")
final <- final %>% mutate(vaccin_dosenum = ifelse(vaccinate == 0, 0, vaccin_dosenum))
library(tableone)

final <- final %>% mutate_at(.vars=c("covid19_test_result", "sx_any", "sx_any_PCS", "hospitalization"), .funs=funs(ifelse(.==-1, NA, .)))
Vars <- c("age", "sex", "obesity", "smoking", "sx_any", "sx_any_PCS", "hospitalization", colnames(final)[grepl("^com_", colnames(final))], "prevaccination")
catVars <- c("sex", "obesity", "smoking", "sx_any", "sx_any_PCS", "hospitalization", colnames(final)[grepl("^com_", colnames(final))], "prevaccination")
tableOne <- CreateTableOne(vars = Vars , strata = c("covid19_test_result"), data = final,
                           factorVars = catVars)

tab3Mat <- print(tableOne, quote=F, noSpaces=TRUE)
summary(tableOne)
write.csv(tab3Mat, file = "myTable1.csv")
