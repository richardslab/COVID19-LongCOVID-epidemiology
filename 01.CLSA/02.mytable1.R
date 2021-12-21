setwd("/home/richards/tomoko.nakanishi/scratch/09.COVID19/06.CLSA")
covid <- readRDS("coviddata.20211007.rds")

library(tableone)

Vars <- c("age", "sex", "obesity", "smoking", colnames(covid)[grepl("^com_", colnames(covid))], "Sx_any", "Sx_any_PCS", "hospital")
catVars <- c("sex", "obesity", "smoking", colnames(covid)[grepl("^com_", colnames(covid))], "Sx_any", "Sx_any_PCS", "hospital")

tableOne <- CreateTableOne(vars = Vars , strata = c("C2"), data = covid,
                           factorVars = catVars )

tab3Mat <- print(tableOne, quote=F, noSpaces=TRUE)
summary(tableOne)
write.csv(tab3Mat, file = "myTable1.csv")
