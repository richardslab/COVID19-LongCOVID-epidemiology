setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID")

data <- read.csv("/home/richards/tomoko.nakanishi/scratch/09.COVID19/05.BQC/BQC_phenotype/Release5/Clinical_Phenotypic/redcap_clinical_data-2021-12-09.csv", header = T, fileEncoding="latin1", sep="\t")
date <- data %>% select(BQC.ID, colnames(data)[grepl(glob2rx("Date.of.*.COVID.test.") , colnames(data))],
                         Date.of.PCR.test.determining.the.participant.s.COVID.status.)
colnames(date) <- c("BQC.ID", paste0(c("test.date."),rep(2:10)),paste0(c("test.date."),1))

data_long <- date %>% pivot_longer(!(c(BQC.ID)),
                                    names_to="test",
                                    values_to="data")

data_long1 <- data_long %>% mutate(test = gsub("test.date.", "", test))

res <- data %>% select(BQC.ID, colnames(data)[grepl(glob2rx("Result.of.*.COVID.test.") , colnames(data))],
                        Result.of.the.PCR.test.determining.the.participant.s.COVID.status)
colnames(res) <- c("BQC.ID", paste0(c("test.date."),rep(2:10)),paste0(c("test.date."),1))


data_long <- res %>% pivot_longer(!(c(BQC.ID)),
                                   names_to="test",
                                   values_to="result")

data_merged <- bind_cols(data_long1, data_long2$result)
colnames(data_merged) <- c("BQC.ID", "instance", "date", "result")
data_merged <- data_merged  %>% mutate_all(.funs=funs(ifelse(.=="", NA, .)))

data_merged <- data_merged %>% filter(!is.na(date) & !is.na(result))

data_merged <- data_merged %>% arrange(BQC.ID, instance)

data <- data_merged %>% group_by(BQC.ID) %>% 
  mutate(Result = ifelse(any(result == "Positive"), 1, 0)) %>%
  ungroup() %>%
  distinct(BQC.ID, .keep_all = TRUE)



data <- data %>% group_by(BQC.ID) %>% 
  mutate(Date = case_when(Result == 1 ~ min(as.Date(date))
                          
                          
                          
  ungroup()

