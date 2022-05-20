setwd("/home/richards/tomoko.nakanishi/09.COVID19/scratch/05.BQC/10.longCOVID")

final <- readRDS("clinical_bqc19.rds")


CHUM <- read.xlsx("/home/richards/tomoko.nakanishi/scratch/09.COVID19/05.BQC/BQC_phenotype/CHUM/20220126_BQC19_Lab&Demo_Share_Lock.xlsx")
library(stringr)

CHUM <- CHUM %>% mutate(ID = case_when(Center == "JGH" ~ paste0("BQC_JGH",str_pad(Local_Code, 4, pad = "0")),
                                       Center == "CHUM" ~ paste0("BQC_CRCHUM",Local_Code)))
CHUM <- CHUM %>% mutate(covid_status = case_when(`COVID-19_Status` == "POSITIF" ~ "Positive",
                                                 `COVID-19_Status` == "NEGATIF" ~ "Negative"))                        

CHUM <- CHUM %>% mutate(first_date_of_symptom = as.Date(as.numeric(Date_COVID_Symptom_Onset), origin = "1899/12/30"))

CHUM <- CHUM %>% select(ID, covid_status, first_date_of_symptom)

CHUM <- unique(CHUM)

map <- read.xlsx("/home/richards/tomoko.nakanishi/09.COVID19/data/05.BQC/01.genotype/v5.0/01.batch/Master_list_WHOLE_BLOOD_CORRECTED_10112021.xlsx", sheet = "concilie")
map <- map %>% select(Alias.BQCid, Individual.id)
map <- unique(map)

clinical <- read.csv("/home/richards/tomoko.nakanishi/scratch/09.COVID19/05.BQC/BQC_phenotype/Release5/Clinical_Phenotypic/redcap_clinical_data-2021-12-09.csv", header = T, fileEncoding="latin1", sep="\t")
clinical <- clinical %>% select(BQC.ID, Date.of.earliest.symptom.s.., Final.COVID.status.)

clinical <- unique(clinical)
clinical <- clinical %>% group_by(BQC.ID) %>%
  distinct(BQC.ID, .keep_all = T)
clinical1 <- clinical %>% merge(map, by.x="BQC.ID", by.y="Alias.BQCid", all.x = T)

final <- merge(clinical1, CHUM, by.x="Individual.id", by.y="ID")
final %>% write.xlsx("/home/richards/tomoko.nakanishi/scratch/09.COVID19/05.BQC/BQC_phenotype/sanitycheck20220207.xlsx")

#final <- read.xlsx("/home/richards/tomoko.nakanishi/scratch/09.COVID19/05.BQC/BQC_phenotype/sanitycheck20220207.xlsx")
final %>% filter(Date.of.earliest.symptom.s.. != first_date_of_symptom) %>% dim()

final %>% filter(grepl("CRCHUM", Individual.id)) %>%
  filter(abs(as.Date(Date.of.earliest.symptom.s..) - as.Date(first_date_of_symptom)) > 3) %>% write.xlsx("/home/richards/tomoko.nakanishi/scratch/09.COVID19/05.BQC/BQC_phenotype/sanitycheck20220214.xlsx")




