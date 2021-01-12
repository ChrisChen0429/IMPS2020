library(tidyverse)
setwd("~/Desktop/research/conference/large-scale assessment/IMPS2020/new")
library(haven)
COG <- read_sav("data/CY07MSU_QMC_STU_COG.sav")
TTM <- read_sav("data/CY07_QMC_STU_TTM.SAV")

data_a <- COG %>% 
  select(CNTSTUID,starts_with("CM") | starts_with("PM") | starts_with("CR") | 
           starts_with("CR") | starts_with("CR")) %>% 
  select(CNTSTUID,ends_with("S") | ends_with("SA") | ends_with("SB") | ends_with("SC"))
colnames(data_a) <- c("ID",str_replace(colnames(data_a)[2:length(colnames(data_a))],"S",""))
data_t <- TTM %>% 
  select(CNTSTUID,starts_with("CM") | starts_with("PM") | starts_with("CR") | 
                           starts_with("CR") | starts_with("CR")) %>% 
  select(CNTSTUID,ends_with("TT"))
colnames(data_t) <- c("ID",str_replace(colnames(data_t)[2:length(colnames(data_t))],"TT",""))

common_col <- intersect(colnames(data_a),colnames(data_t))
data_a <- data_a %>% select(common_col)
data_t <- data_t %>% select(common_col)

un_missing <- c()
for (i in 1:nrow(data_a)){
  un_missing <- c(un_missing,sum(! is.na(data_a[i,])))
}

table(un_missing)

complete.cases(data_a[,2:ncol(data_a)])


