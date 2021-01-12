library(dplyr)
library(tidyr)
library(foreign)
library(ggplot2)


stud_QQQ_2018 <- read.spss("~/Downloads/QMC/CY07MSU_QMC_STU_QQQ.sav", use.value.labels = TRUE, to.data.frame = TRUE)
stud_TIM_2018 <- read.spss("~/Downloads/QMC/CY07MSU_QMC_STU_TIM.sav", use.value.labels = TRUE, to.data.frame = TRUE)
stud_COG_2018 <- read.spss("~/Downloads/QMC/CY07MSU_QMC_STU_COG.sav", use.value.labels = TRUE, to.data.frame = TRUE)


backgourd <-  stud_QQQ_2018 %>% select(CNTSTUID,CNTSCHID,BOOKID,ST001D01T,
                                        ST003D02T,ST004D01T,ST005Q01TA,ST007Q01TA,
                                        ST011Q01TA,ST011Q02TA,ST011Q03TA,ST011Q04TA,
                                        ST011Q05TA,ST011Q06TA,ST011Q07TA,ST011Q08TA,
                                        ST011Q09TA,ST011Q010TA,ST011Q011TA,ST011Q012TA,
                                        ST011Q16NA,ST011D17TA)


item_response <- stud_COG_2018 %>% select(CNTSTUID,CM033Q01S,CM474Q01S,CM155Q01S,
                                          CM155Q04S,CM411Q01S,CM411Q02S,CM803Q01S,
                                          CM442Q02S,CM034Q01S,CM305Q01S,CM496Q01S,
                                          CM496Q02S,CM423Q01S,CM192Q01S,CM603Q01S,
                                          CM571Q01S,CM564Q01S,CM564Q02S,CM447Q01S,
                                          CM273Q01S,CM408Q01S,CM420Q01S,CM446Q01S,
                                          CM559Q01S,CM828Q03S,CM464Q01S,
                                          CM800Q01S,CM982Q01S,CM982Q02S,CM982Q03S,
                                          CM982Q04S,CM992Q01S,CM992Q02S,CM915Q01S,
                                          CM915Q02S,CM906Q01S,CM909Q01S,CM909Q02S,
                                          CM909Q03S,CM949Q01S,CM949Q02S,
                                          CM00GQ01S,CM998Q04S,CM905Q01S,
                                          CM919Q01S,CM919Q02S,CM954Q01S,
                                          CM954Q04S,CM943Q01S,CM943Q02S,CM953Q03S,
                                          CM948Q01S,CM948Q02S,CM948Q03S,CM936Q01S,
                                          CM961Q03S,CM939Q01S,CM939Q02S,
                                          CM967Q01S,CM967Q03S)

response_time <- stud_COG_2018 %>% select(CNTSTUID,CM033Q01T,CM474Q01T,CM155Q01T,
                                          CM155Q04T,CM411Q01T,CM411Q02T,CM803Q01T,
                                          CM442Q02T,CM034Q01T,CM305Q01T,CM496Q01T,
                                          CM496Q02T,CM423Q01T,CM192Q01T,CM603Q01T,
                                          CM571Q01T,CM564Q01T,CM564Q02T,CM447Q01T,
                                          CM273Q01T,CM408Q01T,CM420Q01T,CM446Q01T,
                                          CM559Q01T,CM828Q03T,CM464Q01T,
                                          CM800Q01T,CM982Q01T,CM982Q02T,CM982Q03T,
                                          CM982Q04T,CM992Q01T,CM992Q02T,CM915Q01T,
                                          CM915Q02T,CM906Q01T,CM909Q01T,CM909Q02T,
                                          CM909Q03T,CM949Q01T,CM949Q02T,
                                          CM00GQ01T,CM998Q04T,CM905Q01T,
                                          CM919Q01T,CM919Q02T,CM954Q01T,
                                          CM954Q04T,CM943Q01T,CM943Q02T,CM953Q03T,
                                          CM948Q01T,CM948Q02T,CM948Q03T,CM936Q01T,
                                          CM961Q03T,CM939Q01T,CM939Q02T,
                                          CM967Q01T,CM967Q03T)


# delete the items which are not binary scored.
for (i in 1:ncol(item_response)){
  if (length(table(item_response[,i])) > 2){
    print(names(item_response)[i])
  }
}






item_id_ra <- c("CM033Q01S","CM474Q01S","CM155Q01S","CM155Q04S",
                "CM411Q01S","CM411Q02S","CM803Q01S","CM442Q02S",
                "CM034Q01S","CM305Q01S","CM496Q01S","CM496Q02S",
                "CM423Q01S","CM192Q01S","CM603Q01S","CM571Q01S",
                "CM564Q01S","CM564Q02S","CM447Q01S","CM273Q01S",
                "CM408Q01S","CM420Q01S","CM446Q01S","CM559Q01S",
                "CM828Q03S","CM464Q01S","CM800Q01S","CM982Q01S",
                "CM982Q02S","CM982Q03S","CM982Q04S","CM992Q01S",
                "CM992Q02S","CM915Q01S","CM915Q02S","CM906Q01S",
                "CM909Q01S","CM909Q02S","CM909Q03S","CM949Q01S",
                "CM949Q02S","CM00GQ01S","CM998Q04S",
                "CM905Q01S","CM919Q01S","CM919Q02S","CM954Q01S",
                "CM954Q04S","CM943Q01S","CM943Q02S","CM953Q03S",
                "CM948Q01S","CM948Q02S","CM948Q03S","CM936Q01S",
                "CM961Q03S","CM939Q01S","CM939Q02S","CM967Q01S",
                "CM967Q03S")

student_id <- unique(item_response$CNTSTUID)

II <- c()
JJ <- c()
RA <- c()
RT <- c()
for (i in 1:nrow(item_response)){
  stud_index <- which(student_id == as.numeric(item_response$CNTSTUID[i]))
  for (j in 2:ncol(item_response)){
    item_index <- which(item_id_ra == names(item_response)[j])
    if (! is.na(item_response[i,j])){
      II <- c(II,stud_index)
      JJ <- c(JJ,item_index) 
      RA <- c(RA,as.numeric(item_response[i,j])-1)
    }
    if (! is.na(response_time[i,j])){
      RT <- c(RT,as.numeric(as.character(response_time[i,j])))
    }
  }
}

for (i in 1:nrow(item_response)){
  for (j in 2:ncol(item_response)){
    if (! is.na(item_response[i,j]) & is.na(response_time[i,j])){
      print(c(i,j))
    }
  }
}

for (i in 1:ncol(item_response)){
  print(c(names(item_response)[i],names(response_time)[i]))
}

row.names(item_response) == row.names(response_time)
