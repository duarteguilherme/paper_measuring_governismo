
library(rstan)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)

# Loading datasets
cham_dataset <- readRDS('data/cham_total.rds') %>% distinct()
sen_dataset <- readRDS('data/sen_total.rds') %>% distinct()

# president: Cardoso
cham1 <- dplyr::filter(cham_dataset, vote_date >= lubridate::dmy("01/01/1999") & 
                  vote_date <= lubridate::dmy("31/12/2002"))
sen1 <- dplyr::filter(sen_dataset, vote_date >= lubridate::dmy("01/01/1999") & 
                         vote_date <= lubridate::dmy("31/12/2002"))


# president: Lula
cham2 <- dplyr::filter(cham_dataset, vote_date >= lubridate::dmy("01/01/2003") & 
                         vote_date <= lubridate::dmy("31/12/2010"))
sen2 <- dplyr::filter(sen_dataset, vote_date >= lubridate::dmy("01/01/2003") & 
                        vote_date <= lubridate::dmy("31/12/2010"))


# president: Dilma
cham3 <- dplyr::filter(cham_dataset, vote_date >= lubridate::dmy("01/01/2011") & 
                         vote_date <= lubridate::dmy("17/04/2016"))
sen3 <- dplyr::filter(sen_dataset, vote_date >= lubridate::dmy("01/01/2011") & 
                        vote_date <= lubridate::dmy("17/04/2016"))


# president: Temer
cham4 <- dplyr::filter(cham_dataset, vote_date >= lubridate::dmy("18/04/2016") )
sen4 <- dplyr::filter(sen_dataset, vote_date >= lubridate::dmy("18/04/2016") )


# First analysis
# Measuring governismo by month

