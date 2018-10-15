
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

cham <- list()
sen <- list()

# president: Cardoso
cham[[1]] <- dplyr::filter(cham_dataset, vote_date >= lubridate::dmy("01/01/1999") & 
                  vote_date <= lubridate::dmy("31/12/2002"))
sen[[1]] <- dplyr::filter(sen_dataset, vote_date >= lubridate::dmy("01/01/1999") & 
                         vote_date <= lubridate::dmy("31/12/2002"))


# president: Lula
cham[[2]] <- dplyr::filter(cham_dataset, vote_date >= lubridate::dmy("01/01/2003") & 
                         vote_date <= lubridate::dmy("31/12/2010"))
sen[[2]] <- dplyr::filter(sen_dataset, vote_date >= lubridate::dmy("01/01/2003") & 
                        vote_date <= lubridate::dmy("31/12/2010"))


# president: Dilma
cham[[3]] <- dplyr::filter(cham_dataset, vote_date >= lubridate::dmy("01/01/2011") & 
                         vote_date <= lubridate::dmy("17/04/2016"))
sen[[3]] <- dplyr::filter(sen_dataset, vote_date >= lubridate::dmy("01/01/2011") & 
                        vote_date <= lubridate::dmy("17/04/2016"))


# president: Temer
cham[[4]] <- dplyr::filter(cham_dataset, vote_date >= lubridate::dmy("18/04/2016") )
sen[[4]] <- dplyr::filter(sen_dataset, vote_date >= lubridate::dmy("18/04/2016") )


# First analysis
# Measuring governismo by month



obtain_governismo <- function(data) {
  data %>%
    mutate(month = lubridate::month(vote_date),
           year = lubridate::year(vote_date)) %>%
   group_by(month, year, rollcall_id) %>%
    summarise(governismo = mean(governismo, na.rm=T)) %>%
    mutate(date = lubridate::dmy(glue::glue("27/{month}/{year}"))) %>%
    group_by(date) %>%
    summarise(governismo = mean(governismo, na.rm=T))
}
  

obtain_governismo_party <- function(data) {
  data %>%
    mutate(month = lubridate::month(vote_date),
           year = lubridate::year(vote_date)) %>%
    group_by(month, year, rollcall_id, legislator_party) %>%
    summarise(governismo = mean(governismo, na.rm=T)) %>%
    mutate(date = lubridate::dmy(glue::glue("27/{month}/{year}"))) %>%
    group_by(date, legislator_party) %>%
    summarise(governismo = mean(governismo, na.rm=T))
}

create_lag3 <- function(governismo) {
   (governismo + lag(governismo, 1) +
             lag(governismo,2))/3
} 
create_lag6 <- function(governismo) {
  (governismo + lag(governismo, 1) +
     lag(governismo,2) +
     lag(governismo,3) +
     lag(governismo,4) +
     lag(governismo,5))/6
} 


create_lag12 <- function(governismo) {
  (governismo + lag(governismo, 1) +
     lag(governismo,2) +
     lag(governismo,3) +
     lag(governismo,4) +
     lag(governismo,5) +
     lag(governismo,6) +
     lag(governismo,7) +
     lag(governismo,8) +
     lag(governismo,9) +
     lag(governismo,10) +
     lag(governismo,11))/12
} 


governismo_cham <- map(cham, obtain_governismo)
governismo_sen <- map(sen, obtain_governismo)

governismos <- bind_rows(bind_rows(governismo_cham) %>%
                           mutate(casa = "Chamber"),
                         bind_rows(governismo_sen) %>%
                           mutate(casa = "Senate"))


governismos <- governismos %>%
  arrange(date) %>%
  mutate(governismo3 = create_lag3(governismo),
         governismo6 = create_lag6(governismo),
         governismo12 = create_lag12(governismo)) %>%
  gather(lag, governismo, -date, -casa) %>%
  mutate(lag = ifelse(lag == "governismo", "0",
                      ifelse(lag == "governismo3", "3", 
                             ifelse(lag == "governismo6", "6", "12"))))


# Plotting comparative cham and senate
ggplot(data = governismos, aes(x = date, y = governismo)) +
  geom_line() +
  facet_grid(lag ~ casa)

# Governismo in the Senate and in the Chamber aren't correlated


# Plotting scatterplots of governismo between the Chamber and the Senate
ggplot(data = governismos %>%
         spread(casa, governismo), 
       aes(x = `Chamber`, y = `Senate`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(~ lag) 


ggplot(data = bind_rows(governismo_sen), aes(x = date, y = governismo)) +
  geom_line()





# Analyzing the same results regarding party

governismo_cham_party <- map(cham, obtain_governismo_party)
governismo_sen_party <- map(sen, obtain_governismo_party)




governismos_party <- bind_rows(bind_rows(governismo_cham_party) %>%
                           mutate(casa = "Chamber"),
                         bind_rows(governismo_sen_party) %>%
                           mutate(casa = "Senate"))


governismos_party <- governismos_party %>%
  arrange(date) %>%
  group_by(legislator_party) %>%
  mutate(governismo = create_lag6(governismo)) %>%
  ungroup() %>%
  filter(legislator_party %in% c("PT", "PSDB", "PMDB", "PP"))


# Plotting comparative cham and senate
ggplot(data = governismos_party, aes(x = date, y = governismo)) +
  geom_line() +
  facet_grid(legislator_party ~ casa)

# Governismo in the Senate and in the Chamber aren't correlated


# Plotting scatterplots of governismo between the Chamber and the Senate
ggplot(data = governismos %>%
         spread(casa, governismo), 
       aes(x = `Chamber`, y = `Senate`)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(~ lag) +
  scale_x_log10() +
  scale_y_log10()


ggplot(data = bind_rows(governismo_sen), aes(x = date, y = governismo)) +
  geom_line()

