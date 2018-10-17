
library(rstan)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(furrr)

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
   group_by(month, year) %>%
    summarise(governismo = mean(governismo, na.rm=T)) %>%
    mutate(date = lubridate::dmy(glue::glue("27/{month}/{year}"))) %>%
    group_by(date) %>%
    summarise(governismo = mean(governismo, na.rm=T))
}
  

obtain_governismo_party <- function(data) {
  data %>%
    mutate(month = lubridate::month(vote_date),
           year = lubridate::year(vote_date)) %>%
    group_by(month, year, legislator_party) %>%
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
comparative_ma_cham_sen <- ggplot(data = governismos, aes(x = date, y = governismo)) +
  geom_line() +
  facet_grid(lag ~ casa)
ggsave(plot = comparative_ma_cham_sen, filename = "img/comparative_ma_cham_sen.png")

# Governismo in the Senate and in the Chamber aren't correlated


# Plotting scatterplots of governismo between the Chamber and the Senate
cor_ma_cham_sen_governismo <- 
  ggplot(data = governismos %>%
         spread(casa, governismo) %>%
           filter(lag == "6"), 
       aes(x = `Chamber`, y = `Senate`) ) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(~ lag) 
ggsave(plot = cor_ma_cham_sen_governismo, filename = "img/cor_ma_cham_sen_governismo.png")





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
governismo_party <- ggplot(data = governismos_party, aes(x = date, y = governismo)) +
  geom_line() +
  facet_grid(legislator_party ~ casa)
ggsave(filename = "img/governismo_party.png",governismo_party)
# Governismo in the Senate and in the Chamber aren't correlated




# ***********************************************************
#  Second model 1 - Statical model
# ***********************************************************




add_id <- function(dataset) {
  # This function gets a dataset of legislative data
  # and returns a similar dataset with new index
  
  # Creating index
  dataset <- dataset %>%
    mutate(stan_legislator_id = as.integer(as.factor(legislator_id)),
           stan_party_id = as.integer(as.factor(legislator_party)))
  
  dataset
}


create_stan_dataset <- function(dataset, time = F, party = F) {
  # This function creates a dataset
  # for running models in Stan
  # It gets a dataset produced in
  # add_t_and_id
  stan_list <- list(
    n = nrow(dataset),
    max_l = max(dataset$stan_legislator_id),
    y = dataset$governismo,
    l = dataset$stan_legislator_id
  )
  if ( time ) {
    stan_list <- c(stan_list, list(max_t = max(dataset$t),
    t = dataset$t))
  }
  if ( party ) {
    stan_list <- c(stan_list, 
                   list(max_party = max(dataset$stan_party_id),    
                        p = dataset$stan_party_id)
    )
  }
  stan_list
}



cham_datasets <- map(cham, add_id)
sen_datasets <- map(sen, add_id)



cham_stan_lists <- map(cham_datasets, create_stan_dataset)
sen_stan_lists <- map(sen_datasets, create_stan_dataset)


model1_code <- 
  '
data {
  int<lower=0> n;
  int<lower=0> max_l;
  int<lower=0> y[n];
  int<lower=0> l[n];
}
parameters {
  vector<lower=0, upper=1>[max_l] mu;
}
model {
  mu ~ uniform(0,1);

  for (i in 1:n) {
    y[i] ~ bernoulli(mu[l[i]]);
  }
}
'

cat("Compiling model1 \n\n")
cat("**************************************************************************\n\n")

model1 <- stan_model(model_code = model1_code)

# Saving model1
saveRDS(object = model1, file = "data/model1.rds")
model1 <- readRDS("data/model1.rds")


model1_samples <- future_map(c(cham_stan_lists, 
                                  sen_stan_lists),
  ~ sampling(model1, 
            data = .x,
            iter = 2500, chains = 1))

saveRDS(object = model1_samples[1:4], file = 'data/model1_cham_samples_test.rds')


saveRDS(object = model1_samples[5:8], file = 'data/model1_sen_samples_test.rds')



# ******************************************************************
### Second part - Estimating individual governismo
# ******************************************************************




add_t_and_id <- function(dataset) {
# This function gets a dataset of legislative data
# and returns a similar dataset with new index
# and t variable
  
  
  # Creating index
  dataset <- dataset %>%
    mutate(stan_legislator_id = as.integer(as.factor(legislator_id)),
           stan_party_id = as.integer(as.factor(legislator_party)))

  # Creating variable t
  # t will represent a particular week
  # we could define it as a day
  # however, the model will take too long to run
  
  day_interval <- 30 # each 120 days a new measure
  t_dataset <- tibble(
    week_date = seq(min(dataset$vote_date), max(dataset$vote_date), day_interval)
  ) %>%
    arrange(week_date) %>%
    mutate(t = as.integer(as.factor(week_date))) %>%
    mutate(vote_date = map(week_date, function(x) x + 0:day_interval )) %>%
    unnest(vote_date)
  
  dataset <- inner_join(dataset, t_dataset)
  dataset
}





cham_datasets <- map(cham, add_t_and_id)
sen_datasets <- map(sen, add_t_and_id)



cham_stan_lists <- map(cham_datasets, ~ create_stan_dataset(.x, time = T))
sen_stan_lists <- map(sen_datasets, ~ create_stan_dataset(.x, time = T))


model2_code <- 
'
data {
  int<lower=0> n;
  int<lower=0> max_l;
  int<lower=0> y[n];
  int<lower=0> l[n];
  int<lower=0> max_t;
  int<lower=0> t[n];
}
parameters {
  vector<lower=0, upper=1>[max_l] mu[max_t];
  
}
model {
   mu[1] ~ uniform(0,1);
   for (i in 2:max_t)
      mu[i] ~ normal(mu[i-1], 0.008);
  
   for (i in 1:n) {
      y[i] ~ bernoulli(mu[t[i], l[i]]);
   }
}
'

cat("Compiling model2 \n\n")
cat("**************************************************************************\n\n")

model2 <- stan_model(model_code = model2_code)

# Saving model1
saveRDS(object = model1, file = "data/model2.rds")
model2 <- readRDS("data/model2.rds")



model2_samples <- future_map(c(cham_stan_lists, 
                               sen_stan_lists),
                             ~ sampling(model2, 
                                        data = .x,
                                        iter = 2500, chains = 1))

saveRDS(object = model2_samples[1:4], file = 'data/model2_cham_samples_test.rds')


saveRDS(object = model2_samples[5:8], file = 'data/model2_sen_samples_test.rds')


