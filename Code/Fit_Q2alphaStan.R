library(rstan)


load('./Processed_data/Rawdata_andStanlistforfitting.rdat') # Qdat=observed raw data, stan_file=used for fitting
RL_learn50000<-stan(file='./Code/Qstan_alpha2.stan',data=stan_file,iter=50000,chains=4)

#note: the mode of the posterior distributions were used to extract indvidual subject parameters for explore/exploit
#note: subject sub-046 was originally included for fitting, but later excluded because of movement in the scanner.
#note:Qdat is a list with all the learning data from the subjects (used for fitting)