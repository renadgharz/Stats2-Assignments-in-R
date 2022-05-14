##Distribution parameters

##Mean
pckg_subs_mean <- 25*0.25

##Standard deviation
pckg_subs_sd <- 
  round(
    sqrt(pckg_subs_mean*0.75),
    digits = 4)

##P-value using binomial
pckg_subs_prob <-
  round(
    1- pbinom(10, 
              25, 
              p=0.25), 
    digits = 4)

##Validating test
pckg_subs_prob < 0.05
