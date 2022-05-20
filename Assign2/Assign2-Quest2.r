#Loading libraries
library(readxl)
library(DescTools)

#Loading data
fuel <- read_excel('./Assign2/Assign2Data.xlsx', sheet = 'Fuel')

#2.a
##SBS boxplots
boxplot(
  fuel$`City Fuel Efficiency` ~ 
    fuel$`Transmission (Auto, Manual)`,
  main = "Boxplot of City Fuel Efficiency",
  xlab = "Transmission",
  ylab = "City Fuel Efficiency",
  col = "light blue")

##Mean auto transmission
fuel_auto_mean <- mean(as.numeric(
  unlist(
    fuel[fuel$`Transmission (Auto, Manual)`
         =="Auto", 'City Fuel Efficiency'])))

##Mean manual transmission
fuel_manual_mean <- mean(as.numeric(
  unlist(
    fuel[fuel$`Transmission (Auto, Manual)`
         =="Manual", 'City Fuel Efficiency'])))

##SD auto transmission
fuel_auto_sd <- round(sd(as.numeric(
  unlist(
    fuel[fuel$`Transmission (Auto, Manual)`
         =="Auto", 'City Fuel Efficiency']))), 
  digits = 3)

##SD manual transmission
fuel_manual_sd <- round(sd(as.numeric(
  unlist(
    fuel[fuel$`Transmission (Auto, Manual)`
         =="Manual", 'City Fuel Efficiency']))), 
  digits = 3)

#2.b
##Hypothesis test
a2_q2_b_ht <- t.test(
  fuel$`City Fuel Efficiency`~
    fuel$`Transmission (Auto, Manual)`, 
  conf.level = 0.95)

##Degrees of freedom
a2_q2_b_df <- round(
  a2_q2_b_ht$parameter, 
  digits = 0) 

##Critical value
a2_q2_b_ct <- round(qt(
  0.05/2,
  a2_q2_b_df, 
  lower.tail = FALSE),
  digits = 3)

##Validating test
a2_q2_b_ht$statistic > a2_q2_b_ct

#2.c
a2_q2_c_ci <- round(MeanDiffCI(
  fuel$`City Fuel Efficiency`~
    fuel$`Transmission (Auto, Manual)`,
           conf.level = 0.95,
           sides = "two.sided",
           paired = FALSE),
      digits = 2)

