#Libraries
library(readxl)
library(dplyr)
library(DescTools)

#Loading data
climate <- read_excel('./Assign2/Assign2Data.xlsx', sheet = 'Climate')

#1.a
#No coding required

#1.b
##Difference between 2018 and 1948
climate['diff'] <- 
  climate$dx90_2018 - climate$dx90_1948

##Mean of difference
climate_diff_mean <- round(
  mean(climate$diff), 
  digits = 3)


##Standard deviation of difference
climate_diff_sd <- round(
  sd(climate$diff),
  digits = 3)

##Plotting histogram
climate_histogram <- 
  hist(climate$diff,
       main = "Histogram of difference",
       xlab = "Difference",
       xlim = c(-75, 75),
       prob = TRUE,
       col = "light blue")

##Adding normal curve to histogram
curve(dnorm(
  x,
  mean = climate_diff_mean,
  sd = climate_diff_sd), 
  add = TRUE)

#1.c
##Hypothesis test
a2_q1_c_ht <- t.test(
  climate$diff, 
  alternative = "greater", 
  conf.level = 0.95)

##Validating test
a2_q1_c_ht$p.value < 0.05

#1.d
##95% C.I. for mean diff
a2_q1_c_ci <- round(MeanCI(
  climate$diff, 
  conf.level = 0.95, 
  sides = "left"),
  digits = 3)

#1.e
#No coding required

