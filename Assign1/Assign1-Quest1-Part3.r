#Loading libraries
library(DescTools)
library(samplingbook)
library(plyr)

#Same data as Q1 P1 & P2

#1.3.a
##Recommended sample size for mu
required_n_mean <- round_any(
  MeanCIn(ci=c(realestate_mean - 50, 
               realestate_mean + 50),
          sd = 641,
          conf.level = 0.95,
          norm = TRUE),
  1, f = ceiling)


#1.3.b
require_n_prop <- round_any(
  sample.size.prop(0.02, 
                   sample1_ideal_props,
                   level = 0.9)$n, 
  1, f = ceiling)
