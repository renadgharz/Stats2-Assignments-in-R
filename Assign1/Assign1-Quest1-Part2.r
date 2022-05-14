#Loading libraries
library(LearningStats)

#Data is the same as Q1 P1

#1.2.a
##Hypothesis test
a1_q1_p2_a_ht <- 
  t.test(realestate$`Sample 1`, mu = 2000)

##Critical value
a1_q1_p2_a_ct <- 
  round(
    qt(0.05/2, 39, lower.tail = FALSE),
    digits = 3)

##Validating results
a1_q1_p2_a_ht$statistic > a1_q1_p2_a_ct

#1.2.b
##Hypothesis test
a1_q1_p2_b_ht <- 
  proportion.test(sample1_ideal_props,
                  length(na.omit(
                    realestate$`Sample 1`)),
                  p0 = 0.2,
                  alternative = "less",
                  alpha = 0.1,
                  plot = TRUE)

##Critical value
a1_q1_p2_b_ct <- 
  round(qnorm(0.05), 
        digits = 3)

##Validating test
a1_q1_p2_b_ht$statistic > a1_q1_p2_b_ct
