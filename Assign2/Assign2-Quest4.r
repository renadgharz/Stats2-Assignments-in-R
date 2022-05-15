#Libraries

#Loading data
uo_students <- data.frame(
  "Management" = c(30,35,20,10,5,7),
  "Engineering" = c(45,50,50,40,20,8),
  row.names = c(
    "90-10",
    "80-90",
    "70-80",
    "60-70",
    "50-60",
    "< 50"))

#4.a
##No coding required


#4.b
##Expected counts
chisqe_exp_counts <- 
  chisq.test(uo_students)$expected

#4.c
##Hypothesis test
a2_q4_c_ht <- chisq.test(uo_students)

##Critical value
a2_q4_c_ct <- round(qchisq(
  0.05, 
  a2_q4_c_ht$parameter,
  lower.tail = FALSE), 
  digits = 3)

##Validating test
round(a2_q4_c_ht$statistic, 3) > a2_q4_c_ct


