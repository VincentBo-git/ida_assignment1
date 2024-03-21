set.seed(1)
library(mice)
require(mice)
load("dataex2.RData")

# Set the initial value
true_beta1 <- 3
cover_count1 <- 0
cover_count2 <- 0
cover_count3 <- 0

for (i in 1:100){
  # Load the data of an interval
  data2 <- data.frame("X" = dataex2[,"X",i], "Y" = dataex2[,"Y",i])
  
  # Use mice() to multiple imputation with norm.boot method
  mice_data2_boot <- mice(data2, m = 20,method = "norm.boot", printFlag = FALSE, seed = 1)
  with_data2_boot <- with(mice_data2_boot, lm(Y ~ X))
  pool_data2_boot <- pool(with_data2_boot)
  
  # Use mice() to multiple imputation with norm.nob method
  mice_data2_nob <- mice(data2, m = 20,method = "norm.nob", printFlag = FALSE, seed = 1)
  with_data2_nob <- with(mice_data2_nob, lm(Y ~ X))
  pool_data2_nob <- pool(with_data2_nob)
  
  # Use mice() to multiple imputation with norm method
  mice_data2 <- mice(data2, m = 20,method = "norm", printFlag = FALSE, seed = 1)
  with_data2 <- with(mice_data2, lm(Y ~ X))
  pool_data2 <- pool(with_data2)
  
  # Update the empirical coverage count of the 95% confidence for norm.boot method
  s1 <- summary(pool_data2_boot, conf.int = TRUE)
  ci_lower1 <- s1[2, 7]
  ci_upper1 <- s1[2, 8]
  if (ci_lower1 <= true_beta1 && ci_upper1 >= true_beta1) {
    cover_count1 <- cover_count1 + 1
  }
  
  # Update the empirical coverage count of the 95% confidence for norm.nob method
  s2 <- summary(pool_data2_nob, conf.int = TRUE)
  ci_lower2 <- s2[2, 7]
  ci_upper2 <- s2[2, 8]
  if (ci_lower2 <= true_beta1 && ci_upper2 >= true_beta1) {
    cover_count2 <- cover_count2 + 1
  }
  
  # Update the empirical coverage count of the 95% confidence for norm method
  s3 <- summary(pool_data2, conf.int = TRUE)
  ci_lower3 <- s3[2, 7]
  ci_upper3 <- s3[2, 8]
  if (ci_lower3 <= true_beta1 && ci_upper3 >= true_beta1) {
    cover_count3 <- cover_count3 + 1
  }
}

# Calculate the empirical coverage probability
coverage_norm.boot <- cover_count1/100
coverage_norm.nob <- cover_count2/100
coverage_norm <- cover_count3/100
print(coverage_norm.boot)
print(coverage_norm.nob)
print(coverage_norm)
