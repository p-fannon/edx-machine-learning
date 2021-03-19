# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

# Calculates probability of a positive test
mean(test)

# Calculates probability of having a disease given a negative test
mean(disease[test==0])

# Calculates probability of having a disease given a positive test
mean(disease[test==1]==1)

# Calculates the prevalence of disease in people who test positive to the overall disease prevalence
mean(disease[test==1]==1)/mean(disease==1)
