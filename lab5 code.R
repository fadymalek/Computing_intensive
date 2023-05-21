#STEP 1: The step’s objective is to install and call the required package
#install.packages("boot",dependences = TRUE)
library(boot)


#STEP 2: Withdraw a sample of 900 points from a standard normal and we know that 95% confidence interval for the mean
#is [xbar - 1.96/30,xbar + 1.96/30]
datasize=900
y = rnorm(datasize,0,1) # we use this also in the step 3 
ybar= mean(y)
cithero_lower = ybar-1.96/sqrt(datasize)
cithero_upper = ybar+1.96/sqrt(datasize)
cithero_lower
cithero_upper



#STEP 3: Applying the concept of bootstrap to create 200 samples (from the previous sample) and calculate the mean for 
#each sample

boot_data_calc_mean = function(data){
  boot_samp = sample(data,replace = TRUE)
  mean(boot_samp)
}
boot_mean_reps = replicate(n=200,boot_data_calc_mean(y))
standard_error_boot = sd(boot_mean_reps)
standard_error_boot

##or using boot function
boot_data_calc_mean = function(data,idx){
  mean(data[idx])
}
set.seed(61)           #y is the data        #boot_data_calc_mean is statistic function
bootstrap_data <- boot(y, boot_data_calc_mean, R = 200) #R = 200 to indicate the number of bootstrap replicates.
bootstrap_data


#STEP 4: Calculate the bootstrap confidence interval
#using percentile

cibootstrap_per = quantile(boot_mean_reps,c(0.025,0.975))
cibootstrap_per

## or using boot.ci function 
boot.ci(boot.out = bootstrap_data,type="perc")


#step 5:calculate the bootstrap confidence interval 
#using normal distribution 
cibootstrap_lower= mean(boot_mean_reps) - 1.96*standard_error_boot
cibootstrap_upper= mean(boot_mean_reps) + 1.96*standard_error_boot
cibootstrap_lower
cibootstrap_upper

#step 6 : calculate the bootstrap confidence interval 
#using empirical method 
xbar = mean(y)
deltastar = boot_mean_reps - xbar
d = quantile(deltastar,c(0.025,0.975))
ci = xbar - c(d[2],d[1]) # we minus the biggest then the smallest
ci


#step 7 : install the required package 
#install.packages("gcookbook",dependencies = TRUE)
library(gcookbook)
View(heightweight)

#STEP 7: calculate the difference in sample means
mean(heightweight$weightLb[heightweight$sex == "m"])
mean(heightweight$weightLb[heightweight$sex == "f"])
Obs.Diff.In.Means = mean(heightweight$weightLb[heightweight$sex == "m"]) - mean(heightweight$weightLb[heightweight$sex == "f"])
Obs.Diff.In.Means

#STEP 8: let's bootstrap
m=heightweight$weightLb[heightweight$sex=="m"]
f=heightweight$weightLb[heightweight$sex=="f"]
boot_data_calc_mean=function(data){ 
  boot_samp= sample(data,replace=TRUE) 
  mean(boot_samp) 
}
boot_mean_m=replicate(n=200,boot_data_calc_mean(m))
boot_mean_f=replicate(n=200,boot_data_calc_mean(f))


#STEP 9: calculate the the "PERCENTILE" bootstrap confidence interval
Boot.Diff.In.Means <- boot_mean_m - boot_mean_f #these are vectors 
length(Boot.Diff.In.Means)
quantile(Boot.Diff.In.Means, prob=0.025)
quantile(Boot.Diff.In.Means, prob=0.975)

