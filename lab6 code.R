#STEP 1: The step’s objective is to install and call the required package
#install.packages("bootstrap",dependencies = TRUE)
library(bootstrap)


#STEP 2: Define the sample data with size=20
data=c(3.56,0.69,0.1,1.84,3.93,1.25,0.18,1.13,0.27,0.5,0.67,0.01,0.61,0.82,1.70,0.39,0.11
       ,1.2,1.21,0.72)

#STEP 3: estimate the sample mean and the jackknife mean
#sample mean
mean(data)
#jackknife the mean 
jackmean <- jackknife(data,mean)
jackmean # the means of each sameple 

#sample mean
jack_es = mean(jackmean$jack.values)

#Bias-corrected jackknife estimate
meanjack = mean(data) - jackmean$jack.bias

jack_es
mean(data)

jackmean$jack.bias
meanjack

jackmean$jack.se

#Calculate 95% confidence interval for the sample mean 
LowerCI = meanjack-1.96* jackmean$jack.se
upperCI = meanjack+1.96* jackmean$jack.se
LowerCI
upperCI


#STEP 4: estimate the sample standard deviation
# Sample standard deviation
sd(data)

# Jackknife the standard deviation
jacksd = jackknife(data,sd)
jack_es = mean(jacksd$jack.values)

#Bias-corrected jackknife estimate
sdjack=sd(data) - jacksd$jack.bias
jack_es #mean jackknife estimated (fady)
sd(data)
jacksd$jack.bias
sdjack
jacksd$jack.se

LowerCI = sdjack-1.96* jacksd $jack.se
upperCI = sdjack+1.96* jacksd $jack.se
LowerCI
upperCI


#STEP 4: estimate the Sample standard deviation with n in denominator (MLE)
# Sample standard deviation with n in denominator (MLE)
sdmle <- function(data)(sqrt((length(data)-1)/length(data))*sd(data))
#sdmle function calculates the standard deviation using the maximum likelihood estimation (MLE) approach
sdmle(data)

# Jackknife the MLE standard deviation (denominator with n)
jacksdmle <- jackknife(data,sdmle)
jack_es=mean(jacksdmle$jack.values)

# Bias-corrected jackknife estimate
sdmlejack = sdmle(data) - jacksdmle$jack.bias
jack_es
sdmle(data)
jacksdmle$jack.bias
sdmlejack
jacksdmle$jack.se

LowerCI = sdmlejack-1.96* jacksdmle $jack.se
upperCI = sdmlejack+1.96* jacksdmle $jack.se
LowerCI
upperCI

#STEP 5: estimate the Sample variance
#smple variance 
var(data)
#jackkife the variance 
jackvar = jackknife(data,var)
jack_es = mean(jackvar$jack.values)

#Bias-corrected jackknife estimate
varjack = var(data) - jackvar$jack.bias
jack_es
var(data)
jackvar$jack.bias
varjack
jackvar$jack.se


##parametric bootstrapping 
#STEP 6: Define a data from binomial (100, θ) for an unknown θ
x = c(95,94,95,94,91)
n = length(x)  #sample size 
binomSize = 100 
thetahat = mean(x) / 100 


#STEP 7: now we know the estimated population we can withdraw samples from it and efficiently estimate the unknown 
#parameter and sampling distribution
thetahat_es = c()
for(i in 1:2000){
  boot_sampl = rbinom(5,100,thetahat)
  thetahat_es[i] = mean(boot_sampl)/100
}
boot_thet_par_es= mean(thetahat_es)
boot_thet_par_es

#STEP 8: Calculate the empirical CI for the estimated parameter
deltastar = thetahat_es - thetahat
d = quantile(deltastar,c(0.025,0.975))
ci = thetahat - c(d[2],d[1])
ci
############################
#Exercies 
#apply bootstrap resampling 
x = c(0.55,-0.41, -0.11, -1.15, 0.22, 0.57, -0.23, -1.07, 1.06, 1.38, 1.08, 0.18, -0.79, -1.35, -1.37, 0.67, 0.17, 1.48, -0.89, 0.60)

boot_data_calc_mean = function(data){
  boot_samp = sample(data,replace = TRUE)
  mean(boot_samp)
}
boot_mean_reps = replicate(n=200,boot_data_calc_mean(x))
standard_error_boot = sd(boot_mean_reps)
standard_error_boot
mean(x)-mean(boot_mean_reps)

boot_data_calc_mean = function(data,idx){
  mean(data[idx])
}
                  #y is the data        #boot_data_calc_mean is statistic function
bootstrap_data <- boot(x, boot_data_calc_mean, R = 200) #R = 200 to indicate the number of bootstrap replicates.
bootstrap_data

cibootstrap_per = quantile(boot_mean_reps,c(0.025,0.975))
cibootstrap_per

######################apply jackknife method 
x = c(0.55,-0.41, -0.11, -1.15, 0.22, 0.57, -0.23, -1.07, 1.06, 1.38, 1.08, 0.18, -0.79, -1.35, -1.37, 0.67, 0.17, 1.48, -0.89, 0.60)

mean(x)
#jackknife the mean 
jackmean <- jackknife(x,mean)
jackmean # the means of each sameple 

#sample mean
jack_es = mean(jackmean$jack.values)

#Bias-corrected jackknife estimate
meanjack = mean(x) - jackmean$jack.bias

jack_es #x.
mean(x)

jackmean$jack.bias
meanjack

jackmean$jack.se

#Calculate 95% confidence interval for the sample mean 
LowerCI = meanjack-1.96* jackmean$jack.se
upperCI = meanjack+1.96* jackmean$jack.se
LowerCI
upperCI

##########################
#if you know that the previous sample has been withdraw from a normal distribution with known SD=1 and unknown mean = m 
#use parmeteric bootstrap to estimate the sampling distribution for the mean and calculate 95% bootstrap confidence interval
x
n = length(x)
mean = sum(x)/length(x)

sdd = 1
thetahat_es = c()

for(i in 1 : 2000){
  boot_sampl=rnorm(20,mean,sdd)
  thetahat_es[i] = mean(boot_sampl)
}
sample_meanslast = mean(thetahat_es)
sample_meanslast

cithero_lower = mean-1.96/sqrt(length(x))
cithero_upper = mean+1.96/sqrt(length(x))
cithero_lower
cithero_upper



cibootstrap_per = quantile(x,c(0.025,0.975))
cibootstrap_per








