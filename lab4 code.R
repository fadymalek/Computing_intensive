#STEP 1: the step’s objective is to implement the first and second steps in the simulation procedures 
#1-(define a functionthat take random sample of size n from a normal distribution 𝑁(𝜇 = 3, 𝜎^2 = 4) and 
#2- calculate the mean for that sample 
sim_norm_calc_mean = function(){
  data = rnorm(n =20 , mean =3 , sd = 2)
  sum(data)/length(data)
}

#STEP 2: the step’s objective is to implement the third step in the simulation procedures 
#(repeat the previous steps for B times) B=2500
sim_norm_mean = replicate(n=2500,sim_norm_calc_mean())


#STEP 3: the step’s objective is to draw the estimated statistical parameter with the true parameter
hist(sim_norm_mean,probability = T) #probability = TRUE : the y axis represent the probability if False the y-axis represent the occurences 
grid()
box()
#add = TRUE specifies that the curve should be added to an existing plot instead of creating a new one.
curve(dnorm(x,3,sqrt(4/20)),add=T,col='red') #dnorm : calculates the probbability desnsity function (pdf) of the normal distribution at the values of x 


#STEP 4: the step’s objective is to draw the ecdf and true cdf for the sampling distribution of the mean.
ecdf(sim_norm_mean) #empirical cdf
plot(ecdf(sim_norm_mean))
curve(pnorm(x,mean=3 , sd = sqrt(4/20)),add = T , col='red') #pnorm(): is used to calculate the cumulative distribution function(CDF) of normal distribution #true cdf 

#STEP 5: the step’s objective is to investigate the effect of replications on the estimation accuracy
plot(ecdf(sim_norm_mean))
par(mfrow=c(1,3))
sim_norm_mean1=replicate(n=50, sim_norm_calc_mean())
sim_norm_mean2=replicate(n=500, sim_norm_calc_mean())
sim_norm_mean3=replicate(n=2500, sim_norm_calc_mean())

plot(ecdf(sim_norm_mean1))
curve(pnorm(x,3,sqrt(4/20)),add = T , col = 'red')

plot(ecdf(sim_norm_mean2))
curve(pnorm(x,3,sqrt(4/20)),add = T , col = 'red')

plot(ecdf(sim_norm_mean2))
curve(pnorm(x,3,sqrt(4/20)),add = T , col = 'red')

#######################################################
#STEP 6: the step’s objective is to install and call the required package
#install.packages("palmerpenguins",dependencies = TRUE)
library(palmerpenguins)

#STEP 7: the step’s objective is to remove the unnecessary data
peng=na.omit(penguins)
mass = peng$body_mass_g
mass

#STEP 8: the step’s objective is to implement the first and second steps in the bootstrapping procedures 
#1-define afunction that resamples from existing sample data of size n 
#2- calculate the median for that resampled data
boot_data_calc_median=function(data){
  boot_samp = sample(data,replace=TRUE) #The sample size taken in the code is equal to the length of the input data vector.
  median(boot_samp)
}

#STEP 9: the step’s objective is to implement the third step in the bootstrapping procedures (repeat the previous steps for B times) 
boot_median_reps = replicate(n = 5000,boot_data_calc_median(mass))

#STEP 10: the step’s objective is to draw the estimated statistical parameter (sampling distribution for the median)

par(mfrow=c(1,1)) #is used to set the plotting parameter mfrow, which controls the arrangement of multiple plots in a grid. 
hist(boot_median_reps,probability = T)
grid()
box()



#####################the exercies Dsicussion
sim_exponential_calc_median = function(){
  data = rexp(20, 3.1)
  median(data)
}

sim_exponential_median = replicate(n=2500,sim_exponential_calc_median())
mean(sim_exponential_calc_median())

########### Exercies 2 
sim_passion_calc_vaiance = function(){
  lambda_param = 4.2
  data = rexp(13, lambda)
  var(data)
}

sim_possoino_variance = replicate(n=2500,sim_passion_calc_vaiance())
mean(sim_possoino_variance )
# the result will 3.792548


#-use the bootstrapping method to estimate the mean mass of the penguins 
boot_data_calc_mean=function(data){
  boot_samp = sample(data,replace=TRUE) #The sample size taken in the code is equal to the length of the input data vector.
  mean(boot_samp)
}
boot_mean_reps = replicate(n = 5000,boot_data_calc_mean(mass))
mean(boot_mean_reps)
# the result will 4207.379 
