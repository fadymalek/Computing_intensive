#STEP 1: the step’s objective is to install the required packages
#STEP 2: the step’s objective is to load the required packages
library(dplyr)
library(ggplot2)
library(detzrcr)
library(gcookbook)

#STEP 3: the step’s objective is to define the population data 
data(heightweight)
heightweight
population = heightweight$heightIn

#STEP 4: the step’s objective is to calculate true values of the mean μ and the SD σ of the population
mu = mean(population) 
mu
sigma = sd(population)
sigma

#STEP 5: Assume we do not have access for the population, and we need to calculate the mean. The solution is taking a 
#sample and calculating the sample mean. This sample mean is called a point estimate for the population mean.
population = data.frame(heightweight)
sampl = sample_n(population,size = 50)
mean_sampl = sum(sampl$heightIn)/length(sampl$heightIn)

#STEP 6: the step’s objective is to calculate the confidence interval using z-CI with known population (confidence intervals 
#at 95% level)
#qnorm(1-(alph/2)
z_star_95 <- qnorm(0.975)
lower = mean_sampl - 1.96 * sigma/sqrt(50)
upper = mean_sampl + 1.96 * sigma/sqrt(50)
lower
upper

#STEP 7: The step’s objective is to estimate some parameters from the ecdf
#STEP 8: The step’s objective is to estimate the distribution mean from the ecdf
population = data.frame(heightweight)
sampl=sample_n(population,size = 200)
ggplot(sampl, aes(x = heightIn)) +
  stat_ecdf()
mean_est=sum(sampl$heightIn)/length(sampl$heightIn)
mean_est

#STEP 9: The step’s objective is to estimate the distribution variance from the ecdf
var_est <- sum((sampl$heightIn-mean_est)^ 2)/ length (sampl$heightIn)
var_est
sqrt(var_est)

#STEP 10: The step’s objective is to calculate the lower and upper bounds
#for the ecdf using DKW inequality
cal_ecdf=calc_dkw(sampl, column = "heightIn", alpha = 0.05)
cal_ecdf

#STEP 11: The step’s objective is to draw the lower and upper bounds for the ecdf using DKW inequality
cal_ecdf=calc_dkw(sampl, column = "heightIn", alpha = 0.05)
cal_ecdf
# we remove the first and last rows that the plugin function does 
cal_ecdf=cal_ecdf[2:201,]
ggplot() +
  stat_ecdf(data=heightweight, aes(x = heightIn),colour="#0072B2", size=.7)+
  geom_step(data = cal_ecdf, aes (x=x,y=low)) +
  geom_step(data = cal_ecdf, aes (x=x,y=high))

#- The previous point describes the bounds of ecdf for a sample of size 200 now let’s try a sample of size 50
population = data.frame(heightweight)
sampl=sample_n(population,size = 50)
cal_ecdf=calc_dkw(sampl, column = "heightIn", alpha = 0.05)
#to clean the Data 
clean_data <- na.omit(cal_ecdf)

cal_ecdf=cal_ecdf[2:51,]
ggplot() +
  stat_ecdf(data=heightweight, aes(x = heightIn),colour="#0072B2", size=.7)+
  geom_step(data = cal_ecdf,aes(x=x,y=low)) +
  geom_step(data = cal_ecdf,aes(x=x,y=high))

