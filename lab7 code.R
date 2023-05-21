library(ggplot2)
#method of moment estimation 
#Bern. pop --------------------
n = 10000 
x = rbinom(n,1,0.71)
p = sum(x) / n 
p 

#Normal. pop-------------------
n=10000 
x = rnorm(n,mean=10 ,sd=3)
mean_est = sum(x)/n
var_est=(sum(x^2)/n) - (mean_est)^2
mean_est 
var_est


#uniform populaiton------------
n=100 
x = runif(n,min=1,max = 5)
m1=sum(x)/n
m2=sum(x^2)/n
a_est=m1-sqrt(3*(m2-m1^2))
b_est=m1+sqrt(3*(m2-m1^2))
a_est
b_est




#maximum likelihood estimation 
#Exp. pop --------------------
sample = rexp(1000,rate=10)
mle = optim(par = c(rate = .5), fn = NLL, data = sample,
            control = list(parscale = c(rate = .5)))
NLL = function(pars, data) {
  # Extract parameters from the vector
  rate = pars[1]
  # Calculate Negative Log-LIkelihood   #dexp to calculate the pdf 
  NLL= -sum(dexp(x = data, rate = rate, log = TRUE))
}
mle$par


#Normal .pop-------------
sample = rnorm(1000,mean=4,sd=2)
mle = optim(par = c(mu = 0.2, sigma = 1.5), fn = NLL, data = sample,
            control = list(parscale = c(mu = 0.2, sigma = 1.5)))
NLL = function(pars, data) {
  # Extract parameters from the vector
  mu = pars[1]
  sigma = pars[2]
  # Calculate Negative Log-LIkelihood
  NLL= -sum(dnorm(x = data, mean = mu, sd = sigma, log = TRUE))
}
mle$par
#3) Likelihood-based confidence intervals-------------
#Let the distribution of 𝑋 be Binomial (P) of size 32, for a given sample of size n, use the method of moments to estimate 
#𝑃
sample = rnorm(1000,mean=4,sd=2)
mle = optim(par = c(mu = 0.2, sigma = 1.5), fn = NLL, data = sample,
            control = list(parscale = c(mu = 0.2, sigma = 1.5)))

sample = rbinom(23, size = 32, p = 0.7)
mle = optim(par = c(p = 0.2), fn = NLL, data = sample,
            control = list(parscale = c(p = 0.2)))
NLL = function(pars, data) {
  # Extract parameters from the vector
  p = pars[1]
  # Calculate Negative Log-LIkelihood
  NLL= -sum(dbinom(x = data, size=32,prob= p, log = TRUE))
}
mle$par

#calculate the confidence interival
Chi_value= qchisq(0.05, df=1, lower.tail=FALSE)/2
proportion <- seq(0.4, 0.9, by = 0.01)
va=numeric(length( proportion ))

sample = rbinom(1000, size = 32, p = 0.7)
for (i in 1:length(proportion))
{
  va[i]=likva(proportion[i],sample)
}
likva = function(par, data) {
  p = par
  sum(dbinom(x = data, size=32,prob= p, log = TRUE))
}
likeResults=data.frame(Proportion = proportion, Loglikelihood = va)
ggplot(data=likeResults, aes(x = Proportion,y=va))+
  geom_line()+geom_hline(yintercept = max(va), linetype = "dashed")

phat= proportion[va == max(va)]

lower <- max( proportion[proportion <= phat & va <= max(va) - 1.92] )
upper <- min( proportion[proportion >= phat & va <= max(va) - 1.92] )
lower
upper


###########################
#video 7.2 
library(MASS)
a = rexp(10 , 20)
f = fitdistr(a,"exponential")
f
#(rate) 18.086277 
#(5.719383) standard error
a = rexp(10000 , 20)
f = fitdistr(a,"exponential")
f
#(the standard error decreased)
#to calculate the confidence interval 
confint(f,level = 0.9)

###### normal distribution 
b=rnorm(100,5,0.5)
f = fitdistr(b,"normal")
f
confint(f,level = .95)
#########################solve 1
#solving assignment in the pdf 7 
sample_data <- c(1.34, 1.89, 4.14, 1.84, 3.00, 2.16, 4.894, 3.47, 2.87, 2.16, 1.87, 1.38, 1.40, 3.5031, 1.5061, 3.2319, 1.4956, 1.7329, 3.11, 1.3254)

# Calculate sample moments
sample_mean <- mean(sample_data)
sample_variance <- var(sample_data)

# Solve for moment estimators
a_hat <- (sample_mean^2) / sample_variance
b_hat <- sample_variance / sample_mean
a_hat
b_hat
#########################solve 2 using mom
sample_data <-c(0.57,0.39,0.18,0.93,0.82,1.49,0.20,2.769,0.13,0.31,0.05,0.041,0.44,0.29,0.48,0.68,0.32,0.52,1.04,0.55)
x = sum(sample_data)/length(sample_data)
rate = 1/x
f=fitdistr(sample_data,"exponential")
f
confint(f,level = .95)
#########################solve 2 using mle
sample = -c(0.57,0.39,0.18,0.93,0.82,1.49,0.20,2.769,0.13,0.31,0.05,0.041,0.44,0.29,0.48,0.68,0.32,0.52,1.04,0.55)
mle = optim(par = c(rate = .5), fn = NLL, data = sample,
            control = list(parscale = c(rate = .5)))
NLL = function(pars, data) {
  # Extract parameters from the vector
  rate = pars[1]
  # Calculate Negative Log-LIkelihood   #dexp to calculate the pdf 
  NLL= -sum(dexp(x = data, rate = rate, log = TRUE))
}
mle$par

