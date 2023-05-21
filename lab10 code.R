###calculation using R pdf to know the mean and varinace and sd
x = c(0.28,0.33,0.3,0.32,0.27,0.29,0.27,0.31,0.33)
mean(x)
var(x)
sd(x)

##########lab10 
#Example 1 : Estimate the rate parameter of an exponential distribution 
library(MASS)
n = 1000 
sample = rexp(n, 0.5)
sample_mean = mean(sample)
lambda = 1/sample_mean
lambda
### new Method ### 
rate_estimation = fitdistr(sample,"exponential")
rate_estimation
confint(rate_estimation , level = 0.95)

### old Method ### using maximum lilkelihood
mle = optim(par=c(rate = 0.5) , fn = NLL , data = sample, 
            control = list(parscale = c(rate = 0.5)))
NLL = function(pars , data){
  #Extract parameters from the vector 
  rate = pars[1]
  #calculate Negative Log-liklihood
  NLL = -sum(dexp(x=data, rate = rate, log = TRUE))
}
mle$par

### old method ###
p = seq(.1,1,by=0.01)
Llikhood = numeric(length(p))
for(i in 1:length(p)){
  Llikhood[i] = likva(p[i],sample)
}
likva=function(p,data){
  sum(dexp(x=data, rate = p , log = TRUE))
}
likeResults = data.frame(proportion = p , Llikhood = Llikhood)
Chi_value = qchisq(0.05,df = 1 , lower.tail=FALSE)/2
phat= p[Llikhood == max(Llikhood)]
lower = max(p[p<=phat & Llikhood <= max(Llikhood) - 1.92])
upper = min(p[p>=phat & Llikhood <= max(Llikhood) - 1.92])
lower
upper

#Example(2) : Estimate the mean and standard deviation parameters of normal distribution 
library(MASS)
n = 1000 
sample = rnorm(n,1,2)
x = fitdistr(sample, 'normal')
x  
confint(x,level = 0.96)

#Exmaple(3):Estimate the parameter of geometric distribution (probability of success)
library(MASS)
n = 1000 
sample= rgeom(n,0.25)
### New method ###
p_estimate = fitdistr(sample,"geometric")
p_estimate
confint(p_estimate,level=0.96)


###part 3 : Final Exam Guide Model
##Example 1 :
data = c(7.50,8.41,8.02,7.76,8.54,10.01,8.84,10.00,8.82,6.41)
t.test(data , mu =8 )
x_est = fitdistr(data, "normal")
confint(x_est) # the confidence level used for that is 0.95

##Example 2 :
sample_means = c()
data = rexp(10000,2)  #this are the population 
n = 100 
for(i in 1 : n){
  sample_means[i] = mean(sample(data,size=81 , replace = TRUE))
}
mean(sample_means)
var(sample_means)
hist(sample_means)



##Example 3 : 
sim_exp_calc=function(){
  data= rexp(n=20,rate=2)
  var(data)
}
sim_exp = replicate(n=2500,sim_exp_calc())
x=mean(sim_exp) #smapling distribution
plot(ecdf(sim_exp))
1/x
x

##Example 4 : 
n = 5 
x = c(3,3,2,5,4,5,3,4,3,5,3,3,2,5,4,2,4,4,4,2,3,4,4,5,4,1,3)
y = sum(x)/length(x)
z = y/n
y 
z
