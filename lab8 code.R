##########################Task1 (T-Test with a Single Sample)
#A) T-Test for Hypothesis Testing

#Step1 install and load the required packages
#install.packages("dplyr")
#install.packages("shiny")
#install_github("StatsWithR/statsr")
library(statsr)
library(dplyr)

#Step2 Load the data
data(nc)
names(nc)

#Step3Test the mean of the data against mu=7
t.test(nc$weight,mu=7)
 
###########################Task2 (T-Test with two samples) independent

#Step4 summarize the data
by(nc$weight, nc$habit, summary)
by(nc$weight, nc$habit, mean)

#Step5 compare the weight for smoker and nonsmoker
t.test(weight ~ habit, data = nc, conf.level = 0.95)

#Step6 compare the weight for smoker and nonsmoker using inference function
inference(y = weight, x = habit, data = nc, statistic = "mean", type = "ht", null = 0, 
          alternative = "twosided", method = "theoretical")


######################Task3 (T-Test with two samples) paired
#Step7 compare the weight of mice before treatment and after treatment

# Weight of the mice before treatment
before <-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
# Weight of the mice after treatment
after <-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
# Create a data frame
my_data <- data.frame( 
  group = rep(c("before", "after"), each = 10),
  weight = c(before, after)
)
my_data
t.test(weight ~ group , data = my_data ,paired = TRUE, conf.level = 0.95)


#B) Empirical Likelihood
#Step8: Illustration example
#install.packages('emplik',dependencies = TRUE)
library(emplik)
n<-25
## (1) generating the data from N(0,1)
X<-rnorm(n,0,1)
      #empirical likelihood 
el.obj<-el.test(X,mu=0)
el.obj$Pval
t.test(X,mu=0)$p.value # p-value of t-test

# Exponential distribution 
X<-rexp(n,rate=1)-1
#Empirical likelihood 
el.obj<-el.test(X,mu=0)
el.obj$Pval
# t-test 
t.test(X,mu=0)$p.value


###################Task4 (estimating the mean and find its confidence interval)
#Step9 Empirical likelihood inference for means
data=c(6.1, -8.4, 1.0, 2.0, 0.7, 2.9, 3.5, 5.1, 1.8, 3.6, 7.0, 3.0, 9.3, 7.5,-6.0)
t.test(data,mu=0)
# But it is not accurate as the data do not appear to be normal (with a heavy left tail)
hist(data)
el.test(data,mu=0)

#Step9 Empirical likelihood to find the confidence interval
# to calculate -2LLR
myfun6 <- function(theta, data1) {
  el.test(data1,mu=theta)
}
                #to give it the -2LLR
findUL(step=0.5, fun=myfun6, MLE=0, data1=data,level = qchisq(0.95, df=1)
)
myfun6 <- function(theta, data1) {
  el.test(data1,mu=theta)
}
findUL(step=0.5, fun=myfun6, MLE=0, data1=data,level = qchisq(0.95, df=1)
)

#################Task5 Empirical likelihood for two samples

#step 10 Empirical likelihood to for tow samples here, we use function El.means in R package EL.
#install.packages("EL")
library(EL)
x<-c(96.8,57.2,37.4,44.0,55.0,41.8,46.2,41.8,41.8,59.4,44.0,52.8,33.0,52.8,41.8,44.0,52.8
     ,59.4,37.4,77.0,39.6,57.2,57.2, 41.8,39.6)
y<-c(26.4,33.0,30.8,35.2,44.0,48.4,61.6,41.8,26.4,28.6,55.0, 61.6,63.8,24.2,37.4,48.4,52.8
     ,46.2,57.2,68.2,46.2,37.4,46.2, 52.8,35.2)
t.test(x, y)
EL.means(x, y)
