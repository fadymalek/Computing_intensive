#STEP 1: the step’s objective is to install the required packages
#STEP 2: the step’s objective is to load the required packages
library(dplyr)
library(ggplot2)
library(ggpubr)
library(gcookbook)
#STEP 3: the step’s objective is to plot the theoretical cdf for the standard normal distribution

p1 = ggplot(data.frame(x = c(-3,3)),aes (x = x))+
  stat_function(fun = pnorm, colour="#D55E00",size = 1)
p1

#STEP 4: the step’s objective is to construct a sample of size 10 from a standard normal distribution
df <- data.frame(sampl = rnorm(10))

#STEP 5: the step's objective is to construct ecdf
p2 <- 
  ggplot(df, aes(x = sampl))+
  stat_ecdf(geom="point",colour="#0072B2")+
  stat_ecdf(colour = "#0072B2" , size =1 )
p2

#STEP 6: the step’s objective is to plot cdf and ecdf on the same graph using ggpubr package 
figure <- ggarrange(p1,p2,
                    labels = c("tcdf","ecdf"),
                    ncol = 1 , nrow = 2)
figure

#STEP 7: the step’s objective is to plot the ecdf for different sample sizes 100 and 1000
p3 = ggplot(data.frame(x = c(-3,3)),aes (x = x))+
  stat_function(fun = pnorm, colour="#D55E00",size = 1)
p3
df <- data.frame(sampl = rnorm(100))
p100 <- 
  ggplot(df, aes(x = sampl))+
  stat_ecdf(geom="point",colour="#0072B2")+
  stat_ecdf(colour = "#0072B2" , size =1 )
p100
figure <- ggarrange(p1,p3,p2,p100,
                    labels = c("t","t","e","e"),
                    ncol = 2 , nrow = 2)
figure
#######################################
#Dscussion
df <- data.frame(sampl = rnorm(1000))
p1000 <- 
  ggplot(df, aes(x = sampl))+
  stat_ecdf(geom="point",colour="#0072B2")+
  stat_ecdf(colour = "#0072B2" , size =1 )
p1000
#########################################

#STEP 8: The step’s objective is to load a practical data then plot its ecdf
data(heightweight)
heightweight

#STEP 9: The step’s objective is to plot the ecdf for the height of people in the data set
ggplot(heightweight, aes(x=heightIn))+
  stat_ecdf()

#STEP 9: The step’s objective is to plot the ecdf for the weight of people but with grouping
ggplot(heightweight,aes(x= weightLb, col = sex))+
  stat_ecdf()
