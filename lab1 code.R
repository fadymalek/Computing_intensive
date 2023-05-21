#STEP 1: the step’s objective is to install the required packages
#STEP 2: the step’s objective is to load the required packages
library(devtools)
library(statsr)
library(dplyr)
library(shiny)
library(ggplot2)

#STEP 3: the step’s objective is to load Population data
data(ames)

#STEP 4: the step’s objective is to summarize the statistics of population
ggplot(data = ames, aes(x = area)) +geom_histogram(binwidth = 200)

#the use of this operator is to give the before it in the function after it (fady)
print("hello world")
"hello world" %>% print()

ames %>%
  summarise(mu = mean(area), pop_med = median(area), 
            sigma = sd(area), pop_iqr = IQR(area),
            pop_min = min(area), pop_max = max(area),
            pop_q1 = quantile(area, 0.25), # first quartile, 25th percentile
            pop_q3 = quantile(area, 0.75)) # third quartile, 75th percentile

#STEP 5: the step’s objective is to take a random sample form the population
sampl <- ames %>%
  sample_n(size = 50)

#STEP 6: the step’s objective is to summarize the statistics of random sampl
ggplot(data = sampl, aes(x = area)) +
  geom_histogram(binwidth = 100)

sampl %>%
  summarise(mu_sample = mean(area), sample_med = median(area), 
            sigma_sample = sd(area), sample_iqr = IQR(area),
            sample_min = min(area), sample_max = max(area),
            sample_q1 = quantile(area, 0.25), # first quartile, 25th percentile
            sample_q3 = quantile(area, 0.75)) # third quartile, 75th percentile


#STEP 7: the step’s objective is to take one more sample of size 50, and view the mean area in this sample
ames %>% 
  sample_n(size = 50) %>%
  summarise(x_bar = mean(area))

#STEP 8: the step’s objective is to build up the sampling distribution for the sample mean
#after finish this code the smaple_means50 will be a vector that contain the x_bar for all the samples [x_bar1, x_bar2 ... ,x_bar15000]

sample_means50 <- ames %>%
  rep_sample_n(size = 1500, reps = 15000, replace = TRUE) %>%
  summarise(x_bar = mean(area))

ggplot(data = sample_means50, aes(x = x_bar)) + geom_histogram(binwidth = 20)

#STEP 9: the step’s objective is to notice the relation between the average mean and variability for the distribution of the 
#samples means and compare them with the population mean and its standard deviation
mean(sample_means50$x_bar)
sd(sample_means50$x_bar)

#STEP 10: the step’s objective is to sample the population by different sample sizes
sample_means10 <- ames %>%
  rep_sample_n(size = 10, reps = 15000, replace = TRUE) %>%
  summarise(x_bar = mean(area))
ggplot(data = sample_means10, aes(x = x_bar)) + geom_histogram(binwidth = 20)

sample_means100 <- ames %>%
  rep_sample_n(size = 100, reps = 15000, replace = TRUE) %>%
  summarise(x_bar = mean(area))
ggplot(data = sample_means100, aes(x = x_bar)) + geom_histogram(binwidth = 20)

sample_means1000 <- ames %>%
  rep_sample_n(size = 1000, reps = 15000, replace = TRUE) %>%
  summarise(x_bar = mean(area))
ggplot(data = sample_means1000, aes(x = x_bar)) + geom_histogram(binwidth = 20)

