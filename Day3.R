# Day3 
# data distributions : ch 5 
# 17 April 2018


# 1. discrete data 
# 2. continuous data: main mode for biologists because we take measurements 


# Discrete distribution  --------------------------------------------------
# fixed number of possible outcomes 

# bernoulli distribution (probability)
# law of large numbers (As the number of experiments increases, the actual ratio of outcomes will converge on the theoretical, or expected, ratio of outcomes.  )

# binomial (either or) - 1 of 2 outcomes 

# negative binomial : counts number of successes before a failure 

# geometric distribution: number of trials before a siingle success. eg. dice 

# poisson distribution: number of events in given time or space interval


# continuous distribution  ------------------------------------------------

# gaussian: enough samples, the mean will approx the distribution of the data that was sampled 

# normal distribution / gaussian = bell shaped 

# data with normal distribution are independent of each other, ,

# outliers: meaningful? is their ecological or economical significance? asks questions as to why is it occurring? 

# normal: mean NB and = median, most measurements of sample clustered around mean 
# 68.2% of data within 2 sd above and below mean ... thus sd and mean of vital importance 
# median and q1 and q3 show skewness 

# uniform distribution: within bounds, every interval has an equal probability of occurring 
# min and max value, but no predictive way of saying one value occurs more than another 

# student t distribution: calculating mean from samples from a population 
# sample from calcution from many means 

# chi squared tests: statistical usefulness 

# exponential distribution: (see poisson) amount of time elapsed between events, right skewed distrbution 
# rate affects steepness: rate of decay 

# f distribution: number of variances 

# gamma distribution: 

# beta distribution: dont come across in biological science 

### 

# Determining your distribution  ------------------------------------------

# Cullen and Frey graph 
# based on the position of the big blue dot, u can approximate a distribution, no definite way 

library(fitdistrplus)
library(logspline)

# Generate log-normal 

r_norm <-  rnorm(n = 1000, mean = 13, sd = 1)

hist(r_norm)               #basic r = hist 
descdist(r_norm, discrete = FALSE, boot = 100)


# uniformly distributed data
y <- runif(100)
par(mfrow = c(2, 2))
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)

y <-rnorm(n = 200, m = 13, sd = 2)
par(mfrow = c(2, 2))
# using some basic base graphics as ggplot2 is overkill;
# we can get a histogram using hist() statement
hist(y, main = "Histogram of observed data")
plot(density(y), main = "Density estimate of data")
plot(ecdf(y), main = "Empirical cumulative distribution function")
# standardise the data
z.norm <- (y - mean(y)) / sd(y) 
# make a qqplot
qqnorm(z.norm)
# add a 45-degree reference line
abline(0, 1)


# T-test  -----------------------------------------------------------------

# what do u want to do? then pick a test accordingly
# ch 6: Inferences about one or two populations

# t-test: comparing 2 things 
# anova: comparing more than 2 things 

#One-sample t-tests: only one sample set of data that we wish to compare against a known population mean:
      #one-sided one-sample t-tests
      #two-sided one-sample t-tests
#Two-sample t-tests: the means of two groups are compared against each other:
 # independent sample t-tests
      #one-sided two-sample t-tests
      #two-sided two-sample t-tests
# paired sample t-tests
      #one-sided
      #two-sided


# assumptions  ------------------------------------------------------------

# 1. data are normally distributed 
# 2. data are similar = homoscedastic

library(tidyverse)
library(plotly)

# create random data
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))

# check assumptions

# normality == use Shapiro-wilk test 
shapiro.test(r_dat$dat)
shapiro.test(r_dat$dat)[1]   # only output w value
shapiro.test(r_dat$dat)[2]  # only output p value 

# if p < 0.05 significantly different from normal
# p > 0.05 not significantly different from normal (want?)


# but that is testing the whole dataset together
# be clever about it 
r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]))  #2 = group A and group B 
# test for normality 
# data are normal when p > 0.05
# data are non-normal when p < 0.05


#  check homoscedasticity -------------------------------------------------

# many ways to check for homo.
# for which is the similarity of variance between sample sets 
# for now say that assumption is met when 
# the varience of the samples are mot more than 2 - 4 times greater 
# than one another 

#do everything at once 
var(r_dat$dat) 
#WRONG!!!!

# do it the tidy way 
r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
            r_norm_var = var(dat)) 

# more assumptions 
# are data independent of each other 
# dependent variable must be continuous (measurements / numbers)

two_assum <- function(x) {
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)
}



# One sample t-test -------------------------------------------------------

r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")

# test normality
shapiro.test(r_one$dat)    # p = 0.6282 = normal 

# visualisation 
 hist(r_one$dat)

# run the test 
# choose column, population mean = mu, 
 t.test(r_one$dat, mu = 20)      # p = 0.61 = normal, mean = 20.63, not sig different

# run a test that we know would produce a significant result
t.test(r_one$dat, mu = 30)     # p = 3.0068e-07, sig different


# Pick a side  ------------------------------------------------------------

# one sample t-test

# are these data smaller/less than the population mean? 
t.test(r_one$dat, mu = 20, alternative = "less")                #alternative = in either one of the tails 
# or greater?
t.test(r_one$dat, mu = 20, alternative = "greater") 

# but what about for a larger population mean?
# are the samples less than the population of 30? 
t.test(r_one$dat, mu = 30, alternative = "less") 
# what about greater than?
t.test(r_one$dat, mu = 30, alternative = "greater") 
# no


# two-sample t-test -------------------------------------------------------
# subtract mean of 1 sample by another sample and divide by group sd 


r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

# run default/ basic test:                                
# perform t-test
# note how we set the `var.equal` argument to TRUE because we know 
# our data has the same SD (they are simulated as such!)

t.test(dat ~ sample, data = r_two, var.equal = TRUE)   #~ = what are the data u comparing, and categorical data
                                                      # defualt = var not the same (false)
# sample size is too small, could depend on day of sampling 
# p values vary between sig and not sig

# compare means
compare_means(dat ~ sample, data = r_two, method = "t.test", var.equal = TRUE)

# pick a side 
# is A less than B = yes p = 0.00197
 t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "less")
# is A greater than B = no, p = 0.998
 t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "greater")
 

#  T-test workflow 6.6 upto Exercise 1 ----------------------------------------------------

# loading data
 
ecklonia <- read_csv("ecklonia.csv") %>% 
   gather(key = "variable", value = "value", -species, -site, -ID)
 
# visualising data 
 
 ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
   geom_boxplot() +
   coord_flip()
 
# formulating a hypothesis
# filter the data
 ecklonia_sub <- ecklonia %>% 
   filter(variable == "stipe_mass")
 
# then create a new figure
 ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
   geom_boxplot() +
   coord_flip() +
   labs(y = "stipe mass (kg)", x = "") +
   theme(axis.text.y = element_blank(),
         axis.ticks.y = element_blank())
 
# checking assumptions 
 ecklonia_sub %>% 
   group_by(site) %>% 
   summarise(stipe_mass_var = var(value)[1],
             stipe_mass_norm = as.numeric(shapiro.test(value)[2]))
 
#running an analysis
 
# traditional output
 t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")
 
# dataframe output
 compare_means(value ~ site, data = ecklonia_sub, method = "t.test", var.equal = TRUE, alternative = "greater")
 
 
# drawing conclusions 
# The stipe mass (kg) of the kelp Ecklonia maxima was found to be significantly greater at Batsata Rock
# than at Boulders Beach (p = 0.03, t = 1.87, df = 24)
 

# Exercise 1 --------------------------------------------------------------

# Find or create your own normally distributed data and think of a hypothesis you could use a t-test for.
# Write out the hypothesis, test it, and write a one sentence conclusion for it.
# Provide all of the code used to accomplish this. 
 
# Question: Are the stipe lengths at St. James greater than the stipe lengths at Oudekraal? 
# Null hypothesis (H0): Stipe lengths are not greater at St. James than at Oudekraal.
# (H1):  Stipe lengths are greater at St. James than at Oudekraal.
 
# (HO): A </= B
# (H1): A > B  
 
# load data  
 morphology <- read_csv("morphology.csv")

# convert wide data to long data 
 morph_long <- morphology %>% 
   gather(key = "variable", value = "value", -site, -ind, -date, -depth, -fertile)

# visualising data 
ggplot(data = morph_long, aes(x = variable, y = value, fill = site)) +
   geom_boxplot() +
   coord_flip()
 
# filter the data
morph_sub <- morph_long %>% 
  filter(variable == "stipe_length")

# then create a new figure
ggplot(data = morph_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "stipe length (cm)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# checking assumptions 
morph_sub %>% 
  group_by(site) %>% 
  summarise(frond_length_var = var(value)[1],
            frond_length_norm = as.numeric(shapiro.test(value)[2]))
 
#running an analysis

# traditional output
t.test(value ~ site, data = morph_sub, var.equal = TRUE, alternative = "less")
 
# dataframe output
compare_means(value ~ site, data = ecklonia_sub, method = "t.test", var.equal = TRUE, alternative = "greater")

# We reject the null hypothesis that stipe length is greater at St. James than at Oudekraal. 
# Stipe length (cm) of Ecklonia maxima was found to be significantly greater at St. James than at Oudekraal.
# (p = 0.00, t = -4.16, df = 24).

 
 
