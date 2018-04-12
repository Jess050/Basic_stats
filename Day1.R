# Day 1.R
# day one of basic stats
# to practice some of the concepts we will encounter 
# 12 April 2018 



# load libraries  ---------------------------------------------------------

library(tidyverse)


# Integers (discrete and continuous)----------------------------------------------------------------

#  nominal/ discrete data: how to generate integers  ----------------------
integer_r <- as.integer(seq(5, 14, by = 1))

#list of values in dataset  
integer_r
#brief summary of dataset
summary(integer_r)

# continuous data: generate -----------------------------------------------

numeric_r <-seq(23, 40, length.out = 10)

#length.out to generate 13 equal spaces between 23 and 43 
#length and length.out is same thing, length out is the proper code


# Dates  ------------------------------------------------------------------
# distinct from continuous data 
#different kind of dates 

#can perform arithmetic with dates 
as.Date("2005-12-31") - as.Date("2005-12-12")

#international standard year-month-day
#or for example generate sequence 
dates_r <- seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day")

dates_r

# create base dataframe with 3 columns (make sure all columns  are the same lengths)
df_r <- data.frame(integers = integer_r,
                  numeric = numeric_r,
                  dates = dates_r)

# upgrade it to a tibble 
df_r <- as_tibble(df_r)

summary(df_r)


# Qualitative data: categorical data -------------------------------------------------------

# electronics 
elec_r <- as.factor(c("laptop", 
                      "desktops",
                      "cell phones"))
#People
people_r <- as.factor(c("funny", 
                        "beautiful",
                        "beanies"))
#colours 
colour_r <- as.factor(c("red", "blue"))

#2 categories, 2 levels with same name
colour_r
# factor variable is NB, certain functions work on factors only, not continuous or discrete data


# Qualitative data: Ordinal data ------------------------------------------------------------

#qualitative data with some order/ ranks
# function = ordered, also need to specify the ordering which = levels 
#where green is warmer than blue but red is hottest
colour_qual <- ordered(c("blue", "green", 
                         "yellow", "orange", 
                         "red", 
                            levels = c("blue", "green",
                                       "yellow", "orange",
                                       "red")))
colour_qual


# binary data -------------------------------------------------------------
#computer reads as TRUE or FALSE 
#only 1 of 2 possible outcomes (measurements) eg. true/false, alive/dead
binary_r <- c(TRUE, FALSE, TRUE, FALSE)

summary(binary_r) #counts number of true and false 


# character data ----------------------------------------------------------
# 

sites_r <- c("yzerfontein", "Betty`s Bay", "Gansbaai", "Sea Point")

summary(sites_r)


# Missing values  ---------------------------------------------------------

#NA = not available ; case is present, but the value is not available (i.e nest 10 was not sampled)
# number of eggs recorded in a nest
chicks_nest <-c(3, 2, 0, 10, 5, 6, 8, 2, 4, NA)

summary(chicks_nest)
#mean 
mean(chicks_nest)
#standard deviation 
sd(chicks_nest)


# complex numbers  --------------------------------------------------------

#have two parts, a real part and imaginary part


# viewing data ------------------------------------------------------------

ChickWeight
#or 
summary(ChickWeight)
#or 
chicks <- (ChickWeight)
View(chicks)
#or 
head(chicks, 7)
tail(chicks, 15)

#summary MOUNT OF ROWS AND COLUMNS 
ChickWeight[1:3,]
#SUMMARY specific rows 
ChickWeight[c(1,54,61,12),]
#summary specific row from column 2 
ChickWeight[c(1,54,61,12), 2]


# descriptive stats -------------------------------------------------------

#first create dataframe 
chicks <- as_tibble(ChickWeight)
#scientific notationfor number of things is n
chicks %>% 
  summarise(chicken_count = n())
#or
nrow(chicks)


# Measures of central tendency --------------------------------------------

#calculate mean weight
chicks %>%
  summarise(mn_wt = mean(weight))
#be more specific (== equivalent to)
central_chicks <- chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mn_wt = mean(weight), 
            median_wt = median(weight)) #means can easily be corrupted by outliers therefore use median 
#if means and medians are similar the data is normalised, if not.. 

#visualise the density of the data
#density plot shows how data is distributed, only x axis
ggplot(data = filter(central_chicks, Time == 21),
       aes(x = weight, fill = Diet)) +
  geom_density(alpha = 0.4) + 
  geom_vline(aes(xintercept = mn_wt,
                 colour = Diet), size = 1.5) +
 geom_vline(data = central_chicks,
            aes(xintercept = median_wt,
                              colour = Diet), size = 1.5, linetype = "dashed")
#1 and 3 are skewed right and left respectively, 4 and 2 are normalised
#mean solid line, median dashed line

# Skewness  ---------------------------------------------------------------
#skewness is a measure of symmetry
#right skewed to the left, & left skewed to the right (use hands squish data down)

#calculate the numeric value
#load libraries 
library(e1071)

#compare the difference in mean and median against skewness 
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mn_wt = mean(weight), 
            median_wt = median(weight),
            skew_wt = skewness(weight))
#value of mean denotes skewness 
#right skewed is positive, mean is ahead(right) of median (rule of thumb, not always TRUE)
#left skewed is negative, mean is behind (left) median 
#see ggplot -> geom_vline 


# Kurtosis ----------------------------------------------------------------

#kurtosis describes the tail shape of the data`s distribution 
#positive = fat-tailed (large number of outliers)
#negative = thin-tailed (bunch of fat chicks)

#calculate the kurtosis of the tails of a distribution 
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mn_wt = mean(weight), 
            median_wt = median(weight),
            skew_wt = skewness(weight),
            kurtosis_wt = kurtosis(weight))

# 1SD on left+right side of median/mean is 68%


# Variance  ---------------------------------------------------------------
#SD is the sqrt of the variance, just the weight

#below is a summary of many different statistical properties 
wt_summary <- chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(wt_mean = mean(weight),
            wt_median = median(weight),
            wt_quart2 = quantile(weight, 0.5),  #where median is 50% quartile
            wt_var = var(weight),
            wt_sd = sd(weight),
            wt_min = min(weight),
            wt_quart1 = quantile(weight, 0.25),
            wt_quart3 = quantile(weight, 0.75)) 
wt_summary




