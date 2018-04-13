# Day2.R
# Exercises for Day1
# we will be looking at data visualisations and distributions
# Friday the 13th April 2018


# load libraries  ---------------------------------------------------------

library(tidyverse)


# Manual calculations  ----------------------------------------------------

# the mean 
# generating random normal data (((r(name of distribution) for random data)))
# also generate as dataframe 
r_dat <- data.frame(dat = rnorm(n = 600, mean = 372, sd = 50),
                    sample = "A")

# quick visualisation 
ggplot(r_dat, aes(x = dat)) +
  geom_density()

ggplot(r_dat, aes(x = dat)) +
  geom_histogram(binwidth = 20, colour = "black", fill = "white")


#  The Mean ---------------------------------------------------------------

# sum of all points 
# divided by
# the number of points 

r_dat %>%
  summarise(r_sum = sum(dat),
            r_n = n(),                  #could be r_n = 600, but what if sample size changes? better to keep standard formula instead of values 
            r_mean = r_sum/r_n, 
            r_mean_func = mean(dat))


#  The Median  ------------------------------------------------------------
 
# Brute force with base R
# order data before calculating median therefore getting different values 
order(r_dat$dat)[length(r_dat$dat)/2]          #dataframe $ column in dataframe
#or the tidy automagic way
r_dat %>% 
  summarise(r_median = median(dat))
# use Tidy and order data
r_dat %>% 
  arrange(dat) %>% 
  slice(n()/2)      #slice doesnt remove from only one column, so cant depict column name  



# Variance  ---------------------------------------------------------------

# the sum of Each value
  # minus the mean 
    # squared
# divided by 
  # the count of samples minus one 

r_dat %>% 
  mutate(r_error = dat-mean(dat),    # mutate = anomaly values 
  r_error_square = r_error * r_error %>% 
  summarise(r_square_sum = sum(r_error_square),
            r_var = r_square_sum/n()-1))
# or
r_var_func = var(dat)
  
# the Standard Deviation
r_dat %>%  
  summarise(r_var = var(dat),
            r_sd = sqrt(r_var),
            r_sd_func = sd(dat))
  

# Exercise 1 --------------------------------------------------------------

#Notice how the data summary for chicken weights contained within 
#wt_summary is very similar to the summary returned for weight
#when we apply summary(chicks).
#Please use the summarise() approach and construct a data summary 
#with exactly the same summary statistics for weight as that which summary()returns.
  
summary(ChickWeight$weight)

# statistical summary
ChickWeight %>%
  summarise(wt_min = min(weight),
            wt_quart1 = quantile(weight, 0.25),
            wt_median = median(weight),
            wt_mean = mean(weight),
            wt_quart3 = quantile(weight, 0.75),
            wt_max = max(weight))


# Visualisation Ch 4 -----------------------------------------------------------

# fist load libraries 
# these few packages contain most functions necessary
# to make publication ready figures 

library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(viridis) # colour palette (colour blind)

# load S.A_time.csv data

sa_time <- read.csv("S.A_time.csv")


# edit our data 
#create a new column called human starting at row 1 to row(n)
sa_time <- sa_time %>% 
  mutate(human = seq(1, n(), 1), 
         geo = c(rep(c("Cape Town", "George", "PE"), times = 6),
                 rep("Joburg", 2))) 

#wide values convert to long data format
sa_long <- sa_time %>% 
  gather(key = "time_type", value = "minutes", -human)  #key = name of column with combined columns


# Qualitative -------------------------------------------------------------

# visualise non numerical data by showing count or proprtion against other things
# proportion = count/sum -- relative a known value (better than count)

# create a count of qualitative values
sa_count <- sa_long %>% 
  count(time_type) %>% 
  mutate(prop = n/sum(n))  #new column called proportion
#sa_count is 3 columns long and 3 rows long

# Stacked bar graphs
# to graphically represent the proportion of time type 

ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +   #needs stat = identity
  labs(title = "Stacked bar graph", subtitle = "cumulative sum",
       x = NULL, y = "Count") +
  theme_minimal()  # publication ready
#20 humans answered all three time questions 

# Stacked proportion bar graph
# proportion of distribution of categories of sa time

ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "relative proportions",
       x = NULL, y = "Proportion") +
  theme_minimal()

# Pie chart
# NEVER TO BE USED IN PROFESSIONAL SETTING!!!

ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Pie Chart", subtitle = "but why though?",
       x = NULL, y = NULL) +
  coord_polar("y", start = 0)+
  theme_minimal()

# Continuous data ------------------------------------------------------------

# Histograms 
# 2D, count of values along y-axis 

ggplot(sa_long, aes(x = minutes)) +
  geom_histogram()

# oh no, get rid of the outlier
sa_clean <- sa_long %>% 
  filter(minutes < 300)

# try again 
# a faceted histogram 
ggplot(sa_clean, aes(x = minutes)) +
  geom_histogram(aes(fill = time_type), 
                 position = "dodge") +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")
# now, now now and just now all mean the same thing based on data
# now <2.5 mins, just now < 20 mins , now_now < 100 minutes 

# Relative Proportion Histogram
ggplot(sa_clean, aes(x = minutes)) +
  geom_histogram(aes(y = ..density.., fill = time_type), 
                 position = "dodge", binwidth = 1) +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

# Boxplots ----------------------------------------------------------------

# x axis is a qualitative value, y axis is quantitative value 
# thick black middle line = median
# dots = outliers 
# edge of box  = 1st and 3rd quartile = interquartile range (the closer the smaller the data)
# tails = interquartile range * 1.5


ggplot(sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type))
 
#notched boxplots 

ggplot(sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE)
# areas in boxes that are overlapping within the notch is not statistically different. 
# not necessary to test for analysis of varience if u have a notched boxplot
# now_now and just now are not statistically different 
# now is statistically different 
# BOXPLOT OVER BAR GRAPH

# calculate summary stats for plotting over the boxplots 
sa_summary_stats <- sa_clean %>% 
  group_by(time_type) %>% 
  summarise(time_type_mean = mean(minutes))

ggplot(sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE) +
  geom_point(data = sa_summary_stats, size = 6, shape = 18,
             aes(y = time_type_mean), colour = "goldenrod")
#mean and median are very different 


# Relationships  ----------------------------------------------------------

#can filter out outliers or limit axis
# a basic Scatter plot 
ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point(aes(colour = geo))+
  coord_equal(xlim = c(0, 60), ylim = c(0, 60))
# now_now and just now are related in some way according to different human opinion
# no agreement on length of time 

#adding trendlines 
ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point(aes(colour = geo))+
  geom_smooth(aes(colour = geo), method = "lm") +
  coord_equal(xlim = c(0, 60), ylim = c(0, 60))
#angle of line shows relationship between x and y variables 
# eg. PE as understanding of just now increases so does understanding of now_now,
# just at different time scales 
