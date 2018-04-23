# Day 5 
# 420 2018 
# Anova examples 


# Anova assumptions  ------------------------------------------------------

#Normally distributed data
#Homogeneity of variances
#Independence of data  (violated: same snakes measured over time)
#In our case, we will encourage also that the data are balanced

# load libraries  ----------------------------------------------------------

library(tidyverse)
library(pgirmess)
library(Rmisc)

# load data 
snakes <- read_csv("snakes.csv") %>% 
  mutate(day = as.factor(day))           #without as.factor, reads day as linear regression 
#or 
# snakes$day <- as.factor(snakes$day)     

# manipulate data ---------------------------------------------------------

snakes_summary <- snakes %>%
  group_by(day, snake) %>%                 #counts how many times a snake responds per day, but only one response per day 
  summarise(snakes_mean = mean(openings),
            snakes_sd = sd(openings))            #require replicate measurements of an individual snake per day 

#therefore: mean of all snakes per day
snakes_summary <- snakes %>%
  group_by(day) %>%               
  summarise(snakes_mean = mean(openings),
            snakes_sd = sd(openings)) 


# formulate hypothesis  ---------------------------------------------------

# HO: there is no difference in the number of openings from day to day
# H1: there is a difference in the number of openings from day to day
# binary set of outcomes 
# 95% probability should explian the alternative hypothesis

# Test the hypothesis  ----------------------------------------------------

# Rmisc package
# first calculate CI (confidence interval) and SE (standard error)
snakes.summary2 <- summarySE(data = snakes,
                             measurevar = "openings",
                             groupvars = c("day"))

#visualise
ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, 
                                           y = openings - ci, yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

#CI gives suggestion of where the differences are, 
#show distribution of raw data = informative (small sample size but large and evenly spread)
# not a proper normal distribution, kurtosis 

# show all snakes behave the same way across all the days 
# H0: There is no difference between snakes with respect to the number of openings at which they habituate.
# H0: There is no difference between days in terms of the number of openings at which the snakes habituate.

#test the "Day" hypothesis 
snakes.day.aov <- aov(openings ~ day, data = snakes)
summary(snakes.day.aov)
# report: df, sum of squares, f-value, prob < 0.05 (less than 5% chance of rejecting )

# test both hypotheses
snakes.all.aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes.all.aov)
#knowing about snakes aids data and takes away some var from the residual
# snakes are not having a sig effect on the openings (prob = 0.3382)


#  testing assumptions afterwards  ----------------------------------------

# Checking assumptions...
# make a histogram of the residuals (unexplained variation);
# they must be normal
snakes.res <- residuals(snakes.all.aov)
hist(snakes.res)
# normal/ fairly even distribution 

# make a plot of residuals and the fitted values;
# # they must be normal and homoscedastic
plot(fitted(snakes.all.aov), residuals(snakes.all.aov))
# hetero = number of dots above and below 0 would be even 
# also evenly spread throughout 

# which pair of days is the effect 
snakes.tukey <- TukeyHSD(snakes.all.aov, which = "day")
plot(snakes.tukey)
# day 4-1 

# all snakes become more generally used to the opening occassions 
# therefore no sig was really found 

# visualise the factor interaction
ggplot(snakes, aes(x = as.numeric(day),
                   y = openings, 
                   colour = snake)) +
  geom_line(size = 3) +
  geom_point(size = 4)


# Exercise ----------------------------------------------------------------

library(ggpubr)

# get the moth data from github
# form hypotheses 
# run a two way anova on them 
# background: invasive moths species, bad for trees 
# which types of moth traps worked best, traps at 3 levels and recorded number of moths in 48 hr period 
# used 3 type of traps: scented, sugar, and chemical 
# is there a sig diff of which traps works best and which location of traps is best 

# load data

moths <- read_csv("moths.csv") %>% 
  gather(key = "trap", value = "count", -Location)

#manipulate data 
moths_summary <- moths %>%
  group_by(Location) %>%               
  summarise(moths_mean = mean(count),
           moths_sd = sd(count)) 


# formulate hypothesis  ---------------------------------------------------

# HO: there is no difference in the number of moths at various locations of the traps 
# H1: there is a difference in the number of moths at various locations of the traps 


#  test the hypothesis  ---------------------------------------------------

# first calculate CI (confidence interval) and SE (standard error)
moths.summary2 <- summarySE(data = moths,
                             measurevar = "count",
                             groupvars = c("Location"))

#visualise
ggplot(data = moths, aes(x = Location, y = count)) +
  geom_segment(data = moths.summary2, aes(x = Location, xend = Location, 
                                           y = count - ci, yend = count + ci, colour = Location),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = Location), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

# show that the type of traps have no effect on the number of moths caught 
# HO: there is no difference between the trap types with respect to location of the traps 
# HO: there is no difference in the number of moths at various locations of the traps

#test the "Location" hypothesis 
moths.loc.aov <- aov(count ~ Location, data = moths)
summary(moths.loc.aov)

#            Df Sum Sq Mean Sq F value   Pr(>F)    
# Location     3   1981   660.5   11.34 6.46e-06 ***
#  Residuals   56   3262    58.2                     

# we reject the null hypothesis that there is no difference in the number of moths at various locations. 

# test both hypotheses
moths.all.aov <- aov(count ~ Location + trap, data = moths)
summary(moths.all.aov)

#            Df Sum Sq Mean Sq F value   Pr(>F)    
# Location     3   1981   660.5  11.327 7.17e-06 ***
# trap         2    113    56.5   0.969    0.386    
# Residuals   54   3149    58.3                     

# we fail to reject the null hypothesis that there is no difference between trap types with respect to the location of the traps 

#  testing assumptions afterwards  ----------------------------------------

# Checking assumptions...
moths.res <- residuals(moths.all.aov)
hist(moths.res)

# make a plot of residuals and the fitted values;
# # they must be normal and homoscedastic
plot(fitted(moths.all.aov), residuals(moths.all.aov))

# which pair of Locations is the effect 
moths.tukey <- TukeyHSD(moths.all.aov, which = "Location")
plot(moths.tukey)
# significant differences at lower-ground, middle-ground, top-lower and top-middle
# top-ground and middle-lower locations are not significant 

# visualise the factor interaction
plot3 <- ggplot(data = moths, aes(x = Location, y = count)) +
    geom_boxplot(aes(fill = trap)) 


plot1 <- ggplot(data = moths, aes(x = Location, y = count)) +
  geom_boxplot()+
  geom_jitter(width = 0.05, shape = 22)

plot2 <- ggplot(data = moths, aes(x = trap, y = count)) +
  geom_boxplot() +
  geom_jitter(width = 0.05, shape = 22)

ggarrange(plot1, plot2, plot3, nrow = 2, ncol = 2)


# Regressions  ------------------------------------------------------------

# what is the relationship between different levels in the data 
# as one thing changes, does something else change and to what degree
# effect of independant data on dependant data
# strength of causality / causation tested 

# residual: difference from observed and predicted values 
# r^2: creates boxes, sum of sqs minimised = best fit. r^2 = 1, p = sig 
# r^2 = the var in y is explained by x, the lower the value the less related r^2 = 0, p = 1

# for the explanation of this statistical analysis 
# we are going to use eruption data from ol` faithfull 

View(faithful)
#look at the top of the data 
head(faithful)

#plot a quick scatterplot 
ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = "hotpink")

# question: there is a significant waiting time for a duration of an eruption 
#HO: the waiting time does not influence the duration of an eruption 
#H1: waiting time does influence tbe duration of an eruption 


# test the hypothesis  --------------------------------------------------

faithful_lm <- lm(eruptions ~waiting, data = faithful)
summary(faithful_lm)

#intercept: test hypothesis that there is no sig diff of the intercept from 0 
#waiting: test hypothesis that there is no sig diff in waiting time from 0 
#multiple r^2: 81% var explained by knowing waiting time 


# linear regression graph showing standard error 

slope <- round(faithful_lm$coef[2], 3)
# p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) # it approx. 0, so...
p.val = 0.001
r2 <- round(summary(faithful_lm)$r.squared, 3)

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") +
  labs(title = "Old Faithful eruption data",
       subtitle = "Linear regression",
       x = "Waiting time (minutes)",
       y = "Eruption duration (minutes)")


# Correlation  ------------------------------------------------------------

# assumptions 
# pairwise data
#absence of outliers
#linearity
#normality of distribution
#homoscedasticity
#level (type) of measurement
#Continuous data (Pearson correlation) (default)
#Ordinal data (Spearman correlation)
# Kendal Rank (when data are not normally distributed)

# Load libraries
library(tidyverse)
library(ggpubr)
library(corrplot)

# Load data
ecklonia <- read_csv("data/ecklonia.csv")


# formulate a hypothesis  ------------------------------------------------

# HO: there is no relationship between frond length and stipe length for the kelp Ecklonia maxima 
# H1: there is a relationship between frond length and stipe length for the kelp Ecklonia maxima 


#  test a hypothesis  -----------------------------------------------------
cor.test(ecklonia$frond_length, ecklonia$stipe_length)
# p-value: 0.0003% chance data is random
# CI dont cross over 0 = significant
# report Pearson coeff = 0.6524911

# visualise data 
ggplot(data = ecklonia, aes(x = frond_length, y = stipe_length)) +
  geom_point()


# run multiple correlations at once  --------------------------------------

ecklonia_sub <- ecklonia %>% 
  #select(-species, - site, - ID)
  select(stipe_length:epiphyte_length)

# just produces cor coefficients for whole dataset  
ecklonia_cor <- cor(ecklonia_sub)
ecklonia_cor


# Spearman Rank test ------------------------------------------------------

# create ordinal data
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length + ecklonia$frond_length), breaks = 3))
# size classes (breaks = 3)
# not really neccessary 

# run a Spearman test 
cor.test(ecklonia$length, ecklonia$stipe_diameter, method = "spearman")
# rho value = 0.6036462
# p- value = 0.001095  = not random, sig diff
# warning = little samples and range of ranking 


# Kendall rank  -----------------------------------------------------------
# not normal data 

# test for normlity -------------------------------------------------------

ecklonia_norm <- ecklonia_sub %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))

ecklonia_norm
#primary blade length not normal

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")
# z value 
# tau = 0.3426171
# p = 0.01827


# Visualise all the things  -----------------------------------------------

ecklonia_pearson <- cor(ecklonia_sub)

corrplot(ecklonia_pearson, method = "circle")
# try in ggplot 
# positive or negative influence size of dots 


# still to do
# heatmap

# Heatmap  ----------------------------------------------------------------

library(reshape2)

melt.eck <- melt(ecklonia_pearson)

ggplot(melt.eck, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkorchid3", name = "Pearson Correlation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))



