# Day4
# ANOVA CH.7: analysing more than 2 samples 
# 19 April 2018 

# As t-tests, ANOVAs require that some assumptions are met:
# Normally distributed data
# Homogeneity of variances
# Independence of data
# In our case, we will encourage also that the data are balanced (same #ind per group)
# multiple t-tests increases the probability of type 1 error


# load data  --------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(pgirmess)

# t-test ------------------------------------------------------------------

# First grab the data
chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)

#t-test
t.test(weight ~ Diet, data = chicks_sub)
# p-value = 0.2176
# we do not reject (accept) the null hypothesis that there is a difference between diet 1 and 2.


# one-way anova -----------------------------------------------------------
# one factor analysis of varience (diet), with 4 levels 

# research question: is there a difference in chicken mass attained after 21 days 
# after the chickens having been fed four different diets? 
# H0: there is no difference in chciken mass at 21 days after being fed one of four different diets 

chicks_21 <-chicks %>% 
  filter(Time == 21)

#output of Anova 
chicks.aov1 <- aov(weight ~ Diet, data = chicks_21)
summary(chicks.aov1)

# or chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))

# Pr(>F) = 0.00686  (sig diff somewhere, but need to pinpoint which diet has an effect)
# we do not accept the null hypothesis, weaccept the alternative hypothesis 
# which states that there is a significant difference in mass at day 21 

# notched boxplot
ggplot(chicks_21, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet), notch = TRUE)

# notch = indentation in boxplot
# extend imaginary grey band across screen, if bands overlap = no sig different between them 
# where bands cease to overlap = sig difference between them 
# sig diff between 1&3 
# no sig diff between 1&2, 2&3, and 3&4, 4&2, 1&4
# extent of notch whether > /< Q1 and Q3? = ensures symmetrical notch 
# width of notch = 1.5* IQR


# Tukey HSD test ----------------------------------------------------------

chicks_tuk <- TukeyHSD(chicks.aov1)
# positive lower confidence interval = a sig diff

ggplot(chicks_21, aes(x = Diet, y = weight, fill = Diet)) +
  geom_boxplot(notch = TRUE, colour = "grey50") +
  geom_segment(aes(x = Diet, xend = Diet, y = weight, yend = weight + 2))

# confidence intervals = likelihood that the upper and lower limit dont overlap 
# if lower and upper range go across 0 = sig diff (95% confident that 3 and 1 are different)

# segments showing confidence intervals 
# dataframe of segments 
# creates df from output of tukey test
chick_tukey <- as.data.frame(TukeyHSD(aov(weight ~ Diet, data = chicks_21))$Diet)

#
chick_tukey$pairs <- as.factor(row.names(chick_tukey))

# Base R 
plot(TukeyHSD(aov(weight ~ Diet, data = chicks_21)))
# or 
plot(TukeyHSD(chicks.aov1, "Diet"))

# or use ggplot
ggplot(chick_tukey)+
  geom_segment(aes(x = pairs, xend = pairs, y = lwr, yend = upr + 2), lineend = "butt") +
  geom_hline(aes(yintercept = 0), size = 0.5, colour = "red", linetype = "dashed") +
  coord_flip()

#or 
ggplot(chick_tukey, aes(x = pairs, y  = diff))+
  geom_errorbar(aes(ymin = lwr, ymax = upr)) +
  geom_point(aes(colour = pairs)) +
  geom_hline(aes(yintercept = 0), colour = "red", linetype = "dashed")+
  theme_bw()

#  videos  ----------------------------------------------------------------

# vid 1: violin plot 
# density distribution as boxplot
# adding more data point, the anova is more confident that the data is similar
# decreasing confidence intervals: mean pulls towards actual population mean 
# need more samples to get true pop mean

#vid 2:
# changing the mean 
# differences in means give significance 

# vid 3: 
# variance changes 
# confidence intervals get wider, results less sinificant
# var = FALSE


# multiple factor Anova  --------------------------------------------------

# Question: how is chicken mass changing over time? 
# H0: ther is no change in chicken mass over time (from day 0 to day 21). 

chicks_0_21 <-ChickWeight %>% 
  filter( Time %in% c(0, 2, 21))

# visualise data
ggplot(chicks_0_21, aes(x = as.factor(Time), y = weight)) +
  geom_boxplot(notch = TRUE, aes( fill = as.factor(Time)))

# run an anova 
summary(aov(weight ~ as.factor(Time), data = filter(chicks_0_21)))

# perform Tukey post-hoc test
TukeyHSD(aov(weight ~ as.factor(Time), data = filter(chicks_0_21)))

#look at confidence intervals 
plot(TukeyHSD(aov(weight ~ as.factor(Time), data = filter(chicks_0_21))))

#look only at day 0 and 21 for both time and diet 
summary(aov(weight ~ Diet + as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

# or look  at all the time 
# which is not the hypothesis 
summary(aov(weight ~ Diet + as.factor(Time), data = ChickWeight))
# note the increase in degrees of freedom for the time factor 
#but no increase in DF of Diet

#no sig diff in weight based on diet on day 0 
summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(0))))
# sig diff in weight based on diet on day 2, 10, 21 etc 
summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(2))))
 

# two way anova  ----------------------------------------------------------
# To specify the different factors we put them in our formula and separate them with a +:

# compare effects on diet and time (0, 21) simultaneously on weight 
summary(aov(weight ~ Diet + as.factor(Time), data = filter(chicks_0_21, Time %in% c(0, 21))))

#  interaction effect between grouping variables(effect of time on diet)
#  replace the + in our formula with *:
# interactions between factors 
summary(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))


# lets look at the tukey results 
TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

plot(TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21)))))

# create a line graph to help explain this concept
# first create mean values of time and diet
chicks_mean <-  ChickWeight %>%  
  group_by(Diet, Time) %>% 
  summarise(weight_mean = mean(weight, na.rm = TRUE))


ggplot(chicks_mean, aes(x = Time, y = weight_mean , colour = Diet)) +
 geom_line(size = 2) +
  geom_point(shape = 15, size = 5)
 
# at time 20, the effects of diets are different 
# effect of diet is not consistent across time (interaction effect)
# yes there is an effect, but it depends 



# non-parametric tests  ---------------------------------------------------

#but what if...
# we dont have normal data 

# for a t-test we rather use wilcox rank sum test
wilcox.test() # and then one fills this in the same as a t-test()

# Kruskal-Wallis
kruskal.test(weight~ Diet, data = chicks_0_21)

#load package for non-parametric post-hoc test 
library(pgirmess)
kruskalmc(weight ~ Diet, data = chicks_0_21)

#no non-parametric version of multi way anova 




