# Ch. 7 Anova excercises 
# practising anova tests 
# 23 April 2018 


# load libraries  ---------------------------------------------------------

library(tidyverse)
library(pgirmess)


# Exercise 1 --------------------------------------------------------------

#Here is bunch of data for pigs raised on different diets. 
#The experiment is similar to the chicken one.
#Does feed type have an effect on the mass of pigs at the end of the experiment?

# enter the mass at the end of the experiment
feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

# make a dataframe
bacon <- as.tibble(data.frame(
  feed = c(
    rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))),
  mass = c(feed_1, feed_2, feed_3, feed_4)))

# H0: there is no difference in pig mass after being fed one of four different diets. 

#output of Anova 
bacon.aov1 <- aov(mass ~ feed, data = bacon)
summary(bacon.aov1)

# Pr(>F) = 1.06e-11 (sig diff somewhere, but need to pinpoint which diet has an effect)
# we do not accept the null hypothesis, we accept the alternative hypothesis 
# which states that there is a significant difference in mass after being fed one of four diets

# notched boxplot
ggplot(bacon, aes(x = feed, y = mass)) +
  geom_boxplot(aes(fill = feed), notch = TRUE)

# notched boxplot suggests a sig diff between all the four different feeds.

#identify where the significant differences are 
bacon_tuk <- TukeyHSD(bacon.aov1)


ggplot(bacon, aes(x = feed, y = mass, fill = feed)) +
  geom_boxplot(notch = TRUE, colour = "grey50") +
  geom_segment(aes(x = feed, xend = feed, y = mass, yend = mass + 2))

# creates df from output of tukey test
bacon_tukey <- as.data.frame(TukeyHSD(aov(mass ~ feed, data = bacon))$feed)

#identify significant pairs
bacon_tukey$pairs <- as.factor(row.names(bacon_tukey))

#visualise
plot(TukeyHSD(aov(mass ~ feed, data = bacon)))

#or
ggplot(bacon_tukey, aes(x = pairs, y  = diff))+
  geom_errorbar(aes(ymin = lwr, ymax = upr)) +
  geom_point(aes(colour = pairs)) +
  geom_hline(aes(yintercept = 0), colour = "red", linetype = "dashed")+
  theme_bw()+
  coord_flip()

# each pair of feeds is significantly different from each other,
# feed 3 produces higher mass per pig, than the other feeds. 


# Exercise 2 --------------------------------------------------------------

#Construct suitable null and alternative hypotheses for the built-in ToothGrowth data,
#and test your hypotheses using an ANOVA.

teeth <- datasets::ToothGrowth

# Question: Is there a chnage in tooth length after taking vitamin c supplements? 
# HO: tooth length does not change after taking vit c supplements 
# H1: tooth length does change after taking vit c supplements 

teeth_supp <-teeth %>% 
  filter(supp  %in% c("VC", "OJ"))

# visualise data
ggplot(teeth_supp, aes(x = as.factor(supp), y = len)) +
  geom_boxplot(notch = TRUE, aes( fill = as.factor(supp)))

# run an anova 
summary(aov(len ~ as.factor(supp), data = filter(teeth_supp)))
# pr (>F) = 0.0604

# perform Tukey post-hoc test
teeth_tukey <- TukeyHSD(aov(len ~ as.factor(supp), data = filter(teeth_supp)))
# p = 0.0603934 
# not significant as the CI overlaps 0

#look at confidence intervals 
plot(TukeyHSD(aov(len ~ as.factor(supp), data = filter(teeth_supp))))

#look at both supp and dose 
summary(aov(len ~ dose + as.factor(supp), data = teeth_supp))

# compare effects of dose and supplement type simultaneously on tooth length 
summary(aov(len ~ dose + as.factor(supp), data = teeth_supp))

# interactions between factors: effect of dose on supplement type 
summary(aov(len ~ dose * as.factor(supp), data = teeth_supp))
#significance in dose, supp type and the interaction between them

# lets look at the tukey results 
TukeyHSD(aov(len ~ dose * as.factor(supp), data = teeth_supp))
# p = 0.0008936

plot(TukeyHSD(aov(len ~ dose * as.factor(supp), data = teeth_supp)))



# Exercise 3 --------------------------------------------------------------

#Find or generate your own data that lend themselves to being analysed by a two-way ANOVA.
#Generate suitable hypotheses about your data, and analyse it. 
#Supplement your analysis by providing a suitable descriptive statistical summary and graph(s) of your data.

datasets::iris

View(iris)

# Question: are the sepal lengths different in iris species 
# HO: the Sepal length means are not different in each species of iris.  
# H1: the sepal length means are different in each species of iris. 

ggplot(iris, aes(x = Sepal.Length)) +
  geom_density(aes(group = Species, colour = Species, fill = Species, alpha = 0.3))
#sepal length for each species has a normal distribution.

iris.aov1 <- aov(Sepal.Length ~ Species, data = iris)
summary(iris.aov1)
# pr(>F) = <2e-16
# we reject the null hypothesis that sepal lengths are not different in each species of iris. 
# we accept the alternative hypothesis

iris_tukey <- TukeyHSD(aov(Sepal.Length ~ Species, data = iris))

plot(iris_tukey, col = "red")
# CI`s are above 0, therefore there is a significant difference between all pairs of species 

#############################


iris_1 <- iris %>% 
  filter(Species  %in% c("setosa", "versicolor", "virginica"))

# visualise data
ggplot(iris, aes(x = as.factor(Species), y = Sepal.Length)) +
  geom_boxplot(notch = TRUE, aes( fill = as.factor(Species)))
# all three species are significantly different from one another 


# run an anova 
summary(aov(Sepal.Length ~ as.factor(Species), data = filter(iris_1)))
# pr (>F) = <2e-16

# perform Tukey post-hoc test
iris_tukey <- TukeyHSD(aov(Sepal.Length ~ as.factor(Species), data = filter(iris_1)))
# p = 0
# significant as the CI does not cross over 0

# visualise confidence intervals 
plot(TukeyHSD(aov(Sepal.Length ~ as.factor(Species), data = filter(iris_1))))

#look at both sepal length and sepal width 
summary(aov(Sepal.Length ~ Sepal.Width + as.factor(Species), data = iris_1))

# compare differences of sepal length and sepal width simultaneously on iris species 
summary(aov(Sepal.Length ~ Sepal.Width + as.factor(Species), data = iris_1))
# pr (>F) = 0.00746(sepal width) <2e-16 (species)

# interactions between factors: effect of sepal length on sepal width
summary(aov(Sepal.Length * Sepal.Width + as.factor(Species), data = iris_1))


# lets look at the tukey results 
TukeyHSD(aov(Sepal.Length ~ Sepal.Width * as.factor(Species), data = iris_1))


plot(TukeyHSD(aov(Sepal.Length ~ Sepal.Width * as.factor(Species), data = iris_1)), col = "red")




