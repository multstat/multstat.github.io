### Multivariate statistical analysis, 2019 ###
### Mixed-effects models (part 2)

### seminar: mixed-effects models for time dynamics. Time specification
### model for means and variances: between-group variance

install.packages("haven")
install.packages("psych")
install.packages("arm")
install.packages("multilevel")
install.packages("lattice")
install.packages("sjPlot")
install.packages("influence.ME")
install.packages("gplots")
install.packages("ggplot2")
install.packages("lmerTest")

library(haven)
library(psych)
library(arm)
library(multilevel)
library(lattice)
library(sjPlot)
library(influence.ME)
library(gplots)
library(ggplot2)
library(lmerTest)

# set a working directory
# open your data (MSA_lab5.dta)
ME <- read_dta(file.choose())

###Do we need mixed-effects models to analyze the dynamics? 
###Use graphs to visualize data and variance decomposition

##################### PRELIMINARY VISUALIZATION #####################

# at the first step get the first approximation of a time trajectory (linear or non-linear effect)
plotmeans(fh_polity ~ period, data = ME, n.label = FALSE, main="Means through the Time",xlab="Period",ylab="fh_polity")

# examine trajectories in the countries
# graph for each country
ggplot(data = ME, aes(x = period, y = fh_polity)) +
  geom_line() + geom_point()+facet_wrap( ~ country)

# How much variance is due to country-level characteristics?
null <- lmer(fh_polity ~ 1 + (1|country), ME, REML = FALSE)
null
anovaicc <- aov(fh_polity ~ as.factor(country), ME)
ICC1(anovaicc)
# If the sample is unbalanced, calculate ICC manually

# make sure that between-group variance is NOT due to outliers
graph.ran.mean(ME$fh_polity, ME$country, nreps=1000, bootci=TRUE)

##################### MODELING. LINEAR TIME EFFECT  #####################
# recode "period" to obtain the variable "time" with "0" as a starting point 
# to facilitate the interpretation of coefficients
ME$time0 <- ME$period-1

# specify the "time" part
m1 <- lmer(fh_polity ~ time0 +(1|country), data = ME, REML = FALSE)
summary(m1)
ranef(m1)
dotplot(ranef(m1, condVar=TRUE))

m2 <- lmer(fh_polity ~ time0 +(1 + time0|country), data = ME, REML = FALSE)
summary(m2)
ranef(m2)
dotplot(ranef(m2, condVar=TRUE))

anova(m1, m2)

##################### QUADRATIC TIME EFFECT  #####################
m3 <- lmer(fh_polity ~ time0 + I(time0^2) + (1 + time0|country), data = ME, REML = FALSE)
summary(m3)

anova(m3,m2)

m4 <- lmer(fh_polity ~ time0 + I(time0^2) + (1 + time0 + I(time0^2)|country), data = ME, REML = FALSE)
summary(m4)

anova(m4,m3)
dotplot(ranef(m4, condVar=TRUE))
