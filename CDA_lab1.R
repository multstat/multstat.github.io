### Categorical Data Analysis
### Lab 1. January 26, 2021
### Binary response models. Part 1: estimation and interpretation

install.packages("haven") 
install.packages("psych") 
install.packages("dplyr") 
install.packages("ggplot2")
install.packages("glm.predict")
install.packages("questionr")
install.packages("DAMisc")
install.packages("erer")

library(haven) 
library(psych)
library(dplyr)
library(ggplot2)
library(glm.predict)
library(questionr)
library(DAMisc)
library(erer)

# open data (CDA_lab1.dta)
lits <- read_dta(file.choose())

# Description
# q804: In the past two years, have you or anyone else in your household had to take any of the following measures as the
# result of a decline in income or other economic difficulty?
# q804a - Reduced consumption of staple foods such as milk / fruits / vegetables / bread
# q804b - Reduced consumption of luxury goods
# q804c - Reduced consumption of alcoholic drinks such as beer, wine, etc.
# q804d - Reduced use of own car
# q804e - Reduced vacations
# q804f - Reduced tobacco smoking
# q804g - Postponed/withdrew from university
# q804h - Enrolled in further education because of lack of job opportunities
# q804i - Postponed/withdrew from training course (i.e. Language, computer, vocational, etc)
# q804j - Postponed or skipped visits to the doctor after falling ill
# q804k - Cancelled health insurance (for self-employment activity)
# q804l - Stopped buying regular medications
# q804m - Stopped/reduced help to friends or relatives who you helped before
# q804n - Delayed payments on utilities (gas, water, electric)
# q804o - Had utilities cut because of delayed payment
# q804p - Cut TV / phone / internet service
# q804q - Delayed or defaulted on a loan instalment
# q804r - Sell an asset
# q804s - Forced to move

# q228 - Please imagine a ten-step ladder where on the bottom, the first step, stand the poorest 10% people in our country, and
# on the highest step, the tenth, stand the richest 10% of people in our country.
# On which step was your household 4 years ago?

# Head_Of_ - gender of the household head (1-male, 0 - female)

# respondentage - age of the household head (in years)

# tablec - settlement type (1 - urban, 2 - rural, 3 - metropolitan)

# Children - number of children in a household

##########################################################################
# prepare your data
# generate a new variable (our outcome variable)
lits$measures = 0
lits$measures[lits$q804a == 1 | lits$q804g == 1 | lits$q804j == 1| lits$q804l == 1 | lits$q804n == 1| lits$q804o == 1| lits$q804p == 1 | lits$q804q == 1 | lits$q804r == 1 | lits$q804s == 1 ] <- 1
table(lits$measures)

table(lits$q228)
lits$q228[lits$q228==-97 | lits$q228==-99] <- NA

table(lits$respondentage)
lits$respondentage[lits$respondentage==-1] <- NA
lits <- rename(lits, ladder = q228, gender = Head_Of_, settlement = tablec)

lits <- na.omit(lits)

# run your first (maybe not) logit and probit models

logit <- glm(measures ~ respondentage, data = lits, 
             family = binomial(link = "logit"))
summary(logit)

range <- data.frame(respondentage=seq(18, 93, 1))
pred.logit <- predict(logit, range, type="response")
pred.data <- mutate(range, pred.logit)
pred.data
summary(pred.logit)

ggplot(pred.data, aes(x=respondentage, y=pred.logit)) +  geom_line() +
  labs(x = "Age", y = "Predicted Probability", title = "Predicted Probability of Taking Measures During the Crisis") + theme_light()

m1_logit <- glm(measures ~ gender + respondentage + as.factor(settlement) + Children + ladder, data = lits, 
             family = binomial(link = "logit"))
summary(m1_logit)

m1_probit <- glm(measures ~ gender + respondentage + as.factor(settlement) + Children + ladder, data = lits, 
                family = binomial(link = "probit"))
summary(m1_probit)

# compare the results
round(data.frame(logit = m1_logit$coefficients, probit = m1_probit$coefficients, 
                 quotient = m1_logit$coefficients/m1_probit$coefficients), 3)

# Prediction and interpretation
lits <- mutate(lits, rural = ifelse(lits$settlement == 2, 1, 0))
lits <- mutate(lits, metropolitan = ifelse(lits$settlement == 3, 1, 0))
m1_logit_ <- glm(measures ~ gender + respondentage + rural + metropolitan + Children + ladder, data = lits, 
                family = binomial(link = "logit"), x = TRUE) 
summary(m1_logit_)
# x = TRUE is needed to compute the marginal effects

lits <- mutate(lits, pred = m1_logit_$fitted.values)

ggplot(lits, aes(pred)) + geom_histogram(binwidth=0.03, color="black",
                                          fill="violet") + theme_bw()

basepredict(m1_logit_, c(1, 1, mean(lits$respondentage), 0, 0, 0, mean(lits$ladder)))
basepredict(m1_logit_, c(1, 0, mean(lits$respondentage), 0, 0, 0, mean(lits$ladder)))

### Discrete Change in predicted probabilities
dc(m1_logit_, values1 = c(1, 1, mean(lits$respondentage), 0, 0, 0, mean(lits$ladder)),
   values2 = c(1, 0, mean(lits$respondentage), 0, 0, 1, mean(lits$ladder)))

dc(m1_logit_, values1 = c(1, 1, mean(lits$respondentage), 0, 0, 0, max(lits$ladder)),
   values2 = c(1, 1, mean(lits$respondentage), 0, 0, 0, min(lits$ladder)))

### Discrete change for many variables
glmChange(m1_logit_, data = lits, diffchange=c("range")) # min->max

### Marginal effects
maBina(m1_logit_)                 # marginal effects at the means of independent variables
maBina(m1_logit_, x.mean = FALSE) # marginal effects are calculated for each observation and then averaged

### Odds ratio
summary(m1_logit_)
odds.ratio(m1_logit_, level = 0.95, signif.stars = T)
