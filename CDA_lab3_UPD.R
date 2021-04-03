### Categorical data analysis, 2021
### Ordinal logit models (part 1): testing proportional odds assumption

install.packages("foreign")
install.packages("VGAM")
install.packages("MASS")
install.packages("glm.predict")
install.packages("erer")
install.packages("brant")
install.packages("memisc")

library(foreign)
library(VGAM)
library(MASS)
library(glm.predict)
library(erer)
library(brant)
library(memisc)

# The example data from S. Long (ordwarm.dta)
lab3 <- read.dta(file.choose())

# Brief description (the General Social Survey):
# warm - agreement with the statement "A working mother can establish just as warm and secure
# a relationship with her children as a mother who does not work"
# from SD - strongly disagree to SA - strongly agree (4 categories)

# yr89 - survey wave: 1 - 1989, 0 - 1977
# male - male: 1 - male, 0 - female
# white: 1 - white, 0 - nonwhite (race)
# age: in years
# ed: years of education
# prst: occupational prestige

# Make a dependent variable be an ordinal one
lab3$warm <- ordered(lab3$warm, levels=c('SD', 'D', 'A', 'SA'), labels=c('SD', 'D', 'A', 'SA'))

# Run an ordinal logistic regression (MASS package: polr)
m1_ologit <- polr(warm ~ yr89 + male + white + age + ed + prst, Hess = TRUE, 
               method="logistic", na.omit(lab3)) 
summary(m1_ologit)
exp(coef(m1_ologit))
exp(-coef(m1_ologit))

### Interpretation
# Discrete Change in predicted probabilities
dc(m1_ologit, values1 = c(0, 0, 0, mean(lab3$age), mean(lab3$ed), mean(lab3$prst)), 
   values2 = c(0, 0, 0, mean(lab3$age), mean(lab3$ed), mean(lab3$prst)))

dc(m1_ologit, values1 = c(1, 0, 0, mean(lab3$age), mean(lab3$ed), max(lab3$prst)), 
   values2 = c(1, 0, 0, mean(lab3$age), mean(lab3$ed), min(lab3$prst)))

# Marginal effects
ocME(m1_ologit) 
ocME(m1_ologit)$out # show whether the marginal effects are significant

# Standardized coefficients
coef <- m1_ologit$coefficients
coef
SDofX <- c(NA, NA, NA, sd(lab3$age), sd(lab3$ed), sd(lab3$prst))
SDofX
SDofLatentY <- sqrt(var(m1_ologit$lp) + ((pi^2)/3))
SDofLatentY
std_data <- data.frame(coef= coef, SDofX = SDofX, OR = exp(coef), SD_OR = exp(coef*SDofX),
                       coef_StdY = coef/SDofLatentY, coef_StdXY = (coef*SDofX)/SDofLatentY)
std_data

# the same model but with VGLM package 
m1_ologit_vglm <- vglm(warm ~ yr89 + male + white + age + ed + prst, 
               family=cumulative(parallel=T~yr89 + male + white + age + ed + prst - 1), data=na.omit(lab3))
# proportional odds model allows the intercept vary
summary(m1_ologit_vglm)
# odds: exp^(coef)

## Test whether the parallel regression assumption is true
# preliminary evidence: compare J-1 binary logit models' coefficients

lab3$warm1 <- ifelse(as.numeric(lab3$warm) > 1, 0, 1)
lab3$warm2 <- ifelse(as.numeric(lab3$warm) > 2, 0, 1)
lab3$warm3 <- ifelse(as.numeric(lab3$warm) > 3, 0, 1)

m1_logit <- glm(warm1 ~ yr89 + male + white + age + ed + prst, 
              family = binomial(link="logit"), data = lab3)
m2_logit <- glm(warm2 ~ yr89 + male + white + age + ed + prst, 
              family = binomial(link="logit"),  data = lab3)
m3_logit <- glm(warm3 ~ yr89 + male + white + age + ed + prst,  
              family = binomial(link="logit"),  data = lab3)

mtable(m1_logit, m2_logit, m3_logit)

# Brant test:
# If parallel regression assumption were not violated,
# all of these coefficients (except the intercepts) would be the same
# no statistical difference
brant(m1_ologit)

# Null hypothesis is rejected -> parallel regression assumption is violated
# Is this assumption violated for each of the independent variables?

## Choose the most appropriate model specification
# gologit without any restrictions: all the coefficients vary
gologit_unconstrained <- vglm(warm ~ yr89 + male + white + age + ed + prst, 
                  family=cumulative(parallel=F), data=na.omit(lab3))
summary(gologit_unconstrained)
# 1: compare 1 and 2, 3, 4 (2,3,4 as a reference category)
# 2: compare 1,2 and 3, 4 (3,4 as a reference category)
# 3: compare 1,2,3 and 4 (4 as a reference category)

# partial proportional odds model: only some X meet the parallel regression assumption
gologit_partial <- vglm(warm ~ yr89 + male + white + age + ed + prst, 
                  family=cumulative(parallel=T ~ white + ed + prst - 1), data=na.omit(lab3))
summary(gologit_partial)

# Compare the proportional odds model VS the partial proportional odds model
# Confirm that the proportional odds model is too restrictive
lrtest(m1_ologit_vglm, gologit_partial)

# Compare the partial proportional odds model VS unconstrained gologit
# Confirm that partial proportional odds model is not too restrictive
lrtest(gologit_partial, gologit_unconstrained)

# Make a final conclusion: choose the best model specification