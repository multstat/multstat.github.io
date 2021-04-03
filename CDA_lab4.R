### Categorical data analysis, 2021
### Ordinal logit models (part 2): interpretation, visualization and testing alternative models
### Take the data from the paper by Joan Esteban, Laura Mayoral, Debraj Ray 
### "Ethnicity and Conflict: an Empirical Study"
### American Economic Review 2012, 102(4): 1310–1342

install.packages("foreign")
install.packages("psych")
install.packages("MASS")
install.packages("VGAM")
install.packages("ordinal")
install.packages("brant")
install.packages("lmtest")
install.packages("generalhoslem")
install.packages("pscl")
install.packages("erer")
install.packages("glm.predict")
install.packages("aod")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("memisc")

library(foreign)
library(psych)
library(MASS)
library(VGAM)
library(ordinal)
library(brant)
library(lmtest)
library(generalhoslem)
library(pscl)
library(erer)
library(glm.predict)
library(aod)
library(ggplot2)
library(dplyr)
library(memisc)

# Use CDA_lab4.dta

lab4 <- read.dta(file.choose())
lab4 <- dplyr::select(lab4, prioInt, p, f, gdp, pop, mount, ncont, prioIntLag)

### Brief description

# prioInt (dependent variable) - conflict intensity indicator from PRIO
# "peace" is assigned a value of 0, 
# events satisfying prio25 that are not prio1000 are assigned a value of 1,
# events recorded as prio1000 are assigned a value of 2.

### Make a dependent variable be an ordinal one
lab4$prioInt <- ordered(lab4$prioInt, levels = c(0,1,2), labels = c("peace", "weak", "strong"))

# for more details see https://www.econ.nyu.edu/user/debraj/Papers/EstebanMayoralRayAER.pdf 
# p -  Polarization (Ethnologue)
# f - Fractionalization (Ethnologue; Fearon (2003))
# gdp - Log of real GDP per capita corresponding to the first year of each five-year period (Maddison (2008))
# pop - Log of population in the first year of each five-year period (Maddison (2008))
# mount - Percent mountainous terrain (Fearon and Laitin (2003), who use the codings of geographer A. J. Gerard)
# ncont - Noncontiguous states (Fearon and Laitin (2003))
# prioIntLag - lagged dependent variable

lab4 <- na.omit(lab4)

describe(lab4)

### Frequency table with proportions
lab4 %>% 
  count(prioInt = factor(prioInt)) %>% 
  mutate(prop = prop.table(n))

### Plot distribution of the Conflict Intensity
lab4 %>% 
  count(prioInt = factor(prioInt)) %>% 
  mutate(prop = prop.table(n)) %>% 
  ggplot(aes(x = prioInt, y = prop, label = scales::percent(prop))) + 
  geom_col() + 
  geom_text(vjust = - 0.125, size = 3) + scale_y_continuous(labels = scales::percent) +
  labs(y = 'Percent', x = 'Conflict Intensity')

### Create dummies of the lagged variable prioInt to include them in a model
lab4$prev_conf_1 <- ifelse(lab4$prioIntLag == 0, 1, 0)
lab4$prev_conf_2 <- ifelse(lab4$prioIntLag == 1, 1, 0)
lab4$prev_conf_3 <- ifelse(lab4$prioIntLag == 2, 1, 0)
head(lab4)

### Run an ordinal regression model (MASS package)
ologit.polr <- polr(prioInt ~ p + f + gdp + pop + mount + ncont + prev_conf_2 + prev_conf_3, Hess=TRUE,
                    data=lab4, method="logistic")
summary(ologit.polr)
exp(-coef(ologit.polr))

### goodness-of-fit test (Hosmer-Lemeshow)
logitgof(lab4$prioInt, fitted(ologit.polr), g = 5, ord = TRUE)

### Run the same model but use the package "ordinal"
ologit.clm <- clm(prioInt ~ p + f + gdp + pop + mount + ncont + prev_conf_2 + prev_conf_3, data = lab4, link = "logit")
summary(ologit.clm)

### Test whether the coefficients are significant
### Wald test
wald.test(b = coef(ologit.clm), Sigma = vcov(ologit.clm), Terms = 3) # polarization
wald.test(b = coef(ologit.clm), Sigma = vcov(ologit.clm), Terms = 4) # fractionalization
wald.test(b = coef(ologit.clm), Sigma = vcov(ologit.clm), Terms = 9:10) # lagged variables

### Test nested models
### LR test
ologit.clm.noncont <- clm(prioInt ~ p + f + gdp + pop + mount + prev_conf_2 + prev_conf_3, data = lab4, link = "logit")
lrtest(ologit.clm, ologit.clm.noncont)

### Test non-nested and nested models
BIC(ologit.clm.noncont)
BIC(ologit.clm)

### Predicted Probabilities
### Calculate PPs
pred <- round(predict(ologit.polr, lab4, type = "prob"), 3)
head(pred)

### Get PPs descriptive statistics
summary(pred)

### Plot predicted probabilities for each category of conflict intensity
pred.l <- list(peace = pred[,1], weak = pred[,2], strong = pred[,3])
head(pred.l)
pred.stack <- stack(pred.l)
ggplot(pred.stack, aes(x=values)) + 
  geom_density(aes(group=ind, colour=ind, fill=ind), alpha=0.3) +
  theme_bw() + labs(x='Predicted Probabilities')

###  Predicted Probabilities: Graphing
# fix X:
pr.data.f <- data.frame(f=seq(0, 1, length.out = 50),
                        p=mean(lab4$p),  
                        gdp = mean(lab4$gdp),
                        pop=mean(lab4$pop), 
                        mount=mean(lab4$mount),
                        ncont=1, 
                        prev_conf_2 = 1,
                        prev_conf_3 = 0)

pr.data.f

pr.f <- predict(ologit.clm, pr.data.f, type="prob")
head(pr.f)
pr.cum.f <- predict(ologit.clm, pr.data.f, type="cum.prob")
head(pr.cum.f$cprob1)

pr.data.f <- cbind(pr.data.f, pr.f)
pr.data.f
colnames(pr.data.f)[9:11] <- c("prob.peace", "prob.weak", "prob.strong" )

pr.data.f <- cbind(pr.data.f, pr.cum.f$cprob1)
colnames(pr.data.f)[12:14] <- c("cum.peace", "cum.weak", "cum.strong" )

# Predicted Probability of Conflict Intensity depending on Fractionalization
ggplot(pr.data.f, aes(f)) + 
  geom_line(aes(y = prob.peace, colour = "peace")) + 
  geom_line(aes(y = prob.weak, colour = "weak")) +
  geom_line(aes(y = prob.strong, colour = "strong")) + theme_bw() + 
  labs(x='Fractionalization', y = 'Predicted Probability of Conflict Intensity')

# Cumulative Probability depending on Fractionalization
ggplot(pr.data.f, aes(f)) + 
  geom_line(aes(y = cum.peace, colour = "peace")) + 
  geom_line(aes(y = cum.weak, colour = "weak")) +
  geom_line(aes(y = cum.strong, colour = "strong")) + theme_bw() + 
  labs(x='Fractionalization', y = 'Cumulative Probability')

### Brant Test of Parallel Regression Assumption
brant(ologit.polr)

### Confusion Matrix
pred_class <- predict(ologit.polr, lab4)
head(pred_class, 3)
caret::confusionMatrix(pred_class, lab4$prioInt)

### optional: ROC-curve
predictions <- predict(ologit.polr, lab4, type = 'prob')
head(predictions, 3)
install.packages("pROC")
### devtools::install_github("xrobin/pROC") 
library(pROC)
pROC::multiclass.roc(lab4$prioInt, predictions, plot = T)


