### Categorical Data Analysis
### Lab 2. February 9, 2021
### Binary response models. Part 2: testing goodness-of-fit, comparing models

### Packages
install.packages("haven")
install.packages("dplyr")
install.packages("ResourceSelection")
install.packages("caret")
install.packages("ROCR")
install.packages("InformationValue")
install.packages("pscl")
install.packages("broom")
install.packages("memisc")
install.packages("car")

### Libraries
options(warning = -1)
library(haven)
library(dplyr)
library(ResourceSelection)
library(caret)
library(ROCR)
library(InformationValue)
library(pscl)
library(broom)
library(memisc)
library(car)

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
lits$q228[(lits$q228==-97 | lits$q228==-99)] <- NA

table(lits$respondentage)
lits$respondentage[lits$respondentage==-1] <- NA
lits <- dplyr::rename(lits, ladder = q228, gender = Head_Of_, settlement = tablec)

lits <- na.omit(lits)

# run the logit model which is already familiar to you

m1_logit <- glm(measures ~ gender + respondentage + as.factor(settlement) + Children + ladder, data = lits, 
                family = binomial(link = "logit"),  x = TRUE)
summary(m1_logit)

### Measures of fit: Pseudo R2 McFadden
m0_logit <- glm(measures ~ 1, data = lits, family = binomial(link = "logit"))
1-logLik(m1_logit)/logLik(m0_logit)
pR2(m1_logit) 
# r2CU - Nagelkerke / Cragg & Uhler’s R2
# It adjusts Cox & Snell’s so that the range of possible values extends to 1.
# Then, if the full model perfectly predicts the outcome and has a likelihood of 1, Nagelkerke/Cragg & Uhler’s R-squared = 1.When L(Mfull) = 1, then R2 = 1; When L(Mfull) = L(Mintercept), then R2 = 0.

### Goodness-of-fit test using observed and predicted quantities
## Hosmer-Lemeshow goodness-of-fit test using # quantiles
### and display table of groups used for test
hl <- hoslem.test(m1_logit$y, fitted(m1_logit), g=10)
hl.groups <- data.frame(cbind(hl$observed,hl$expected))
hl.groups <- mutate(hl.groups, total = y0 + y1)
hl.groups
hl

### Baseline Accuracy 
data.frame(table(lits$measures))
max(data.frame(table(lits$measures))[,2])/sum(data.frame(table(lits$measures))[,2])

### Quality of classification. Confusion matrix
pred <- predict(m1_logit, lits, type='response')
plotROC(lits$measures, pred)

### Plot: Specificity and Sensitivity 
pred.inf <- prediction(pred, lits$measures)
plot(performance(pred.inf, measure="sens", x.measure = "cutoff"), col = "red", ann = F)
par(new=TRUE)
plot(performance(pred.inf, measure="spec", x.measure = "cutoff"), ann = F)
legend("bottom", inset = -1, legend=c("Sensitivity", "Specificity"),
       col=c("red", "black"), lty = 1:1, cex = 0.7, xpd = T)

### Threshold: minimize difference between Specification and Sensitivity 
sens <- performance(pred.inf,  measure="sens", x.measure="cutoff")
spec <- performance(pred.inf,  measure="spec", x.measure="cutoff")
mindiff <- which.min(abs(sens@y.values[[1]]-spec@y.values[[1]]))
sens@x.values[[1]][mindiff]

### Display classification table for all observations in the data
### Probabilities < intersection point are classified as 0
pred1 <- ifelse(predict(m1_logit, type = "response") < sens@x.values[[1]][mindiff], 0, 1)
pred1 <- as.factor(pred1)
lits$measuresf <- as.factor(lits$measures)
caret::confusionMatrix(pred1, lits$measuresf, positive= "1")

### Minimize misclassification error
cutoff <- optimalCutoff(lits$measures, pred) # by default minimizes the misclassification error
cutoff
misClassError(lits$measures, pred, threshold = cutoff)
pred2 <- ifelse(predict(m1_logit, type = "response") < cutoff, 0, 1)
pred2 <- as.factor(pred2)
caret::confusionMatrix(pred2, lits$measuresf, positive = "1")

### Maximize sensitivity 
cutoff <- optimalCutoff(lits$measures, pred, optimiseFor = "Ones")  # maximizes detection of "Ones"
cutoff
pred3 <- ifelse(predict(m1_logit, type = "response") < cutoff, 0, 1)
pred3 <- as.factor(pred3)
caret::confusionMatrix(pred3, lits$measuresf, positive = "1")

### Maximize specificity
cutoff <- optimalCutoff(lits$measures, pred, optimiseFor = "Zeros") # maximizes detection of "Zeros"
cutoff
pred4 <- ifelse(predict(m1_logit, type = "response") < cutoff, 0, 1)
pred4 <- as.factor(pred4)
caret::confusionMatrix(pred4, lits$measuresf, positive = "1")

### Maximize Youden's index
cutoff <- optimalCutoff(lits$measures, pred, optimiseFor = "Both") # Youden's index
cutoff
pred5 <- ifelse(predict(m1_logit, type = "response") < cutoff, 0, 1)
pred5 <- as.factor(pred5)
caret::confusionMatrix(pred5, lits$measuresf, positive = "1")

