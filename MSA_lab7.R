### Multivariate statistical analysis, 2019 ###
### Instrumental variables

IV <- readRDS("MSA_lab7.Rda")

IV$afam <-as.numeric(IV$ethnicity=='afam')
IV$hispanic <-as.numeric(IV$ethnicity=='hispanic')
IV$male<-as.numeric(IV$gender=='male')
IV$mothereduc<-as.numeric(IV$fcollege=='yes')
IV$fathereduc<-as.numeric(IV$mcollege=='yes')
IV$urban<-as.numeric(IV$urban=='yes')

m <- lm(education ~ distance + mcollege + fcollege + afam + hispanic + male, data = na.omit(IV))
firststage <- m$fitted.values
m_1 <- lm(log(wage) ~ firststage + afam + hispanic + male, data = na.omit(IV))
summary(m_1)

install.packages("AER")
library("AER")
m1 <- ivreg(log(wage) ~ education + afam + hispanic + male | distance + afam + hispanic + male, data = na.omit(IV))
summary(m1, vcov = sandwich, diagnostics = TRUE)

m2 <- ivreg(log(wage) ~ education + afam + hispanic + male| afam + hispanic + male +  mothereduc + fathereduc + distance, data = na.omit(IV))
summary(m2, vcov = sandwich, diagnostics = TRUE)
