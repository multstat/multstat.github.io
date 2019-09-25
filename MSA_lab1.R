### Multivariate statistical analysis, 2019 ###
### Lab 1. FE- and RE-models (part 1) ###

install.packages("plm")
install.packages("ggplot2")
install.packages("haven")
install.packages("dplyr")

library(plm)
library(ggplot2)
library(haven)
library(dplyr)

panel<-read_dta("MSA_lab1.dta")
head(panel) # nested data

attach(panel)

# run a least-squares dummy-variable model  
LSDV <- lm(fh_polity~state_capacity + country)
summary(LSDV)

# change a base (reference) category
countryfa1 <- relevel(factor(country), ref ="Russia")
LSDV_Rus <- lm(fh_polity~state_capacity + countryfa1)
summary(LSDV_Rus)

# save predicted values
ypredicted <- LSDV$fitted
# combine the main dataset and a new variable into a new dataset "panel1" 
panel1 <- data.frame(panel, ypredicted) 
head(panel1)

# visualize the results of modeling: the intercepts vary, but the slope is the same 
ggplot(panel, aes(x = state_capacity, y = ypredicted, color = country))+geom_smooth(method = lm)
# to change colours manually, use the following option: + scale_colour_manual(values=c())

# run a fixed-effects model (only intercepts are different). Within-group transformation
fe <- plm(fh_polity~state_capacity, data = panel, index=c("country", "period"), model="within")
summary(fe)

# do we need a fixed-effects model? 
# run a pooled model by using plm package
ols <- plm(fh_polity~state_capacity, data=panel, model="pooling")
summary(ols)

pFtest(fe, ols)

# run a random-effects model (only intercepts are different)
re <- plm(fh_polity~state_capacity, data=panel, index=c("country", "period"), model="random")
summary(re)

# Breush-Pagan test. Do we need a random-effects model?  
plmtest(ols, type=c("bp"))

# Hausman test: FE, RE-transformed VS RE original
phtest(fe, re)

### How to get a FE-model coefficient?
# For ease of understanding, the code is divided into steps .
# However, you can use use %>% to make the code more elegant.
#Step 1. Group (country) variances of X (state_capacity)
var <- summarize(group_by(panel, country), var(state_capacity))
var

#Step 2. Run separate regressions (for each country) 
a <- group_by(panel, country) %>%
  do(data.frame(beta = coef(lm(fh_polity ~ state_capacity, data = .))[2]))
a 

#Step 3. Weighted sum of coefficients from separate regression models. 
# Use conditional variances as weights
m <- as.data.frame(merge(a, var, by ="country"))
m
m$mult <- m$beta*(m$`var(state_capacity)`/sum(m$`var(state_capacity)`))
sum(m$mult)
