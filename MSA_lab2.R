### Multivariate statistical analysis, 2019 ###
### Lab 2. FE-models and RE-models (part 2) ###

install.packages("haven")
install.packages("plm")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lmtest")
install.packages("sandwich")

library(haven)
library(plm)
library(ggplot2)
library(dplyr)
library(lmtest)
library(sandwich)

panel<-read_dta("MSA_lab2.dta")
head(panel) 

# Only country effects
LSDV <- lm(fh_polity~state_capacity + country, data = panel)
summary(LSDV)
# The same model using plm. Country effects
fe <- plm(fh_polity~state_capacity, data = panel, index=c("country", "period"), effect = "individual", model="within")
summary(fe)
# extract country effects
summary(fixef(fe))

# Only time effects
LSDV_time <- lm(fh_polity ~ state_capacity + as.factor(period), data = panel)
summary(LSDV_time)
fe_time <- plm(fh_polity ~ state_capacity, data = panel, index=c("country", "period"), effect = "time", model = "within")
summary(fe_time)
# extract time effects
summary(fixef(fe_time))

# Both country and time effects
fe_twoways <- plm(fh_polity ~ state_capacity, data = panel, index=c("country", "period"), effect = "twoways", model = "within")
summary(fe_twoways)

# Compare more and less parsimonious models (without and with time effects)
pFtest(fe_twoways, fe)

# Heteroskedasticity adjustment 
bptest(fe_twoways, studentize = F)
coeftest(fe_twoways, vcov = vcovHC, type = "HC3")

### Test whether the model fits well
LSDV_twoways <- lm(fh_polity ~ state_capacity + country + as.factor(period), data = panel)
y_pred <- LSDV_twoways$fitted 
panel1 <- data.frame(panel, y_pred) 
# subset the data by countries and find how strongly y observed and predicted are correlated
merged <- panel1 %>% group_by(country)%>% summarize(., cor(fh_polity, y_pred))%>% merge(panel1, ., by="country")
# test whether results are robust to excluding observations with small correlations
merged$new <- ifelse(abs(merged$`cor(fh_polity, y_pred)`)<0.3,1,0)
fe_twoways_2 <- plm(fh_polity ~ state_capacity, merged[merged$new == 0,], index=c("country", "period"), effect = "twoways")
coeftest(fe_twoways_2, vcov = vcovHC, type = "HC3")

### test a FE-model with varying slopes
feslope <- lm(fh_polity~state_capacity*country + country, data = panel)
summary(feslope)
feslope_pred <- feslope$fitted
# plot varying slopes 
colors <- colors(distinct = TRUE)
mypalette <- sample(colors, 27)
ggplot(panel, aes(x = state_capacity, y = feslope_pred, color = country))+ geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = mypalette)
