### Multivariate statistical analysis, 2019 ###
### Mixed-effects models (part 3). Some useful stuff

install.packages("haven")
install.packages("psych")
install.packages("arm")
install.packages("multilevel")
install.packages("lattice")
install.packages("ggplot2")
install.packages("sjPlot")
install.packages("lmerTest")
install.packages("dplyr")
install.packages("margins")

library(haven)
library(psych)
library(arm)
library(multilevel)
library(lattice)
library(ggplot2)
library(sjPlot)
library(lmerTest)
library(dplyr)
library(margins)

# open MSA_lab6_1.dta 
data <- read_dta(file.choose())

# visualize what happens if we regress political regime on period by groups (different countries)
data$period <- data$period - 1
ggplot(data, aes(x=period, y=fh_polity, group = country))  + theme_classic()+
  geom_smooth(method=lm, se = TRUE, color = "black") + geom_point() + facet_wrap(~country)

model1 <- lmer(fh_polity ~ period + (1 + period|country), REML = FALSE, data)
summary(model1)

# plot predicted values (only fixed part)
plot_model(model1, type = "pred")

# save predicted values (fixed + random part)
pr_re <- predict(model1)
pr_re
# save predicted values (only fixed part)
pr_fe <- predict(model1, re.form = NA)
pr_fe

# plot predicted values with a random part (+BLUP)
data %>% 
  # add predicted values to the dataset
  mutate(pr_re = predict(model1), pr_fe = predict(model1, re.form = NA)) %>% 
  # visualize
  ggplot(aes(x=period, y=pr_re, group = country)) + theme_light() + geom_line() +
  geom_line(color = "red", aes(period, pr_fe)) + facet_wrap(~country)

# Do we need a quadratic term? Visualize the data
ggplot(data, aes(x = period, y = fh_polity)) + theme_light()+
  geom_line() + geom_point()+facet_wrap( ~ country)

# you can you loess as a smooth function, but we do not have enough observations. The example is as follows:
# ggplot(data, aes(x=period, y=fh_polity))  + theme_light()+
# geom_smooth(method=loess, se = TRUE, color = "black") + geom_point() + facet_wrap(~country)

# add fixed and random effects for the period and the squared period variables
model2 <- lmer(fh_polity ~ period + I(period^2) + (1 + period + I(period^2)|country), REML = FALSE, data)
# plot predicted values (only fixed part). You can extend the period for a smoother graph 
plot_model(model2, type = "pred", terms = "period [0:15]")

# plot predicted values with a random part (+BLUP)
data %>% 
  # add predicted values to the dataset
  mutate(pr_re2 = predict(model2), pr_fe2 = predict(model2, re.form = NA)) %>% 
  # visualize
  ggplot(aes(x=period, y=pr_re2, group = country)) + theme_light() + geom_line() +
  geom_line(color = "red", aes(period, pr_fe2)) + facet_wrap(~country)

# you can run this code without facet_wrap to plot all the lines at once. But this may result in an overloaded graph

# model heteroskedasticity
model3 <- lme(fh_polity ~ period + I(period^2), 
            random = ~1 + period + period^2|country, method = "ML", weights = varIdent(form= ~ 1|country), data)
summary(model3)
model3_1 <- lme(fh_polity ~ period + I(period^2), 
              random = ~1 + period + period^2|country, method = "ML", data)
summary(model3_1)
anova(model3, model3_1)

# add the country-level observations to the individual-level data 
country_level <- read.csv('MSA_lab6_2.csv', sep = ";")
head(country_level)
merged <- merge(data, country_level, id=c('country'))

# interaction
# we skipped a random effect for the squared period to provide a more parsimonious model
m_inter <- lmer(fh_polity ~ period + I(period^2) + state_capacity + priv_binary + state_capacity*priv_binary + (1 + period + state_capacity|country), REML = FALSE, data = merged)
summary(m_inter)
# Do not forget to use marginal effects to interpret interaction terms
margins(m_inter, at = list(priv_binary = 0:1))

# 2 types of centering
# grand-mean centering
m_inter_1 <- lmer(fh_polity ~ period + I(period^2) + I(state_capacity-mean(state_capacity)) + priv_binary + I(state_capacity-mean(state_capacity))*priv_binary + (1 + period + I(state_capacity-mean(state_capacity))|country), REML = FALSE, data = merged)
summary(m_inter_1)
# group-mean centering 
m_inter_2 <- lmer(fh_polity ~ period + I(period^2) + I(state_capacity-ave(state_capacity, country)) + priv_binary + I(state_capacity-ave(state_capacity, country))*priv_binary + (1 + period + state_capacity|country), REML = FALSE, data = merged)
summary(m_inter_2)
tab_model(m_inter, m_inter_1, m_inter_2)
