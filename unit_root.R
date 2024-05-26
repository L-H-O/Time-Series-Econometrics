library(tidyverse)
library(CADFtest)
library(tseries)
library(weakARMA)
library(lmtest)
library(readxl)
library(stargazer)
library(car)
library(aTSA)

# Extração e seleção de ordens

df <- read_excel("Base.xlsx") 

rel_d <- df  %>% select(imr, pol_violence, gdp_pc) 

imr <- ts(rel_d$imr, start = 1961, end = 2018)
pol_violence <- ts(rel_d$pol_violence, start = 1961, end = 2018)
gdp_pc <- ts(rel_d$gdp_pc, start = 1961, end = 2018)

ci_imr <- imr %>% ARMA.selec(P = 3, Q = 3)

ci_pol_violence <-  pol_violence %>% ARMA.selec(P = 3, Q = 3)

ci_gdp_pc <-  gdp_pc %>% ARMA.selec(P = 3, Q = 3)

ci_imr$AIC # AIC IMR -> AR(1)
ci_imr$BIC # BIC IMR -> AR(1)
ci_imr$HQ # HQ IMR -> AR(1)

imr_ar1 <-  imr %>% arima(order = c(1, 0, 0))

ci_pol_violence$AIC # AIC Pol. Violence -> ARMA(2,1)
ci_pol_violence$BIC # BIC Pol. Violence -> AR(1)
ci_pol_violence$HQ  # HQ Pol. Violence -> AR(1)

pv_arma21 <-  pol_violence %>%
              arima(order = c(2, 0, 1), method = 'ML')

pv_ar1 <- pol_violence %>%
          arima(order = c(1,0,0))

ci_gdp_pc$AIC # AIC GDP Pc -> AR(1)
ci_gdp_pc$BIC # BIC GDP Pc -> AR(1)
ci_gdp_pc$HQ # HQ GDP Pc -> AR(1)

gdp_ar1 <- gdp_pc %>% arima(order = c(1, 0, 0))


stargazer(imr_ar1, pv_arma21, pv_ar1, gdp_ar1,
          type = 'text',
          column.labels = c('AR(2)', 'ARMA(2,1)', 'AR(1)', 'AR(1)'),
          align = TRUE,
          dep.var.labels = c('IMR - Pol. Violence - Pol. Violence - GDP pc.'),
          add.lines = list(c('Method', 'CSS-ML', 'ML', 'CSS-ML', 'CSS-ML'))
)

# Testes

CADFtest(imr, type = "drift", max.lagy.y = ceiling(12*length(imr)/100^(1/4)),
         criterion = 'MAIC') # Não rejeitamos H0 de não-estacionariedade - IMR

CADFtest(pol_violence, type = "drift", max.lagy.y = ceiling(12*length(pol_violence)/100^(1/4)),
         criterion = 'MAIC')  # Não rejeitamos H0 de não-estacionariedade - Pol. Violence

CADFtest(gdp_pc, type = "trend", max.lagy.y = ceiling(12*length(gdp_pc)/100^(1/4)),
         criterion = 'MAIC')  # Não rejeitamos H0 de não-estacionariedade - GDP Pc.

# Algoritmo de Enders para IMR


ttest_imr <- CADFtest(imr,
                      type = 'trend',
                      max.lagy.y = ceiling(12*length(imr)/100^(1/4)),
                      criterion = 'MAIC')

print(ttest_imr)                      

#O teste t NÃO rejeita a hipótese nula de uma raiz unitária, a 5% (p-valor é de 0.8026 > 0.05)
#Vamos conduzir o teste F para detectar se o modelo de fato apresenta tendência linear

print(ttest_imr$est.model)
linearHypothesis(ttest_imr$est.model,c("trnd = 0", "L(y, 1) = 0" ))

#A estatística F é de 8.2046. Para uma amostra n = 50, temos um valor crítico de
#7.81 ao nível de significância de 2.5%, como 8.2046 > 7.81, rejeitamos a hipótese 
#nula de que a tendência e o coeficiente associado a y_{t-1} são ambos zero. 
#Como temos componentes determinísticos e/ou não há raiz unitária.
#Assim, estimamos um teste t padrão ao componente de tendência

trend_imr <-  1:(length(imr))
model_trend <-  lm(imr~trend_imr)

coeftest(model_trend, vcov. = vcovHAC)

# Não rejeitamos a hipótese nula de componente determinístico!
# Realizamos, portanto, um teste t convencional ao modelo completo

conv_imr <- lm(imr ~ stats::lag(imr, 1)) 

coeftest(conv_imr, vcov. = vcovHAC)

### Testes alternativos

# ADF-GLS (Elliot, Rothemberg e Stock)

ur.ers(imr, model = "constant") %>% summary()
ur.ers(imr, model = "trend") %>% summary()

ur.ers(pol_violence, model = "trend") %>% summary()
ur.ers(pol_violence, model = "constant") %>% summary()

ur.ers(gdp_pc, model = "trend") %>% summary()
ur.ers(gdp_pc, model = "constant") %>% summary()


# PP

aTSA::pp.test(imr, "Z_tau") %>% summary()

aTSA::pp.test(pol_violence, "Z_tau") %>% summary()

aTSA::pp.test(gdp_pc, "Z_tau") %>% summary()

# KPSS

aTSA::kpss.test(imr) %>% summary()

aTSA::kpss.test(pol_violence) %>% summary()

aTSA::kpss.test(gdp_pc) %>% summary()

## Testes às séries transformadas


CADFtest(imr %>% diff %>% diff, type = "trend", max.lagy.y = ceiling(12*length(imr)/100^(1/4)) - 2,
         criterion = 'MAIC') 

imr %>% diff %>% diff %>% aTSA::kpss.test() %>% summary

CADFtest(pol_violence %>%  diff, type = "trend", max.lagy.y = ceiling(12*length(pol_violence)/100^(1/4)) - 1,
         criterion = 'MAIC') 

pol_violence %>% diff %>% aTSA::kpss.test() %>% summary

CADFtest(gdp_pc %>% diff, type = "none", max.lagy.y = ceiling(12*length(gdp_pc)/100^(1/4)) -1,
         criterion = 'MAIC') 

gdp_pc %>% diff %>%  aTSA::kpss.test() %>% summary


