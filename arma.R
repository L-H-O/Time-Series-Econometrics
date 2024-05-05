library(tidyverse)
library(forecast)
library(lmtest)
library(tseries)
library(ggplot2)
library(stargazer)
library(gridExtra)
library(weakARMA)
library(FinTS)
library(tsibble)
library(readxl)

df <- read_excel("Base.xlsx")

ts_df <-  df %>%  
          filter(Year < '2018-01-01') %>% 
          select(-Year) %>%  
          ts(start = 1961, freq = 1)
ggplot(
  df %>% filter(Year < '2018-01-01') %>% select(c(Year, imr)),
  aes(x = Year, y = imr, color = 'Red')
) +
  geom_line() +
  labs(
      title = 'Infant Mortality Rate',
      subtitle = 'Deaths per 1000 births',
      x = 'Year',
      y = 'IMR'
  )

df %>% filter(Year < '2018-01-01') %>% 
       select(imr) %>%  
       ts(start = 1961, freq = 1) %>% 
       adf.test

grid.arrange(ggAcf(ts_df[, "imr"]), ggPacf(ts_df[, "imr"]))

ts_df[, "imr"] %>%
        diff() %>%
        ts.plot(xlab = 'Year', ylab = 'IMR', main = 'IMR - 1st Diff')

ts_df[,"imr"] %>% 
       diff() %>% 
       adf.test

ts_df[, "imr"] %>% 
        diff() %>% 
        diff() %>% 
        ts.plot(xlab = 'Year', ylab = 'IMR', main = 'IMR - 2nd Diff')

ts_df[,"imr"] %>% 
      diff() %>% 
      diff() %>% 
      adf.test

ts_diff <- df %>%
          filter(Year < '2018-01-01') %>% 
          select(imr) %>% 
          ts(start = 1961, freq = 1) %>% 
          diff %>% 
          diff

ts_diff %>% adf.test

grid.arrange(ggAcf(ts_diff), ggPacf(ts_diff))

ts_ic <-  ARMA.selec(ts_diff, P = 3, Q = 3)
ts_ic$AIC
ts_ic$BIC
ts_ic$HQ

aic_model <- arima(ts_diff, order = c(3,0,2), method = 'ML')
bic_model <- arima(ts_diff, order = c(0,0,2))

stargazer(aic_model, bic_model,
          type = 'text', 
          add.lines = list(c('Method', 'ML', 'CSS-ML')),
          column.labels = c('ARMA(3,2)', 'MA(2'),
          align = TRUE,
          dep.var.labels = c('IMR -2nd Diff'))
          

model_1 <- arima(ts_diff, order = c(1,0,0))
model_2 <- arima(ts_diff, order = c(2, 0, 0))
model_3 <- arima(ts_diff, order = c(1, 0, 1))
model_4 <- arima(ts_diff, order = c(1, 0, 2))
model_5 <- arima(ts_diff, order = c(2, 0, 1))
model_6 <- arima(ts_diff, order = c(2, 0, 2))

stargazer(model_1, model_2, model_3, model_4, model_5, model_6,
          type = 'text',
          column.labels = c('AR(1)', 'AR(2)', 'ARMA(1,1)', 'ARMA(1,2)', 'ARMA(2,1)', 'ARMA(2,2)'),
          align = TRUE,
          dep.var.labels = c('IMR -2nd Diff'),
          add.lines = list(c('Method', 'CSS-ML', 'CSS-ML', 'CSS-ML', 'CSS-ML', 'CSS-ML', 'CSS-ML'))
          )

model_1_css_ml <- arima(ts_diff, order = c(1,0,0), method = 'CSS-ML')
model_1_ml <- arima(ts_diff, order = c(1,0,0), method = 'ML')
model_2_css_ml <- arima(ts_diff, order = c(2,0,2), method = 'CSS-ML')
model_2_ml <- arima(ts_diff, order = c(2,0,2), method = 'ML')

stargazer(model_1_css_ml, model_1_ml, model_2_css_ml, model_2_ml,
          type = 'text',
          align = TRUE,
          dep.var.labels = c('IMR -2nd Diff'),
          column.labels = c('AR(1)', 'AR(1)', 'ARMA(2,2)', 'ARMA(2,2)'),
          add.lines = list(c('Method', "CSS-ML", 'ML', 'CSS-ML', 'ML'))
          )

checkresiduals(model_1) # Não REJ H0 -> não há autocorr nos res
checkresiduals(model_6) # Não REJ H0 -> não há autocorr nos res

grid.arrange(ggAcf(model_1$residuals), ggPacf(model_1$residuals))
grid.arrange(ggAcf(model_6$residuals), ggPacf(model_6$residuals))

qqnorm(residuals(model_1), main = 'AR(1,0) Residuals')
qqline(residuals(model_1))

qqnorm(residuals(model_6), main = 'ARMA(2,2) Residuals')
qqline(residuals(model_6))

ArchTest(model_1$residuals) # Rej H0 -> Heterocedasticidade condicional presente
ArchTest(model_6$residuals) # Rej H0 -> Heterocedasticidade condicional presente

shapiro.test(model_1$residuals) # Rej H0 -> Não Normalidade
shapiro.test(model_6$residuals) # Rej H0 -> Não Normalidade

jarque.bera.test(residuals(model_1)) # Rej H0 -> Não Normalidade
jarque.bera.test(residuals(model_6)) # Rej H0 -> Não Normalidade

lagged_ts <- df %>% 
             filter(Year < '2018-01-01') %>% 
             select(Year, imr) %>% 
             mutate(imr_lag = imr %>% lag,
                    imr_lag2 = imr_lag %>% lag,
                    d2016 = if_else(Year == year('2016-01-01'), 1, 0),
                    d2017 = if_else(Year == year('2017-01-01'), 1, 0)) 

model_lag <-  lm('imr ~ imr_lag', data = lagged_ts)
summary(model_lag)

resettest(model_lag, type = 'fitted')

plot(residuals(model_1), main = 'Residuals - AR(1,0)')

ts_diff2 <- ts_diff %>% 
            as_tsibble() %>% 
            mutate(d2018 = if_else(index == 2018, 1, 0),
                   d2017 = if_else(index == 2017, 1, 0),
                   d2016 = if_else(index == 2016, 1, 0),
                   log_value = log(value))

a <- arima(ts_diff2$value, order = c(1,0,0), xreg = ts_diff2 %>% select(d2016))
c <- arima(ts_diff2$value, order = c(1,0,0), xreg = ts_diff2 %>% select(d2017))
e <- arima(ts_diff2$value, order = c(1,0,0), xreg = ts_diff2 %>% select(d2016, d2017), method = 'ML')
b <- arima(ts_diff2$value, order = c(2,0,2), xreg = ts_diff2 %>% select(d2016))
d <- arima(ts_diff2$value, order = c(2,0,2), xreg = ts_diff2 %>% select(d2017))
f <- arima(ts_diff2$value, order = c(2,0,2), xreg = ts_diff2 %>% select(d2016, d2017))

stargazer(a,c,e,b,d,f,
          type = 'text',
          column.labels = c('AR(1)', 'AR(1)', 'AR(1)', 'ARMA(2,2)', 'ARMA(2,2)', 'ARMA(2,2)'),
          align = TRUE,
          dep.var.labels = c('IMR -2nd Diff'),
          add.lines = list(c('Method', 'CSS-ML', 'CSS-ML', 'ML', 'CSS-ML', 'CSS-ML', 'CSS-ML'))
          )

stargazer(model_6, b,d,f,
          type = 'text',
          column.labels = c( 'ARMA(2,2)', 'ARMA(2,2)', 'ARMA(2,2)', 'ARMA(2,2)'),
          align = TRUE,
          dep.var.labels = c('IMR -2nd Diff'),
          add.lines = list(c('Method','CSS-ML' , 'CSS-ML', 'CSS-ML', 'CSS-ML'))
)

model_6$residual %>% shapiro.test # ARMA(2.2)
b$residuals %>% shapiro.test # ARMA(2.2) com dummy 2016
d$residuals %>% shapiro.test # ARMA(2.2) com dummy 2017
f$residuals %>% shapiro.test # ARMA(2.2) com dummy 2016 e 2017

model_6$residual %>% jarque.bera.test # ARMA(2.2)
b$residuals %>% jarque.bera.test # ARMA(2.2) com dummy 2016
d$residuals %>% jarque.bera.test # ARMA(2.2) com dummy 2017
f$residuals %>% jarque.bera.test # ARMA(2.2) com dummy 2016 e 2017

model_6$residual %>% ArchTest # ARMA(2.2)
b$residuals %>% ArchTest # ARMA(2.2) com dummy 2016
d$residuals %>% ArchTest # ARMA(2.2) com dummy 2017
f$residuals %>% ArchTest # ARMA(2.2) com dummy 2016 e 2017

checkresiduals(b) # ARMA(2.2) com dummy 2016
checkresiduals(d) # ARMA(2.2) com dummy 2017
checkresiduals(f) # ARMA(2.2) com dummy 2016 e 2017

x <- arima(ts_diff2$value, order = c(2,0,0), xreg = ts_diff2 %>% select(d2017))
y <- arima(ts_diff2$value, order = c(2,0,0), xreg = ts_diff2 %>% select(d2016, d2017))

stargazer(x, y,
          type = 'text',
          column.labels = c('AR(2)', 'AR(2)'),
          align = TRUE,
          dep.var.labels = c('IMR -2nd Diff'),
          add.lines = list(c('Method', 'CSS-ML', 'CSS-ML'))
)

checkresiduals(x)
checkresiduals(y)

model_6$residual %>% shapiro.test # ARMA(2.2)
x$residuals %>% shapiro.test # AR(2) com dummy 2016
y$residuals %>% shapiro.test # AR(2) com dummy 2017

model_6$residual %>% jarque.bera.test # ARMA(2.2)
x$residuals %>% jarque.bera.test # AR(2) com dummy 2016
y$residuals %>% jarque.bera.test # AR(2) com dummy 2017

model_6$residual %>% ArchTest # ARMA(2.2)
x$residuals %>% ArchTest # AR(2) com dummy 2016
y$residuals %>% ArchTest # AR(2) com dummy 2017

lagged_ar <- ts_diff2 %>% mutate(imr_lag = value %>% lag,
                                 imr_lag2 = imr_lag %>% lag)

lag_x <-  lm('value ~ imr_lag + imr_lag2 + d2016', data = lagged_ar)
summary(lag_x)
resettest(lag_x, type = 'fitted')

lag_y <-  lm('value ~ imr_lag + imr_lag2 + d2017', data = lagged_ar)
summary(lag_y)
resettest(lag_y, type = 'fitted')


               

