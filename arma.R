library(tidyverse)
library(forecast)
library(lmtest)
library(tseries)
library(ggplot2)
library(stargazer)
library(gridExtra)
library(weakARMA)
library(FinTS)

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

model_1 <- arima(ts_diff, order = c(1,0,0))
model_2 <- arima(ts_diff, order = c(2, 0, 0))
model_3 <- arima(ts_diff, order = c(1, 0, 1))
model_4 <- arima(ts_diff, order = c(1, 0, 2))
model_5 <- arima(ts_diff, order = c(2, 0, 1))
model_6 <- arima(ts_diff, order = c(2, 0, 2))

stargazer(model_1, model_2, model_3, model_4, model_5, model_6, type = 'text')

model_1_css_ml <- arima(ts_diff, order = c(1,0,0), method = 'CSS-ML')
model_1_ml <- arima(ts_diff, order = c(1,0,0), method = 'ML')
model_2_css_ml <- arima(ts_diff, order = c(2,0,2), method = 'CSS-ML')
model_2_ml <- arima(ts_diff, order = c(2,0,2), method = 'ML')

stargazer(model_1_css_ml, model_1_ml, model_2_css_ml, model_2_ml, type = 'text', add.lines = list(c('Method', "CSS-ML", 'ML', 'CSS-ML', 'ML')), model.names = TRUE)

checkresiduals(model_1)
checkresiduals(model_6)

grid.arrange(ggAcf(model_1$residuals), ggPacf(model_1$residuals))
grid.arrange(ggAcf(model_6$residuals), ggPacf(model_6$residuals))

ArchTest(model_1$residuals)
ArchTest(model_6$residuals)

shapiro.test(model_1$residuals)
shapiro.test(model_6$residuals) 

