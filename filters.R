library(tidyverse)
library(readxl)
library(forecast)
library(hpfilter)
library(CADFtest)
library(tseries)
library(neverhpfilter)


df <- read_excel("Base.xlsx") %>% 
      select(Year, imr, pol_violence, gdp_pc) %>% 
      filter(Year < '2018-01-01')

imr <- ts(df$imr, start = 1961, end = 2018)
pol_violence <- ts(df$pol_violence, start = 1961, end = 2018)
gdp_pc <- ts(df$gdp_pc, start = 1961, end = 2018)

## Mesmas séries da atividade anterior, sabemos que são não-estacionárias
## em nível.

## Filtros two-sided e one-sided

y <- data.frame(imr, pol_violence, gdp_pc)

gtrend <- hp2(y, lambda = 100)
gtrend_one <- hp1(y, lambda = 100)

ccycle <- y - gtrend
ccycle_one <- y - gtrend_one

# Gráfico One-sided

plot(y$imr, xlab = "",  # Nome eixo x
     las = 2,        # Rotação label X
     ylab = "",
     main = "",
     type = "l",     # Tipo linha
     lty = 1,        # Tipo da Linha
     lwd = 1,  # Espessura da linha
     col = "red" # cor
)       

op <- par(new = TRUE)

plot(gtrend_one$imr, xlab = "", ylab = "",
     type = "l",
     xaxt = "n", yaxt = "n",
     lty = 1)

title(main = "IMR - One sided HP Filter", line = 1.5)

title(main = "Cycle", line = 0.5,
      cex = 0.8,
      font.main = 1)

legend("top", col = c("black", "red"), lty = 1,
       legend=c("Trend", "IMR")
)


# Componente cíclico one-sided

plot(ccycle_one$imr, type = "l")
abline(h = 0)
title(main = "IMR - One sided cyclical component")

# Gráfico Two-sided

plot(y$imr, xlab = "",  # Nome eixo x
     las = 2,        # Rotação label X
     ylab = "",
     main = "",
     type = "l",     # Tipo linha
     lty = 1,        # Tipo da Linha
     lwd = 1,  # Espessura da linha
     col = "red" # cor
)       

op <- par(new = TRUE)

plot(gtrend$imr, xlab = "", ylab = "",
     type = "l",
     xaxt = "n", yaxt = "n",
     lty = 1)

title(main = "IMR - Two Sided HP Filter", line = 1.5)

title(main = "Cycle", line = 0.5,
      cex = 0.8,
      font.main = 1)

legend("top", col = c("black", "red"), lty = 1,
       legend=c("Trend", "IMR")
)

# Componente cíclico two_sided

plot(ccycle$imr, type = "l")
abline(h = 0)
title(main = "IMR - Two sided cyclical component")

# Componente de tendência de cada filtro

trend_imr <- ts(gtrend$imr, start = 1961, end = 2018)
trend_imr_one <- ts(gtrend_one$imr, start = 1961, end = 2018)

plot(trend_imr, type="l", col="black", lty=1)
lines(trend_imr_one, col="#066462")
title("IMR - HP Trends")
legend("bottom", horiz=TRUE, cex=0.75, c("TWO SIDED", "ONE SIDED"), lty = 1, col = c("black", "#066462"))

# Componente cíclico de cada filtro 

cycle_imr <- ts(ccycle$imr, start = 1961, end = 2018)
cycle_imr_one <- ts(ccycle_one$imr, start = 1961, end = 2018)

plot(cycle_imr, type="l", col="black", lty=1)
lines(cycle_imr_one, col="#066462")
title("IMR - HP Cycles")
legend("bottom", horiz=TRUE, cex=0.75, c("TWO SIDED", "ONE SIDED"), lty = 1, col = c("black", "#066462"))

## Pol. Violence

plot(y$pol_violence, xlab = "",  # Nome eixo x
     las = 2,        # Rotação label X
     ylab = "",
     main = "",
     type = "l",     # Tipo linha
     lty = 1,        # Tipo da Linha
     lwd = 1,  # Espessura da linha
     col = "red" # cor
)       

op <- par(new = TRUE)

plot(gtrend_one$pol_violence, xlab = "", ylab = "",
     type = "l",
     xaxt = "n", yaxt = "n",
     lty = 1)

title(main = "Pol. Violence - One sided HP Filter", line = 1.5)

title(main = "Cycle", line = 0.5,
      cex = 0.8,
      font.main = 1)

legend("top", col = c("black", "red"), lty = 1,
       legend=c("Trend", "Pol. Violence")
)

# Gráfico Two-sided

plot(y$pol_violence, xlab = "",  # Nome eixo x
     las = 2,        # Rotação label X
     ylab = "",
     main = "",
     type = "l",     # Tipo linha
     lty = 1,        # Tipo da Linha
     lwd = 1,  # Espessura da linha
     col = "red" # cor
)       

op <- par(new = TRUE)

plot(gtrend$pol_violence, xlab = "", ylab = "",
     type = "l",
     xaxt = "n", yaxt = "n",
     lty = 1)

title(main = "Pol. Violence - Two Sided HP Filter", line = 1.5)

title(main = "Cycle", line = 0.5,
      cex = 0.8,
      font.main = 1)

legend("top", col = c("black", "red"), lty = 1,
       legend=c("Trend", "Pol. Violence")
)

# Componente de tendência de cada filtro

trend_pol_violence <- ts(gtrend$pol_violence, start = 1961, end = 2018)
trend_pol_violence_one <- ts(gtrend_one$pol_violence, start = 1961, end = 2018)

plot(trend_pol_violence, type="l", col="black", lty=1)
lines(trend_pol_violence_one, col="#066462")
title("Pol. Violence - HP Trends")
legend("bottom", horiz=TRUE, cex=0.75, c("TWO SIDED", "ONE SIDED"), lty = 1, col = c("black", "#066462"))

# Componente cíclico de cada filtro 

cycle_pol_violence <- ts(ccycle$pol_violence, start = 1961, end = 2018)
cycle_pol_violence_one <- ts(ccycle_one$pol_violence, start = 1961, end = 2018)

plot(cycle_pol_violence, type="l", col="black", lty=1)
lines(cycle_pol_violence_one, col="#066462")
title("Pol. Violence - HP Cycles")
legend("bottom", horiz=TRUE, cex=0.75, c("TWO SIDED", "ONE SIDED"), lty = 1, col = c("black", "#066462"))

## GDP Pc.

plot(y$gdp_pc, xlab = "",  # Nome eixo x
     las = 2,        # Rotação label X
     ylab = "",
     main = "",
     type = "l",     # Tipo linha
     lty = 1,        # Tipo da Linha
     lwd = 1,  # Espessura da linha
     col = "red" # cor
)       

op <- par(new = TRUE)

plot(gtrend_one$gdp_pc, xlab = "", ylab = "",
     type = "l",
     xaxt = "n", yaxt = "n",
     lty = 1)

title(main = "GDP. Pc. - One sided HP Filter", line = 1.5)

title(main = "Cycle", line = 0.5,
      cex = 0.8,
      font.main = 1)

legend("top", col = c("black", "red"), lty = 1,
       legend=c("Trend", "GDP. Pc.")
)

# Gráfico Two-sided

plot(y$gdp_pc, xlab = "",  # Nome eixo x
     las = 2,        # Rotação label X
     ylab = "",
     main = "",
     type = "l",     # Tipo linha
     lty = 1,        # Tipo da Linha
     lwd = 1,  # Espessura da linha
     col = "red" # cor
)       

op <- par(new = TRUE)

plot(gtrend$gdp_pc, xlab = "", ylab = "",
     type = "l",
     xaxt = "n", yaxt = "n",
     lty = 1)

title(main = "GDP. Pc. - Two Sided HP Filter", line = 1.5)

title(main = "Cycle", line = 0.5,
      cex = 0.8,
      font.main = 1)

legend("top", col = c("black", "red"), lty = 1,
       legend=c("Trend", "GDP. Pc.")
)

# Componente de tendência de cada filtro

trend_gdp_pc <- ts(gtrend$gdp_pc, start = 1961, end = 2018)
trend_gdp_pc_one <- ts(gtrend_one$gdp_pc, start = 1961, end = 2018)

plot(trend_gdp_pc, type="l", col="black", lty=1)
lines(trend_gdp_pc_one, col="#066462")
title("GDP. Pc. - HP Trends")
legend("bottom", horiz=TRUE, cex=0.75, c("TWO SIDED", "ONE SIDED"), lty = 1, col = c("black", "#066462"))

# Componente cíclico de cada filtro 

cycle_gdp_pc <- ts(ccycle$gdp_pc, start = 1961, end = 2018)
cycle_gdp_pc_one <- ts(ccycle_one$gdp_pc, start = 1961, end = 2018)

plot(cycle_gdp_pc, type="l", col="black", lty=1)
lines(cycle_gdp_pc_one, col="#066462")
title("GDP. Pc - HP Cycles")
legend("bottom", horiz=TRUE, cex=0.75, c("TWO SIDED", "ONE SIDED"), lty = 1, col = c("black", "#066462"))

# Previsões

imr_ar1 <- arima(y$imr, c(1,2,1))
pol_ar1 <- arima(y$pol_violence, c(1,1,0))
gdp_ar1 <- Arima(y$gdp_pc, c(1,1,0), include.drift = TRUE)

# Séries acrescidas

imr_plus <- ts(c(y$imr, forecast(imr_ar1)$mean), start = 1961, end = 2028)
pol_violence_plus <- ts(c(y$pol, forecast(pol_ar1)$mean), start = 1961, end = 2028)
gdp_pc_plus <- ts(c(y$gdp_pc, forecast(gdp_ar1)$mean), start = 1961, end = 2028)

y_plus <- data.frame(imr_plus, pol_violence_plus, gdp_pc_plus)

gtrend_plus <- hp2(y_plus, lambda = 100)
gtrend_plus_one <- hp1(y_plus, lambda = 100)

ccycle_plus <- y_plus - gtrend_plus
ccycle_plus_one <- y_plus - gtrend_plus_one

## IMR Plus

plot(y_plus$imr_plus, xlab = "",  # Nome eixo x
     las = 2,        # Rotação label X
     ylab = "",
     main = "",
     type = "l",     # Tipo linha
     lty = 1,        # Tipo da Linha
     lwd = 1,  # Espessura da linha
     col = "red" # cor
)       

op <- par(new = TRUE)

plot(gtrend_plus_one$imr_plus, xlab = "", ylab = "",
     type = "l",
     xaxt = "n", yaxt = "n",
     lty = 1)

title(main = "IMR - One sided HP Filter", line = 1.5)

title(main = "Cycle", line = 0.5,
      cex = 0.8,
      font.main = 1)

legend("top", col = c("black", "red"), lty = 1,
       legend=c("Trend", "IMR")
)

# Gráfico Two-sided

plot(y_plus$imr_plus, xlab = "",  # Nome eixo x
     las = 2,        # Rotação label X
     ylab = "",
     main = "",
     type = "l",     # Tipo linha
     lty = 1,        # Tipo da Linha
     lwd = 1,  # Espessura da linha
     col = "red" # cor
)       

op <- par(new = TRUE)

plot(gtrend_plus$imr_plus, xlab = "", ylab = "",
     type = "l",
     xaxt = "n", yaxt = "n",
     lty = 1)

title(main = "IMR - Two Sided HP Filter", line = 1.5)

title(main = "Cycle", line = 0.5,
      cex = 0.8,
      font.main = 1)

legend("top", col = c("black", "red"), lty = 1,
       legend=c("Trend", "IMR")
)

# Componente de tendência de cada filtro

trend_imr_plus <- ts(gtrend_plus$imr_plus, start = 1961, end = 2028)
trend_imr_plus_one <- ts(gtrend_plus_one$imr_plus, start = 1961, end = 2028)

plot(trend_imr_plus, type="l", col="black", lty=1)
lines(trend_imr_plus_one, col="#066462")
title("IMR - HP Trends")
legend("bottom", horiz=TRUE, cex=0.75, c("TWO SIDED", "ONE SIDED"), lty = 1, col = c("black", "#066462"))

# Componente cíclico de cada filtro 

cycle_imr_plus <- ts(ccycle_plus$imr_plus, start = 1961, end = 2028)
cycle_imr_plus_one <- ts(ccycle_plus_one$imr_plus, start = 1961, end = 2028)

plot(cycle_imr_plus, type="l", col="black", lty=1)
lines(cycle_imr_plus_one, col="#066462")
title("IMR - HP Cycles")
legend("bottom", horiz=TRUE, cex=0.75, c("TWO SIDED", "ONE SIDED"), lty = 1, col = c("black", "#066462"))

## Pol Violence Plus

plot(y_plus$pol_violence_plus, xlab = "",  # Nome eixo x
     las = 2,        # Rotação label X
     ylab = "",
     main = "",
     type = "l",     # Tipo linha
     lty = 1,        # Tipo da Linha
     lwd = 1,  # Espessura da linha
     col = "red" # cor
)       

op <- par(new = TRUE)

plot(gtrend_plus_one$pol_violence_plus, xlab = "", ylab = "",
     type = "l",
     xaxt = "n", yaxt = "n",
     lty = 1)

title(main = "Pol. Violence - One sided HP Filter", line = 1.5)

title(main = "Cycle", line = 0.5,
      cex = 0.8,
      font.main = 1)

legend("top", col = c("black", "red"), lty = 1,
       legend=c("Trend", "Pol. Violence")
)

# Gráfico Two-sided

plot(y_plus$pol_violence_plus, xlab = "",  # Nome eixo x
     las = 2,        # Rotação label X
     ylab = "",
     main = "",
     type = "l",     # Tipo linha
     lty = 1,        # Tipo da Linha
     lwd = 1,  # Espessura da linha
     col = "red" # cor
)       

op <- par(new = TRUE)

plot(gtrend_plus$pol_violence_plus, xlab = "", ylab = "",
     type = "l",
     xaxt = "n", yaxt = "n",
     lty = 1)

title(main = "Pol. Violence - Two Sided HP Filter", line = 1.5)

title(main = "Cycle", line = 0.5,
      cex = 0.8,
      font.main = 1)

legend("top", col = c("black", "red"), lty = 1,
       legend=c("Trend", "Pol. Violence")
)

# Componente de tendência de cada filtro

trend_pol_violence_plus <- ts(gtrend_plus$pol_violence_plus, start = 1961, end = 2028)
trend_pol_violence_plus_one <- ts(gtrend_plus_one$pol_violence_plus, start = 1961, end = 2028)

plot(trend_pol_violence_plus, type="l", col="black", lty=1)
lines(trend_pol_violence_plus_one, col="#066462")
title("Pol. Violence - HP Trends")
legend("bottom", horiz=TRUE, cex=0.75, c("TWO SIDED", "ONE SIDED"), lty = 1, col = c("black", "#066462"))

# Componente cíclico de cada filtro 

cycle_pol_violence_plus <- ts(ccycle_plus$pol_violence_plus, start = 1961, end = 2028)
cycle_pol_violence_plus_one <- ts(ccycle_plus_one$pol_violence_plus, start = 1961, end = 2028)

plot(cycle_pol_violence_plus, type="l", col="black", lty=1)
lines(cycle_pol_violence_plus_one, col="#066462")
title("Pol. Violence - HP Cycles")
legend("bottom", horiz=TRUE, cex=0.75, c("TWO SIDED", "ONE SIDED"), lty = 1, col = c("black", "#066462"))

## GDP Pc Plus

plot(y_plus$gdp_pc_plus, xlab = "",  # Nome eixo x
     las = 2,        # Rotação label X
     ylab = "",
     main = "",
     type = "l",     # Tipo linha
     lty = 1,        # Tipo da Linha
     lwd = 1,  # Espessura da linha
     col = "red" # cor
)       

op <- par(new = TRUE)

plot(gtrend_plus_one$gdp_pc_plus, xlab = "", ylab = "",
     type = "l",
     xaxt = "n", yaxt = "n",
     lty = 1)

title(main = "GDP. PC - One sided HP Filter", line = 1.5)

title(main = "Cycle", line = 0.5,
      cex = 0.8,
      font.main = 1)

legend("top", col = c("black", "red"), lty = 1,
       legend=c("Trend", "GDP. PC")
)

# Gráfico Two-sided

plot(y_plus$gdp_pc_plus, xlab = "",  # Nome eixo x
     las = 2,        # Rotação label X
     ylab = "",
     main = "",
     type = "l",     # Tipo linha
     lty = 1,        # Tipo da Linha
     lwd = 1,  # Espessura da linha
     col = "red" # cor
)       

op <- par(new = TRUE)

plot(gtrend_plus$gdp_pc_plus, xlab = "", ylab = "",
     type = "l",
     xaxt = "n", yaxt = "n",
     lty = 1)

title(main = "GDP. PC - Two Sided HP Filter", line = 1.5)

title(main = "Cycle", line = 0.5,
      cex = 0.8,
      font.main = 1)

legend("top", col = c("black", "red"), lty = 1,
       legend=c("Trend", "GDP. PC")
)

# Componente de tendência de cada filtro

trend_gdp_pc_plus <- ts(gtrend_plus$gdp_pc_plus, start = 1961, end = 2028)
trend_gdp_pc_plus_one <- ts(gtrend_plus_one$gdp_pc_plus, start = 1961, end = 2028)

plot(trend_gdp_pc_plus, type="l", col="black", lty=1)
lines(trend_gdp_pc_plus_one, col="#066462")
title("GDP. PC - HP Trends")
legend("bottom", horiz=TRUE, cex=0.75, c("TWO SIDED", "ONE SIDED"), lty = 1, col = c("black", "#066462"))

# Componente cíclico de cada filtro 

cycle_gdp_pc_plus <- ts(ccycle_plus$gdp_pc_plus, start = 1961, end = 2028)
cycle_gdp_pc_plus_one <- ts(ccycle_plus_one$gdp_pc_plus, start = 1961, end = 2028)

plot(cycle_gdp_pc_plus, type="l", col="black", lty=1)
lines(cycle_gdp_pc_plus_one, col="#066462")
title("GDP. PC - HP Cycles")
legend("bottom", horiz=TRUE, cex=0.75, c("TWO SIDED", "ONE SIDED"), lty = 1, col = c("black", "#066462"))

# Projeções Lineares

series_hamilton <- xts(df[, -1], order.by = df$Year)

base_hamilton <- read_excel("Base_hamilton.xlsx") %>% drop_na

plot.zoo(series_hamilton$imr, xlab = "",  # Nome eixo x
         las = 2,        # Rotação label X
         ylab = "",
         main = "",
         type = "l",     # Tipo linha
         lty = 1,        # Tipo da Linha
         lwd = 1,  # Espessura da linha
         col = "red" # cor
)       

op <- par(new = TRUE)

plot.zoo(base_hamilton$imr_trend, xlab = "", ylab = "",
         type = "l",
         xaxt = "n", yaxt = "n",
         lty = 1)

title(main = "IMR - Hamilton", line = 1.5)

title(main = "Cycles", line = 0.5,
      cex = 0.8,
      font.main = 1)

legend("top", col = c("black", "red"), lty = 1,
       legend=c("IMR", "IMR - Hamilotn")
)

trend_imrh <- ts(base_hamilton$imr_trend, start = 1963, end = 2018)

plot(trend_imr, type="l", col="black", lty=1)
lines(trend_imr_one, col = "#032362")
lines(trend_imrh, col="#066462")
title("IMR - HP Trends AND Hamilton")
legend("bottom", horiz=TRUE, cex=0.75, c("TWO SIDED", "ONE SIDED", "HAMILTON"), lty = 1, col = c("black",  "#032362", "#066462"))

cycle_imrh <- ts(base_hamilton$imr_cycle, start = 1963, end = 2018)

plot(cycle_imr, type="l", col="black", lty=1)
lines(cycle_imrh, col ="#032362")
lines(cycle_imr_one, col="#066462")
title("IMR - HP Cycles AND Hamilton")
legend("bottom", horiz=TRUE, cex=0.75, c("TWO SIDED", "ONE SIDED", "HAMILTON"), lty = 1, col = c("black", "#066462", "#032362"))

# PL Pol. Violence

plot.zoo(series_hamilton$pol_violence, xlab = "",  # Nome eixo x
         las = 2,        # Rotação label X
         ylab = "",
         main = "",
         type = "l",     # Tipo linha
         lty = 1,        # Tipo da Linha
         lwd = 1,  # Espessura da linha
         col = "red" # cor
)       

op <- par(new = TRUE)

plot.zoo(base_hamilton$pol_violence_trend, xlab = "", ylab = "",
         type = "l",
         xaxt = "n", yaxt = "n",
         lty = 1)

title(main = "Pol. Violence - Hamilton", line = 1.5)

title(main = "Cycles", line = 0.5,
      cex = 0.8,
      font.main = 1)

legend("top", col = c("black", "red"), lty = 1,
       legend=c("Pol. Violence", "Pol. Violence - Hamilotn")
)

trend_polvh <- ts(base_hamilton$pol_violence_trend, start = 1963, end = 2018)

plot(trend_pol_violence, type="l", col="black", lty=1)
lines(trend_pol_violence_one, col = "#032362")
lines(trend_polvh, col="#066462")
title("Pol. Violence - HP Trends AND Hamilton")
legend("bottom", horiz=TRUE, cex=0.75, c("TWO SIDED", "ONE SIDED", "HAMILTON"), lty = 1, col = c("black",  "#032362", "#066462"))

cycle_polvh <- ts(base_hamilton$pol_violence_cycle, start = 1963, end = 2018)

plot(cycle_pol_violence, type="l", col="black", lty=1)
lines(cycle_polvh, col ="#032362")
lines(cycle_pol_violence_one, col="#066462")
title("Pol. Violence - HP Cycles AND Hamilton")
legend("bottom", horiz=TRUE, cex=0.75, c("TWO SIDED", "ONE SIDED", "HAMILTON"), lty = 1, col = c("black", "#066462", "#032362"))

# PL GDP. PC

plot.zoo(series_hamilton$gdp_pc, xlab = "",  # Nome eixo x
         las = 2,        # Rotação label X
         ylab = "",
         main = "",
         type = "l",     # Tipo linha
         lty = 1,        # Tipo da Linha
         lwd = 1,  # Espessura da linha
         col = "red" # cor
)       

op <- par(new = TRUE)

plot.zoo(base_hamilton$gdp_pc_trend, xlab = "", ylab = "",
         type = "l",
         xaxt = "n", yaxt = "n",
         lty = 1)

title(main = "GDP. PC - Hamilton", line = 1.5)

title(main = "Cycles", line = 0.5,
      cex = 0.8,
      font.main = 1)

legend("top", col = c("black", "red"), lty = 1,
       legend=c("GDP. PC", "GDP. PC - Hamilton")
)

trend_gdph <- ts(base_hamilton$gdp_pc_trend, start = 1963, end = 2018)

plot(trend_gdp_pc, type="l", col="black", lty=1)
lines(trend_gdp_pc_one, col = "#032362")
lines(trend_gdph, col="#066462")
title("GDP. PC - HP Trends AND Hamilton")
legend("bottom", horiz=TRUE, cex=0.75, c("TWO SIDED", "ONE SIDED", "HAMILTON"), lty = 1, col = c("black",  "#032362", "#066462"))

cycle_gdph <- ts(base_hamilton$gdp_pc_cycle, start = 1963, end = 2018)

plot(cycle_gdp_pc, type="l", col="black", lty=1)
lines(cycle_gdph, col ="#032362")
lines(cycle_gdp_pc_one, col="#066462")
title("GDP. PC - HP Cycles AND Hamilton")
legend("bottom", horiz=TRUE, cex=0.75, c("TWO SIDED", "ONE SIDED", "HAMILTON"), lty = 1, col = c("black", "#066462", "#032362"))
