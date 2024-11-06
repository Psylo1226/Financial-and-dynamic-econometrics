library("tidyverse")
library("moments")
library("tseries")
library("forecast")
library("rugarch")
library("FinTS")
install.packages("rugarch")
install.packages("FinTS")
install.packages("tinytex")
izo <- read.csv("C:/Visual_Studio/Ekonometria finansowa i dynamiczna/git/Projekt_2/izo_d.csv")

#Stopa logarytmiczna
st_l <- function(a,b) {
  return(log(a/b))
}

#Zaczytanie danych
dane <- izo[,c(1,5)]
indeksy <- dane %>% map_df(rev)

#Wyliczanie stóp logarytmicznych
dt <- c()
for (i in 1:(nrow(indeksy)-1)) {
  dt <- c(dt,as.numeric(st_l(indeksy[i,2],indeksy[i+1,2])))
}

#Podział na zbiór uczący i odłożone dane
stopy <- rev(tail(dt, -4))
ceny <- unname(head(log(dane[2]), -4))
stopy_oct <- rev(head(dt,4))
ceny_oct <- unname(tail(log(dane[2]), 4))
ceny_n <- unname(tail(dane[2], 4))

#Test ADF
adf.test(stopy)
#Bardzo niskie p-value -> szereg stacjonarny

#ARMA
model <- auto.arima(stopy)
summary(model)
#Zaproponowany model: ARMA(3,2)

#Reszty modelu
res <- residuals(model)
Box.test(res, type = "Ljung-Box")
#P-value większe od 0.05 -> brak aurokorelacji
jarque.bera.test(res)
#P-value mniejsze od 0.05 -> reszty nie mają rozkładu normalnego
qqnorm(res)

#Prognoza logarytmicznych stóp dla 4 przyszłych notowań i
#porównanie z odłożonymi wartościami
stopy_wyniki <- data.frame(
  Odłożone_Wartości = stopy_oct,
  Początek_Przedziału = forecast(model,4)$lower[, 2],
  Koniec_Przedziału = forecast(model,4)$upper[, 2],
  Średnia_Prognoza = forecast(model,4)$mean
)

#Model dla logarytmów cen
model2 <- auto.arima(ceny)
summary(model2)
res2 <- residuals(model2)
Box.test(res2, type = "Ljung-Box")
#P-value większe od 0.05 -> brak aurokorelacji
jarque.bera.test(res)
#P-value mniejsze od 0.05 -> reszty nie mają rozkładu normalnego

arima_forecast <- forecast(model2,4)$mean
#Porównanie z odłożonymi wartościami
ceny_wyniki <- data.frame(
  Odłożone_Wartości = ceny_oct,
  Początek_Przedziału = forecast(model2,4)$lower[, 2],
  Koniec_Przedziału = forecast(model2,4)$upper[, 2],
  Średnia_Prognoza = arima_forecast
)

#Analogicznie dla cen niezlogarytmowanych
ceny_n_wyniki <- data.frame(
  Odłożone_Wartości = ceny_n,
  Początek_Przedziału = exp(forecast(model2,4)$lower[, 2]),
  Koniec_Przedziału = exp(forecast(model2,4)$upper[, 2]),
  Średnia_Prognoza = exp(arima_forecast)
)

#Metody symulacyjne
# Liczba symulacji
n <- 1000
# Długość prognozy (4 notowania)
h <- 4

# Wartość początkowa logarytmów cen (ostatnia znana wartość w cenach)
last_price <- as.numeric(tail(ceny, 1))

# Symulacje Monte Carlo
set.seed(123)  # Ustawienie ziarna dla powtarzalności wyników
symulacje <- matrix(0, nrow = n, ncol = h)
for (i in 1:n) {
  # Generowanie losowych reszt modelu
  sim_res <- rnorm(h, mean(res2), sd(res2))
  # Symulacja logarytmów cen przy użyciu modelu
  symulacje[i, 1] <- last_price + sim_res[1]
  for (j in 2:h) {
    symulacje[i, j] <- (symulacje[i, j-1] + sim_res[j])
  }
}

# Obliczenie 95% przedziałów ufności dla logarytmów cen
log_price_forecast_mean_mc <- colMeans(symulacje)
log_price_forecast_lower <- apply(symulacje, 2, quantile, probs = 0.025)
log_price_forecast_upper <- apply(symulacje, 2, quantile, probs = 0.975)

# Konwersja logarytmów cen do rzeczywistych cen
symulacje_ceny <- exp(symulacje)

# Obliczenie 95% przedziałów ufności dla rzeczywistych cen
price_forecast_mean <- colMeans(symulacje_ceny)
price_forecast_lower <- apply(symulacje_ceny, 2, quantile, probs = 0.025)
price_forecast_upper <- apply(symulacje_ceny, 2, quantile, probs = 0.975)

# Tworzenie tabeli porównawczej wyników
wyniki_monte_carlo <- data.frame(
  Odłożone_Wartości_Log = ceny_oct,
  Prognoza_Log_Ceny = log_price_forecast_mean_mc,
  Przedzial_Log_Ceny_Dolny = log_price_forecast_lower,
  Przedzial_Log_Ceny_Gorny = log_price_forecast_upper,
  Odłożone_Wartości = exp(ceny_oct),
  Prognoza_Ceny = price_forecast_mean,
  Przedzial_Ceny_Dolny = price_forecast_lower,
  Przedzial_Ceny_Gorny = price_forecast_upper
)

#Metody bootstrapowe
set.seed(123)  # Ustawienie ziarna dla powtarzalności wyników
bootstrap <- matrix(0, nrow = n, ncol = h)
for (i in 1:n) {
  # Generowanie losowych reszt modelu
  boot_res <- sample(res2, h, replace = TRUE)
  # Symulacja logarytmów cen przy użyciu modelu
  bootstrap[i, 1] <- last_price + boot_res[1]
  for (j in 2:h) {
    bootstrap[i, j] <- (bootstrap[i, j-1] + boot_res[j])
  }
}

# Obliczanie średnich prognoz i 95% przedziałów ufności dla logarytmów cen
log_price_forecast_mean_bootstrap <- colMeans(bootstrap)
log_price_forecast_lower <- apply(bootstrap, 2, quantile, probs = 0.025)
log_price_forecast_upper <- apply(bootstrap, 2, quantile, probs = 0.975)

# Konwersja logarytmów cen na rzeczywiste ceny
bootstrap_prices <- exp(bootstrap)

# Obliczanie średnich prognoz i 95% przedziałów ufności dla rzeczywistych cen
price_forecast_mean <- colMeans(bootstrap_prices)
price_forecast_lower <- apply(bootstrap_prices, 2, quantile, probs = 0.025)
price_forecast_upper <- apply(bootstrap_prices, 2, quantile, probs = 0.975)

# Tworzenie tabeli wynikowej z prognozami i przedziałami ufności
wyniki_bootstrap <- data.frame(
  Odłożone_Wartości_Log = ceny_oct,
  Prognoza_Log_Ceny = log_price_forecast_mean_bootstrap,
  Przedzial_Log_Ceny_Dolny = log_price_forecast_lower,
  Przedzial_Log_Ceny_Gorny = log_price_forecast_upper,
  Odłożone_Wartości = exp(ceny_oct),
  Prognoza_Ceny = price_forecast_mean,
  Przedzial_Ceny_Dolny = price_forecast_lower,
  Przedzial_Ceny_Gorny = price_forecast_upper
)

#Porównanie obu metod
wyniki_porownawcze <- data.frame(
  Odlozone_Wartosci = ceny_oct,
  Prognoza_Monte_Carlo = log_price_forecast_mean_mc,
  Prognoza_Bootstrap = log_price_forecast_mean_bootstrap
)

#Efekt ARCH
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(3, 2), include.mean = TRUE),
  distribution.model = "norm"
)
model_garch <- ugarchfit(spec = spec, data = ceny)

#Prognozy modelu ARMA-GARCH
forecast_garch <- ugarchforecast(model_garch, n.ahead = h)
log_garch_mean <- fitted(forecast_garch)

#Reszty z modelu GARCH
res3 <- residuals(model_garch)

set.seed(123)
mcg <- matrix(0, nrow = n, ncol = h)
for (i in 1:n) {
  # Generowanie losowych reszt modelu
  sim_res <- rnorm(h, mean(res3), sd(res3))
  # Symulacja logarytmów cen przy użyciu modelu
  mcg[i, 1] <- last_price + sim_res[1]
  for (j in 2:h) {
    mcg[i, j] <- (mcg[i, j-1] + sim_res[j])
  }
}

set.seed(123)
bootg <- matrix(0, nrow = n, ncol = h)
for (i in 1:n) {
  # Generowanie losowych reszt modelu
  boot_res <- sample(res3, h, replace = TRUE)
  # Symulacja logarytmów cen przy użyciu modelu
  bootg[i, 1] <- last_price + boot_res[1]
  for (j in 2:h) {
    bootg[i, j] <- (bootg[i, j-1] + boot_res[j])
  }
}

log_gmc_mean <- colMeans(mcg)
gmc_mean <- colMeans(exp(mcg))
log_bootg_mean <- colMeans(bootg)
bootg_mean <- colMeans(exp(bootg))

ostatnie_porównanie <- data.frame(
  Odłożone_Wartości = ceny_oct,
  Prognoza_ARIMA = arima_forecast,
  Prognoza_MC = log_price_forecast_mean_mc,
  Prognoza_Bootstrap = log_price_forecast_mean_bootstrap,
  Prognoza_GARCH = unname(log_garch_mean),
  Prognoza_MC_GARCH = log_gmc_mean,
  Prognoza_Bootstrap_GARCH = log_bootg_mean
)

# Przeprowadzenie testu ARCH
test_arch1 <- ArchTest(res2)
test_arch2 <- ArchTest(residuals(model_garch))
#Podstawowy model nie wykazuje efektu ARCH, jednak przeprowadzając go na
#modelu GARCH uzyskujemy odwrotny wynik
