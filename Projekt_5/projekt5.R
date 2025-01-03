library(zoo)
library(rugarch)
library(forecast)

dane <- read.csv("nvda_us_d.csv")
dane <- dane[c(1,5)]
dane$Data <- as.Date(dane$Data)

#Dane z pierwszych dwóch lat
batch <- dane[dane$Data < as.Date("2016-01-01"),]


modele <- function(dt) {
  results <- matrix(NA, ncol = 3, nrow = 6)
  results[,1] <- c("garch_norm","garch_std","gjr_norm","gjr_std","egarch_norm","egarch_std")
  i = 1
  
  # Dopasowanie modelu ARMA
  arma_fit <- auto.arima(dt, max.p = 5, max.q = 5, stationary = TRUE)
  ar_order <- arimaorder(arma_fit)[1]  # Rząd AR
  ma_order <- arimaorder(arma_fit)[3]  # Rząd MA
  
  #Modele Garch z dynamicznie przypisywaną specyfikacją
  specs <- list(
    ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(ar_order, ma_order)),
      distribution.model = "norm"
    ),
    ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(ar_order, ma_order)),
      distribution.model = "std"
    ),
    ugarchspec(
      variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(ar_order, ma_order)),
      distribution.model = "norm"
    ),
    ugarchspec(
      variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(ar_order, ma_order)),
      distribution.model = "std"
    ),
    ugarchspec(
      variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(ar_order, ma_order)),
      distribution.model = "norm"
    ),
    ugarchspec(
      variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
      mean.model = list(armaOrder = c(ar_order, ma_order)),
      distribution.model = "std"
    )
  )
  
  #Budowa poszczególnych modeli
  for (spec in specs) {
    fit <- ugarchfit(spec = spec, data = dt)
    fc <- ugarchforecast(fit, n.ahead = 1)
    results[i,2] = quantile(fc, probs = 0.01)
    results[i,3] = quantile(fc, probs = 0.05)
    i = i + 1
  }
  results <- as.data.frame(results)
  colnames(results) <- c("Metoda", "VaR_1%", "VaR_5%")
  return(results)
}

#Metoda historyczna
historyczna <- function(dt) {
  results <- matrix(NA, ncol = 3, nrow = 1)
  results[,1] <- c("Historyczna")
  results[,2] <- quantile(dt, probs = 0.01)
  results[,3] <- quantile(dt, probs = 0.05)
  results <- as.data.frame(results)
  colnames(results) <- c("Metoda", "VaR_1%", "VaR_5%")
  return(results)
}

#Metoda macierzy wariancji-kowariancji
macierzv <- function(dt) {
  mu <- mean(dt)
  sd <- sd(dt)
  results <- matrix(NA, ncol = 3, nrow = 1)
  results[,1] <- c("Macierz wariancji")
  results[,2] <- mu + qnorm(0.01) * sd
  results[,3] <- mu + qnorm(0.05) * sd
  results <- as.data.frame(results)
  colnames(results) <- c("Metoda", "VaR_1%", "VaR_5%")
  return(results)
}

symulacje <- function() {
  wyniki1 <- list()
  wyniki2 <- list()
  wyniki3 <- list()
  end <- nrow(batch)
  for (j in 1:(nrow(dane)-end)) {
    print(j)
    df <- diff(log(dane[j:(j+end),2]))
    wyniki1[[j]] <- tryCatch({
      modele(df)
    }, error = function(e) {
      # W przypadku błędu zwróć poprzednie wyniki
      last_valid
    })
    if (!is.null(wyniki1[[j]])) last_valid <- wyniki1[[j]]
    wyniki2[[j]] <- historyczna(df)
    wyniki3[[j]] <- macierzv(df)
  }
  return(list(ModeleGarch = wyniki1, MetodaHistoryczna = wyniki2, MetodaMacierzy = wyniki3))
}

r <- symulacje()

plot

