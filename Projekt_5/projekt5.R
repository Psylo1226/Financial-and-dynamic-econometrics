library(zoo)
library(rugarch)

dane <- read.csv("nvda_us_d.csv")
dane <- dane[c(1,5)]
dane$Data <- as.Date(dane$Data)

batch <- dane[dane$Data < as.Date("2016-01-01"),]

#Specyfikacje modeli Garch
specs <- c(
  ugarchspec(
    variance.model = list(model = "sGARCH"),
    mean.model = list(armaOrder = c(0, 0)),
    distribution.model = "norm"
  ),
  ugarchspec(
    variance.model = list(model = "sGARCH"),
    mean.model = list(armaOrder = c(0, 0)),
    distribution.model = "std"
  ),
  ugarchspec(
    variance.model = list(model = "gjrGARCH"),
    mean.model = list(armaOrder = c(0, 0)),
    distribution.model = "norm"
  ),
  ugarchspec(
    variance.model = list(model = "gjrGARCH"),
    mean.model = list(armaOrder = c(0, 0)),
    distribution.model = "std"
  ),
  ugarchspec(
    variance.model = list(model = "eGARCH"),
    mean.model = list(armaOrder = c(0, 0)),
    distribution.model = "norm"
  ),
  ugarchspec(
    variance.model = list(model = "eGARCH"),
    mean.model = list(armaOrder = c(0, 0)),
    distribution.model = "std"
  )
)

modele <- function(df) {
  dt <- df[,2]
  results <- matrix(NA, ncol = 3, nrow = 6)
  results[,1] <- c("garch_norm","garch_std","gjr_norm","gjr_std","egarch_norm","egarch_std")
  i = 1
  for (spec in specs) {
    fit <- ugarchfit(spec = spec, data = dt)
    fc <- ugarchforecast(fit, n.ahead = 1)
    results[i,2] = quantile(fc, probs = 0.01)
    results[i,3] = quantile(fc, probs = 0.05)
    i= i + 1
  }
  return(results)
}

wyniki <- list()
symulacje <- function() {
  end <- nrow(batch)
  for (i in 1:(nrow(dane)-end)) {
    df <- dane[i:(i+end),]
    wyniki[i] <- modele(df)
  }
}

symulacje()

