library(vars)
library(sandwich)
library(lmtest)
library(ggplot2)
library(MASS)


simulate_var <- function(n1, n2, phi, sigma, k) {
  # Parametry VAR(1)
  A <- matrix(c(phi, 0.2, 0.2, phi), nrow = 2) # Macierz autoregresji
  sigma_mat <- diag(c(sigma, sigma))          # Macierz kowariancji
  
  # Pierwsza część danych: stała wariancja
  eps1 <- mvrnorm(n1, mu = c(0, 0), Sigma = sigma_mat)
  data1 <- matrix(0, n1, 2)
  for (t in 2:n1) {
    data1[t, ] <- A %*% data1[t - 1, ] + eps1[t, ]
  }
  
  # Druga część danych: zwiększona wariancja
  sigma_mat2 <- diag(c(k * sigma, k * sigma)) # Zmieniona macierz kowariancji
  eps2 <- mvrnorm(n2, mu = c(0, 0), Sigma = sigma_mat2)
  data2 <- matrix(0, n2, 2)
  data2[1, ] <- data1[n1, ] # Kontynuacja z poprzedniego końca
  for (t in 2:n2) {
    data2[t, ] <- A %*% data2[t - 1, ] + eps2[t, ]
  }
  
  # Połączenie danych
  data <- rbind(data1, data2)
  
  # Dodaj nazwy kolumn
  colnames(data) <- c("V1", "V2")
  
  return(data)
}



test_granger <- function(data, lags, hc_type) {
  # Dopasowanie modelu VAR
  var_model <- VAR(data, p = lags, type = "const")
  
  # Test Grangera
  granger_test <- causality(var_model, cause = "V1")
  
  return(list(granger = granger_test))
}




# Parametry symulacji
n1 <- 100
n2 <- 100
phi_values <- c(0.2, 0.5, 0.8) # Siła autokorelacji
k_values <- c(1.5, 2, 3)       # Wzrost wariancji
lags <- 1
sigma <- 1
reps <- 100

# Wyniki
results <- list()

for (phi in phi_values) {
  for (k in k_values) {
    for (rep in 1:reps) {
      data <- simulate_var(n1, n2, phi, sigma, k)
      test <- test_granger(data, lags, hc_type = "HC3")
      results[[paste(phi, k, rep)]] <- test
    }
  }
}

# Analiza wyników (rozmiar testu, moc itp.)
