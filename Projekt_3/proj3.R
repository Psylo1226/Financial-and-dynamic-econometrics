library(forecast)
library(tseries)
library(MASS)

# Funkcja do symulacji modelu AR(1) i dopasowania modelu AR(3)
simulate_AR1_to_AR3 <- function(N, phi, n_iter = 1000) {
  # Wektory do przechowywania wyników
  estimates <- matrix(NA, nrow = n_iter, ncol = 3)
  stderrs <- matrix(NA, nrow = n_iter, ncol = 3)
  residual_acf <- matrix(NA, nrow = n_iter, ncol = 20)  # Przechowuj pierwsze 20 wartości ACF reszt
  
  for (i in 1:n_iter) {
    # 1. Generowanie danych z modelu AR(1)
    ar1_data <- arima.sim(n = N, model = list(ar = phi))
    
    # 2. Dopasowanie modelu AR(3) do danych z modelu AR(1)
    fit_ar3 <- Arima(ar1_data, order = c(3, 0, 0))
    
    # 3. Zapisz oszacowania parametrów, odchylenia standardowe oraz autokorelację reszt
    estimates[i, ] <- fit_ar3$coef[1:3] # Zapisz współczynniki AR(3)
    stderrs[i, ] <- sqrt(diag(fit_ar3$var.coef))[1:3] # Zapisz odchylenia standardowe
    residual_acf[i, ] <- acf(residuals(fit_ar3), plot = FALSE)$acf[2:21] # ACF reszt
  }
  
  # Zwróć wyniki
  list(estimates = estimates, stderrs = stderrs, residual_acf = residual_acf)
}

# Przeprowadzenie symulacji dla różnych wartości N i różnych parametrów modelu
N_values <- c(100, 500, 1000)
phi_values <- c(0.5, 0.9)
results <- list()

for (N in N_values) {
  for (phi in phi_values) {
    results[[paste("N", N, "phi", phi, sep = "_")]] <- simulate_AR1_to_AR3(N, phi)
  }
}

# Analiza wyników symulacji
analyze_results <- function(sim_results) {
  estimates <- sim_results$estimates
  residual_acf <- sim_results$residual_acf
  
  # Oblicz średnie i odchylenia standardowe oszacowań parametrów
  mean_estimates <- colMeans(estimates, na.rm = TRUE)
  sd_estimates <- apply(estimates, 2, sd, na.rm = TRUE)
  
  # Autokorelacja reszt - sprawdzenie, czy reszty są autokorelacyjne
  mean_acf <- colMeans(residual_acf, na.rm = TRUE)
  
  # Wyświetl wyniki
  cat("Średnie oszacowania parametrów AR(3):\n", mean_estimates, "\n")
  cat("Odchylenie standardowe oszacowań:\n", sd_estimates, "\n")
  cat("Średnie wartości ACF reszt:\n", mean_acf, "\n")
}

# Przykładowa analiza dla jednego z zestawów wyników
analyze_results(results[["N_100_phi_0.5"]])

# W sytuacji odwrotnej, gdy dane pochodzą z modelu AR(3) i dopasowujemy AR(1),
# Można użyć analogicznej funkcji:
simulate_AR3_to_AR1 <- function(N, phi_vec, n_iter = 1000) {
  # Przygotowanie kontenerów na wyniki
  estimates <- rep(NA, n_iter)
  stderrs <- rep(NA, n_iter)
  
  for (i in 1:n_iter) {
    # Generacja danych z modelu AR(3)
    ar3_data <- arima.sim(n = N, model = list(ar = phi_vec))
    
    # Dopasowanie modelu AR(1)
    fit_ar1 <- Arima(ar3_data, order = c(1, 0, 0))
    
    # Zapisz oszacowania parametru i odchylenie standardowe
    estimates[i] <- fit_ar1$coef[1]
    stderrs[i] <- sqrt(diag(fit_ar1$var.coef))[1]
    
    # Wypisz postęp
    if (i %% 100 == 0) cat("Iteracja: ", i, "\n")
  }
  
  # Zwróć wyniki
  list(estimates = estimates, stderrs = stderrs)
}

# Przeprowadzenie symulacji dla modelu AR(3) dopasowanego jako AR(1)
results_reverse <- list()
phi_vec_values <- list(c(0.5, 0.2, -0.1), c(0.9, -0.4, 0.2))

for (N in N_values) {
  for (phi_vec in phi_vec_values) {
    results_reverse[[paste("N", N, "phi_vec", paste(phi_vec, collapse = "_"),
                           sep = "_")]] <- simulate_AR3_to_AR1(N, phi_vec)
  }
}

# Analiza wyników odwrotnej sytuacji
analyze_reverse_results <- function(sim_results) {
  estimates <- sim_results$estimates
  
  # Oblicz średnią i odchylenie standardowe oszacowań parametru AR(1)
  mean_estimate <- mean(estimates, na.rm = TRUE)
  sd_estimate <- sd(estimates, na.rm = TRUE)
  
  # Wyświetl wyniki
  cat("Średnie oszacowanie parametru AR(1):\n", mean_estimate, "\n")
  cat("Odchylenie standardowe oszacowania:\n", sd_estimate, "\n")
}

# Przykładowa analiza dla jednego z zestawów wyników odwrotnej sytuacji
analyze_reverse_results(results_reverse[["N_100_phi_vec_0.5_0.2_-0.1"]])
