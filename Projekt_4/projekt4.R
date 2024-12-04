# Wymagane pakiety
library(vars)
library(knitr)
library(kableExtra)
library(ggplot2)
library(dplyr)

# Funkcja do symulacji danych VAR z różnymi wariancjami składników losowych
simulate_var <- function(n1, n2, phi, sigma, k) {
  n <- n1 + n2
  errors <- matrix(rnorm(n * 2, mean = 0, sd = sigma), ncol = 2)
  errors[(n1 + 1):n, ] <- errors[(n1 + 1):n, ] * k  # Zwiększenie wariancji w drugiej części
  
  data <- matrix(0, nrow = n, ncol = 2)
  for (t in 2:n) {
    data[t, 1] <- phi * data[t - 1, 1] + errors[t, 1]
    data[t, 2] <- phi * data[t - 1, 2] + errors[t, 2]
  }
  colnames(data) <- c("V1", "V2")
  return(as.data.frame(data))
}

# Funkcja do przeprowadzania testu Grangera
test_granger <- function(data, lags, hc_type = "HC3") {
  var_model <- VAR(data, p = lags, type = "const")
  granger_test <- causality(var_model, cause = "V1")
  p_value <- granger_test$Granger$p.value
  return(p_value)
}

# Parametry symulacji
phi_values <- c(0.3, 0.6)  # Autokorelacja
k_values <- seq(1.0, 2.0, by = 0.25)  # Wzrost wariancji
n1 <- 100  # Liczba obserwacji przed zmianą wariancji
n2 <- 100  # Liczba obserwacji po zmianie wariancji
lags <- 1  # Opóźnienia w modelu VAR
sigma <- 1  # Początkowe odchylenie standardowe
reps <- 50  # Liczba powtórzeń symulacji

# Pętla symulacyjna
results <- data.frame()
set.seed(123)  # Ustawienie ziarna losowego

for (phi in phi_values) {
  for (k in k_values) {
    for (rep in 1:reps) {
      data <- simulate_var(n1, n2, phi, sigma, k)
      p_value <- test_granger(data, lags, hc_type = "HC3")
      results <- rbind(results, data.frame(phi = phi, k = k, rep = rep, p_value = p_value))
    }
  }
}

# Dodanie informacji o odrzuceniu H0 (p-value < 0.05)
results <- results %>%
  mutate(reject_h0 = ifelse(p_value < 0.05, TRUE, FALSE))


###Wizualizacje
# Obliczenie średniego odsetka odrzuceń H0 dla każdej kombinacji phi i k
summary_results <- results %>%
  group_by(phi, k) %>%
  summarize(reject_rate = mean(reject_h0), .groups = "drop")

# Wykres
ggplot(summary_results, aes(x = k, y = reject_rate, color = as.factor(phi), group = phi)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Wpływ wzrostu wariancji (k) na odsetek odrzuceń H0",
    x = "Wzrost wariancji (k)",
    y = "Odsetek odrzuceń H0",
    color = "Phi (autokorelacja)"
  ) +
  theme_minimal()





# Obliczenie średniego p-value dla każdej kombinacji phi i k
p_value_summary <- results %>%
  group_by(phi, k) %>%
  summarize(mean_p_value = mean(p_value), .groups = "drop")

# Wykres
ggplot(p_value_summary, aes(x = phi, y = mean_p_value, group = k, color = as.factor(k))) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Wpływ autokorelacji (phi) na średnie p-value",
    x = "Autokorelacja (phi)",
    y = "Średnie p-value",
    color = "Wzrost wariancji (k)"
  ) +
  theme_minimal()






# Wymagane pakiety
library(vars)
library(ggplot2)
library(dplyr)

# Funkcja do symulacji danych VAR z różnymi wariancjami składników losowych
simulate_var <- function(n1, n2, phi, sigma, k) {
  n <- n1 + n2
  errors <- matrix(rnorm(n * 2, mean = 0, sd = sigma), ncol = 2)
  errors[(n1 + 1):n, ] <- errors[(n1 + 1):n, ] * k  # Zwiększenie wariancji w drugiej części
  
  data <- matrix(0, nrow = n, ncol = 2)
  for (t in 2:n) {
    data[t, 1] <- phi * data[t - 1, 1] + errors[t, 1]
    data[t, 2] <- phi * data[t - 1, 2] + errors[t, 2]
  }
  colnames(data) <- c("V1", "V2")
  return(as.data.frame(data))
}

# Funkcja do przeprowadzania testu Grangera
test_granger <- function(data, lags, hc_type = "HC3") {
  var_model <- VAR(data, p = lags, type = "const")
  granger_test <- causality(var_model, cause = "V1")
  p_value <- granger_test$Granger$p.value
  return(p_value)
}

# Parametry symulacji
phi_values <- c(0.3, 0.6)  # Autokorelacja
k_values <- seq(1.0, 2.0, by = 0.25)  # Wzrost wariancji
n1 <- 100  # Liczba obserwacji przed zmianą wariancji
n2 <- 100  # Liczba obserwacji po zmianie wariancji
lags <- 1  # Opóźnienia w modelu VAR
sigma <- 1  # Początkowe odchylenie standardowe
reps <- 50  # Liczba powtórzeń symulacji

# Pętla symulacyjna
results <- data.frame()
set.seed(123)  # Ustawienie ziarna losowego

for (phi in phi_values) {
  for (k in k_values) {
    for (rep in 1:reps) {
      data <- simulate_var(n1, n2, phi, sigma, k)
      p_value <- test_granger(data, lags, hc_type = "HC3")
      results <- rbind(results, data.frame(phi = phi, k = k, rep = rep, p_value = p_value))
    }
  }
}

# Dodanie informacji o odrzuceniu H0 (p-value < 0.05)
results <- results %>%
  mutate(reject_h0 = ifelse(p_value < 0.05, TRUE, FALSE))
# Obliczenie średniego odsetka odrzuceń H0 dla każdej kombinacji phi i k
summary_results <- results %>%
  group_by(phi, k) %>%
  summarize(reject_rate = mean(reject_h0), .groups = "drop")

# Wykres
ggplot(summary_results, aes(x = k, y = reject_rate, color = as.factor(phi), group = phi)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Wpływ wzrostu wariancji (k) na odsetek odrzuceń H0",
    x = "Wzrost wariancji (k)",
    y = "Odsetek odrzuceń H0",
    color = "Phi (autokorelacja)"
  ) +
  theme_minimal()




# Obliczenie średniego p-value dla każdej kombinacji phi i k
p_value_summary <- results %>%
  group_by(phi, k) %>%
  summarize(mean_p_value = mean(p_value), .groups = "drop")

# Wykres
ggplot(p_value_summary, aes(x = phi, y = mean_p_value, group = k, color = as.factor(k))) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Wpływ autokorelacji (phi) na średnie p-value",
    x = "Autokorelacja (phi)",
    y = "Średnie p-value",
    color = "Wzrost wariancji (k)"
  ) +
  theme_minimal()


# Wykres dla phi = 0.3 i phi = 0.6
ggplot(results, aes(x = k, y = p_value, color = as.factor(phi))) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Wpływ wzrostu wariancji (k) na p-value testu Grangera",
    x = "Wzrost wariancji (k)",
    y = "p-value testu Grangera",
    color = "Phi (autokorelacja)"
  ) +
  theme_minimal()



# Tabela podsumowująca odsetek odrzuceń H0
summary_table <- summary_results %>%
  pivot_wider(names_from = phi, values_from = reject_rate) %>%
  rename(`Phi = 0.3` = `0.3`, `Phi = 0.6` = `0.6`)

# Generowanie tabeli
summary_table %>%
  kable("html", caption = "Podsumowanie wyników: Odsetek odrzuceń H0 dla różnych phi i k") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

