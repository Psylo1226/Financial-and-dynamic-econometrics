# Wymagane pakiety
library(vars)
library(knitr)
library(kableExtra)
library(ggplot2)
library(dplyr)
library(tidyr)

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

# Parametry symulacji 1 - test wpływu autokorelacji i wielkości zmiany odchylenia standardowego
phi_values <- c(0.1, 0.5, 0.9)  # Autokorelacja
k_values <- seq(1.0, 2.0, by = 0.25)  # Wzrost wariancji
n1 <- 500  # Liczba obserwacji przed zmianą wariancji
n2 <- 500  # Liczba obserwacji po zmianie wariancji
lags <- 1  # Opóźnienia w modelu VAR
sigma <- 1  # Początkowe odchylenie standardowe
reps <-  1000 # Liczba powtórzeń symulacji

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
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", size = 0.7) +
  labs(
    title = "Wpływ wzrostu wariancji (k) na odsetek odrzuceń H0",
    x = "Wzrost wariancji (k)",
    y = "Odsetek odrzuceń H0",
    color = "Phi (autokorelacja)"
  ) +
  theme_minimal()


# Tabela podsumowująca odsetek odrzuceń H0
summary_table <- summary_results %>%
  pivot_wider(names_from = phi, values_from = reject_rate) %>%
  rename(`Phi = 0.1` = `0.1`, `Phi = 0.5` = `0.5`, `Phi = 0.9` = `0.9`)

# Generowanie tabeli
summary_table %>%
  kable("html", caption = "Podsumowanie wyników: Odsetek odrzuceń H0 dla różnych phi i k") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))




####

# Parametry symulacji 2 - test wpływu długości danych i wielkości zmiany odchylenia standardowego
phi <- 0.5  # Autokorelacja
k_values <- seq(1.0, 2.0, by = 0.25)  # Wzrost wariancji
n_values <- c(50,200,1000)  # Liczba obserwacji
lags <- 1  # Opóźnienia w modelu VAR
sigma <- 1  # Początkowe odchylenie standardowe
reps <-  1000 # Liczba powtórzeń symulacji

# Pętla symulacyjna
results2 <- data.frame()
set.seed(123)  # Ustawienie ziarna losowego

for (k in k_values) {
  for (n in n_values) {
    for (rep in 1:reps) {
      data <- simulate_var(n, n, phi, sigma, k)
      p_value <- test_granger(data, lags, hc_type = "HC3")
      results2 <- rbind(results2, data.frame(k = k, n = n, rep = rep, p_value = p_value))
    }
  }
}

# Dodanie informacji o odrzuceniu H0 (p-value < 0.05)
results2 <- results2 %>%
  mutate(reject_h0 = ifelse(p_value < 0.05, TRUE, FALSE))

# Obliczenie średniego odsetka odrzuceń H0 dla każdej kombinacji k i n
summary_results2 <- results2 %>%
  group_by(k, n) %>%
  summarize(reject_rate = mean(reject_h0), .groups = "drop")

# Wykres
ggplot(summary_results2, aes(x = k, y = reject_rate, color = as.factor(n), group = n)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", size = 0.7) +
  labs(
    title = "Wpływ wzrostu wariancji (k) na odsetek odrzuceń H0",
    x = "Wzrost wariancji (k)",
    y = "Odsetek odrzuceń H0",
    color = "N (liczba obserwacji)"
  ) +
  theme_minimal()


# Tabela podsumowująca odsetek odrzuceń H0
summary_table2 <- summary_results2 %>%
  pivot_wider(names_from = n, values_from = reject_rate) %>%
  rename(`N = 50` = `50`, `N = 200` = `200`, `N = 1000` = `1000`)

# Generowanie tabeli
summary_table2 %>%
  kable("html", caption = "Podsumowanie wyników: Odsetek odrzuceń H0 dla różnych n i k") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))