library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyr)

wig <- read.csv("wig20_d.csv")
dax <- read.csv("dax_d.csv")
cac <- read.csv("cac_d.csv")
moex <- read.csv("moex_d.csv")

wig <- wig[,c(1,5)]
dax <- dax[,c(1,5)]
cac <- cac[,c(1,5)]
moex <- moex[,c(1,5)]

merged <- wig |>
  inner_join(dax, by = "Data") |>
  inner_join(cac, by = "Data") |>
  inner_join(moex, by = "Data")

colnames(merged) <- c("Data","WIG","DAX","CAC","MOEX")
merged$Data <- as.Date(merged$Data)

pivot_merged <- pivot_longer(merged, cols = -Data, names_to = "Indeks", values_to = "val")

ggplot(pivot_merged, aes(x = Data, y = val, color = Indeks)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linetype = "dashed", color = "black", linewidth = 0.6) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-24")), linetype = "dashed", color = "black", linewidth = 0.6) +
  labs(title = "Wartości zamknięcia", x = "Data", y = "Wartość") +
  theme_minimal()

zwroty <- merged %>%
  dplyr::mutate(
    across(
      .cols = c(WIG, DAX, CAC, MOEX),
      .fns = ~ c(NA, diff(log(.)))
    )
  )

zwroty <- na.omit(zwroty)

pivot_zwroty <- pivot_longer(zwroty, cols = -Data, names_to = "Indeks", values_to = "val")

ggplot(pivot_zwroty, aes(x = Data, y = val, color = Indeks)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linetype = "dashed", color = "black", linewidth = 0.6) +
  geom_vline(xintercept = as.numeric(as.Date("2022-02-24")), linetype = "dashed", color = "black", linewidth = 0.6) +
  labs(title = "Wartości zamknięcia", x = "Data", y = "Wartość") +
  theme_minimal()

#Covid

z1 <- zwroty[zwroty$Data < as.Date("2020-03-01"),-1]
z2 <- zwroty[zwroty$Data <= as.Date("2020-06-30") & zwroty$Data >= as.Date("2020-03-01"),-1]
z3 <- zwroty[zwroty$Data <= as.Date("2021-12-31") & zwroty$Data >= as.Date("2020-07-01"),-1]

cor1 <- cor(z1)
cor2 <- cor(z2)
cor3 <- cor(z3)

#Wojna na Ukrainie

y1 <- zwroty[zwroty$Data < as.Date("2022-02-24") & zwroty$Data >= as.Date("2021-01-01"),-1]
y2 <- zwroty[zwroty$Data <= as.Date("2022-03-24") & zwroty$Data >= as.Date("2022-02-24"),-1]
y3 <- zwroty[zwroty$Data <= as.Date("2024-12-31") & zwroty$Data >= as.Date("2022-03-25"),-1]

cor4 <- cor(y1)
cor5 <- cor(y2)
cor6 <- cor(y3)

