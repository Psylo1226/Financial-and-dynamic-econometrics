library(dplyr)
library(reshape2)
library(ggplot2)

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

zwroty <- merged %>%
  dplyr::mutate(
    across(
      .cols = c(WIG, DAX, CAC, MOEX),
      .fns = ~ c(NA, diff(log(.)))
    )
  )

zwroty <- zwroty[-1,]
zwroty$Data <- as.Date(zwroty$Data)

#Covid

z1 <- zwroty[zwroty$Data < as.Date("2020-01-01"),-1]
z2 <- zwroty[zwroty$Data <= as.Date("2020-06-30") & zwroty$Data >= as.Date("2020-01-01"),-1]
z3 <- zwroty[zwroty$Data <= as.Date("2021-12-31") & zwroty$Data >= as.Date("2020-07-01"),-1]

cor1 <- cor(z1)
cor2 <- cor(z2)
cor3 <- cor(z3)

#Wojna na Ukrainie

y1 <- zwroty[zwroty$Data < as.Date("2022-02-24") & zwroty$Data >= as.Date("2021-01-01"),-1]
y2 <- zwroty[zwroty$Data <= as.Date("2022-05-31") & zwroty$Data >= as.Date("2022-02-24"),-1]
y3 <- zwroty[zwroty$Data <= as.Date("2024-12-31") & zwroty$Data >= as.Date("2022-06-01"),-1]

cor4 <- cor(y1)
cor5 <- cor(y2)
cor6 <- cor(y3)

