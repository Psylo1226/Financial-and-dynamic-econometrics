library("tidyverse")
library("moments")
library("tseries")
library("forecast")
izo <- read.csv("izo_d.csv")

#Stopa logarytmiczna
st_l <- function(a,b) {
  return(log(a/b))
}

dane <- izo[,c(1,5)]
indeksy <- dane %>% map_df(rev)

dt <- c()
for (i in 1:(nrow(indeksy)-1)) {
  dt <- c(dt,as.numeric(st_l(indeksy[i,2],indeksy[i+1,2])))
}
stopy <- rev(tail(dt, -4))
ceny <- head(log(dane[2]), -4)

stopy_oct <- rev(head(dt,4))
ceny_oct <- tail(log(dane[2]), 4)

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
  Odłożone_Wartości = unname(stopy_oct),
  Początek_Przedziału = forecast(model,4)$lower[, 2],
  Koniec_Przedziału = forecast(model,4)$upper[, 2],
  Średnia_Prognoza = forecast(model,4)$mean
)

#Model dla logarytmów cen
model2 <- auto.arima(ceny)
summary(model2)

#Porównanie z odłożonymi wartościami
ceny_wyniki <- data.frame(
  Odłożone_Wartości = unname(ceny_oct),
  Początek_Przedziału = forecast(model2,4)$lower[, 2],
  Koniec_Przedziału = forecast(model2,4)$upper[, 2],
  Średnia_Prognoza = forecast(model2,4)$mean
)

exp(prognoza_cen)



