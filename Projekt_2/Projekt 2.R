library("tidyverse")
library("moments")
library("tseries")
library("forecast")
izo <- read.csv("izo_d.csv")

#Stopa logarytmiczna
st_l <- function(a,b) {
  return(log(a/b))
}

indeksy <- izo[,c(1,5)]
indeksy <- indeksy %>% map_df(rev)
prognoza_stopy <- head(indeksy,4)
dane <- tail(indeksy,-4)

stopy <- c()
for (i in 1:(nrow(dane)-1)) {
  stopy <- c(stopy,as.numeric(st_l(dane[i,2],dane[i+1,2])))
}

ceny <- log(dane[2])

#Test ADF
adf.test(stopy)
#Bardzo niskie p-value -> szereg stacjonarny

#ARMA
model <- auto.arima(stopy)
summary(model)
#Zaproponowany model: ARMA(2,2)

#Reszty modelu
res <- residuals(model)
Box.test(res, type = "Ljung-Box")
#P-value większe od 0.05 -> brak aurokorelacji
jarque.bera.test(res)
#P-value mniejsze od 0.05 -> reszty nie mają rozkładu normalnego
qqnorm(res)

#Prognoza logarytmicznych stóp dla 4 przyszłych notowań
forecast(model,4)$mean

