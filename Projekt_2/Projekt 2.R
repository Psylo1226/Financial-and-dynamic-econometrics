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
prognoza <- head(indeksy,4)
dane <- tail(indeksy,-4)

szereg <- c()
for (i in 1:(nrow(dane)-1)) {
  szereg <- c(szereg_l,as.numeric(st_l(dane[i,2],dane[i+1,2])))
}

plot(szereg, type='l')

#Test ADF
adf.test(szereg)
#Bardzo niskie p-value -> szereg stacjonarny

#ARMA
model <- auto.arima(szereg)
summary(model)
#Zaproponowany model: ARMA(2,2)

#Reszty modelu
res <- residuals(model)
Box.test(res, type = "Ljung-Box")
#P-value większe od 0.05 -> brak aurokorelacji
jarque.bera.test(res)
#P-value mniejsze od 0.05 -> reszty nie mają rozkładu normalnego
qqnorm(res)



