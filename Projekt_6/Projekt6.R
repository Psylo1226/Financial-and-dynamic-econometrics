library(dplyr)

llb <- read.csv("11b.txt")
att <- read.csv("att.txt")
tpe <- read.csv("tpe.txt")

llb <- llb[llb$X.TIME. != 90000,c(3,4,8)]
att <- att[att$X.TIME. != 90000,c(3,4,8)]
tpe <- tpe[tpe$X.TIME. != 90000,c(3,4,8)]

merged <- llb %>%
  inner_join(att, by = c("X.DATE.","X.TIME.")) %>%
  inner_join(tpe, by = c("X.DATE.","X.TIME."))

merged$Time <- merged$Time/100

colnames(merged) <- c("Date","Time","11B","ATT","TPE")

min10 <- seq(from=905,to=1645,by=10)
min15 <- seq(from=905,to=1645,by=15)
min20 <- seq(from=905,to=1645,by=20)
min25 <- seq(from=905,to=1645,by=25)
min30 <- seq(from=905,to=1645,by=30)

stopy5 <- apply(merged[,c(3:5)], 2, function(x) diff(log(x)))
stopy10 <- apply(merged[merged$Time %in% min10,c(3:5)], 2, function(x) diff(log(x)))
stopy15 <- apply(merged[merged$Time %in% min15,c(3:5)], 2, function(x) diff(log(x)))
stopy20 <- apply(merged[merged$Time %in% min20,c(3:5)], 2, function(x) diff(log(x)))
stopy25 <- apply(merged[merged$Time %in% min25,c(3:5)], 2, function(x) diff(log(x)))
stopy30 <- apply(merged[merged$Time %in% min30,c(3:5)], 2, function(x) diff(log(x)))

c5 <- cor(stopy5, method = "spearman")
c10 <- cor(stopy10, method = "spearman")
c15 <- cor(stopy10, method = "spearman")
c20 <- cor(stopy10, method = "spearman")
c25 <- cor(stopy10, method = "spearman")
c30 <- cor(stopy10, method = "spearman")




