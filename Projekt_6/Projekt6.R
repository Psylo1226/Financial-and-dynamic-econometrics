library(dplyr)
library(reshape2)

att <- read.csv("att.txt")
tpe <- read.csv("tpe.txt")
mbk <- read.csv("mbk.txt")

att <- att[,c(3,4,8)]
tpe <- tpe[,c(3,4,8)]
mbk <- mbk[,c(3,4,8)]

merged <- att %>%
  inner_join(tpe, by = c("X.DATE.","X.TIME.")) %>%
  inner_join(mbk, by = c("X.DATE.","X.TIME."))

colnames(merged) <- c("Date","Time","ATT","TPE","MBK")
merged$Time <- merged$Time/100

toRemove <- c(900,905,910,1650,1655,1700)
merged <- merged[!merged$Time %in% toRemove,]

plot(1:nrow(merged),scale(merged$ATT),type="l",col="blue")
lines(scale(merged$TPE),col="red")
lines(scale(merged$MBK),col="green")

typeof(merged$ATT)

stopy <- list()
for (i in seq(from = 5, to = 30, by = 5)) {
  minutes <- seq(from = 905, to = 1645, by = i)
  stopy[[i / 5]] <- merged[merged$Time %in% minutes, ] %>%
    group_by(Date) %>%
    reframe(across(c(ATT, TPE, MBK), ~ diff(log(.)))) %>%
    ungroup()
  stopy[[i / 5]] <- stopy[[i / 5]][-1]
}

plot(1:nrow(stopy[[1]][1]),unlist(stopy[[1]][1]),type="l",col="blue")
lines(unlist(stopy[[1]][2]),col="red")
lines(unlist(stopy[[1]][3]),col="green")



c5 <- cor(stopy[[1]], method = "spearman")
c10 <- cor(stopy[[2]], method = "spearman")
c15 <- cor(stopy[[3]], method = "spearman")
c20 <- cor(stopy[[4]], method = "spearman")
c25 <- cor(stopy[[5]], method = "spearman")
c30 <- cor(stopy[[6]], method = "spearman")

m5 <- melt(c5)

ggplot(m5, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = round(value,3)), color = "white", size = 4)

