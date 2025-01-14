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

colnames(merged) <- c("Date","Time","11B","ATT","TPE")
merged$Time <- merged$Time/100

stopy <- list()
for (i in seq(from = 5, to = 30, by = 5)) {
  minutes <- seq(from = 905, to = 1645, by = i)

  stopy[[i / 5]] <- merged[merged$Time %in% minutes, ] %>%
    group_by(Date) %>%
    reframe(across(c(`11B`, ATT, TPE), ~ diff(log(.)))) %>%
    ungroup()
  stopy[[i / 5]] <- stopy[[i / 5]][-1]
}

c5 <- cor(stopy[[1]], method = "spearman")
c10 <- cor(stopy[[2]], method = "spearman")
c15 <- cor(stopy[[3]], method = "spearman")
c20 <- cor(stopy[[4]], method = "spearman")
c25 <- cor(stopy[[5]], method = "spearman")
c30 <- cor(stopy[[6]], method = "spearman")




