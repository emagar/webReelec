#setwd("/home/eric/Desktop/data/elecs/MXelsCalendGovt/reelec/encItam")

dat <- read.csv("new.csv", stringsAsFactor = FALSE, header = TRUE)
colnames(dat)

# quita espacios terminales
dat$w1 <- gsub(pattern = " +$", replacement = "", dat$w1)

# reconstruye 2a y 3a palabra en misma observación
tmp <- c(dat$w1[2:nrow(dat)], NA); dat$tmp2 <- tmp # lag1
tmp <- c(dat$w1[3:nrow(dat)], NA, NA); dat$tmp3 <- tmp # lag2
sel <- which(dat$rep==3) #tercera
sel.n <- dat$n[sel]
sel <- which(dat$n %in% sel.n & dat$rep==1)
dat$w3[sel] <- dat$tmp3[sel]
#
sel <- which(dat$rep==2) #segunda
sel.n <- dat$n[sel]
sel <- which(dat$n %in% sel.n & dat$rep==1)
dat$w2[sel] <- dat$tmp2[sel]
#
dat$tmp2 <- dat$tmp3 <- NULL # limpieza

# duplica datos con 1a, 2a y 3a resupestas separadas
dat123 <- dat[dat$rep==1,]; dat123$rep <- NULL

# datos con todas respuestas en mismo vector
dat$word <- dat$w1
dat$orden <- "0"
dat$orden[which(dat$rep==1)] <- "1a"
dat$orden[which(dat$rep==2)] <- "2a"
dat$orden[which(dat$rep==3)] <- "3a"
dat$rep <- dat$w1 <- dat$w2 <- dat$w3 <- NULL

# positivo/negativo
dat$eval <- dat$eva
dat$eval[dat$eval=="-"] <- "neutro"

# datos agregados con frecuencia
dat.freq <- dat
dat.freq$freq <- ave(dat.freq$n, as.factor(dat.freq$word), FUN=function(x) length(x))
dat.freq <- dat.freq[duplicated(dat$word)==FALSE,]

dat.freq$freq

install.packages("slam")
library(wordcloud)
wordcloud(words = dat.freq$word, freq = dat.freq$freq, max.words = 149, random.order = FALSE)

wordcloud(words = dat.freq$word, freq = dat.freq$freq, max.words = 25, random.order = FALSE)


wordcloud(words = dat.freq$word, freq = dat.freq$freq, max.words = 25, random.order = FALSE, ordered.colors = TRUE, colors = c("#ef3b2c","#bdbdbd","#4292c6")[factor(dat.freq$eval)])

