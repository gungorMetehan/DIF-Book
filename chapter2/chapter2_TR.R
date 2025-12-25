# difR paketini indirme ve kurma
install.packages("difR")
library(difR)

# veri setini içe aktarma
library(readr)
dataDIF <- read_csv("dataDIF.csv")
View(dataDIF)

# 1. Angoff'un DMG yöntemi
TIDStats <- difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1)
TIDStats

## orijinal grafik
plot(difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1))

## kitaptaki grafi€in oluﬂturulmas›
### x ve y eksenlerindeki de€erlerin belirlenmesi
perp <- TIDStats$Dist 
items <- 1:20
### grafi€in çizdirilmesi
plot(items, perp, ylim = c(-3, 3), xaxt = "n", ylab = "Dik Uzakl›klar", xlab = "Maddeler",
     col = ifelse(1:20 == 2 | 1:20 == 16 | 1:20 == 18 | 1:20 == 19, "red", "black"))
abline(h = -1.5, col = "blue") 
abline(h = 1.5, col = "blue") 
axis(1, at = seq(1, 20))
text(2, -2.12, "M2", cex = .8)
text(16, 1.85, "M16", cex = .8)
text(18, -2.24, "M18", cex = .8)
text(19, 2.50, "M19", cex = .8)

## orijinal grafik (delta)
plot(difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1), plot = "delta")

## kitaptaki grafik
plot(difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1), plot = "delta", xlab = "Referans Grup", ylab = "Odak Grup")

## üç farkl› saflaﬂt›rma yöntemine göre analizlerin gerçekleﬂtirilmesi
TIDStats_IPP1 <- difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = T, purType = "IPP1")
TIDStats_IPP2 <- difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = T, purType = "IPP2", thrTID = "norm")
TIDStats_IPP3 <- difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = T, purType = "IPP3", thrTID = "norm")

## üç farkl› saflaﬂt›rma yönteminin orijinal grafikleri
par(mfcol = c(3, 1))
plot(difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = T, purType = "IPP1"), plot = "delta")
plot(difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = T, purType = "IPP2", thrTID = "norm"), plot = "delta")
plot(difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = T, purType = "IPP3", thrTID = "norm"), plot = "delta")

# kitaptaki grafikler (üç farkl› saflaﬂt›rma yöntemi için)
par(mfcol = c(3, 1))
plot(difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = T, purType = "IPP1"), plot = "delta",
     xlab = "Referans Grup", ylab = "Odak Grup")
plot(difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = T, purType = "IPP2", thrTID = "norm"), plot = "delta",
     xlab = "Referans Grup", ylab = "Odak Grup")
plot(difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = T, purType = "IPP3", thrTID = "norm"), plot = "delta",
     xlab = "Referans Grup", ylab = "Odak Grup")

dev.off()

# 2. Breslow-Day
## homojen iliﬂkinin testi (Aguerri vd, 2009)
BDStats_Aguerri <- difBD(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1)

### orijinal grafik
plot(difBD(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1))

### kitaptaki grafik
#### x ve y eksenlerindeki de€erlerin belirlenmesi
BDStats <- BDStats_Aguerri$BD[, 1]
items <- 1:20
#### grafi€in çizdirilmesi
plot(items, BDStats, ylim = c(0, 60), xaxt = "n", ylab = "BD", xlab = "Maddeler",
     col = ifelse(1:20 == 17, "red", "black"))
axis(1, at = seq(1, 20))
text(17, 54.88, "M17", cex = .8)

## e€ilim testi istatisti€i (Penfield, 2003)
BDStats_Penfield <- difBD(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, BDstat = "trend")

### orijinal grafik
plot(difBD(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, BDstat = "trend"))

### kitaptaki grafik
#### x ve y eksenlerindeki de€erlerin belirlenmesi
BDStats_p <- BDStats_Penfield$BD[, 1]
items <- 1:20
#### grafi€in çizdirilmesi
plot(items, BDStats_p, ylim = c(0, 30), xaxt = "n", ylab = "BD", xlab = "Maddeler",
     col = ifelse(1:20 == 7 | 1:20 == 10 | 1:20 == 17 | 1:20 == 18, "red", "black"))
abline(h = 3.8415, col = "blue") 
axis(1, at = seq(1, 20))
text(7, 8.03, "M7", cex = .8)
text(10, 15.52, "M10", cex = .8)
text(17, 29.51, "M17", cex = .8)
text(18, 6.52, "M8", cex = .8)

### Breslow-Day (Aguerri et vd., 2009) - saflaﬂt›rmal› & Breslow-Day (Penfield, 2003) - saflaﬂt›rmal›

#### Breslow-Day (Aguerri vd., 2009) - saflaﬂt›rmal›
BDStats_Aguerri_IPP <- difBD(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = TRUE)

##### orijinal grafik - Aguerri vd., 2009
plot(difBD(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = TRUE))

#### Breslow-Day (Penfield, 2003) - saflaﬂt›rmal›
BDStats_Penfield_IPP <- difBD(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, BDstat = "trend", purify = TRUE)

##### orijinal grafik - Penfield, 2003
plot(difBD(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, BDstat = "trend", purify = TRUE))

###### kitaptaki grafikler
dev.new()
par(mfrow = c(2, 1), mar = c(4, 4, 2, 2))

###### kitaptaki grafik - Aguerri vd. 2009
####### x ve y eksenlerindeki de€erlerin belirlenmesi
BDStatsIPP <- BDStats_Aguerri_IPP$BD[, 1]
items <- 1:20
####### grafi€in çizdirilmesi
plot(items, BDStatsIPP, ylim = c(0, 50), xaxt = "n", ylab = "BD", xlab = "Maddeler",
     col = ifelse(1:20 == 10 | 1:20 == 12 | 1:20 == 16 | 1:20 == 17, "red", "black"))
axis(1, at = seq(1, 20))
text(10, 27.13, "M10", cex = .8)
text(12, 34.05, "M12", cex = .8)
text(16, 31.12, "M16", cex = .8)
text(17, 50.20, "M17", cex = .8)
mtext("a)", side = 3, adj = 0, line = 0.3, cex = 1.2)

###### kitaptaki grafik - Penfield, 2003
####### x ve y eksenlerindeki de€erlerin belirlenmesi
BDStatsIPP_p <- BDStats_Penfield_IPP$BD[, 1]
items <- 1:20
####### grafi€in çizdirilmesi
plot(items, BDStatsIPP_p, ylim = c(0, 30), xaxt = "n", ylab = "BD", xlab = "Maddeler",
     col = ifelse(1:20 == 7 | 1:20 == 10 | 1:20 == 17 | 1:20 == 18, "red", "black"))
abline(h = 3.8415, col = "blue")
axis(1, at = seq(1, 20))
text(7, 7.13, "M7", cex = .8)
text(10, 15.48, "M10", cex = .8)
text(17, 28.87, "M17", cex = .8)
text(18, 5.46, "M18", cex = .8)
mtext("b)", side = 3, adj = 0, line = 0.3, cex = 1.2)

# 3. Standardizasyon
StdStats <- difStd(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1)

## orijinal grafik
plot(difStd(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1))

## kitaptaki grafik
### x ve y eksenlerindeki de€erlerin belirlenmesi
STDPDIF <- StdStats$PDIF 
items <- 1:20
### grafi€in çizdirilmesi
plot(items, STDPDIF, ylim = c(-1, 1), xaxt="n", ylab = "STD P-DIF", xlab = "Maddeler",
     col = ifelse(1:20 == 2 | 1:20 == 16 | 1:20 == 18 | 1:20 == 19, "red", "black"))
abline(h = -.1, col = "blue") 
abline(h = .1, col = "blue") 
axis(1, at = seq(1, 20))
text(2, -.35, "M2", cex = .8)
text(16, .32, "M16", cex = .8)
text(18, -.36, "M18", cex = .8)
text(19, .41, "M19", cex = .8)

## Standardization - saflaﬂt›rmal›
StdStats_Purify <- difStd(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = TRUE)

### orijinal grafik - saflaﬂt›rmal›
plot(difStd(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = TRUE))

### kitaptaki grafik
#### x ve y eksenlerindeki de€erlerin belirlenmesi
STDPDIF_IPP <- StdStats_Purify$PDIF 
items <- 1:20
#### grafi€in çizdirilmesi
plot(items, STDPDIF_IPP, ylim = c(-1, 1), xaxt = "n", ylab = "STD P-DIF", xlab = "Maddeler",
     col = ifelse(1:20 == 2 | 1:20 == 16 | 1:20 == 18 | 1:20 == 19, "red", "black"))
abline(h = -.1, col = "blue") 
abline(h = .1, col = "blue") 
axis(1, at = seq(1, 20))
text(2, -.33, "mM2", cex = .8)
text(16, .30, "M6", cex = .8)
text(18, -.35, "M18", cex = .8)
text(19, .39, "M19", cex = .8)