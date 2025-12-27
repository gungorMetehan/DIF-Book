# downloading and installing the difR package
install.packages("difR")
library(difR)

# importing the dataset
library(readr)
dataDIF <- read_csv("dataDIF.csv")
View(dataDIF)

# 1. Angoff's TID method
TIDStats <- difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1)
TIDStats

## original plot
plot(difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1))

## reproducing the plot from the book
### defining the values on the x and y axes
perp <- TIDStats$Dist 
items <- 1:20
### drawing the plot
plot(items, perp, ylim = c(-3, 3), xaxt = "n", ylab = "Perpendicular Distances", xlab = "Items",
     col = ifelse(1:20 == 2 | 1:20 == 16 | 1:20 == 18 | 1:20 == 19, "red", "black"))
abline(h = -1.5, col = "blue") 
abline(h = 1.5, col = "blue") 
axis(1, at = seq(1, 20))
text(2, -2.12, "I2", cex = .8)
text(16, 1.85, "I16", cex = .8)
text(18, -2.24, "I18", cex = .8)
text(19, 2.50, "I19", cex = .8)

## original plot (delta)
plot(difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1), plot = "delta")

## conducting analyses based on three different purification methods
TIDStats_IPP1 <- difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = T, purType = "IPP1")
TIDStats_IPP2 <- difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = T, purType = "IPP2", thrTID = "norm")
TIDStats_IPP3 <- difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = T, purType = "IPP3", thrTID = "norm")

## original plots for the three different purification methods
par(mfcol = c(3, 1))
plot(difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = T, purType = "IPP1"), plot = "delta")
plot(difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = T, purType = "IPP2", thrTID = "norm"), plot = "delta")
plot(difTID(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = T, purType = "IPP3", thrTID = "norm"), plot = "delta")

dev.off()

# 2. Breslow-Day
## test of homogeneous association (Aguerri et al., 2009)
BDStats_Aguerri <- difBD(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1)

### original plot
plot(difBD(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1))

### plot from the book
#### defining the values on the x and y axes
BDStats <- BDStats_Aguerri$BD[, 1]
items <- 1:20
#### drawing the plot
plot(items, BDStats, ylim = c(0, 60), xaxt = "n", ylab = "BD", xlab = "Items",
     col = ifelse(1:20 == 17, "red", "black"))
axis(1, at = seq(1, 20))
text(17, 54.88, "I17", cex = .8)

## trend test statistic (Penfield, 2003)
BDStats_Penfield <- difBD(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, BDstat = "trend")

### original plot
plot(difBD(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, BDstat = "trend"))

### plot from the book
#### defining the values on the x and y axes
BDStats_p <- BDStats_Penfield$BD[, 1]
items <- 1:20
#### drawing the plot
plot(items, BDStats_p, ylim = c(0, 30), xaxt = "n", ylab = "BD", xlab = "Items",
     col = ifelse(1:20 == 7 | 1:20 == 10 | 1:20 == 17 | 1:20 == 18, "red", "black"))
abline(h = 3.8415, col = "blue") 
axis(1, at = seq(1, 20))
text(7, 8.03, "I7", cex = .8)
text(10, 15.52, "I10", cex = .8)
text(17, 29.51, "I17", cex = .8)
text(18, 6.52, "I8", cex = .8)

### Breslow-Day (Aguerri et al., 2009) - purified & Breslow-Day (Penfield, 2003) - purified

#### Breslow-Day (Aguerri et al., 2009) - purified
BDStats_Aguerri_IPP <- difBD(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = TRUE)

##### original plot - Aguerri et al., 2009
plot(difBD(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = TRUE))

#### Breslow-Day (Penfield, 2003) - purified
BDStats_Penfield_IPP <- difBD(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, BDstat = "trend", purify = TRUE)

##### original plot - Penfield, 2003
plot(difBD(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, BDstat = "trend", purify = TRUE))

###### plots from the book
dev.new()
par(mfrow = c(2, 1), mar = c(4, 4, 2, 2))

###### plot from the book - Aguerri et al. 2009
####### defining the values on the x and y axes
BDStatsIPP <- BDStats_Aguerri_IPP$BD[, 1]
items <- 1:20
####### drawing the plot
plot(items, BDStatsIPP, ylim = c(0, 50), xaxt = "n", ylab = "BD", xlab = "Items",
     col = ifelse(1:20 == 10 | 1:20 == 12 | 1:20 == 16 | 1:20 == 17, "red", "black"))
axis(1, at = seq(1, 20))
text(10, 27.13, "I10", cex = .8)
text(12, 34.05, "I12", cex = .8)
text(16, 31.12, "I16", cex = .8)
text(17, 50.20, "I17", cex = .8)
mtext("a)", side = 3, adj = 0, line = 0.3, cex = 1.2)

###### plot from the book - Penfield, 2003
####### defining the values on the x and y axes
BDStatsIPP_p <- BDStats_Penfield_IPP$BD[, 1]
items <- 1:20
####### drawing the plot
plot(items, BDStatsIPP_p, ylim = c(0, 30), xaxt = "n", ylab = "BD", xlab = "Items",
     col = ifelse(1:20 == 7 | 1:20 == 10 | 1:20 == 17 | 1:20 == 18, "red", "black"))
abline(h = 3.8415, col = "blue")
axis(1, at = seq(1, 20))
text(7, 7.13, "I7", cex = .8)
text(10, 15.48, "I10", cex = .8)
text(17, 28.87, "I17", cex = .8)
text(18, 5.46, "I18", cex = .8)
mtext("b)", side = 3, adj = 0, line = 0.3, cex = 1.2)

# 3. Standardization
StdStats <- difStd(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1)

## original plot
plot(difStd(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1))

## plot from the book
### defining the values on the x and y axes
STDPDIF <- StdStats$PDIF 
items <- 1:20
### drawing the plot
plot(items, STDPDIF, ylim = c(-1, 1), xaxt="n", ylab = "STD P-DIF", xlab = "Items",
     col = ifelse(1:20 == 2 | 1:20 == 16 | 1:20 == 18 | 1:20 == 19, "red", "black"))
abline(h = -.1, col = "blue") 
abline(h = .1, col = "blue") 
axis(1, at = seq(1, 20))
text(2, -.35, "I2", cex = .8)
text(16, .32, "I16", cex = .8)
text(18, -.36, "I18", cex = .8)
text(19, .41, "I19", cex = .8)

## Standardization - purified
StdStats_Purify <- difStd(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = TRUE)

### original plot - purified
plot(difStd(Data = dataDIF[1:20], group = dataDIF$group1, focal.name = 1, purify = TRUE))

### plot from the book
#### defining the values on the x and y axes
STDPDIF_IPP <- StdStats_Purify$PDIF 
items <- 1:20
#### drawing the plot
plot(items, STDPDIF_IPP, ylim = c(-1, 1), xaxt = "n", ylab = "STD P-DIF", xlab = "Items",
     col = ifelse(1:20 == 2 | 1:20 == 16 | 1:20 == 18 | 1:20 == 19, "red", "black"))
abline(h = -.1, col = "blue") 
abline(h = .1, col = "blue") 
axis(1, at = seq(1, 20))
text(2, -.33, "I2", cex = .8)
text(16, .30, "I6", cex = .8)
text(18, -.35, "I18", cex = .8)
text(19, .39, "I19", cex = .8)