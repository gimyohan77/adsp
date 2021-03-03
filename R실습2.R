#상관분석
iris
x = iris$Sepal.Length
z = iris$Sepal.Width
#분산
var(x, y = NULL, na.rm = T)

#공분산
iris.s = iris
iris.s$Species = NULL
library(MASS)
cov(x, z, use = "everything", method = c("pearson", "kendall", "spearman"))

#상관관계
cor(iris.s, y = NULL, use = "everything", method = "pearson")
install.packages("Hmisc")
library(Hmisc)
#rcorr(matrix(iris.s), type = "pearson")

data(mtcars)
mtcars
a = mtcars$mpg
b = mtcars$hp
cor(a,b)
cov(a,b)
cor.test(a, b, method = "pearson")

#회귀분석
#qqplot 
c = cbind(a,b)
qqnorm(c)
qqline(c)

x = c(19, 23, 26, 92, 30, 38, 39, 46, 59)
y = c(33, 51, 40, 49, 50, 69, 70, 64, 89)
z = cbind(x, y)
z
lm(y~x)
summary(lm(y~x))

head(Cars93)
attach(Cars93)
l = lm(Price ~ EngineSize + RPM + Weight, data = Cars93)
summary(l)

#로지스틱 회귀분석
library(boot)
data(nodal)
a = c(2, 4, 6, 7)
data = nodal[,a]
g = glm(r~., data = data, family = "binomial")
summary(g)

#최적회귀방정식 선택(변수선택법)
re = lm(Price~EngineSize + RPM + Weight + MPG.city + MPG.highway, data = Cars93)
summary(re)

step(re, direction = "forward")
step(re, dircetion = "backward")
step(re, direction = "both")

#시계열 ARIMA분석
library(tseries)
library(forecast)
library(TTR)
king = scan("http://robjhyndman.com/tsdldata/misc/kings.dat", skip = 3)
king
king.ts = ts(king)
king.ts
plot.ts(king.ts)

king3 = SMA(king.ts, n=3)
plot.ts(king3)
king8 = SMA(king.ts, n=8)
plot.ts(king8)

king.ff1 = diff(king.ts, differences = 1) #차분
plot.ts(king.ff1)
#ACF와 PACF를 통한 적합한 ARIMA모델 결정
acf(king.ff1, lag.max = 20)
acf(king.ff1, lag.max = 20, plot = F)

pacf(king.ff1, lag.max = 20)
pacf(king.ff1, lag.max = 20, plot = F)

auto.arima(king)
king.arima = arima(king, order = c(0, 1, 1))
king.forecast = forecast(king.arima)
king.forecast


#다차원척도법

#계량적 MDS
library(MASS)
loc = cmdscale(eurodist)
x = loc[,1]
y = -loc[,2]
plot(x,y, type = "n", asp = 1, main = "Metric MDS")
text(x, y, rownames(loc), cex = 0.7)
abline(v = 0, h = 0, lty = 2, lwd = 0.5)

#비계량적 MDS
#isoMDS
data(swiss)
swiss.x = as.matrix(swiss[, -1])
swiss.dist = dist(swiss.x)
swiss.mds = isoMDS(swiss.dist)
plot(swiss.mds$points, type = "n")
text(swiss.mds$points, labels = as.character(1:nrow(swiss.x)))
abline(v=0, h=0, lty = 2, lwd = 0.5)

#sammon
swiss.sammon = sammon(dist(swiss.x))
plot(swiss.sammon$points, type = "n")
text(swiss.sammon$points, labels = as.character(1:nrow(swiss.x)))

#주성분분석
library(datasets)
data(USArrests)
pairs(USArrests, pane = panel.smooth, main = "USArrests data")
us = princomp(USArrests, cor = T)
summary(us)
screeplot(us, npcs = 4, type = "lines")
loadings(us)
us$scores

arrests.pca = prcomp(USArrests, center = T, scale. = T)
biplot(arrests.pca, scale = 0)
