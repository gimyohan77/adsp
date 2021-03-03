#reshape
airquality
head(airquality)
a = airquality
library(reshape)
#원데이터 형태로 만들기
melt(a, id=c("Month", "Day"), na.rm = T)
aqm = melt(a, id=c("Month", "Day"), na.rm = T)


#요약 형태로 만들기
cast(aqm, Day ~ Month ~ variable)

#sql처럼 이용하기
library(sqldf)
sqldf("select * from iris")

#데이터 가공
library(ggplot2)
data(diamonds)
diamonds
dia = diamonds
head(dia)
summary(dia)

#변수중요도
library(klaR)
library(HDclassif)
data(wine)
head(wine)

#wilks lambda = 집단내분산/총분산
w_ob = greedy.wilks(class~. , data = wine, niveau = 0.1) #niveau = 유의수준
w_ob

impo = wine[, c("V7", "V10", "class")]
plineplot(class~., data = impo, method = "lda", x = impo$V7, xlab = "V7")


#결측값 인식

complete.cases(Cars93)
is.na(Cars93)
c1 = centralImputation(Cars93)
c2 = knnImputation(Cars93)
c2

install.packages("Amelia")
library(Amelia)
#amelia() 시계열자료에 사용

#상자그림으로 이상값 찾기
boxplot(Cars93$Price)


