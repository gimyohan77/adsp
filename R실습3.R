#ROCR(성과분석)
library(rpart)
library(party)
library(ROCR)
k = kyphosis

x = k[sample(1:nrow(k), nrow(k), replace = F), ]
x.train = k[1:floor(nrow(X)*0.75)]
x.eval = k[floor(nrow(x) * 0.75):nrow(x),]
x.model = cforest(Kyphosis ~ Age+Number+Start, data = x.train)
x.eval$prediction = predict(x.model, newdata = x.eval)
x.eval$correct = x.eval$prediction == x.eval$Kyphosis
print(paste("% of predicted classification correct", mean(x.eval$correct)))
x.eval$proba = 1 - unlist(treeresponse(x.model,
                                     newdata = x.eval),
                        use.names = F)[seq(1, nrow(x.eval)*2, 2)]

pred = prediction(x.eval$proba, x.eval$Kyphosis)
perf = performance(pred, "tpr", "fpr")
plot(perf, main = "ROC", colorize = T)

perf = performance(pred, "litf", "rpp")
 plot(perf, main = "lift", colorize = T)

#로지스틱 회귀분석
a = iris[iris$Species == "setosa" || iris$Species == "versicolor",]
b = glm(Species ~ Sepal.Length, data = a, family = binomial)
summary(b)

pairs(iris)

#의사결정나무
library(party)
idx = sample(2, nrow(iris), replace =T, prob = c(0.7, 0.3))
train.data = iris[idx==2,]
test.data = iris[idx ==1,]
iris.tree = ctree(Species~., data = train.data)
plot(iris.tree)

table(predict(iris.tree), train.data$Species)
test.pre = predict(iris.tree, newdata = test.data)
mean(test.pre == test.data$Species)
table(test.pre, test.data$Species)

#랜덤포레스트
idx = sample(2, nrow(iris), replace = T, prob=c(0.7, 0.3))
train.data = iris[idx==2,]
test.data = iris[idx==1,]
r.f = randomForest(Species~., data = train.data, ntree = 100, proximity = T)

table(predict(r.f), train.data$Species)
test.rf = predict(r.f, newdata = test.data)
mean(test.rf == test.data$Species)

plot(r.f)
varImpPlot(r.f)

#군집분석
idx = sample(1:dim(iris)[1], 40)
iris.s = iris[idx,]
iris.s$Species = NULL
hc = hclust(dist(iris.s), method = "ave")
plot(hc, hang = -1, labels=iris$Species[idx])

data(iris)
niris = iris
niris$Species = NULL
kc = kmeans(niris, 3)
table(iris$Species, kc$cluster)

plot(niris[c("Sepal.Length", "Sepal.Width")], col = kc$cluster)

#연관분석
library(arules)
data(Groceries)
g = Groceries
rules = apriori(g, parameter = list(support = 0.01, confidence = 0.3))
inspect(sort(rules, by=c("lift"), decreasing = T)[1:20])
