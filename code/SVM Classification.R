#svm
library('data.table')
library('e1071')
library('dplyr')

file='C:/Users/zhihuan/Dropbox/data/terminals_labelled.csv'
dt=fread(file);
dt=dt[(uniques>5)&(N>10)];head(dt);dim(dt);setkey(dt,fcid)
set.seed(1000)
traindt=sample_frac(dt,0.66);dim(traindt)
testdt=data.table(anti_join(dt,traindt[,list(fcid)],'fcid'))

x <- traindt[,list(fcid,uniques,mean,median,sd,N,norm,tripRate)]
y <- as.factor(traindt$label)
x1 <- testdt[,list(fcid,uniques,mean,median,sd,N,norm,tripRate)]
y1 <- as.factor(testdt$label)

svm_model1 <- svm(x,y)
summary(svm_model1)

pred <- predict(svm_model1,x)
system.time(pred <- predict(svm_model1,x))

table(pred,y)

# use the original model
pred <- predict(svm_model1,x1)
system.time(pred <- predict(svm_model1,x1))

table(pred,y1)

#tune
svm_tune <- tune(svm, train.x=x, train.y=y, 
                 kernel="radial", ranges=list(cost=2^(1:6), gamma=seq(0.01,1,0.02)))
print(svm_tune)

svm_model_after_tune <- svm(x,y, kernel="radial", cost=16, gamma=0.11)
summary(svm_model_after_tune)
pred <- predict(svm_model_after_tune,x)
system.time(predict(svm_model_after_tune,x))
table(pred,y)

pred <- predict(svm_model_after_tune,x1)
system.time(predict(svm_model_after_tune,x1))

table(pred,y1)