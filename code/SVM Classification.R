#svm
library('data.table')
library('e1071')
library('dplyr')

file='C:/Users/zhihuan/Dropbox/data/terminals_labelled.csv'
dt=fread(file);
dt=dt[(uniques>=5)&(N>=20)];head(dt);dim(dt);setkey(dt,fcid)
set.seed(1000)
traindt=sample_frac(dt,0.33);dim(traindt)
testdt=data.table(anti_join(dt,traindt[,list(fcid)],'fcid'))

#x <- traindt[,list(fcid,uniques,mean,median,sd,N,norm,tripRate)]
x <- traindt[,list(uniques,mean,median,N,norm,tripRate)]
y <- as.factor(traindt$label)
#x1 <- testdt[,list(fcid,uniques,mean,median,sd,N,norm,tripRate)]
x1 <- testdt[,list(uniques,mean,median,N,norm,tripRate)]
y1 <- as.factor(testdt$label)

svm_model1 <- svm(x,y)
summary(svm_model1)

pred <- predict(svm_model1,x)
system.time(pred <- predict(svm_model1,x))

t=table(pred,y);t
precision=t[2,2]*100/(t[2,1]+t[2,2]);precision
# use the original model
pred <- predict(svm_model1,x1)
system.time(pred <- predict(svm_model1,x1))

t=table(pred,y1);t
precision=t[2,2]*100/(t[2,1]+t[2,2]);precision
#tune
svm_tune <- tune(svm, train.x=x, train.y=y, 
                 kernel="radial", ranges=list(cost=(seq(16,64,4)), gamma=seq(0.01,0.1,0.01)))
print(svm_tune)

svm_model_after_tune <- svm(x,y, kernel="radial", cost=60, gamma=0.05)
summary(svm_model_after_tune)
pred <- predict(svm_model_after_tune,x)
system.time(predict(svm_model_after_tune,x))
t=table(pred,y);t
precision=t[2,2]*100/(t[2,1]+t[2,2]);precision

pred <- predict(svm_model_after_tune,x1)
system.time(predict(svm_model_after_tune,x1))

t=table(pred,y1);t
precision=t[2,2]*100/(t[2,1]+t[2,2]);precision

#------------logistic regression
