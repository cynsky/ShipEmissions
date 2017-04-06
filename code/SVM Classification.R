#svm
library('data.table')
library('e1071')
library('dplyr')

file='C:/Users/wzh/Dropbox/data/terminals_labelled.csv'
ships=fread('D:/Git/Rprojects/basedata/ships.csv');
#head(ships);dim(ships)
#dt=data.table(left_join(sp,ships[,list(mmsi,speed,grosston)],'mmsi'));head(sp1)
file2='D:/Git/Rprojects/ShipEmissions/data/terminals_labelled.csv'
dt0=fread(file2)
tt=dt0[lon<124&lon>105&lat>19&lat<41][(uniques>=10)&(N>=20)];dim(tt);plot(tt$lon,tt$lat)
dt=fread(file);

dt=dt[(uniques>=10)&(N>=20)];head(dt);dim(dt);setkey(dt,fcid)
set.seed(1000)
traindt=sample_frac(dt,0.19);dim(traindt)
#traindt=tt;dim(traindt);dim(traindt)
testdt=data.table(anti_join(dt,traindt[,list(fcid)],'fcid'));dim(testdt)

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

#svm_tune <- tune(svm, train.x=x, train.y=y, 
#                 kernel="radial", ranges=list(cost=(seq(16,64,4)), gamma=seq(0.01,0.1,0.01)))
svm_tune <- tune(svm, train.x=x, train.y=y,kernel="radial", ranges=list(cost=(seq(0.1,1,0.1)), gamma=seq(0,0.2,0.01)))

print(svm_tune)

#svm_model_after_tune <- svm(x,y, kernel="radial", cost=0.8, gamma=0.07)
svm_model_after_tune <- svm(x,y, kernel="radial", cost=1, gamma=0.05)
summary(svm_model_after_tune)
pred <- predict(svm_model_after_tune,x)
system.time(predict(svm_model_after_tune,x))
t=table(pred,y);t
precision=t[2,2]*100/(t[2,1]+t[2,2]);precision

pred <- predict(svm_model_after_tune,x1)
system.time(predict(svm_model_after_tune,x1))

t=table(pred,y1);t
precision=t[2,2]*100/(t[2,1]+t[2,2]);precision

TF=t[1,1];FN=t[1,2];FP=t[2,1];TP=t[2,2]
ACC=(TP+TF)/(TF+FN+FP+TP);ACC
F1=2*TP/(2*TP+FP+FN);F1
precision=TP/(TP+FP);precision
recall=TP/(TP+FN);recall


# svm for another methed

#file2='D:/Git/Rprojects/ShipEmissions/data/individual_ship_based/candidates_terminals_label.csv'
file3='D:/Git/Rprojects/ShipEmissions/data/individual_ship_based/eps0.002minp5/candidates_terminals_label.csv'


dt=fread(file3);dim(dt);dim(dt[label==1])
dt=dt[(shipNum>=10)&(tripNum>=20)];head(dt);dim(dt);setkey(dt,sid);dim(dt[label==1])
set.seed(10001)
traindt=sample_frac(dt,0.33);dim(traindt)
testdt=data.table(anti_join(dt,traindt[,list(sid)],'sid'))

#x <- traindt[,list(fcid,uniques,mean,median,sd,N,norm,tripRate)]
#x <- traindt[,list(shipNum,tripNum,meanShipSpeed,meanGrosston,meanDur,medianDur,sdDur,tripShipRatio,durGap,normRatio)]
x <- traindt[,list(shipNum,tripNum,meanDur,medianDur,sdDur,tripShipRatio,normRatio)]
y <- as.factor(traindt$label)
#x1 <- testdt[,list(fcid,uniques,mean,median,sd,N,norm,tripRate)]
x1 <- testdt[,list(shipNum,tripNum,meanDur,medianDur,sdDur,tripShipRatio,normRatio)]
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
                 kernel="radial", ranges=list(cost=(seq(0.1,2,0.2)), gamma=seq(0.01, 0.2,0.02)))
print(svm_tune)

svm_model_after_tune <- svm(x,y, kernel="radial", cost=1.9, gamma=0.09)
summary(svm_model_after_tune)
pred <- predict(svm_model_after_tune,x)
system.time(predict(svm_model_after_tune,x))
t=table(pred,y);t
precision=t[2,2]*100/(t[2,1]+t[2,2]);precision

pred <- predict(svm_model_after_tune,x1)
system.time(predict(svm_model_after_tune,x1))

t=table(pred,y1);t
precision=t[2,2]*100/(t[2,1]+t[2,2]);precision


TF=t[1,1];FN=t[1,2];FP=t[2,1];TP=t[2,2]
ACC=(TP+TF)/(TF+FN+FP+TP);ACC
F1=2*TP/(2*TP+FP+FN);F1
precision=TP/(TP+FP);precision
recall=TP/(TP+FN);recall
