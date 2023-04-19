library(kernlab)
data=read_csv("turkiye-student-evaluation_R_Specific.csv")
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
trainDataset  <- data[sample, ]
testDataset   <- data[!sample, ]
bestAccuracy=0
bestKernal=""
bestNu=0
bestEpsilon=0
iteration=1000
kernelList=c('rbfdot','polydot','vanilladot','tanhdot','laplacedot','anovadot')
fitnessFunction<-function(k,n,e){
  model<-ksvm(as.matrix(trainDataset[,1:32]),trainDataset[,33], kernel=k,nu=n,epsilon=e,kpar=list())
  predicted=round(predict(model,testDataset[,1:32]))
  actual=testDataset[,33]
  accuracy=round(mean(actual==predicted)*100,2)
  return (accuracy)
}


x=(0)
for(i in 1:iteration){
  print(i)
  k=sample(kernelList,1)
  n=runif(1)
  e=runif(1)
  Accuracy=fitnessFunction(k,n,e)
  x=append(x,Accuracy)
  if(Accuracy>bestAccuracy){
    bestKernel=k
    bestNu=n
    bestEpsilon=e
    bestAccuracy=Accuracy
  }
}
print(bestAccuracy)
print(bestKernel)
print(bestEpsilon)
print(bestNu)
plot(z,type='l')


