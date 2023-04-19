library(kernlab)
data=read.csv("./turkiye-student-evaluation_R_Specific.csv")

TopBestAccuracy=0

ans=c(c("Sample","Accuracy","Kernel","Epsilon","Nu"))
lol=data.frame(Sample=c(),Accuracy=c(),Kernel=c(),Epsilon=c(),Nu=c())
for(j in 1:10){
  sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
  trainDataset  <- data[sample, ]
  testDataset   <- data[!sample, ]
  bestAccuracy=0
  bestKernel=""
  bestNu=0
  bestEpsilon=0
  iteration=100
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
  if(bestAccuracy>TopBestAccuracy){
    lol=x
  }
  ans=rbind(ans,c(j,bestAccuracy,bestKernel,round(bestEpsilon,3),round(bestNu,3)))
}
print(ans)
write.csv(ans, "./result.csv", row.names=FALSE)
Accuracy=lol
png(file="./plot.png",
    width=1500, height=700)
plot(Accuracy,type='l')
dev.off()



