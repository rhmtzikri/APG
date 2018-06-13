#x<-c(0,1,4,6,1,3,6,1,35,6,1,7,3,1,7,32,7,42)
x<-rnorm(100)
x<-sort(x,decreasing = FALSE)
qq<-c()
length(x)
normq<-c()


for (i in 1:length(x)){
  q<-(i-0.5)/length(x)
  norm<-qnorm(q,0,1)
  normq<-c(normq,norm)
  qq<-c(qq,q)
}
plot(normq,x)
qqnorm(x)
