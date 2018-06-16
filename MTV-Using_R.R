##Multivariate Using R



##Multivariate distribution
#1
sigma <- cbind(c(10,3),c(3,2))
dt <- mvrnorm(1000,c(0,1),sigma)
meandt <- colMeans(dt)
adt <- t(dt) - meandt
s1dt <- sum(adt[1,]^2)/999
s2dt <- sum(adt[2,]^2)/999
bdt<-t(adt)
s12dt <- sum(bdt[,1]*bdt[,2])/999
r12dt <- s12dt/(sqrt(s1dt)*sqrt(s2dt))
r12dt
var(dt)
cor(dt)
plot(dt)

#2
sigam <- matrix(c(10,0,0,4),2)
dat <- rmvnorm(1000,c(0,1),sigam)
meandat <- colMeans(dat)
adat <- t(dat) - meandat
s1dat <- sum(adat[1,]^2)/999
s2dat <- sum(adat[2,]^2)/999
tdat <- t(adat)
s12dat <- sum(tdat[,1]*tdat[,2])/999
vardat <- matrix(c(s1dat,s12dat,s12dat,s2dat),2)
matrix(c(s1dat,s12dat,s12dat,s2dat),2) == var(dat) 

r12dat <- s12dat/(sqrt(s1dat)*sqrt(s2dat))
matrix(c(1,rep(r12dat,2),1),2) 
cor(dat)

plot(dat)


#3
siag <- matrix(c(10,4,4,2),2,2)
hih <- rmvnorm(1000,c(0,1),siag)
plot(hih,xlab="X",ylab = "Y")
cor(hih)

#4
sieh <- matrix(c(5,4,4,4),2,2)
hgih <- rmvnorm(1000,c(0,1),sieh)
cor(hgih)
plot(hgih)



#BIVARIATE NORMAL DISTRIBUTION

#intial parameter
mu1 <- 0
mu2 <- 0.5
sig1 <- 0.5
sig2 <- 2
rho <- 0.5

#initial point sample
xm <- -3
xp <- 3
ym <- -3
yp <- 3

#distribution
x <- seq(xm, xp, length= as.integer((xp + abs(xm)) * 10))
y <- seq(ym, yp, length= as.integer((yp + abs(ym)) * 10))

#function of bivariate distribution n length
bivariate <- function(x,y) {
  term1 <- 1 / (2*pi*sig1*sig2*sqrt(1-rho^2))
  term2 <- (x-mu1)^2/sig1^2
  term3 <- -(2*rho*(x-mu1)*(y-mu2))/(sig1*sig2)
  term4 <- (y-mu2)^2/sig2^2
  z <- term2+term3+term4
  term5 <- term1 * exp((-z/(2*(1-rho^2))))
}

#3rd dimension of bivariate
z<-outer(x,y,bivariate)

#plot the bivariate normal distribution
persp(x,y,z,main="Bivariate Normal Dist.", sub=bquote(bold(mu[1])==.(mu1)~", "~sigma[1]==.(sig1)~", "~mu[2]==.(mu2)~","~sigma[2]==.(sig2)~","~rho==.(rho)),col="orchid2",theta = 55,phi = 30,r=40
      ,d=0.1, expand = 0.5, ltheta = 90,lphi = 180,shade = 0.4,ticktype = "detailed",nticks = 5)




#detecting outlier
v1<-matrix(c(108.28,152.36,95.04,65.45,62.97,263.99,265.19,285.06,92.01,165.68), ncol=1)
v2<-matrix(c(17.05,16.59,10.91,14.14,9.52,25.33,18.54,15.73,8.10,11.13),ncol=1)
Y<-cbind(v1,v2)
chisqplot <- function(X){
  xbar <- matrix(c(colMeans(X)),ncol = 1)
  sinvers <- solve(cov(X))
  
  dj2 <- matrix(nrow = nrow(X))
  quantil <- matrix(nrow = nrow(X))
  
  for (i in 1:nrow(X)){
    dj2[i] <- t(X[i,]-xbar)%*%sinvers%*%(X[i,]-xbar)
    quantil[i] <- qchisq((nrow(X)-i+0.5)/nrow(X), df=2)
  }
  
  dj2 <- sort(dj2, decreasing = FALSE)
  quantil <- sort(quantil,decreasing = F)
  
  plot(quantil,dj2,pch=16,col=2)
}

chisqplot(Y)
