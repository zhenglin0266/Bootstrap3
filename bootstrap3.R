count<-rep(0,100)
for(g in 1:100){
  for(k in 1:100){
    ##Get the sample data##
    n<-400
    x<-runif(n)
    d<-rpois(1, 2)
    a<-rnorm(d+1)
    delta<-rexp(1, 10)
    epx<-rnorm(n, 0, sqrt(delta))
    y<-rep(0, n)
    for(i in 1: n){
      lin<-0
      if(d>0){
        for(j in 1: (d+1)){
          lin<-lin+a[j]*x[i]^(j-1)
        }
        y[i]<-lin+epx[i]
      }else{
        y[i]<-epx[i]
      }
    }  
    
    ##fit with the first 200 observations##
    if(d<=10){
      xx<-matrix(rep(1, n), n, 1)
      for(i in 1: 10){
        xx<-cbind(xx,I(x^i))
      }
      err<-rep(0, 11)
      for(j in 1:11){
        betahat<- ginv(t(xx[1: 200, 1: j])%*%xx[1: 200, 1: j])%*%t(xx[1: 200, 1: j])%*%y[1: 200]
        yhat<-xx[201: 400, 1: j]%*%betahat
        err[j]<-sum((y[201: 400]-yhat)^2)
      }
      
      ##Estimate d##
      dhat<-order(err)[1]-1
      if(dhat==d){
        count[g]<- count[g] + 1
      }
    }
  }
}
hist(count)
mean(count)
##From the histogram and mean, we can see that among the 100 simulations,
##the correct model rate is around 35/100.