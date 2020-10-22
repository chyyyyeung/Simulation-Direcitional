library("mvtnorm")
library("Directional")


rvMF_Wood<-function(n,kappa,p)
{
  if(kappa>0){
    
    #Step1
    b_0 <- (-2*kappa + sqrt(4*kappa^2+(p-1)^2)) / (p-1)
    x_0 <- (1-b_0) / (1+b_0)
    c <- kappa*x_0+(p-1)*log(1-x_0^2)
    
    X <- matrix(data = NA,nrow = n,ncol = p)
    for (i in 1:n) {
      
      while(TRUE){
        Z <- rbeta(n = 1, shape1 = (p-1)/2,shape2 = (p-1)/2)
        U <- runif(n = 1, min = 0,max = 1)
        W <- (1-(1+b_0)*Z) / (1-(1-b_0)*Z)
        criterion = kappa*W + (p-1)*log(1-x_0*W) - c
        if(criterion >= log(U)){
          break;
        }
      }
      
      #uniform distribution
      Y <- rmvnorm(mean=rep(0,p-1),sigma = diag(p-1),n = 1)
      Y.norm <- sqrt(apply(X =Y^2,MARGIN = 1,FUN = sum))
      V <- Y/Y.norm
      #step4
      X[i,] <- c(sqrt(1-W^2)*V,W)
    }
  }
  else
  {
    #reduced to Uniform
    S <- rmvnorm(mean=rep(0,p),sigma = diag(p),n = n)
    S.norm <- sqrt(apply(X =Y^2,MARGIN = 1,FUN = sum))
    V <- S/S.norm
    X = V
  }
  X
}

set.seed(42)
X<-rvMF_Wood(n=200,kappa=10,p=3)

sphereplot(X, col = "RED")




rvMF_Wood_rotation<-function(n,mu,kappa)
{
  #mu is the mean direction
  #kappa is the concerntration parameter
  #n is the size of simulation
  if(kappa>0){
    p<-length(mu) #dimension
    mu<-mu/sqrt(sum(mu^2)) #mean
    R<-rotation(c(rep(0,p-1),1),mu) #rotation matrix
    
    #Step1
    b_0<-(-2*kappa+sqrt(4*kappa^2+(p-1)^2))/(p-1)
    x_0<-(1-b_0)/(1+b_0)
    c<-kappa*x_0+(p-1)*log(1-x_0^2)
    
    X<-matrix(data = NA,nrow = n,ncol = p)
    for (i in 1:n) {
      acc<-FALSE
      #Step2
      while(acc == FALSE){
        Z<-rbeta(n = 1,shape1 = (p-1)/2,shape2 = (p-1)/2)
        U<-runif(n = 1,min = 0,max = 1)
        W<-(1-(1+b_0)*Z)/(1-(1-b_0)*Z)
        
        #step3
        if(kappa*W+(p-1)*log(1-x_0*W)-c>=log(U)){acc <- TRUE}#if not, go to Step2
        
      }
      #uniform distribution
      Y<-rmvnorm(mean=rep(0,p-1),sigma = diag(p-1),n = 1)
      Y.norm<-sqrt(apply(X =Y^2,MARGIN = 1,FUN = sum))
      V<-Y/Y.norm
      #step4
      X[i,]<-cbind(sqrt(1-W^2)*V,W)
    }
    X<-tcrossprod(x = X,y = R)# apply rotation R to all of the random vectors
  }
  else
  {
    #reduced to Uniform
    S<-rmvnorm(mean=rep(0,p),sigma = diag(p),n = n)
    S.norm<-sqrt(apply(X =Y^2,MARGIN = 1,FUN = sum))
    X<-S/S.norm
  }
  
  X
}
X<-rvMF_Wood_rotation(n=200,mu = c(3,1,4),kappa = 10)
sphereplot(X,col="red")

