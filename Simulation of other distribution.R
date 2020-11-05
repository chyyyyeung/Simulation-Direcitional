library("Directional")
library("mvtnorm")
library("circular")
library(ggplot2)
#######################
#this file is intent to simulate some other directional models introduced in the thesis for plotting
###############################
#sample size
n<-200


#Density function
##Wrapped Normal
x<-seq(-pi,pi,by = pi/100)
Y_1<-dwrappednormal( circular(x) , mu = circular(0), rho=0.8)
Y_2<-dwrappednormal( circular(x) , mu = circular(0), rho=0.5)
Y_3<-dwrappednormal( circular(x) , mu = circular(0), rho=0.1)
plot(x,Y_1,type = "l", lty = 2 , lwd = 1 , xlim = c(-pi, pi),
     xaxt = "n",xlab = "",ylab = "density",
     main = "Density of Wrapped Normal distribution")
axis(side=1,at=seq(-pi,pi,by = pi/4),label = c(expression(-pi),expression(-3*pi/4),expression(-pi/2),expression(-pi/4),"0",expression(pi/4),expression(pi/2),expression(3*pi/4),expression(pi)))
lines(x = x,y = Y_2,type = "l", lty = 3 , lwd = 1 , col = "red")
lines(x = x,y = Y_3,type = "l", lty = 4 , lwd = 1 , col = "blue")
#Add a legend to the plot and set legend lty
legend("topleft", legend = c("0.8", "0.5","0.1"),
       col = c("black","red", "blue"), lty = 2:4, cex = 0.8)

#circlar curve
#pi 0.8
png(filename = "Figures/WN_pi_0.8.png",width = 900,height = 700)
ff <- function(x){ dwrappednormal( x , mu = circular(pi), rho=0.8)}
curve.circular(ff, join=TRUE, col="blue", shrink=1.1, lwd=2)
#main= expression(paste("Density of a Wrapped Normal Distribution " , mu ,"=", pi, " , ", rho , "=0.5")))
dev.off()
#pi 0.5
png(filename = "Figures/WN_pi_0.5.png",width = 900,height = 700)
ff <- function(x){ dwrappednormal( x , mu = circular(pi), rho=0.5)}
curve.circular(ff, join=TRUE, col="blue", shrink=1.1, lwd=2)
               #main= expression(paste("Density of a Wrapped Normal Distribution " , mu ,"=", pi, " , ", rho , "=0.5")))
dev.off()

#pi 0.1
png(filename = "Figures/WN_pi_0.1.png",width = 900,height = 700)
ff <- function(x){ dwrappednormal( x , mu = circular(pi), rho=0.1)}
curve.circular(ff, join=TRUE, col="blue", shrink=1.1, lwd=2)
#main= expression(paste("Density of a Wrapped Normal Distribution " , mu ,"=", pi, " , ", rho , "=0.5")))
dev.off()
#0 0.9
png(filename = "Figures/WN_0_0.9.png",width = 900,height = 700)
ff <- function(x){ dwrappednormal( x , mu = circular(0), rho=0.9)}
curve.circular(ff, join=TRUE, col="blue", xlim = c(-1,1.5), shrink=1.1, lwd=2)
#main= expression(paste("Density of a Wrapped Normal Distribution " , mu ,"=", pi, " , ", rho , "=0.5")))
dev.off()
##





#von Mises

x<-seq(-pi,pi,by = pi/1000)
Y_1<-dvonmises( circular(x) , mu = circular(0), kappa = 0.1 )
Y_2<-dvonmises( circular(x) , mu = circular(0), kappa = 1 )
Y_3<-dvonmises( circular(x) , mu = circular(0), kappa = 5 )
Y_4<-dvonmises( circular(x) , mu = circular(0), kappa = 8 )

dvonmises( 0 , mu = circular(0), kappa = 0.1)

plot(x,Y_4,type = "l", lty = 4 , lwd = 1 , xlim = c(-pi, pi),
     xaxt = "n",yaxt = "n" , xlab = "",ylab = "" , frame.plot = FALSE)
axis(side=1,pos = 0,tck = 0.01, las = 0 , at=seq(-pi,pi,by = pi/4),label = c(expression(-pi),expression(-3*pi/4),expression(-pi/2),expression(-pi/4),"0",expression(pi/4),expression(pi/2),expression(3*pi/4),expression(pi)))

axis(2, pos = 0 ,at = seq(0,1.2,0.1),las = 2, tck =0.01)   
lines(x = x,y = Y_2,type = "l", lty = 2 , lwd = 1 )
lines(x = x,y = Y_3,type = "l", lty = 3 , lwd = 1 )
lines(x = x,y = Y_1,type = "l", lty = 1 , lwd = 1 )
#Add a legend to the plot and set legend lty
legend("topleft", legend = c("0.1", "1","5","8"),
        lty = 1:4, cex = 0.8)
?curve
ff <- function(x){ dvonmises( x , mu = circular(0), kappa = 8)}
curve(expr = ff, col="blue", from = -pi,to = pi, n = 1001, add = FALSE,
      type = "l", xname = "x")
ff <- function(x){ dvonmises( x , mu = circular(0), kappa = 5)}
curve(expr = ff, col="blue", from = -pi,to = pi, n = 1001, add = TRUE,
      type = "l", xname = "x")
ff <- function(x){ dvonmises( x , mu = circular(0), kappa = 1)}
curve(expr = ff, col="blue", from = -pi,to = pi, n = 1001, add = TRUE,
      type = "l", xname = "x")
ff <- function(x){ dvonmises( x , mu = circular(0), kappa = 0.1)}
curve(expr = ff, col="blue", from = -pi,to = pi, n = 1001, add = TRUE,
      type = "l", xname = "x")
##compare of two density

#A(kappa)
A<-function(kappa){
    up<-besselI(x = kappa,nu = 1)
    down<-besselI(x = kappa,nu = 0)
    up/down
}
##turns out A is not necessary to define, circular.A1 works the same way

png(filename = "Figures/VM_compare_10.png",width = 900,height = 561)
ff <- function(x){ dvonmises( x , mu = circular(pi), kappa = 10)}
curve.circular(ff, join=TRUE, col="#D6604D",xlim = c(-2,1), shrink=1, lwd=2)
ff <- function(x){ dwrappednormal( x , mu = circular(pi), rho=A(10))}
curve.circular(ff, join=TRUE, col="#92C5DE" , shrink=1, lwd=2,add = T)
legend("topleft",c("Wrapped Normal","von-Mises"), col=c("#D6604D", "#92C5DE"), lwd=3, bty = "n",cex = 2)
dev.off()
png(filename = "Figures/VM_compare_2.png",width = 900,height = 561)
ff <- function(x){ dvonmises( x , mu = circular(pi), kappa = 2)}
curve.circular(ff, join=TRUE, col="#D6604D",xlim = c(-2,1), shrink=1, lwd=2)
ff <- function(x){ dwrappednormal( x , mu = circular(pi), rho=A(2))}
curve.circular(ff, join=TRUE, col="#92C5DE" , shrink=1, lwd=2,add = T)
legend("topleft",c("Wrapped Normal","von-Mises"), col=c("#D6604D", "#92C5DE"), lwd=3, bty = "n",cex = 2)
dev.off()

#wrapped cauchy

for(i in 4:4){
    png(filename = paste("Figures/WC_pi6_",i,".png",sep = ""),width = 900,height = 700)
    ff <- function(x){ dwrappedcauchy( x , mu = circular(pi), rho=0.1+i*0.2)}
    curve.circular(ff, join=TRUE, col=cm.colors(4)[1],xlim = c(-pi,1), shrink=1.1, lwd=2)
    #main= expression(paste("Density of a Wrapped Normal Distribution " , mu ,"=", pi, " , ", rho , "=0.5")))
    dev.off()
}
?cm.colors
###Projected Normal
##unimodal
png(filename ="Figures/PN_unimodal.png",width = 900,height = 700)
ff <- function(x){ dpnorm ( x , mu = c(1,1),sigma= diag(2))}
curve.circular(ff, join=TRUE, col=hcl.colors(2,alpha = 0.8)[2],ylim = c(-0.9,1.3), shrink=1, lwd=2)
dev.off()
##bimodal
png(filename ="Figures/PN_bimodal.png",width = 900,height = 700)
ff <- function(x){ dpnorm ( x , mu = c(-0.5,1),sigma= matrix(c(1,0.8,0.8,1),2,2))}
curve.circular(ff, join=TRUE, col=hcl.colors(2,alpha = 0.8)[2],ylim = c(-0.9,1.4), shrink=1, lwd=2)
dev.off()
##antimodal
png(filename ="Figures/PN_antimodal.png",width = 900,height = 700)
ff <- function(x){ dpnorm ( x , mu = c(0,1),sigma= matrix(c(1,0,0,7),2,2))}
curve.circular(ff, join=TRUE, col=hcl.colors(2,alpha = 0.8)[2],ylim = c(-1.1,1.5), shrink=1, lwd=2)
dev.off()

png(filename ="Figures/Acg.png",width = 900,height = 700)
ff <- function(x){ dpnorm ( x , mu = c(0,0),sigma= matrix(c(4,2,2,4),2,2))}
curve.circular(ff, join=TRUE, col=hcl.colors(4,alpha = 0.8)[2],ylim = c(-1.1,1.5), shrink=1, lwd=2)
dev.off()




ff <- function(x){ dpnorm ( x , mu = c(1,1,1),sigma= diag(3))}
curve.circular(ff, join=TRUE, col=hcl.colors(2,alpha = 0.8)[2],ylim = c(-0.9,1.3), shrink=1, lwd=2)


