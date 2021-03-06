############
library(plot3D)
library(Directional)


#This is an example of plotting Fisher density by using spheresurf3D
###2d grid of spherical cordinated system
###increase res for higher resolution

res<-100
M  <- mesh(seq(0, 2*pi, length.out = res), 
           seq(0,   pi, length.out = res))
###longtitude and latitude
u  <- M$x 
v  <- M$y
###eclidient transform
x <- cos(u)*sin(v)
y <- sin(u)*sin(v)
z <- cos(v)

###calculate the von-Mises fisher density

grid<-data.frame(x = as.vector(x),y = as.vector(y), z = as.vector(z))

#### need to convert the density vector to matrix
colvar <- vmf.density(y = grid, k = 0.1, mu = c(0.8,-1,0))
colvar <- matrix(colvar , nrow = res,ncol = res)

surf3D(x = x,y = y,z = z,col =  ramp.col(col = c("navy","cadetblue2", "yellow","orange","red"), n = 1000),
       colvar = colvar, phi = 10,theta = 45, bty = "b2",ticktype = "detailed", 
       lighting = FALSE, ltheta = 40,border = NA,facets = TRUE, resfac = 3, 
       clim = c(min(colvar),max(max(colvar),20*min(colvar))))

png(filename = "Figures/VMF_density_0.1.png",width = 900,height = 700)
surf3D(x = x,y = y,z = z,col =  ramp.col(col = c("navy","cadetblue2", "yellow","orange","red"), n = 1000),
       colvar = colvar, phi = 10,theta = 45, bty = "b2",ticktype = "detailed", 
       lighting = FALSE, ltheta = 40,border = NA,facets = TRUE, resfac = 3, 
       clim = c(min(colvar),max(max(colvar),3*min(colvar))))
dev.off()

##########there is a unknown reason makes a reverse image.
spheresurf3D(col =  ramp.col(col = c("navy","cadetblue2", "yellow","orange","red"), n = 50),
             colvar = colvar, phi = 10,theta = -45, bty = "b2",ticktype = "detailed", 
             lighting = FALSE, ltheta = 40,border = NA,facets = TRUE, resfac = 2, panel.first = TRUE)
##########
