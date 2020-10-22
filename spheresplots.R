
library('sphereplot')
library('rgl')


#add transparent sphere and axes 
sphereplot_trans <- function(x, col = NULL) {
    
    if ( is.null(col) )  col <- rep(1, dim(x)[1])
    rgl::open3d()
    y1 <- x[, 1]
    y2 <- x[, 2]
    y3 <- x[, 3]
    rgl::points3d(y1, y2, y3, col = col, radius = 1)
    rgl::spheres3d(0, 0, 0, lit = FALSE, alpha=0.5 , color = "white")
    rgl::spheres3d(0, 0, 0, radius = 1, lit = FALSE, alpha=0.5 , color = "black", front = "lines")
    
    
    grid_xyz<-seq(-1,1,length=21)
    grid3d("x")
    grid3d("y", at = list(x=pretty(grid_xyz, n = 4)))
    grid3d("z", at = list(x=pretty(grid_xyz, n = 4)))
    
    axes3d("x", at = pretty(grid_xyz, n = 4), color = "black")
    axes3d("y", at = pretty(grid_xyz, n = 4), color = "black")
    axes3d("z", at = pretty(grid_xyz, n = 4), color = "black")
    title3d(xlab ='x', ylab = 'y', zlab = 'z', color = "black")
}

#rgl.snapshot("sphere.png")