##Fisher density
##
library(Directional)
library(rgl)

library(ggplot2)     # For most of our plotting
library(cowplot)     # grid arrangement of plots
library(Directional) # For spherical density functions
library(maps)        # vector maps of the world
library(hrbrthemes)  # hrbrmstr themes
library(magick)      # For animation
library(mapproj)     # Needed for projection
#########

euclid(c(pi,pi))


# And set some theme defaults
theme_set(theme_ipsum())
# Axis settings we'll reuse a lot
no.axis <- theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(),
                 axis.ticks.x = element_blank(), axis.text.x = element_blank(),
                 axis.title.x = element_blank(), axis.title.y = element_blank())




############
random_points <- function(n_points, lat, lon, concentration) {
    # Directional defines lat + long as 0-180 and 0-360 respectively so we
    # have to shift back and forth
    mu <- euclid(c(lat + 90, lon + 180))[1,]
    pts <- euclid.inv(rvmf(n_points, mu, concentration))
    pts[,1] <- pts[,1] - 90
    pts[,2] <- pts[,2] - 180
    data.frame(pts)
}



offset.pos <- list(Lat = 75, Long = 175)
positions.center <- random_points(1000, 0, 0, 10)
positions.offset <- random_points(1000, offset.pos$Lat, offset.pos$Long, 10)
plot.colors <- hcl(h = c(0:3)*90, c = 50 , l = 70)
g.base <- ggplot(positions.center, aes(x = Long, y = Lat)) +
    scale_y_continuous(breaks = (-2:2) * 30, limits = c(-90, 90)) +
    scale_x_continuous(breaks = (-4:4) * 45, limits = c(-180, 180)) +
    coord_map()

g.broken <- g.base +
    # The centered random points
    geom_density_2d(color = plot.colors[1]) +
    geom_point(size = 0.5, stroke = 0, color = plot.colors[1]) +
    # The offset random points
    geom_density_2d(data = positions.offset, color = plot.colors[2]) +
    geom_point(data = positions.offset, size = 0.5, stroke = 0,
               color = plot.colors[2])

ortho.projections <- plot_grid(
    g.broken + coord_map("ortho", orientation = c(0, 0, 0)) + no.axis,
    g.broken + coord_map("ortho", orientation = c(offset.pos$Lat, offset.pos$Long, 0))
    + no.axis,
    labels = NULL,
    align = 'h')
g.broken
ortho.projections

############spherical density

vmf_density_grid <- function(u, ngrid = 100) {
    # Translate to (0,180) and (0,360)
    u[,1] <- u[,1] + 90
    u[,2] <- u[,2] + 180
    res <- vmf.kerncontour(u, thumb = "none", den.ret = T, full = T,
                           ngrid = ngrid)
    
    # Translate back to (-90, 90) and (-180, 180) and create a grid of
    # coordinates
    ret <- expand.grid(Lat = res$lat - 90, Long = res$long - 180)
    ret$Density <- c(res$den)
    ret
}


densities.center <- vmf_density_grid(positions.center)
densities.offset <- vmf_density_grid(positions.offset)

g.broken <- g.base +
    geom_density_2d(color = plot.colors[1], alpha = .5) +
    geom_point(size = 0.5, stroke = 0, color = plot.colors[1], alpha = .5) +
    geom_density_2d(data = positions.offset, color = plot.colors[2], alpha = .5) +
    geom_point(data = positions.offset, size = 0.5, stroke = 0, color =
                   plot.colors[2], alpha = .5)

g.densities <- g.broken +
    geom_contour(data = densities.center,
                 aes(x=Long, y=Lat, z=Density),
                 color = plot.colors[3]) +
    geom_contour(data = densities.offset,
                 aes(x=Long, y=Lat, z=Density),
                 color = plot.colors[4])

ortho.projections <- plot_grid(
    g.densities + coord_map("ortho", orientation = c(0, 0, 0)) + no.axis,
    g.densities + coord_map("ortho",
                            orientation = c(offset.pos$Lat, offset.pos$Long, 0))
    + no.axis,
    labels = NULL,
    align = 'h')
g.densities
ortho.projections



head(Smoking)
rownames(Smoking) = Smoking$Country

Init3d(family = 'serif', cex = 1.5)
Plot3d( LE ~ CigCon + HealthExpPC | Continent, Smoking)
Axes3d()
Identify3d(pad=1)

?spheresurf3D()


dvmf<-function(x,y){vmf}
rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
    if( new.device | rgl.cur() == 0 ) {
        rgl.open()
        par3d(windowRect = 50 + c( 0, 0, width, width ) )
        rgl.bg(color = bg )
    }
    rgl.clear(type = c("shapes", "bboxdeco"))
    rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
}
rgl_add_axes <- function(x, y, z, axis.col = "grey",
                         xlab = "", ylab="", zlab="", show.plane = TRUE, 
                         show.bbox = FALSE, bbox.col = c("#333377","black"))
{ 
    
    lim <- function(x){c(-max(abs(x)), max(abs(x))) * 1.1}
    # Add axes
    xlim <- lim(x); ylim <- lim(y); zlim <- lim(z)
    rgl.lines(xlim, c(0, 0), c(0, 0), color = axis.col)
    rgl.lines(c(0, 0), ylim, c(0, 0), color = axis.col)
    rgl.lines(c(0, 0), c(0, 0), zlim, color = axis.col)
    
    # Add a point at the end of each axes to specify the direction
    axes <- rbind(c(xlim[2], 0, 0), c(0, ylim[2], 0), 
                  c(0, 0, zlim[2]))
    rgl.points(axes, color = axis.col, size = 3)
    
    # Add axis labels
    rgl.texts(axes, text = c(xlab, ylab, zlab), color = axis.col,
              adj = c(0.5, -0.8), size = 2)
    
    # Add plane
    if(show.plane) 
        xlim <- xlim/1.1; zlim <- zlim /1.1
    rgl.quads( x = rep(xlim, each = 2), y = c(0, 0, 0, 0),
               z = c(zlim[1], zlim[2], zlim[2], zlim[1]))
    
    # Add bounding box decoration
    if(show.bbox){
        rgl.bbox(color=c(bbox.col[1],bbox.col[2]), alpha = 0.5, 
                 emission=bbox.col[1], specular=bbox.col[1], shininess=5, 
                 xlen = 3, ylen = 3, zlen = 3) 
    }
}


library(plot3D)
?vmf.kerncontour
rgl_init()
rgl.spheres(x, y, z, r = 0.2, color = "#D95F02") 
rgl_add_axes(x, y, z, show.bbox = F)
aspect3d(1,1,1)
rgl.material(color = "blue")
identify3d(x, y, z, labels = rownames(iris), n = 5)
x
lon<-seq(from = 0,to = pi,length.out = 100)
lar<-seq(from = 0,to = 2*pi,length.out = 200)

z<-
sphere3d()


?spheres3d



f2 <- function(x, y) x + y
persp3d(f2)

