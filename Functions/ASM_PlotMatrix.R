mat.plotter <- function(x.sim){
   # x.sim is a similarity matrix
   x.dist = as.dist(1-x.sim)
   fit <- cmdscale(x.dist)
   plot(fit)

   cluster = find.cluster(x.sim)
   matplot(fit[cluster,1],fit[cluster,2],col="blue",pch=19,add=TRUE)
   return(0)
}