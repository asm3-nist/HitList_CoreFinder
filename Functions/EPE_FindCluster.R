#EPE find the cluster based on the "overlap" matrix (some edits by arun)
find.cluster <- function(overlap,epsilon){

   overlap.dist = as.dist(1 - overlap) # this isn't a true distance matrix, rather a dissimilarity matrix,
                                       # but all the 1 values in overlap are represented as 0s in overlap.dist (which is what I think you want)
                     
   overlap.db <- dbscan(overlap.dist, eps = epsilon, minPts = 2) 
   clusterResults = overlap.db$cluster
   
   # ORIGINALLY OUTPUT THE CLUSTER WITH THE MOST ELEMENT: EDIT BY ARUN
   if(FALSE){
      q <- find.mode(clusterResults[clusterResults>0])  # arun: want to find mode of cluster results, not of indices
      x <- which(overlap.db$cluster == q)
      return(x) 
   }
   
   # NOW OUTPUT THE ENTIRE CLUSTERRING RESULTS: EDIT BY ARUN
   return(clusterResults)   

   } 