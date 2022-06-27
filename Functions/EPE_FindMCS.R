#EPE function to find mcs: x is vector of positions for cluster, overlap is the overlap matrix, sdf of hitlist 
# can be multiple distinct MCS but takes the first one with most rings (some comments by arun)
find.mcs <- function(x, overlap, sdf){
   small <- overlap[x,x] # subset of overlap matrix based on identified cluster
   small.sdf <- sdf[x,x] # subset of sdf set based on identified cluster
   
   t <- NULL
   for (i in 1:length(x)){
      t[i] <- which.min(small[,i])   # finding the structure that produces the 
      t                              # smallest overlap coefficient for every column in the cluster   
   }
   
   
      p <- NULL
      y <- small.sdf[1] ##needs a "seed" sdf format otherwise spits out a list
      for (i in 1:length(x)){
      mcs <- fmcs(small.sdf[i], small.sdf[t[i]], matching.mode = "aromatic")  # finds maximum common substructures between each cluster structure and the  
      q <- mcs2sdfset(mcs)                                                    # least similarity structure in the cluster
      q <- q[[1]]                                                             # takes just the query MCS structures
      z <- which.max(rings(q, type = "count"))  ##returns mcs of strucutre with most rings (arun: why?)
      p <- q[z]                                 # selects as p the mcs with the most rings (arun: why?)
      y <- c(y,p)                               # adds p to the set y
      #y <- y[-1]  ##looking more at this i think this is the wrong spot for this code
      }
      
      #unique_ids <- makeUnique(sdfid(p))
      #cid(p) <- unique_ids
      ## i think this might be where this should be
      y <- y[-1]
      y  
      
      return(y)
   }