##mcs overlap matric not using matching.mode = "aromatic"  have issues with indole vs indazole 
overlap.matrix.mcs <- function(hitlist){
   q <- NULL
   w <- NULL
   for (i in 1:length(hitlist)){
      w <- fmcsBatch(hitlist[[i]], hitlist[1:i], matching.mode = "aromatic", numParallel = 7)[,"Overlap_Coefficient"]
      length(w) <- length(hitlist)
      q <- rbind(q,w)
   }
   q[upper.tri(q)] = t(q)[upper.tri(q)]
   q
}