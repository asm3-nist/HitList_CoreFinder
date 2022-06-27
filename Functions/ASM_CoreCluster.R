#ASM identify the "core" cluster using the spectral similarity scores
det.core.cluster <- function(clusterResults,scores,compareMode){

   if(compareMode == "MaxElements"){
      q <- find.mode(clusterResults[clusterResults>0])  # arun: want to find mode of cluster results, not of indices
      x <- which(clusterResults == q)
      return(x) 
   }
   
   if(compareMode == "MaxAvgMFInCluster"){
      numClusters = length(unique(clusterResults[clusterResults>0]))
      AvgMFs = numeric(numClusters)
      for(i in 1:numClusters){
         x = which(clusterResults==i)
         AvgMFs[i] = mean(scores[x])
      }
      
      i = which.max(AvgMFs);
      x = which(clusterResults==i);
      return(x)
   }

   if(compareMode == "MaxMaxMFInCluster"){
      
      numClusters = length(unique(clusterResults[clusterResults>0]))
      MaxMFs = numeric(numClusters)
      for(i in 1:numClusters){
         x = which(clusterResults==i)
         MaxMFs[i] = max(scores[x])
      }
      
      i = which.max(MaxMFs);
      x = which(clusterResults==i);
      return(x)
   }
   
   } 