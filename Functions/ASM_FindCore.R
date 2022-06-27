find.core <- function(x){
      # x is an sdf with structure information 
      ig.core = x[which.min(MW(x))] # initial.guess. for a core -- finding the smallest molecule in the cluster
      
      OC = numeric(length(x)); # this sets up all of our overlap coefficients to start as 0. 
                               # Our goal is to find a core structure such that all the overlap coefficients are 1.
      
      check = sum(OC)
      iter = 0
      max.iter = 100;
      
      core = ig.core;
      while(check < length(x) && iter < max.iter){
         
         for(i in 1:length(x)){
            t = fmcs(core,x[i],matching.mode = "aromatic")
            if(as.numeric(stats(t)[5])==1){
               #cat(i);cat("\t perfect overlap\n")
               OC[i] = 1
               next
            } else {
               if(stats(t)[3]<3){ # this is a check if the mcs size becomes less than 3 it breaks the code
                  #cat(i);cat("\t if MCS size is less than 3\n")
                  return("NA")
               } else {
                  #cat(i);cat("\t reset core\n")
                  core = mcs2sdfset(t)$query[1] # update the core to be the new mcs  
                  #plot(core)
                  OC[1:i] = 0 # reset the overlap coefficients  
                  next
               }
            }
         }
         
         check = sum(OC)
         iter = iter + 1;
      }
      
      return(core)   
}