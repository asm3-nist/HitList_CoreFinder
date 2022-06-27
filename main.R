# Prototype code for identifying "core" structure from an
# MS Library search hit list. Code used for preparing ASMS results 2022 and 
# working paper titled "Identifying the core structure of an analyte using 
# ms library search results: a clustering-based approach" by Erisman & Moorthy.
#
# Developers:  EPE; edward.erisman@nist.gov
#              ASM; arun.moorthy@nist.gov
#
# Date: March 16th, 2022
# Revision Date: June 15th, 2022
# ==============================================================================
# Clear Work Space
rm(list=ls())

# LOAD External Packages

library(ChemmineR)  # For Plotting SDF
library(fmcsR)      # For computing MCS
library(dbscan)     # For performing db clustering
library(data.table) # For creating data tables and storing results

# ==============================================================================
# USER INPUTS
hitlist = read.SDFset("ExampleData/408 hitlist.SDF") # structures from hit list

epsilons = c(0)     # cluster neighborhood size (can test up to 4 neighborhood 
                    # sizes by building out epsilons list)

maxHits = 100;      # max hits used in clustering

# ADJUST SELECTION DEPENDING ON WHETHER QUERY INFORMATION IS AVAILABLE
query = read.SDFset("ExampleData/408 single.SDF") # structure of query
# query = NULL

# ==============================================================================
# Custom Functions
functions = list.files("Functions")
for(i in 1:length(functions)){
   filename = paste0("Functions/",functions[i])
   source(filename)
}

## Main Script =================================================================
# The code can be broken into two parts. In the first section, given a hit list, 
# we identify a potential core structure (if possible). In the second section, 
# given a core structure and the query's known structure, we assess the quality
# of the predicted core structure. 

# Section 1: Identifying potential core structure
numEpsilons = length(epsilons)
Results = NULL

stime <- system.time({

   Scores = rep(900,length(hitlist)) ## Stand in because provided hit list structure does not have scores. For full example with 
                                     ## Hit list scores using output from MS Pepsearch, request "CoreFinder_ASMS_Poster.zip" 
                                     ## from Ed or Arun. 

   # calculate "overlap" matrix # TIME CONSUMING COMPUTATION
   hitlist.overlap <- overlap.matrix(hitlist)
   cores = NULL;
   hitlist.clusters = NULL;
   core.clusters = NULL;

   for(k in 1:length(epsilons)){
      # cluster the data based on structural similarity #
      hitlist.clusters[[k]] = hitlist.cluster = c(find.cluster(hitlist.overlap,epsilons[k]))
      
      # identify the cluster to be used in finding core: (1) cluster with most elements, (2) cluster with highest average mf, (3) cluster with highest max mf #
      core.clusters[[k]] = core.cluster = c(det.core.cluster(hitlist.cluster,Scores,"MaxAvgMFInCluster"))
      
      # find core #
      if(length(core.cluster)>1){
         cores[[k]] = find.core(hitlist[core.cluster])
      } else {
         cores[[k]] = "NA"
      }
      
   }
 
   Results = list(HITLIST.SIZE = length(hitlist), CLUSTERS = hitlist.clusters, CORE.CLUSTERS = core.clusters, CORES = cores)
   

})[3]

cat("\nTo access predicted core details, type: \"Results\" in the console.\n\n")

# Section 2: comparing the predicted "cores" to the known query structures.

if(class(query)!="SDFset"){
     cat("No query structure information provided to validate prediction quality.\n\n")
} else {

for(j in 1:numEpsilons){

   if(class(Results$CORES[[j]])!="SDFset"){
     cat("No core identified with selected neighborhood range. \n")   
   } else {
     cat(paste0("Query/Core comparison results using neighboorhood region epsilon = ",epsilons[j],"\n\n"))
     core = Results$CORES[[j]]   
     cat(paste0("Core Mol Form: ", MF(core,addH=TRUE),"\n"))
     cat(paste0("Query Mol Form: ", MF(query,addH=TRUE),"\n\n"))
     keyMCS = fmcs(core,query);
     keyMCSstat = stats(keyMCS)[5];
     cat(paste0("Overlap coefficient: ",round(keyMCSstat,3)," \n"))
     keyMCSstat = stats(keyMCS)[4];
     cat(paste0("Tanimoto coefficient: ",round(keyMCSstat,3)," \n"))
     plot(c(core,query))
   }
   
   cat("\n\n") 

} # end for j
} 



   







