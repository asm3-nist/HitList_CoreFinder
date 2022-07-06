Prototype code for identifying "core" structure from a ms library search hit list.

Developers: 	Edward P. Erisman; edward.erisman@nist.gov
	   	Arun S. Moorthy; arun.moorthy@nist.gov

Date: June 15th, 2022.
===================================================================================

Quick Start:

1. Open main.R in a text editor (or IDE). 
2. Confirm the external libraries specified on lines 17-20 are installed in your version of R. 
3. If external packages are installed, edit lines 23-34 to your requirements.
4. Run "main.R" in your R console (R, Rstudio, etc.)


External Packages Details.

For most external packages, they can be installed using a command as follows:

          install.packages("package.name")

and loaded using the command:

          library(package.name)


To install chemmineR and fmcs package (if not already installed), use the following commands:

 if (!requireNamespace("BiocManager", quietly=TRUE))
     install.packages("BiocManager")
 BiocManager::install("ChemmineR")
 
 if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
 BiocManager::install("fmcsR") 



 
