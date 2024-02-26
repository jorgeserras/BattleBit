
## Run this script once to install all required dependencies

## Automatically installs and loads the required packages
 if(!require('pacman'))install.packages('pacman')
 pacman::p_load(shiny, rJava, here, digest, shinyjs,dplyr,DiagrammeR,shinythemes, shinycustomloader, bsplus, reshape2, ggplot2, gsubfn, jmotif, qdapTools, mclust, Hmisc) 
