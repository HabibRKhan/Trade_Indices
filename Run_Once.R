##---INSTALLING FEWS---##

#1. Run this script only once to install the "FEWS" R package from Github

#2. First install and load the "devtools" package and then use its install_github
    #function to install FEWS from Github (author: Donald Lynch)

install.packages("devtools")
library(devtools)

install_github("Donal-lynch/FEWS_package")

#3. Install package "dplyr" for data manipulation

install.packages("dplyr")
