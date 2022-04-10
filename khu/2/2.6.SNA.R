
# SNA 패키지로 해보자 ----------------------------------------

if (!require("pacman")) install.packages("pacman") 
pacman::p_load(tidyverse, magrittr, # helper packages
               sna # modeling packages
               ) 

rbind(
  c(0,1,1,1,0),
  c(1,0,0,0,0),
  c(1,0,0,0,0),
  c(1,1,0,0,1),
  c(0,0,0,1,0)
) -> amatrix
sna::gden(amatrix)
degree(amatrix, gmode = "graph")
closeness(amatrix, gmode = "graph")
betweenness(amatrix, gmode = "graph")
gplot(amatrix)
gplot(amatrix, mode = "circle")
plot.sociomatrix(amatrix)
