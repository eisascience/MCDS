
# This is the starting point

source("rbin/GausBimodal.R")
library(ggplot2)
source("rbin/effect_size.R")
source("rbin/multiplot.R")



mean_1 = 1
mean_2 = 6

#total events = 1E3 @ 1:99 ----

n_rep=20
f1=.7
f2= 1 - f1
sampSizeTot=1E3


SIM1 <- as.data.frame(bimodal_random_gaudists(
  sampsize1=sampSizeTot*f1, mean1=mean_1, sd1=1.34,
  sampsize2=sampSizeTot*f2, mean2=mean_2, sd2=1.34, 
  pltMe=T, repl = T, save2png = F))


