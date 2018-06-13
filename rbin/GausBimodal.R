


bimodal_random_gaudists <- function(sampsize1=100, mean1=2, sd1=1, sampsize2=100, mean2=2, sd2=1, pltMe=F, repl=F){
  
  # library(ggplot2)
  # source("rbin/effect_size.R")
  # sampsize1=100; mean1=0; sd1=1;
  # sampsize2=100; mean2=5; sd2=1;
  # pltMe = T
  
  
  vec1 <- vector(); vec2 <- vector()
  
  
  vec1 <- rnorm(sampsize1, mean = mean1, sd = sd1)
  vec2 <- rnorm(sampsize2, mean = mean2, sd = sd2)
  
  
  x_bimodal = c(vec1, vec2)
  if(pltMe) plot(density(x_bimodal))
  y_labels <- as.factor(c(rep(0, sampsize1), rep(1, sampsize2)))
  if(pltMe) rug(vec1, col="dodgerblue")
  if(pltMe) rug(vec2, col="red")
  
  
  df1 <- as.data.frame(cbind(x_bimodal, y_labels))
  
  
  if (pltMe){
    pp1 <- ggplot(df1, aes(x=x_bimodal, y=rep(0, nrow(df1)))) + 
      geom_jitter(height = 0.01, aes(colour = as.factor(y_labels))) + 
      theme(legend.position = "none") + 
      ggtitle(paste("    Effect size (Cohen's d) =", round(cohens_d(vec1, vec2),3)," || N1=", length(vec1), ", N2= ",length(vec2), sep=""))
    
    pp2 <- ggplot(df1, aes(x=x_bimodal, fill=as.factor(y_labels))) + 
      geom_density(alpha = 0.5) + theme(legend.position = "bottom") 
    
    pp3 <- ggplot(df1, aes(x=as.factor(y_labels), y=x_bimodal)) + 
      geom_boxplot(outlier.colour="red", outlier.shape=8,
                   outlier.size=4) + theme(legend.position = "none") +
      coord_flip()
    
    png(file = paste("SimViz",sampsize1, mean1, sd1, sampsize2, mean2, sd2,as.character(round(runif(1, 10000, 99999))) , ".png",sep="_"), bg = "transparent", width = 1200, height = 1600, units = "px", res=150)
    multiplot(pp1, pp3, pp2, cols=1)
    dev.off(); multiplot(pp1, pp3, pp2, cols=1)
    
  }
  
  
  #print(multiplot(pp1, pp3, pp2, cols=1))
  
  # #Create figure window and layout
  # plot.new()
  # grid.newpage()
  # pushViewport(viewport(layout = grid.layout(2, 1)))
  # 
  # #Draw ggplot
  # pushViewport(viewport(layout.pos.row = 1))
  # print(pp, newpage = FALSE)
  # popViewport()
  # 
  # #Draw bsae plot
  # pushViewport(viewport(layout.pos.row = 2))
  # par(fig = gridFIG(), new = TRUE)
  # plot(density(vec1, col="red"))
  # plot(lines(desity(vec2, col = "dodgerblue")))
  # popViewport()
  
  
  
  #return(df1[sample(nrow(df1)),])
  ifelse(repl, return(list(x=x_bimodal, y=y_labels)), return(df1))
}


