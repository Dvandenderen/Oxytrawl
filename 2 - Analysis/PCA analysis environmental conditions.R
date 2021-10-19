
# Principal component analysis on sediment and depth data for each sampling station 

# path name 
  path <- paste(getwd(),"1 - Data inputs",sep="/")

# load environmental data
  Env <- read.csv(paste(path,"Env_conditions.csv",sep="/"),header=T,sep=";")

# run PCA and plot
  wdbc.pr <- prcomp(Env[c(3,4,5,7)], center = TRUE, scale = TRUE)
  summary(wdbc.pr)

  pdf(paste(getwd(),"3 - Outputs/PCA_stations_environment.pdf",sep="/"),width=5,height=5) 
  plot(wdbc.pr$x[,1],wdbc.pr$x[,2],xlim=c(-2.5,4),ylim=c(-3.5,2),las=1,
     xlab="PC1 (51.9%)",ylab="PC2 (29.7%)",pch=16)

  xx <- wdbc.pr$x[,1]
  xx <- xx -0.2
  xx[c(2,13,17)] <- xx[c(2,13,17)]+0.4
  text(xx,wdbc.pr$x[,2],c(1:19))
  dev.off()