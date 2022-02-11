
# Principal component analysis on sediment and depth data for each sampling station 

# path name 
  path <- paste(getwd(),"1 - Data inputs",sep="/")

# load environmental data
  Env <- read.csv(paste(path,"Env_conditions.csv",sep="/"),header=T,sep=";")

# plot environmental conditions
  pdf(paste(getwd(),"3 - Outputs/Stations_environment.pdf",sep="/"),width=6,height=6) 
  par(mar=c(1.5, 4.5, 0.5, 0.5)+0.1,mfrow=c(2,2),mai=c(0.5,0.8,0.2,0.1))
  
  plot(Env$Station,Env$depth_BC,las=1,ylab="Depth (m)",xlab="Station number",pch=16)
  plot(Env$Station,Env$gravel,las=1,ylab="Gravel fraction",xlab="Station number",pch=16,ylim=c(0,1))
  #points(Env$Station[c(5,16)],Env$gravel[c(5,16)],pch=16,col="red")
  plot(Env$Station,Env$sand,las=1,ylab="Sand fraction",xlab="Station number",pch=16,ylim=c(0,1))
  #points(Env$Station[c(5,16)],Env$sand[c(5,16)],pch=16,col="red")
  mtext("Station id",side=1,line=2,cex=0.8)
  plot(Env$Station,Env$mud,las=1,ylab="Mud fraction",xlab="Station number",pch=16,ylim=c(0,1))
  #points(Env$Station[c(5,16)],Env$mud[c(5,16)],pch=16,col="red")
  mtext("Station id",side=1,line=2,cex=0.8)
  dev.off()
  
# run PCA and plot
#
#  wdbc.pr <- prcomp(Env[c(3,4,5,7)], center = TRUE, scale = TRUE)
#  summary(wdbc.pr)

#  pdf(paste(getwd(),"3 - Outputs/PCA_stations_environment.pdf",sep="/"),width=5,height=5) 
#  plot(wdbc.pr$x[,1],wdbc.pr$x[,2],xlim=c(-2.5,4),ylim=c(-3.5,2),las=1,
#     xlab="PC1 (51.9%)",ylab="PC2 (29.7%)",pch=16)

#  xx <- wdbc.pr$x[,1]
#  xx <- xx -0.2
#  xx[c(2,13,17)] <- xx[c(2,13,17)]+0.4
#  text(xx,wdbc.pr$x[,2],c(1:19))
#  dev.off()