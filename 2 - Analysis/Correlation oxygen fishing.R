
# plot the correlation between observed oxygen and average SAR
# first open the R project Oxytrawl

# path name 
  path <- paste(getwd(),"1 - Data inputs",sep="/")

# load environmental data
  Env <- read.csv(paste(path,"Env_conditions.csv",sep="/"),header=T,sep=";")

# plot 
  pdf(paste(getwd(),"3 - Outputs/Oxygen_vs_fishing.pdf",sep="/"),width=4.5,height=4.5) 
  plot(Env$SAR_1317,Env$oxygen, xlab="Trawling intensity (per year)",ylab="Oxygen concentration (ml/l)", las=1,pch=16,xlim=c(0,8))
  #lines(x=c(0.5,0.5),y=c(0,100),lty=5)
  lines(x=c(-1,8),y=c(3,3),lty=5)
  dev.off()
  