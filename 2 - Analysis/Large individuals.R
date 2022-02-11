
# analyze trawl effect for all large individuals
# first open the R project Oxytrawl

# libraries
  library(VGAM)
  library(dplyr)
  library(tidyr)

# path name 
  path <- paste(getwd(),"1 - Data inputs",sep="/")

# load environmental data
  Env <- read.csv(paste(path,"Env_conditions.csv",sep="/"),header=T,sep=";")

# load BC data
  Box <- read.csv(paste(path,"BC_biomass.csv",sep="/"),header=T,sep=",")
  Box$Individual.weight <- as.numeric(as.character(Box$Individual.weight))

# load LxW relationships mm / mg
  LW <- read.csv(paste(path,"L-W_relationships.csv",sep="/"),header=T,sep=",")
  Box<-cbind(Box,LW[match(Box$Species,LW$Species),c(4:5)])
  Box$Individual.weight <- Box$Individual.weight*1000
  Box$ind_lengthmm <- 10^((log10(Box$Individual.weight)-Box$intercept)/Box$slope)

  sum(Box$Biomass..g.[Box$ind_lengthmm > 4],na.rm = T) / sum(Box$Biomass..g.,na.rm = T)
  sum(Box$Number.of.individuals[Box$ind_lengthmm > 4],na.rm = T) / sum(Box$Number.of.individuals,na.rm = T)
  
  # now get all large individuals (4 mm)
  largeI <- c()
  
  for (pp in 1:19){
    stat <- subset(Box, Box$Station == pp)
    stat <- subset(stat,stat$ind_lengthmm > 4)
    largeI <- c(largeI,sum(stat$Number.of.individuals))
  } 
  
  Env <- Env[order(Env$Station),] 
  Env$largeI <- largeI
  
  # convert to abundance/biomass per m2
  Env$largeI <- Env$largeI/0.3*1

  m1 <- vglm(largeI ~ oxygen, tobit(Lower = 0), data = Env)
  m2 <- vglm(largeI ~ oxygen+SAR_1317, tobit(Lower = 0), data = Env)
  m3 <- vglm(largeI ~ oxygen*SAR_1317, tobit(Lower = 0), data = Env)
  AIC(m1);AIC(m2);AIC(m3)
  summary(m1);summary(m2);summary(m3)
  summary(m1, lrt0 = TRUE);summary(m2, lrt0 = TRUE);summary(m3, lrt0 = TRUE) # pvalues based on likelihood ratio test
  
# now get all large individuals (15 mm)
  sum(Box$Biomass..g.[Box$ind_lengthmm > 15],na.rm = T) / sum(Box$Biomass..g.,na.rm = T)
  sum(Box$Number.of.individuals[Box$ind_lengthmm > 15],na.rm = T) / sum(Box$Number.of.individuals,na.rm = T)
  
  largeI <- c()
  
  for (pp in 1:19){
    stat <- subset(Box, Box$Station == pp)
    stat <- subset(stat,stat$ind_lengthmm > 15)
    largeI <- c(largeI,sum(stat$Number.of.individuals))
  } 

  Env <- Env[order(Env$Station),] 
  Env$largeI <- largeI
  
  # convert to abundance/biomass per m2
  Env$largeI <- Env$largeI/0.3*1
  
  m1 <- vglm(largeI ~ oxygen, tobit(Lower = 0), data = Env)
  m2 <- vglm(largeI ~ oxygen+SAR_1317, tobit(Lower = 0), data = Env)
  m3 <- vglm(largeI ~ oxygen*SAR_1317, tobit(Lower = 0), data = Env)
  AIC(m1);AIC(m2);AIC(m3)
  summary(m1);summary(m2);summary(m3)
  summary(m1, lrt0 = TRUE);summary(m2, lrt0 = TRUE);summary(m3, lrt0 = TRUE) # pvalues based on likelihood ratio test

# plot the result for > 15mm
  pdf(paste(getwd(),"3 - Outputs/Large individuals.pdf",sep="/"),width=7,height=3.5) 
  par(mar=c(1.5, 4.5, 0.5, 0.5)+0.1,mfrow=c(1,2),mai=c(0.8,0.9,0.1,0.1))
  plot(Env$largeI~Env$oxygen,ylab=TeX("nb. of large individuals per m^{2}"),xlab="Oxygen concentration (ml/l)",
       las = 1,pch=16,ylim=c(0,75),xlim=c(0,6),yaxt="n",xaxt="n")
  axis(1,c(0,3,6))
  axis(2,c(0,25,50,75),las=1)
  text("(a)",x=0.18,y=70)
  
  oxygen <- seq(0,6,0.01)
  newdat <- data.frame(oxygen)
  newdat[,2] <- predict(m1,newdata=newdat)[,1]
  newdat[,2][newdat[,2]<0] <- 0
  lines(newdat[,2]~newdat[,1],col="black")
  
  plot(Env$largeI~Env$SAR_1317,ylab="",xlab="Trawling intensity (per year)",yaxt="n",pch=16,
       xaxt="n",yaxt="n",ylim=c(0,75),xlim=c(0,7.5))
  axis(1,c(0,3.5,7))
  axis(2,c(0,25,50,75),c("","","",""))
  text("(b)",x=0.2,y=70)
  dev.off()
  