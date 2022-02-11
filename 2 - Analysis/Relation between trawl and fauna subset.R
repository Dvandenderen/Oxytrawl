
# check relationships between trawling and total faunal biomass and abundance for subset of data

# libraries
  library(VGAM)
  library(dplyr)
  library(tidyr)
  library(latex2exp)
  library(RColorBrewer)

# path name 
  path <- paste(getwd(),"1 - Data inputs",sep="/")
  
# load environmental data
  Env <- read.csv(paste(path,"Env_conditions.csv",sep="/"),header=T,sep=";")
  
# load Boxcorer data
  Box <- read.csv(paste(path,"BC_biomass.csv",sep="/"),header=T,sep=";")
  Box$uniq<- paste(Box$Station, Box$Replicate,sep="_")

# aggregate per station biomass
  BC_bio <- aggregate(Box$Biomass..g., by=list(Box$uniq), FUN=sum,na.rm=T)
  BC_bio <- as.data.frame(BC_bio)
  BC_bio <- BC_bio %>% separate(1, c("Station", NA))
  BC_bio <- aggregate(BC_bio$x, by=list(BC_bio$Station), FUN=sum,na.rm=T)

# link to environment cond
  BC_bio$Group.1 <- as.numeric(BC_bio$Group.1)
  BC_bio <- BC_bio[order(BC_bio$Group.1),] 
  Env <- Env[order(Env$Station),] 
  Env$BC_bio <- BC_bio[,2]  

# aggregate per station abundance
  BC_abu <- aggregate(Box$Number.of.individuals, by=list(Box$uniq), FUN=sum,na.rm=T)
  BC_abu <- as.data.frame(BC_abu)
  BC_abu <- BC_abu %>% separate(1, c("Station", NA))
  BC_abu <- aggregate(BC_abu$x, by=list(BC_abu$Station), FUN=sum,na.rm=T)

# link to environment cond
  BC_abu$Group.1 <- as.numeric(BC_abu$Group.1)
  BC_abu <- BC_abu[order(BC_abu$Group.1),] 
  Env <- Env[order(Env$Station),] 
  Env$BC_abu <- BC_abu[,2] 

# now load the dredge data
  Dredge <- read.csv(paste(path,"Dredge_biomass.csv",sep="/"),header=T,sep=";")
  Dredge$uniq<- paste(Dredge$Station.ID, Dredge$Replicate.number,sep="_")

# aggregate per station
  Dr_bio <- aggregate(Dredge$Biomass, by=list(Dredge$uniq), FUN=sum,na.rm=T)
  Dr_bio <- as.data.frame(Dr_bio)
  Dr_bio <- Dr_bio %>% separate(1, c("Station", NA))
  Dr_bio <- aggregate(Dr_bio$x, by=list(Dr_bio$Station), FUN=sum,na.rm=T)

# link to environment cond
  Dr_bio$Group.1 <- as.numeric(Dr_bio$Group.1)
  Dr_bio <- Dr_bio[order(Dr_bio$Group.1),] 
  Env <- Env[order(Env$Station),] 
  Env$Dr_bio <- Dr_bio[,2]

# aggregate per station
  Dr_abu <- aggregate(Dredge$Number.at.station, by=list(Dredge$uniq), FUN=sum,na.rm=T)
  Dr_abu <- as.data.frame(Dr_abu)
  Dr_abu <- Dr_abu %>% separate(1, c("Station", NA))
  Dr_abu <- aggregate(Dr_abu$x, by=list(Dr_abu$Station), FUN=sum,na.rm=T)

# link to environment cond
  Dr_abu$Group.1 <- as.numeric(Dr_abu$Group.1)
  Dr_abu <- Dr_abu[order(Dr_abu$Group.1),] 
  Env <- Env[order(Env$Station),] 
  Env$Dr_abu <- Dr_abu[,2]

# select all oxygen stations > 3 ml/O2 per liter
  Env <- subset(Env,Env$percO2 > 40)
  
# convert to abundance/biomass per m2
  Env$BC_bio <- Env$BC_bio/0.3*1
  Env$BC_abu <- Env$BC_abu/0.3*1

# set colours using RColorBrewer and plot
  pal <- colorRampPalette(c("red", "blue"))
  Env$order  <- findInterval(Env$oxygen, sort(Env$oxygen))

# plot
  pdf(paste(getwd(),"3 - Outputs/Trawling_biomass_abundance_subset.pdf",sep="/"),width=7,height=6) 
  par(mar=c(1.5, 4.5, 0.5, 0.5)+0.1,mfrow=c(2,2),mai=c(0.5,0.8,0.2,0.1))

# biomass boxcore
  plot(Env$BC_bio~Env$SAR_1317,las=1, ylab=TeX("Biomass (g ww per m^{2})"),xlab="",
       pch=16,cex=1.3,main="Box core subset",ylim=c(0,130),col=pal(nrow(Env))[Env$order],xaxt="n",yaxt="n")
  axis(1,c(0,3.5,7))
  axis(2,c(0,30,60,90,120),las=1)
  
  m1 <- lm(BC_bio ~ SAR_1317, data = Env)
  SAR_1317 <- c(0:7)
  newdat <- data.frame(SAR_1317)
  newdat[,2] <- predict(m1,newdata=newdat)
  lines(newdat[,2]~newdat[,1],col="black",lty=5)
  text("(a)",x=0.2,y=125)

# biomass dredge
  plot(log10(Env$Dr_bio+1)~Env$SAR_1317,las=1, ylab="Biomass (g ww per trawl)",xlab="",
       pch=16,cex=1.3,xaxt="n",yaxt="n",main = "Dredge subset",ylim=c(0,4),col=pal(nrow(Env))[Env$order])
  axis(1,c(0,3.5,7))
  axis(2,c(log10(0+1),log10(10+1),log10(100+1),log10(1000+1),log10(10000+1)),
       c("0","10","100","1000","10000"),las=1)
  
  m1 <- lm(log10(Env$Dr_bio+1) ~ SAR_1317, data = Env)
  SAR_1317 <- c(0:7)
  newdat <- data.frame(SAR_1317)
  newdat[,2] <- predict(m1,newdata=newdat)
  lines(newdat[,2]~newdat[,1],col="black",lty=5)
  text("(b)",x=0.2,y=log10(6000+1))

# abundance box core
  plot(Env$BC_abu~Env$SAR_1317,las=1, ylab=TeX("Individuals (per m^{2})"),xlab="",
       pch=16,cex=1.3,ylim=c(0,1300),col=pal(nrow(Env))[Env$order],xaxt="n",yaxt="n")
  axis(1,c(0,3.5,7))
  axis(2,c(0,300,600,900,1200),las=1)
  
  m1 <- lm(BC_abu ~ SAR_1317, data = Env)
  SAR_1317 <- c(0:7)
  newdat <- data.frame(SAR_1317)
  newdat[,2] <- predict(m1,newdata=newdat)
  lines(newdat[,2]~newdat[,1],col="black",lty=5)
  mtext("Trawling intensity (per year)",side=1,line=2,cex = 0.8)
  text("(c)",x=0.2,y=1250)

# abundance dredge
  plot(log10(Env$Dr_abu+1)~Env$SAR_1317,las=1, ylab="Individuals (per trawl)",xlab="",
       pch=16,cex=1.3,xaxt="n",yaxt="n",col=pal(nrow(Env))[Env$order],ylim=c(0,4))
  axis(1,c(0,3.5,7))
  axis(2,c(log10(0+1),log10(10+1),log10(100+1),log10(1000+1),log10(10000+1)),
       c("0","10","100","1000","10000"),las=1)
  
  m1 <- lm(log10(Env$Dr_abu+1) ~ SAR_1317, data = Env)
  SAR_1317 <- c(0:7)
  newdat <- data.frame(SAR_1317)
  newdat[,2] <- predict(m1,newdata=newdat)
  lines(newdat[,2]~newdat[,1],col="black",lty=5)
  mtext("Trawling intensity (per year)",side=1,line=2,cex = 0.8)
  text("(d)",x=0.2,y=log10(8000+1))

  dev.off()

