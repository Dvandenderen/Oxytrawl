
# check relationships between trawling and total faunal biomass and abundance for subset of data
# when shell weight is removed

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
  
# wet weight with shell to without shell
  Box$Bivalve <- 1
  Box$Bivalve <- ifelse (Box$Species %in% c("Astarte elliptica"),0.35,Box$Bivalve)
  Box$Bivalve <- ifelse (Box$Species %in% c("Macoma balthica"),0.408,Box$Bivalve)
  Box$Bivalve <- ifelse (Box$Species %in% c("Mytilus trossulus"),0.305,Box$Bivalve)
  Box$Biomass..g. <- Box$Biomass..g. * Box$Bivalve
  
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

# now load the dredge data
  Dredge <- read.csv(paste(path,"Dredge_biomass.csv",sep="/"),header=T,sep=";")
  Dredge$uniq<- paste(Dredge$Station.ID, Dredge$Replicate.number,sep="_")

# wet weight with shell to without shell
  Dredge$Bivalve <- 1
  Dredge$Bivalve <- ifelse (Dredge$Species %in% c("Astarte"),0.35,Dredge$Bivalve)
  Dredge$Bivalve <- ifelse (Dredge$Species %in% c("Macoma balthica"),0.408,Dredge$Bivalve)
  Dredge$Bivalve <- ifelse (Dredge$Species %in% c("Mytilus trossulus"),0.305,Dredge$Bivalve)
  Dredge$Biomass <- Dredge$Biomass * Dredge$Bivalve
  
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

# select all oxygen stations > 3 ml/O2 per liter
  Env <- subset(Env,Env$percO2 > 40)

# set colours using RColorBrewer and plot
  pal <- colorRampPalette(c("red", "blue"))
  Env$order  <- findInterval(Env$oxygen, sort(Env$oxygen))

# no plot needed
#  pdf(paste(getwd(),"3 - Outputs/Trawling_biomass_abundance_subset_shell_free.pdf",sep="/"),width=7,height=6) 
  par(mar=c(1.5, 4.5, 0.5, 0.5)+0.1,mfrow=c(1,2),mai=c(0.5,0.8,0.2,0.1))

# biomass boxcore
  plot(Env$BC_bio~Env$SAR_1317,las=1, ylab=TeX("Biomass (g per 0.3 m^{2})"),xlab="Fishing intensity (y-1)",
       pch=16,cex=1.3,main="Box core subset",ylim=c(0,40),col=pal(nrow(Env))[Env$order])
  
  m1 <- lm(BC_bio ~ SAR_1317, data = Env)
  SAR_1317 <- c(0:7)
  newdat <- data.frame(SAR_1317)
  newdat[,2] <- predict(m1,newdata=newdat)
  lines(newdat[,2]~newdat[,1],col="black",lty=5)
  text("(a)",x=0.2,y=38)

# biomass dredge
  plot(log10(Env$Dr_bio+1)~Env$SAR_1317,las=1, ylab="Biomass (g per trawl)",xlab="Fishing intensity (y-1)",
       pch=16,cex=1.3,yaxt="n",main = "Dredge subset",ylim=c(0,4),col=pal(nrow(Env))[Env$order])
  axis(2,c(log10(0+1),log10(10+1),log10(100+1),log10(1000+1),log10(10000+1)),
       c("0","10","100","1000","10000"),las=1)
  
  m1 <- lm(log10(Env$Dr_bio+1) ~ SAR_1317, data = Env)
  SAR_1317 <- c(0:7)
  newdat <- data.frame(SAR_1317)
  newdat[,2] <- predict(m1,newdata=newdat)
  lines(newdat[,2]~newdat[,1],col="black",lty=5)
  text("(b)",x=0.2,y=log10(6000+1))

  dev.off()

