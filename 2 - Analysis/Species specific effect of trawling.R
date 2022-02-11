
# plot the species-specific plots for the data subset with relatively high oxygen
# first open the R project Oxytrawl

# path name 
  path <- paste(getwd(),"1 - Data inputs",sep="/")

# load environmental data
  Env <- read.csv(paste(path,"Env_conditions.csv",sep="/"),header=T,sep=";")
  Env <- subset(Env,Env$percO2 > 40)
  
  nambent <- c("Astarte","Mytilus trossulus","Macoma balthica","Saduria entomon","Halicryptus spinulosus")
  nambent2 <- c("Astarte elliptica","Mytilus trossulus","Macoma balthica","Saduria entomon","Halicryptus spinulosus")
  
  ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

# create the plots
  pdf(paste(getwd(),"3 - Outputs/Trawling_per_species.pdf",sep="/"),width=4,height=9)
  par(mfrow=c(6,2),oma= c(2,2,1,1),mar=c(2,2,2,1))
  
  for (ii in 1:5){
  # load BC data
    BC <- read.csv(paste(path,"BC_biomass.csv",sep="/"),header=T,sep=",")
    BC$uniq<- paste(BC$Station, BC$Replicate,sep="_")
    tt <- subset(BC,(BC$Species == nambent2[ii]))
    Dr_bio <- aggregate(tt$Biomass..g., by=list(tt$Station), FUN=sum,na.rm=T)
    Dr_bio <- cbind(Env, Dr_bio[match(Env$Station,Dr_bio$Group.1 ),c(2)])
    colnames(Dr_bio)[ncol(Dr_bio)]  <- "Bio"
    Dr_bio$Bio[is.na(Dr_bio$Bio)] <- 0
    Dr_bio$Bio <- Dr_bio$Bio/0.3

    yy <- max(log10(Dr_bio$Bio+1))
    plot(log10(Dr_bio$Bio+1)~Dr_bio$SAR_1317,
         las=1,xlab="Fishing intensity",ylab="log10(Biomass+1)",xaxt="n",yaxt="n", ylim = c(0,ceiling_dec(yy,1)))
    axis(1,c(0,3.5,7),c("","",""))
    axis(2,c(0,ceiling_dec(yy,1)/2,ceiling_dec(yy,1)),las=1,cex=1.5)
    
    mod1 <- lm(log10(Dr_bio$Bio+1)~Dr_bio$SAR_1317)
    pval <- summary(mod1)$coefficient[2,4]
    linetype <- ifelse(pval < 0.05, 1, 3)  
    cf <- coef(mod1)
    sar <- c(0,7)
    yend <- cf[1]+cf[2]*sar
    lines(yend~sar, lty = linetype)
    
    # load dredge data
    Dredge <- read.csv(paste(path,"Dredge_biomass.csv",sep="/"),header=T,sep=";")
    Dredge$uniq<- paste(Dredge$Station.ID, Dredge$Replicate.number,sep="_")
    tt <- subset(Dredge,(Dredge$Species == nambent[ii]))
    Dr_bio <- aggregate(tt$Biomass, by=list(tt$Station.ID), FUN=sum,na.rm=T)
    Dr_bio <- cbind(Env, Dr_bio[match(Env$Station,Dr_bio$Group.1 ),c(2)])
    colnames(Dr_bio)[ncol(Dr_bio)]  <- "Bio"
    Dr_bio$Bio[is.na(Dr_bio$Bio)] <- 0

    yy <- max(log10(Dr_bio$Bio+1))
    plot(log10(Dr_bio$Bio+1)~Dr_bio$SAR_1317,
         las=1,xlab="Fishing intensity",ylab="log10(Biomass+1)",xaxt="n",yaxt="n", ylim = c(0,ceiling(yy)))
    axis(1,c(0,3.5,7),c("","",""))
    axis(2,c(0,ceiling(yy)/2,ceiling(yy)),las=1,cex=1.5)
    
    mod1 <- lm(log10(Dr_bio$Bio+1)~Dr_bio$SAR_1317)
    pval <- summary(mod1)$coefficient[2,4]
    linetype <- ifelse(pval < 0.05, 1, 3)  
    cf <- coef(mod1)
    sar <- c(0,7)
    yend <- cf[1]+cf[2]*sar
    lines(yend~sar, lty = linetype)
    
  } 

  nambent <- c("Scoloplos armiger")
  # load BC data
  BC <- read.csv(paste(path,"BC_biomass.csv",sep="/"),header=T,sep=",")
  BC$uniq<- paste(BC$Station, BC$Replicate,sep="_")
  tt <- subset(BC,(BC$Species == nambent[1]))
  BC_bio <- aggregate(tt$Biomass..g., by=list(tt$Station), FUN=sum,na.rm=T)
  BC_bio <- cbind(Env, BC_bio[match(Env$Station,BC_bio$Group.1 ),c(2)])
  colnames(BC_bio)[ncol(BC_bio)]  <- "Bio"
  BC_bio$Bio[is.na(BC_bio$Bio)] <- 0
  BC_bio$Bio <- BC_bio$Bio/0.3
  
  yy <- max(log10(BC_bio$Bio+1))
  plot(log10(BC_bio$Bio+1)~BC_bio$SAR_1317,
       las=1,xlab="Trawling intensity",ylab="log10(Biomass+1)",xaxt="n",yaxt="n", ylim = c(0,ceiling_dec(yy,1)))
  axis(1,c(0,3.5,7),cex=1.5)
  axis(2,c(0,ceiling_dec(yy,1)/2,ceiling_dec(yy,1)),las=1,cex=1.5)
  
  mod1 <- lm(log10(BC_bio$Bio+1)~BC_bio$SAR_1317)
  pval <- summary(mod1)$coefficient[2,4]
  linetype <- ifelse(pval < 0.05, 1, 3)  
  cf <- coef(mod1)
  sar <- c(0,7)
  yend <- cf[1]+cf[2]*sar
  lines(yend~sar, lty = linetype)
  mtext("Trawling intensity (per year)",side=1,line=2,cex = 0.8)
  
  plot.new()
  
  dev.off()
  
