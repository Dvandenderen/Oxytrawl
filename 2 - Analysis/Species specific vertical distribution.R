# plot of biomass and abundance vertical position - per species
# first open the R project Oxytrawl

# path name 
  path <- paste(getwd(),"1 - Data inputs",sep="/")

# load vertical position per species
  specV <- read.csv(paste(path,"Vertical_biomass_by_species.csv",sep="/"),header=T,sep=",")

# remove unidentified
  specV <- specV[,-14]

# rename to be consistent  
nam <- c("Pygospio elegans",  "Astarte elliptica", "Scoloplos armiger", "Pontoporeia femorata",
         "Diastylis spp." , "Nematoda","Macoma balthica","Aricidea catherinae", "Harmothoe sarsi" ,
         "Nereis diveriscolor",  "Terebellides stroemii", "Ostracoda" ,  "Streblospio benedicti")

# make plot
  pdf(paste(getwd(),"3 - Outputs/Vertical biomass by species.pdf",sep="/"),width=7.5,height=7) 
  par(mfrow=c(4,4),oma= c(5,4,1,1)+ 0.1,mar=c(0,1,4,2)+ 0.5)
  for (i in 5:17){
  x1 <- sum(specV[,i][specV$layer == 1],na.rm = T)
  x2 <- sum(specV[,i][specV$layer == 3],na.rm = T)
  x3 <- sum(specV[,i][specV$layer == 6],na.rm = T)
  x4 <- sum(specV[,i][specV$layer == 8],na.rm = T)
  
  i;x1;x2;x3;x4
  
  yl <- max(pretty(c(0,max(c(x1,x2,x3,x4))),n=2))
  
  barplot(c(x1,x2,x3,x4),c(1,1,1,1),names.arg= c("0-2","2-4","4-8",">8"),xlab="vertical position",
          ylab="total biomass",main=nam[i-4],las=1,yaxt="n",ylim=c(0, yl),cex.main=0.9)
  axis(side = 2,c(0,yl),las=1)  
  
  }
  title(ylab = "Biomass (g)",
        xlab = "Vertical position (cm)",
        outer = TRUE, line = 3,cex.lab=1.2)
 dev.off()

# now again for abundance per species
 specV <- read.csv(paste(path,"Vertical_abundance_by_species.csv",sep="/"),header=T,sep=",")
 
 # remove unidentified
 specV <- specV[,-c(14,19,20)] # jaera and Hali. removed (not measured for biomass, only 1, 2 individuals)
 
 # rename to be consistent  
 nam <- c("Pygospio elegans",  "Astarte elliptica", "Scoloplos armiger", "Pontoporeia femorata",
          "Diastylis spp." , "Nematoda","Macoma balthica","Aricidea catherinae", "Harmothoe sarsi" ,
          "Nereis diveriscolor",  "Terebellides stroemii", "Ostracoda" ,  "Streblospio benedicti")
 
 # make plot
 pdf(paste(getwd(),"3 - Outputs/Vertical abundance by species.pdf",sep="/"),width=7.5,height=7) 
 par(mfrow=c(4,4),oma= c(5,4,1,1)+ 0.1,mar=c(0,1,4,2)+ 0.5)
 for (i in 5:17){
   x1 <- sum(specV[,i][specV$layer == 1],na.rm = T)
   x2 <- sum(specV[,i][specV$layer == 3],na.rm = T)
   x3 <- sum(specV[,i][specV$layer == 6],na.rm = T)
   x4 <- sum(specV[,i][specV$layer == 8],na.rm = T)
   
   yl <- max(pretty(c(0,max(c(x1,x2,x3,x4))),n=2))
   
   barplot(c(x1,x2,x3,x4),c(1,1,1,1),names.arg= c("0-2","2-4","4-8",">8"),xlab="vertical position",
           ylab="total biomass",main=nam[i-4],las=1,yaxt="n",ylim=c(0, yl),cex.main=0.9)
   axis(side = 2,c(0,yl),las=1)  
   
 }
 title(ylab = "Abundance",
       xlab = "Vertical position (cm)",
       outer = TRUE, line = 3,cex.lab=1.2)
 dev.off()
 