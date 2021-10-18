
# Permanova analysis community longevity - observed oxygen and average SAR
# first open the R project Oxytrawl

# load libraries
  library(vegan)

# path name 
  path <- paste(getwd(),"1 - Data inputs",sep="/")

# load environmental data
  Env <- read.csv(paste(path,"Env_conditions.csv",sep="/"),header=T,sep=";")

# load BC data
  Box <- read.csv(paste(path,"BC_biomass.csv",sep="/"),header=T,sep=";")
  
# longevity  
  traits <- read.csv(paste(path,"Species_traits.csv",sep="/"),header=T,sep=";")
  Box <- cbind(Box, traits[match(Box$Species , traits$Species),c(5:8)])
  
# multiply biomass with longevity modality  
  Box$L1_bio <- Box$Biomass..g. * Box$l.1
  Box$L13_bio <- Box$Biomass..g. * Box$l1.2
  Box$L310_bio <- Box$Biomass..g. * Box$l3.10
  Box$L10_bio <- Box$Biomass..g. * Box$l.10

# aggregate per station
  BC_L1_bio <- aggregate(Box$L1_bio, by=list(Box$Station), FUN=sum,na.rm=T)  
  BC_L13_bio <- aggregate(Box$L13_bio, by=list(Box$Station), FUN=sum,na.rm=T)  
  BC_L310_bio <- aggregate(Box$L310_bio, by=list(Box$Station), FUN=sum,na.rm=T)  
  BC_L10_bio <- aggregate(Box$L10_bio, by=list(Box$Station), FUN=sum,na.rm=T)  
  
# add to environmental conditions
  Env$BC_L1_bio <- BC_L1_bio[,2] 
  Env$BC_L13_bio <- BC_L13_bio[,2] 
  Env$BC_L310_bio <- BC_L310_bio[,2] 
  Env$BC_L10_bio <- BC_L10_bio[,2] 

# remove all stations with zero biomass
  Env$Biomass <- rowSums(Env[,c("BC_L1_bio" ,"BC_L13_bio" ,"BC_L310_bio" ,"BC_L10_bio")])
  Env[,c(12:15)] <- Env[,c(12:15)]/Env$Biomass
  Env2 <- subset(Env, Env$Biomass > 0)
  Long <- Env2[,c("BC_L1_bio" ,"BC_L13_bio" ,"BC_L310_bio" ,"BC_L10_bio")]

#permanova  
  mod1 <- adonis2(Long ~ oxygen + SAR_1317, data = Env2, permutations = 999, method="bray");mod1
  mod2 <- adonis2(Long ~ oxygen, data = Env2, permutations = 999, method="bray");mod2
  mod3 <- adonis2(Long ~ SAR_1317, data = Env2, permutations = 999, method="bray");mod3
  
# check how it works for subsets
  #Env3 <- subset(Env2, Env2$oxygen > 3.2)
  #Long <- Env3[,c("BC_L1_bio" ,"BC_L13_bio" ,"BC_L310_bio" ,"BC_L10_bio")]
  #mod2 <- adonis2(Long ~ SAR_1317, data = Env3, permutations = 999, method="bray")
  
  #Env4 <- subset(Env2, Env2$SAR_1317 < 0.5)
  #Long <- Env4[,c("BC_L1_bio" ,"BC_L13_bio" ,"BC_L310_bio" ,"BC_L10_bio")]
  #mod3 <- adonis2(Long ~ oxygen, data = Env4 ,permutations = 50)
