# Permanova analysis vertical position - observed oxygen and average SAR
# first open the R project Oxytrawl

# load libraries
  library(vegan)

# path name 
  path <- paste(getwd(),"1 - Data inputs",sep="/")

# load environmental data
  Env <- read.csv(paste(path,"Env_conditions.csv",sep="/"),header=T,sep=";")

# load vertical core data
  Vcore <- read.csv(paste(path,"vertical_cores.csv",sep="/"),header=T,sep=",")

# get abundance data
  abu <- aggregate(Vcore$Abundance,by=list(Vcore$Station,Vcore$ID),FUN=sum)
  low <- subset(abu,abu$Group.2 == "0_2")
  med <- subset(abu,abu$Group.2 == "2_4")
  medeep <- subset(abu,abu$Group.2 == "4_8")
  deep <- subset(abu,abu$Group.2 == "tot")

# connect observations to environmental conditions
  Env <- cbind(Env, low[match(Env$Station , low$Group.1),c(3)])
  colnames(Env)[ncol(Env)] <- "0_2"
  Env <- cbind(Env, med[match(Env$Station , med$Group.1),c(3)])
  colnames(Env)[ncol(Env)] <- "2_4"
  Env <- cbind(Env, medeep[match(Env$Station , medeep$Group.1),c(3)])
  colnames(Env)[ncol(Env)] <- "4_8"
  Env <- cbind(Env, deep[match(Env$Station , deep$Group.1),c(3)])
  colnames(Env)[ncol(Env)] <- "8_"

  Env[,12:15][is.na(Env[,12:15])] <- 0
  Env$tot <- rowSums(Env[,12:15])

# remove all stations with zero abundance
  new <- subset(Env,Env$tot >0)

# permanova
  vert <- new[,12:15]/new$tot
  mod1 <- adonis2(vert ~ oxygen + SAR_1317 , data = new, permutations = 999, method="bray");mod1
  mod2 <- adonis2(vert ~ SAR_1317 , data = new, permutations = 999, method="bray");mod2
  mod3 <- adonis2(vert ~ oxygen , data = new, permutations = 999, method="bray");mod3

# get biomass data
  bio <- aggregate(Vcore$Biomass,by=list(Vcore$Station,Vcore$ID),FUN=sum)
  low <- subset(bio,bio$Group.2 == "0_2")
  med <- subset(bio,bio$Group.2 == "2_4")
  medeep<- subset(bio,bio$Group.2 == "4_8")
  deep<- subset(bio,bio$Group.2 == "tot")
  
# connect observations to environmental conditions
  Env <- read.csv(paste(path,"Env_conditions.csv",sep="/"),header=T,sep=";")
  Env <- cbind(Env, low[match(Env$Station , low$Group.1),c(3)])
  colnames(Env)[ncol(Env)] <- "0_2"
  Env <- cbind(Env, med[match(Env$Station , med$Group.1),c(3)])
  colnames(Env)[ncol(Env)] <- "2_4"
  Env <- cbind(Env, medeep[match(Env$Station , medeep$Group.1),c(3)])
  colnames(Env)[ncol(Env)] <- "4_8"
  Env <- cbind(Env, deep[match(Env$Station , deep$Group.1),c(3)])
  colnames(Env)[ncol(Env)] <- "8_"
  Env[,12:15][is.na(Env[,12:15])] <- 0
  Env$tot <- rowSums(Env[,12:15])

# remove all stations with zero abundance
  new <- subset(Env,Env$tot >0)
  
# permanova
  vert <- new[,12:15]/new$tot
  mod1 <- adonis2(vert ~ oxygen + SAR_1317 , data = new, permutations = 999, method="bray");mod1
  mod2 <- adonis2(vert ~ SAR_1317 , data = new, permutations = 999, method="bray");mod2
  mod3 <- adonis2(vert ~ oxygen , data = new, permutations = 999, method="bray");mod3