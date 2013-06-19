showMethods("ssb", includeDefs=T)

summary(stk)

qapply(stk, mean)

discards.wt(stk)
discards.wt(stk)[]<-0

FLIndex()
prova<-corse5763TATC[apply(is.na(corse5763TATC), 1, any), ]
megaTATB$NBTOTA[megaTATB$NBTOTA==-1]<-NA 

#colnames(data)[1] <- "Year"


showClass("FLIndex")

# load.R - DESC
# load.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $
# Created:
# Modified:

library(r4ss)
library(plyr)
library(reshape)
library(FLCore)

#########################################################################################
##### ICCAT North Atlantic Albacore ./AlbacoreTuna - MFCL {{{
#########################################################################################

stk <- readMFCL(file=c("AlbacoreTuna/plot-09.par.rep",
                       "AlbacoreTuna/09.par"))

save(stk, file="data/natlalb.RData")

data <- read.csv(file=c("AlbacoreTuna/albN.csv"),header = TRUE,skip = 41, sep=";")
data[data==-1]<-NA 

######## Stock
lst <- lapply(split(data, data$Fishery), function(x) {
  y <- x$Year
  m <- unique(x$Month)
  dnms <- list(year=min(y):max(y), unit=unique(x$Fishery),season=1:12)
  flq <- FLQuant(dimnames=dnms)
  cth <- flq
  eff <- flq
  len <- as.numeric(sub("X", "", names(x)[-c(1:7, 69)]))
  dnms <- list(quant=len, year=min(y):max(y), unit=unique(x$Fishery),season=min(m):max(m))
  flqLen <- FLQuant(dimnames=dnms)
  
  for(i in m){
   xx <- x[x$Month==as.character(i),]
    cth[,as.character(xx$Year),,as.character(i)] <- xx$Catch.t.
    eff[,as.character(xx$Year),,as.character(i)] <- xx$Effort
    flqLen[as.character(len),as.character(xx$Year),,as.character(i)] <- t(xx[-c(1:7, 69)])
  }
  #summary(flq)  
  FLQuants(cth=cth, eff=eff, len=flqLen)
})
#lst[[1]]$cth # cth, eff, len
#summary(lst[[1]])
#lst[[1:10]]$eff[lst[[1:10]]$eff==-1] <- NA     # Asign NA to effort= -1 ?????????

######## Index Abundance
#FLIndex()
#showClass("FLIndex")

######### CPUE for TAIWANESE - Fishery 8 (ALBN08: TAI_LL) 

CPUE <- lst[[8]]$cth/lst[[8]]$eff
summary (CPUE)
seasonSums(CPUE)

    # }}}
##save(stk, idx, CPUE, file="data/Albacore.RData") ?????????

#################################################################################################
# BoB Anchovy ./BiscayAnchovy - {{{
#########################################################################################

data <- read.table("./BiscayAnchovy/anchovy.dat", header=TRUE)

catch <- FLQuant(dimnames=list(year=data$year, age=c(1, 2)))
catch[1,] <- data$c12
catch[2,] <- data$c22

# }}}
#######BIOMASS????????

#################################################################################################
# ./CanaryRockfish - SS
#################################################################################################

data <- SS_readdat(file=c("CanaryRockfish/Canary_data.SS"))


#### catch
cth<-data$catch
flq<- FLQuant(dimnames=list(year=1916:2010,unit=1:12))

for(i in 1:12){
  flq[,as.character(cth$year),as.character(i)] <- cth[,(i)]
}
catch<-flq
catch



CPUE<-data$CPUE


#####length

length<-data$lencomp
len <- as.numeric(sub("f", "", names(length[-c(1:6, 35:68)])))
flq<- FLQuant(dimnames=list(len=len,year=1968:2010,unit=c("F","M")))
survey<-unique(length$FltSvy)

lencomp <- list()
for (i in survey){
  xx<-length[length$FltSvy==(i),]
  flq[as.character(len),as.character(xx$Yr),"F"] <- t(xx[-c(1:6, 35:68)])
  flq[as.character(len),as.character(xx$Yr),"M"] <- t(xx[-c(1:34)])
  lencomp[[as.character(i)]]<-flq
  
}
lencomp[[1]]

lencomp <- lapply(lencomp, function(x) unitSums(x))

###age

###age<-data$agecomp....????????

##### FLstock????? age??????

# computeCatch(data$catch, slot="all")
# catch.n
# catch.wt
# discards<- 0
# discards.n<- 0
# discards.wt<- landings.wt(stk)
# 
# landings
# landings.n
# landings.wt
# stock
# stock.n
# stock.wt
# m
# mat
# harvest
# harvest.spwn
# m.spwn
# name
# desc 
# range



######## Index Abundance
##### CPUE
CPUE<-data$CPUE
summary(CPUE)
flq<- FLQuant(dimnames=list(quant="CPUE",year=1980:2010))
survey<-unique(CPUE$index)
cpue <- list()

for (i in survey){
  xx<-CPUE[CPUE$index==(i),]
  flq[,as.character(xx$year),] <- xx$obs
  cpue[[as.character(i)]]<-flq
  
}

cpue[[2]]
summary(cpue)
######CREATE THE INDEX????????

#################################################################################################
# ./Haddock - VPA {{{
#################################################################################################
stk <- readFLStock("Haddock/hadividx.dat")

# All catch in landings, set discards to 0
discards.n(stk) <- 0
discards.wt(stk) <- landings.wt(stk)

catch(stk) <- computeCatch(stk, slot="all")

# FLIndex
idx <- readFLIndices("Haddock/hadivef.dat")

save(stk, idx, file="data/haddock.RData") # }}}


#################################################################################################
# ./Herring - VPA TODO: FIX broken use of VPA suite codes
#################################################################################################



#################################################################################################
# Iberian Sardine ./IberianSardine - SS
#################################################################################################

data <- SS_readdat(file=c("IberianSardine/sardine.dat"))


#### catch
cth<-data$catch # Catch_biomass (mtons)????????
flq<- FLQuant(dimnames=list(year=1978:2011,unit=1))
flq[,as.character(cth$year)] <- cth$purse_seine
catch<-flq
#

# #####length
# 
# length<-data$lencomp
# len <- as.numeric(sub("f", "", names(length[-c(1:6, 35:68)])))
# flq<- FLQuant(dimnames=list(len=len,year=1968:2010,unit=c("F","M")))
# survey<-unique(length$FltSvy)
# 
# lencomp <- list()
# for (i in survey){
#   xx<-length[length$FltSvy==(i),]
#   flq[as.character(len),as.character(xx$Yr),"F"] <- t(xx[-c(1:6, 35:68)])
#   flq[as.character(len),as.character(xx$Yr),"M"] <- t(xx[-c(1:34)])
#   lencomp[[as.character(i)]]<-flq
#   
# }
# lencomp[[1]]
# 
# lencomp <- lapply(lencomp, function(x) unitSums(x))
# 
# ###age
# 
# ###age<-data$agecomp....????????
# 
# ##### FLstock????? age??????

# computeCatch(data$catch, slot="all")
# catch.n
# catch.wt
# discards<- 0
# discards.n<- 0
# discards.wt<- landings.wt(stk)
# 
# landings
# landings.n
# landings.wt
# stock
# stock.n
# stock.wt
# m
# mat
# harvest
# harvest.spwn
# m.spwn
# name
# desc 
# range



######## Index Abundance
##### CPUE
CPUE<-data$CPUE  ## Biomass?????
summary(CPUE)
flq<- FLQuant(dimnames=list(quant="CPUE",year=1996:2011))
survey<-unique(CPUE$index)
cpue <- list()

xx<-CPUE[CPUE$index==2,]
flq[,as.character(xx$year),] <- xx$obs
cpue[["s2"]]<-flq
flq<- FLQuant(dimnames=list(quant="CPUE",year=1996:2011))
xx<-CPUE[CPUE$index==3,]
flq[,as.character(xx$year),] <- xx$obs
cpue[["s3"]]<-flq
######CREATE THE INDEX????????


#################################################################################################
# NEA Northen Hake ./NorthernHake - SS
#################################################################################################
data <- SS_readdat(file=c("NorthernHake/nhake-update.dat"))

#################################################################################################
# North Sea Cod - ./NSCod - VPA {{{
#################################################################################################
stk <- readFLStock("./NSCod/Cod347_ext.idx")

# All catch in landings, set diacrds to 0
discards.n(stk) <- 0
discards.wt(stk) <- landings.wt(stk)

catch(stk) <- computeCatch(stk, slot="all")

# FLIndex
idx <- readFLIndices("./NSCod/Cod347_2012_ext.tun")

save(stk, idx, file="data/nscod.RData")

# }}}


#################################################################################################
# North Sea Plaice ./Plaice - VPA {{{
#################################################################################################

stk <- readFLStock("./Plaice/Raised_and_Reconstructed/index_IV_VIId_raised+recon.txt")

discards(stk) <- computeDiscards(stk)
catch(stk) <- computeCatch(stk, slot="all")

idx <- readFLIndices("./Plaice/Raised_and_Reconstructed/fleet_trimmed.txt")

save(stk, idx, file="data/nsplaice.RData") # }}}


#################################################################################################
# ./SouthAfriAnchovy - TODO: By hand
#################################################################################################



#################################################################################################
# ./SouthernHorseMack - ADMB
#################################################################################################



#################################################################################################
# ./SpurDog - ADMB
#################################################################################################



#################################################################################################
# ./YellowtailFlounder - ADAPT
#################################################################################################

readFLStock('YellowtailFlounder/ytfounder_datainfo.txt', type='Adapt')
