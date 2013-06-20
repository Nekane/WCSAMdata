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

####The effort is wrong....No Albacore!!!


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

######## TO DO !!!!...Report ICES, maturity...

data <- read.table("./BiscayAnchovy/anchovy.dat", header=TRUE)

catch <- FLQuant(dimnames=list(year=data$year, age=c(1, 2)))
catch[1,] <- data$c12
catch[2,] <- data$c22

# }}}
#######

#################################################################################################
# ./CanaryRockfish - SS
#################################################################################################

### No CONTROL file.... No RockFish!!!

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

### CHECK!!!

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
#Check the Indices

stk <- readFLStock("Herring/-INDEX.txt")

# All catch in landings, set diacrds to 0
discards.n(stk) <- 0
discards.wt(stk) <- landings.wt(stk)

catch(stk) <- computeCatch(stk, slot="all")

# FLIndex
#idx <- readFLIndices("")

#save(stk, idx, file="Herring.RData")


#################################################################################################
# Iberian Sardine ./IberianSardine - SS
#################################################################################################

# Catch mismatch!!

data <- SS_readdat(file=c("IberianSardine/sardine.dat"))
control <- SS_readctl(file=c("IberianSardine/sardine.ctl"))

#### catch
cth<-data$catch # Catch_biomass (mtons)
flq<- FLQuant(dimnames=list(year=1978:2011),units="mtonnes")
flq[,as.character(cth$year)] <- cth$purse_seine
catch<-flq

###age
age<-data$agecomp
age1 <- as.numeric (sub("a", "", names(age[-c(1:9)])))
flq<- FLQuant(dimnames=list(age=age1,year=1978:2011))
survey<-unique(age$FltSvy)
agecomp <- list()
xx<-age[age$FltSvy==1,]
flq[as.character(age1),as.character(xx$Yr)] <- t(xx[-c(1:9)])
agecomp[[1]] <-flq
flq<- FLQuant(dimnames=list(age=age1,year=1978:2011))
xx<-age[age$FltSvy==2,]
flq[as.character(age1),as.character(xx$Yr)] <- t(xx[-c(1:9)])
agecomp[[2]] <-flq

### Assume no catch when there are Na for fleet number 2
#agecomp[[1]][is.na(agecomp[[1]])] <-0
agecomp[[2]][is.na(agecomp[[2]])] <-0
agecomptot <- agecomp[[1]] %+% agecomp[[2]]
units(agecomptot) <- "thousands"

# ### weight at age
# fleet 0 contains begin season pop WT
# fleet -1 contains mid season pop WT
# fleet -2 contains maturity*fecundity in case there's only maturity at age and no fec data,  multiply mat prop by weight at age

wtage <- read.csv(file=c("IberianSardine/wtatage.csv"),header = TRUE,skip = 6, sep=";")

wtage <- wtage[!apply(wtage, 1, function(x) {any(x == -1900)}),]
wtage <- wtage[!apply(wtage, 1, function(x) {any(x == 2012)}),]
age1 <- as.numeric (sub("X", "", names(wtage[-c(1:6)])))
flq<- FLQuant(dimnames=list(age=age1, year=1978:2011),units="kg")
xx<-wtage[wtage$fleet==-1,]
flq[as.character(age1),as.character(xx$yr)] <- t(xx[-c(1:6)])
wtatage<-flq

##### FLstock

### Natural mortality
contr <- read.csv(file=c("IberianSardine/sardine_ctl.csv"),header = TRUE,skip = 24, sep=";")
m <- contr[1,1:7]
flq<- FLQuant(dimnames=list(age=age1, year=1978:2011))
flq[as.character(age1)] <- t(m)
m <-flq

# Maturity, According to ICES WGACEGG Report 2012 page 199 section 6.5, table 6.5.1
#mean values of micro DEPM
mat<-c(0,0.95,1,1,1,1,1) 
flq<- FLQuant(dimnames=list(age=age1,year=1978:2011))
flq[as.character(age1),] <- t(mat)
mat <- flq

stk <- FLStock(catch=catch,catch.n= agecomptot,catch.wt=wtatage, landings=catch,landings.n= agecomptot,landings.wt=wtatage, discards.wt=wtatage, m=m, mat=mat)
discards(stk) <-0
discards.n(stk) <-0
harvest.spwn (stk)<-0.5
m.spwn (stk) <-0.5  


######## Index Abundance
##### CPUE
 # 1 purse_seine
 # 2 Acoustic_survey
 # 3 DEPM_survey
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

save(stk, idx, CPUE, file="data/IberianSardine.RData") 

#################################################################################################
# NEA Northen Hake ./NorthernHake - SS
#################################################################################################

# Problem reading the file


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

# Check plots....recriutment, F, SSB...

stk <- readFLStock("./Plaice/Raised_and_Reconstructed/index_IV_VIId_raised+recon.txt")

discards(stk) <- computeDiscards(stk)
catch(stk) <- computeCatch(stk, slot="all")

idx <- readFLIndices("./Plaice/Raised_and_Reconstructed/fleet_trimmed.txt")

save(stk, idx, file="data/nsplaice.RData") # }}}


#################################################################################################
# ./SouthAfriAnchovy - TODO: By hand
#################################################################################################

data <- read.csv(file=c("SouthAfricAnchovy/SouthAfricAnchovy.csv"),header=FALSE, skip=11, sep="\t")


cage<-cage[1:28,1:2]

# Catch-at-age  from1984to2011(inrows)fromrecruits(age0)toage1(incolumns)for1Nov1984to31Oct2011  
# inbillionsofnumbers

age1<-c(0,1)

flq<- FLQuant(dimnames=list(age=age1,year=1984:2011),units="billions")
flq[as.character(age1)] <- t(cage)

catage<-flq

#### Weight at age

wtage<- read.csv(file= c("WCSAMdata/SouthAfriAnchovy/SouthAfricanAnchovy.csv"), header=FALSE, skip=42, sep="\t"  )
wtage<-wtage[1:28,1:2]


# Weight-at-age  inthecatchfrom1984to2011(inrows)fromrecruits(age0)toage1(incolumns)for1Nov1984to31Oct2011
# ingrams

flq<- FLQuant(dimnames=list(age=age1,year=1984:2011),units="grams")
flq[as.character(age1)] <- t(wtage)

wtatage<-flq


#  Weight-at-age1intheNovembersurveyfrom1984to2011	
#ingrams

wtage1<- read.csv(file= c("WCSAMdata/SouthAfriAnchovy/SouthAfricanAnchovy.csv"), header=FALSE, skip=73, sep="\t"  )
wtage1<-wtage1[1,]
wtage1<-t(as.numeric(t(wtage1)))

flq<- FLQuant(dimnames=list(age=age1,year=1984:2011),units="grams")
flq[as.character(1)]  <- wtage1[1,]

wtatage1<-flq

#  Weight-at-age2+intheNovembersurveyfrom1984to2011#ingrams	

wtage2<- read.csv(file= c("WCSAMdata/SouthAfriAnchovy/SouthAfricanAnchovy.csv"), header=FALSE, skip=77, sep="\t"  )
wtage2<-wtage2[1,]
wtage2<-t(as.numeric(t(wtage2)))

flq<- FLQuant(dimnames=list(age=c(0,1,2),year=1984:2011),units="grams")
flq[as.character(2)]  <- wtage2[1,]

wtatage2<-flq

#Observed  biomass(inthousandtons)intheNovembersurveyforNov1984toNov2011


obsbiom<- read.csv(file= c("WCSAMdata/SouthAfriAnchovy/SouthAfricanAnchovy.csv"), header=FALSE, skip=80, sep="\t"  )
obsbiom<-obsbiom[1,1:28]
obsbiom<-t(as.numeric(t(obsbiom)))
flq<- FLQuant(dimnames=list(year=1984:2011),units="thousand tons")
flq[]  <- obsbiom

biomass<-flq

#Observed  abundanceinDEPmethodforNov1984toNov1993	

abun<- read.csv(file= c("WCSAMdata/SouthAfriAnchovy/SouthAfricanAnchovy.csv"), header=FALSE, skip=87, sep="\t"  )
abun<-abun[1,1:10]
abun<-t(as.numeric(t(abun)))
flq<- FLQuant(dimnames=list(year=1984:2011))
flq[,as.character(c(1984:1993))]  <- abun

abun<-flq

#Observed  Proportion-at-age1intheNovembersurveyforNov1984toNov2011

propage<- read.csv(file= c("WCSAMdata/SouthAfriAnchovy/SouthAfricanAnchovy.csv"), header=FALSE, skip=93, sep="\t"  )
propage<-propage[1,1:28]
propage<-t(as.numeric(t(propage)))
flq<- FLQuant(dimnames=list(year=1984:2011))
flq[]  <- propage

propage<-flq


#################################################################################################
# ./SouthernHorseMack - ADMB
#################################################################################################

seems ok

#################################################################################################
# ./SpurDog - ADMB
#################################################################################################

dodgy ...

#################################################################################################
# ./YellowtailFlounder - ADAPT
#################################################################################################

readFLStock('YellowtailFlounder/ytfounder_datainfo.txt', type='Adapt')
seems ok


