
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
library(FLa4a)

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

######## There is only the table with some catch and some survey... this is the code of Iago!!!!

data <- read.table("BiscayAnchovy/anchovy.dat", header=TRUE)

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

### WORKS!!! 

stk <- readFLStock("Haddock/hadividx.dat")

# All catch in landings, set discards to 0
discards.n(stk) <- 0
discards.wt(stk) <- landings.wt(stk)

catch(stk) <- computeCatch(stk, slot="all")

# FLIndex
idx <- readFLIndices("Haddock/hadivef.dat")

# }}}

#====================================================================
# fishing mortality by age and year (~separable)
# catchability at age without year trend
#====================================================================

#### We have some 0 in catch so we replace them by NA in order to select the proper plusgroups

with(as.data.frame(catch.n(stk)), tapply(data, age, function(x) sum (x==0)))

#### we don't have a plusgroup so we need to set one with the highest value possible (in this case 13 does work, when it is higher we have a strange extimation of the SSB)
stk@range

stk <- setPlusGroup(stk,10)
#### let's look how many survey we have in the index
lapply(idx, range)
#### we have 5, so we need to make a list of 5 factors in the q model

fmodel <- ~factor(age) + factor(year)
qmodel <- list(~factor(age), ~factor(age), ~factor(age), ~factor(age), ~factor(age))
fit1 <- a4a(fmodel, qmodel, stock=stk, indices=idx)

save(stk, idx, fit1, file="data/haddock.RData") 

stk <- stk+fit1

plot(stk)
plot(fit1, stk)


#################################################################################################
# ./Herring - VPA TODO: FIX broken use of VPA suite codes
#################################################################################################
#Check the Indices
# the stock is not loading everything

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
###########################################################################

####################### WORK!!!!!!   

stk <- readFLStock("NSCod/Cod347_ext.idx")

# All catch in landings, set diacrds to 0
discards.n(stk) <- 0
discards.wt(stk) <- landings.wt(stk)

catch(stk) <- computeCatch(stk, slot="all")

# FLIndex
idx <- readFLIndices("NSCod/Cod347_2012_ext.tun")



# }}}


#====================================================================
# fishing mortality by age and year (~separable)
# catchability at age without year trend
#====================================================================

#### We have some 0 in catch so the old a4a doesn't run
with(as.data.frame(catch.n(stk)), tapply(data, age, function(x) sum (x==0)))

#we select the plusgroup 10
stk <- setPlusGroup(stk, 10)

#### we don't have a plusgroup so we need to set one with the highest value possible (in this case 12 does work but nekane try with higher)
stk@range
#### let's look how many survey we have in the index
lapply(idx, range)
#the survey is until the 2012, but the stock data is until 2011, we cut the survey until 2011
idx_x<- window(idx, start=1983, end=2011)
lapply(idx_x, range)
#### we have 1 so we need to make a list of 1 factor in the q model

fmodel <- ~factor(age) + factor(year)
qmodel <- list(~factor(age))
fit1 <- a4a(fmodel, qmodel, stock=stk, indices=idx_x)

save(stk, idx, fit1,idx_x, file="data/nscod.RData")

stk <- stk+fit1

plot(stk)
plot(fit1, stk)


#################################################################################################
# North Sea Plaice ./Plaice - VPA {{{
#################################################################################################

# WORKS!!!!

stk <- readFLStock("Plaice/Raised_and_Reconstructed/index_IV_VIId_raised+recon.txt")

discards(stk) <- computeDiscards(stk)
catch(stk) <- computeCatch(stk, slot="all")

idx <- readFLIndices("Plaice/Raised_and_Reconstructed/fleet_trimmed.txt")

 # }}}

#====================================================================
# fishing mortality by age and year (~separable)
# catchability at age without year trend
#====================================================================

#### there are some -1 in the index so I change them in NA
idx[[3]]@index[,as.character(2003)] <- NA

#### We have some 0 in catch so the old a4a doesn't run
with(as.data.frame(catch.n(stk)), tapply(data, age, function(x) sum (x==0)))

stk@range

#we select the plusgroup 12
stk <- setPlusGroup(stk, 12)

fmodel <- ~factor(age) + factor(year)
#### let's look how many survey we have in the index
lapply(idx, range)
#### we have 3 so we need to make a list of 3 factors in the q model
qmodel <- list(~factor(age),~factor(age),~factor(age))
fit1 <- a4a(fmodel, qmodel, stock=stk, indices=idx)

save(stk, idx, fit1, file="data/nsplaice.RData")

stk <- stk+fit1

plot(stk)
plot(fit1, stk)



#################################################################################################
# ./SouthAfriAnchovy - TODO: By hand
#################################################################################################

# We imported everything that seemed relevant from the file but we are not really sure what they are and what is missing (for sure we didn't find m or mat)... 


##### catch at age

cage<- read.csv(file= c("SouthAfriAnchovy/SouthAfricanAnchovy.csv"), header=FALSE, skip=11, sep="\t"  )
cage<-cage[1:28,1:2]

# Catch-at-age  from  1984	to	2011	(in	rows)	from	recruits	(age	0)	to	age	1	(in	columns)	for	1	Nov	1984	to	31	Oct	2011				
# in	billions	of	numbers

age1<-c(0,1)

flq<- FLQuant(dimnames=list(age=age1,year=1984:2011),units="billions")
flq[as.character(age1)] <- t(cage)

catage<-flq

#### Weight at age

wtage<- read.csv(file= c("SouthAfriAnchovy/SouthAfricanAnchovy.csv"), header=FALSE, skip=42, sep="\t"  )
wtage<-wtage[1:28,1:2]


# Weight-at-age  in	the	catch	from	1984	to	2011	(in	rows)	from	recruits	(age	0)	to	age	1	(in	columns)	for	1	Nov	1984	to	31	Oct	2011	
# in	grams

flq<- FLQuant(dimnames=list(age=age1,year=1984:2011),units="grams")
flq[as.character(age1)] <- t(wtage)

wtatage<-flq


#  Weight-at-age	1	in	the	November	survey	from	1984	to	2011																	
#	in	grams	

wtage1<- read.csv(file= c("SouthAfriAnchovy/SouthAfricanAnchovy.csv"), header=FALSE, skip=73, sep="\t"  )
wtage1<-wtage1[1,1:28]
wtage1<-t(as.numeric(t(wtage1)))

flq<- FLQuant(dimnames=list(age=age1,year=1984:2011),units="grams")
flq[as.character(1)]  <- wtage1[1,]

wtatage1<-flq

#  Weight-at-age	2+	in	the	November	survey	from	1984	to	2011	#	in	grams		

wtage2<- read.csv(file= c("SouthAfriAnchovy/SouthAfricanAnchovy.csv"), header=FALSE, skip=77, sep="\t"  )
wtage2<-wtage2[1,1:28]
wtage2<-t(as.numeric(t(wtage2)))

flq<- FLQuant(dimnames=list(age=c(0,1,2),year=1984:2011),units="grams")
flq[as.character(2)]  <- wtage2[1,]

wtatage2<-flq

#Observed  biomass	(in	thousand	tons)	in	the	November	survey	for	Nov	1984	to	Nov	2011


obsbiom<- read.csv(file= c("SouthAfriAnchovy/SouthAfricanAnchovy.csv"), header=FALSE, skip=80, sep="\t"  )
obsbiom<-obsbiom[1,1:28]
obsbiom<-t(as.numeric(t(obsbiom)))
flq<- FLQuant(dimnames=list(year=1984:2011),units="thousand tons")
flq[]  <- obsbiom

biomass<-flq

#Observed  abundance	in	DEP	method	for	Nov	1984	to	Nov	1993		

abun<- read.csv(file= c("SouthAfriAnchovy/SouthAfricanAnchovy.csv"), header=FALSE, skip=87, sep="\t"  )
abun<-abun[1,1:10]
abun<-t(as.numeric(t(abun)))
flq<- FLQuant(dimnames=list(year=1984:2011))
flq[,as.character(c(1984:1993))]  <- abun

abun<-flq

#Observed  Proportion-at-age	1	in	the	November	survey	for	Nov	1984	to	Nov	2011	

propage<- read.csv(file= c("SouthAfriAnchovy/SouthAfricanAnchovy.csv"), header=FALSE, skip=93, sep="\t"  )
propage<-propage[1,1:28]
propage<-t(as.numeric(t(propage)))
flq<- FLQuant(dimnames=list(year=1984:2011))
flq[]  <- propage

propage<-flq


#Neffective  (weighting	applied	to	proportion-at-age	1)	

Neffective<- read.csv(file= c("SouthAfriAnchovy/SouthAfricanAnchovy.csv"), header=FALSE, skip=96, sep="\t"  )
Neffective<-Neffective[1,1:28]
Neffective<-t(as.numeric(t(Neffective)))
flq<- FLQuant(dimnames=list(year=1984:2011))
flq[]  <- Neffective
Neffective<-flq

#Recruit  catch	prior	to	May	survey	for	1985	to	2011	(in	billions)

Reccatch<- read.csv(file= c("SouthAfriAnchovy/SouthAfricanAnchovy.csv"), header=FALSE, skip=99, sep="\t"  )
Reccatch<-Reccatch[1,c(1:9,11:28)]
Reccatch<-t(as.numeric(t(Reccatch)))
flq<- FLQuant(dimnames=list(year=1984:2011),units="billions")
flq[,as.character(c(1985:2011))]  <- Reccatch

Reccatch<-flq

#Observed  Numbers	(in	billions)	in	the	recruit	survey	for	May	1985	to	May	2011

obsnum<- read.csv(file= c("SouthAfriAnchovy/SouthAfricanAnchovy.csv"), header=FALSE, skip=103, sep="\t"  )
obsnum<-obsnum[1,1:27]
obsnum<-t(as.numeric(t(obsnum)))
flq<- FLQuant(dimnames=list(year=1984:2011),units="billions")
flq[,as.character(c(1985:2011))]  <- obsnum

obsnum<-flq



#################################################################################################
# ./SouthernHorseMack - ADMB
#################################################################################################

# we imported the thing that seemed to be relevant but we cannot find the m... and since there is the fishery and the survey we don't know how to act... sorry 

#  Catch  Biomass																															
#	1992	1993	1994	1995	1996	1997	1998	1999	2000	2001	2002	2003	2004	2005	2006	2007	2008	2009	2010	


ctbiom<- read.csv(file= c("SouthernHorseMack/hm2011_nozero.csv"), header=FALSE, skip=15, sep="\t"  )
ctbiom<-ctbiom[1,1:19]
ctbiom<-t(as.numeric(t(ctbiom)))
flq<- FLQuant(dimnames=list(year=1992:2010))
flq[]  <- ctbiom

biomass<-flq

# Catch at age?
#  Fishery																																
#	0	1	2	3	4	5	6	7	8	9	10	11
cth<- read.csv(file= c("SouthernHorseMack/hm2011_nozero.csv"), header=FALSE, skip=27, sep="\t" ,nrows=19 )

cth<-cth[,1:12]
summary(cth)

age1<-c(0:11)

flq<- FLQuant(dimnames=list(age=age1,year=1992:2010))
flq[as.character(age1)] <- t(cth)

catage<-flq

#  Fishery	wt	age	in	grams
wtage<- read.csv(file= c("SouthernHorseMack/hm2011_nozero.csv"), header=FALSE, skip=47, sep="\t" ,nrows=19 )

wtage<-wtage[,1:12]
summary(wtage)

age1<-c(0:11)

flq<- FLQuant(dimnames=list(age=age1,year=1992:2010),units="grams")
flq[as.character(age1)] <- t(wtage)

wtatage<-flq

#  Biomass	indices	of	survey	*	1000	(numbers	*	weight	*	1000)	

obsbiom<-read.csv(file= c("SouthernHorseMack/hm2011_nozero.csv"), header=FALSE, skip=77, sep="\t" ,nrows=1 )

obsbiom<-obsbiom[1,1:19]
obsbiom<-t(as.numeric(t(obsbiom)))
flq<- FLQuant(dimnames=list(year=1992:2010))
flq[]  <- obsbiom
summary(flq)
biomass<-flq

#  Survey	*	0.3	??????

survey<-read.csv(file= c("SouthernHorseMack/hm2011_nozero.csv"), header=FALSE, skip=79, sep="\t" ,nrows=1 )

survey<-survey[1,1:19]
survey<-t(as.numeric(t(survey)))
flq<- FLQuant(dimnames=list(year=1992:2010))
flq[]  <- survey
summary(flq)
survey<-flq


#  Survey	age	data	(age	zero	is	removed	--	put	=	0)	

cthagesur<- read.csv(file= c("SouthernHorseMack/hm2011_nozero.csv"), header=FALSE, skip=88, sep="\t" ,nrows=19 )

cthagesur<-cthagesur[,1:12]
summary(cthagesur)

age1<-c(0:11)

flq<- FLQuant(dimnames=list(age=age1,year=1992:2010))
flq[as.character(age1)] <- t(cthagesur)

catagesur<-flq


#  Survey	weight	at	age	in	Kg	

wtagesur<- read.csv(file= c("SouthernHorseMack/hm2011_nozero.csv"), header=FALSE, skip=108, sep="\t" ,nrows=19 )

wtagesur<-wtagesur[,1:12]
summary(wtagesur)

age1<-c(0:11)

flq<- FLQuant(dimnames=list(age=age1,year=1992:2010),units="kg")
flq[as.character(age1)] <- t(wtagesur)

wtatagesur<-flq

#  Maturity	at	Age


mat<- read.csv(file= c("SouthernHorseMack/hm2011_nozero.csv"), header=FALSE, skip=131, sep="\t" ,nrows=1 )

mat<-mat[,1:12]
summary(mat)
mat<-t(as.numeric(t(mat)))

age1<-c(0:11)

flq<- FLQuant(dimnames=list(age=age1,year=1992:2010))
flq[as.character(age1)] <- mat

mat<-flq

#################################################################################################
# ./SpurDog - ADMB
###########################################################################

###################### We didn't look at it...

dodgy ...

#################################################################################################
# ./YellowtailFlounder - ADAPT
###########################################################################

###################### We cannot find the things to create the FLIndex but maybe you will... The FLStock should be ok 


# readFLStock('YellowtailFlounder/Georges_Bank_yellowtail_flounder.txt', type='Adapt')

##### years from 1973 to 2011????????????????
# CATCH AT AGE

cth<- read.csv(file= c("YellowtailFlounder/Georges_Bank_yellowtail_flounder.csv"), header=FALSE, skip=64, sep="\t" ,nrows=39 )

cth<-cth[,1:6]
summary(cth)

age1<-c(0:5)

flq<- FLQuant(dimnames=list(age=age1,year=1973:2011))
flq[as.character(age1)] <- t(cth)

catage<-flq


# WEIGHT  AT  AGE	


wtage<- read.csv(file= c("YellowtailFlounder/Georges_Bank_yellowtail_flounder.csv"), header=FALSE, skip=104, sep="\t" ,nrows=39 )

wtage<-wtage[,1:6]
summary(wtage)

age1<-c(0:5)

flq<- FLQuant(dimnames=list(age=age1,year=1973:2011))
flq[as.character(age1)] <- t(wtage)

wtatage<-flq


#FLStock


YelFl <- FLStock(catch.wt=wtatage,catch.n=catage,landings.wt=wtatage,landings.n=catage,discards.wt=wtatage)
m(YelFl)<-0.2
mat(YelFl)<-c(0,  0.462,	0.967,1,1,1	)
# m.spwn(YelFl)<-0.5
# harvest.spwn(YelFl)<-0.5
discards(YelFl)<-0
discards.n(YelFl)<-0

catch(YelFl) <-quantSums(catch.n(YelFl)*catch.wt(YelFl))

# catch(YelFl) <- computeCatch(stk, slot="all")

summary(YelFl)
plot(YelFl) #### it doesn't work...

# YelFl <- setPlusGroup(YelFl, plusgroup=15)
# 
# xsa.control <- FLXSA.control()
# 
# xsa.results <- FLXSA(YelFl, idx, xsa.control)
# 
# YelFl <- YelFl + xsa.results
# plot(YelFl)