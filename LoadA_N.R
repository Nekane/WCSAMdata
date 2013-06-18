showMethods("ssb", includeDefs=T)

summary(stk)

qapply(stk, mean)

discards.wt(stk)
discards.wt(stk)[]<-0

FLIndex()


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
stk <- readMFCL(file=c("AlbacoreTuna/plot-09.par.rep",
                       "AlbacoreTuna/09.par"))

save(stk, file="data/natlalb.RData")

data <- read.csv(file=c("AlbacoreTuna/albN.csv"),header = TRUE,skip = 41, sep=";")


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
FLQuants(cth=cth, eff=eff, len=flqLen)
})

#lst[[1]]$cth # cth, eff, len
#



  flqLen <- 
}

summary(flq)

    # }}}
#################################################################################################
# BoB Anchovy ./BiscayAnchovy - {{{

data <- read.table("./BiscayAnchovy/anchovy.dat", header=TRUE)

catch <- FLQuant(dimnames=list(year=data$year, age=c(1, 2)))
catch[1,] <- data$c12
catch[2,] <- data$c22

# }}}


#################################################################################################
# ./CanaryRockfish - SS
