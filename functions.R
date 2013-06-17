# functions.R - DESC
# functions.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC

# readSS3Stock {{{
readSS3Stock <- function(dir, name="", desc=paste("read from SS3:", dir), ...) {

	# load list from SS3 files woith r4ss::SS_output
	out <- SS_output(dir, verbose=FALSE, hidewarn=TRUE,
		warn=FALSE, printstats=FALSE)

	# what elements do we want?
	quants <- c("M_at_age", "natage", "wtatage", "Z_at_age", "catage",
		"nseasons", "nareas", "nsexes", "endgrowth")
	# subset & rename
	out <- out[quants]
	names(out) <- c("m", "n", "w", "z", "c", "season", "area", "unit", "biol")
	
	# range
	range <- getRange(out)

	# dimnames
	dmns <- getDimnames(out, range)

	# select abundances at start of year: Beg/Mid == 'B'
	out$n <- out$n[out$n[,"Beg/Mid"]=="B",]

	# create FLStock object
	res <- FLStock(
		name=name, desc=desc, range=range,
		harvest = FLQuant(c(t(out$z[, -(1:3)])), dimnames=dmns) -
			FLQuant(out$biol$M, dimnames=dmns),
		stock.n = FLQuant(c(t(out$n[,-(1:11)])), dimnames=dmns),
		stock.wt = FLQuant(out$biol$Wt_Beg, dimnames=dmns),
		m = FLQuant(out$biol$M, dimnames=dmns),
		mat = FLQuant(out$biol$Age_Mat, dimnames=dmns),
		m.spwn = FLQuant(0, dimnames=dmns),
		harvest.spwn=FLQuant(0, dimnames=dmns))

	# subset catch from first year of data
	out$c=subset(out$c, Yr >= range['minyear'])

	# subset weights from fleet > 0
	out$w=subset(out$w,fleet>0)

	# fix names
	names(out$w)[c(1:3)] <- c("year", "season", "unit")
	names(out$c)[c(1:3, 7:8)] <- c("area","fleet","unit","year","season")

	# caa
	caa <- melt(out$c [,-c(4:6, 9:10)], id=c("year","season","unit","fleet","area"))
	caa <- transform(caa, variable=ac(variable))
	caa <- transform(caa, catch=value, age=as.numeric(variable))[,
		c("fleet","age","year","season","unit","catch")]
      
	# waa
	waa <- melt(out$w [,-(4:5)], id=c("year","season","unit","fleet"))
	waa <- transform(waa, wt=value, variable=ac(variable), year=-year)
	waa <- transform(waa, data=value, age=as.numeric(substr(variable, 2,
		nchar(variable))))[, c("fleet","age","year","season","unit","wt")]

	# cw
	cw <- as.FLQuant(ddply(merge(caa, waa), .(age,year,season,unit), with,
		data.frame(data=sum(wt*catch)/sum(catch))))
	cw <- cw[, -dim(cw)[2]]
	# NAs to 0
	cw[is.na(cw)] <- 0
  dimnames(cw) <- dmns
	# *.wt slots = catch.wt
	catch.wt(res) <- cw
	discards.wt(res) <- cw
	landings.wt(res) <- cw
   
	# catch.n
  caa <- as.FLQuant(ddply(caa, .(age,year,season,unit), with,
		data.frame(data=sum(catch))))
	caa <- caa[,-dim(caa)[2]]
	dimnames(caa) <- dmns
	# catch.n = landings.n, no discards
	catch.n(res) <- caa
	landings.n(res) <- caa
	discards.n(res) <- 0

	# harvest
  units(res)$harvest <- "f"
	harvest(res)[ac(range["plusgroup"])] <- harvest(res)[ac(range["plusgroup"]-1)]
  
	# load slots provided
	args <- list(...)
	for (i in names(args))
		slot(res, i) <- args[[i]]

	# compute catch for consistency
	catch(res) <- computeCatch(res)

	return(res)
} # }}}

