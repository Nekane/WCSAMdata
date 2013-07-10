###########################################################################
# The a4a stock assessment model – a nonlinear mixed effects model in
# FLR/R with an interface based on linear and additive model formulae
###########################################################################

# Introduction
# a4a is a new statistical framework for age-based fish stock assessment implemented as an R library called FLa4a that uses existing data
# structures and functions in the Fisheries Libraries in R (FLR) library. The framework was designed to be flexible in terms of model structure,
# able to provide robust results quickly, while also being easy to use. The framework was developed under the scope of the "assessment for
# all" (a4a) initiative of the European Commission Joint Research Centre, which is designed to extend the application of stock assessment
# models and forecasting to a growing number of data-moderate fish stocks and eventually to all fish stocks in a sea basin or ecosystem,
# exploring the benefits arising from using a unified coherent assessment framework.

# Methods
# Utilizing existing powerful model specification tools in the statistical software environment R, in particular the linear and additive model
# formula interfaces, the model provides flexibility and ease of use, giving access to a wide variety of established modelling tools. The fast and
# robust fitting is achieved by using an automatic differentiation based optimiser written in C++ (ADMB), which also provides estimates of the
# parameters' statistical properties.

# library(devtools)
# library(r4ss)
# library(plyr)
# library(reshape)
library(FLCore)
library(FLa4a)
library(ggplot2)
library(RColorBrewer)


###########################################################################
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


with(as.data.frame(catch.n(stk)), tapply(data, age, function(x) sum(x == 0)))

#### we don't have a plusgroup so we need to set one with the highest value possible with no 0 (in this case 10)
stk@range
stk <- setPlusGroup(stk, 10)

### F model 1 is an intercept and slope for age and year

fmodel <- ~factor(age) + factor(year)

#### let's look how many survey we have in the index
lapply(idx, range)

##### we need to remove year 2012

idx<-window(idx, start=1983, end=2011)

#### we have 1 so we need to make a list of 1

qmodel <- list(~factor(age))

fit1 <- a4a(fmodel, qmodel, stock=stk, indices=idx)

stk1 <- stk+fit1

plot(stk1, main="North Sea Cod - F model = ~factor(age) + factor(year)")
#plot(fit1, stk1) # not working in the new version of a4a


### F model 2 is a thin plate spline over age and year
fmodel2 <- ~s(age, k=4) + s(year, k=7)
fit2 <- a4a(fmodel2, qmodel, stock=stk, indices=idx)
stk2 <- stk+fit2
plot(stk2, main="North Sea Cod - F model = ~s(age, k=4) + s(year, k=7)")

### F model 3 with interactions between year and age
fmodel3 <- ~year * age
fit3 <- a4a(fmodel3, qmodel, stock=stk, indices=idx)
stk3 <- stk+fit3
plot(stk3, main="North Sea Cod - F model = ~year * age")

### F model 4 is an intercept and slope for year and a thin plate spline over age
fmodel4 <- ~s(age, k=4) + factor (year)
fit4 <- a4a(fmodel4, qmodel, stock=stk, indices=idx)
stk4 <- stk+fit4
plot(stk4, main="North Sea Cod - F model = ~s(age, k=4) + factor (year)")

### F model 5 is an intercept and slope for age and a thin plate spline over year
fmodel5 <- ~factor(age) + s(year, k=14)
fit5 <- a4a(fmodel5, qmodel, stock=stk, indices=idx)
stk5 <- stk+fit5
plot(stk5, main="North Sea Cod - F model = ~factor(age) + s(year, k=14)")

### F model 0 is costant trough age and year
fmodel0 <- ~1
fit0 <- a4a(fmodel0, qmodel, stock=stk, indices=idx)
stk0 <- stk+fit0
plot(stk0, main="North Sea Cod - F model = ~1")

### we can compare the AIC and BIC to select the best fmodel

AIC(fit1, fit2, fit3, fit4,fit5, fit0)
BIC(fit1, fit2, fit3, fit4,fit5, fit0)


a<-AIC(fit1, fit2, fit3, fit4,fit5, fit0)
b<-BIC(fit1, fit2, fit3, fit4,fit5, fit0)
c<-merge(a,b)
c<-c[c(6,3,2,5,4,1),]
c$Submodel<- c("fmodel1","fmodel2","fmodel3","fmodel4","fmodel5","fmodel0")
c[,c(4,1,2,3)]

mods <- list(stk1, stk2, stk3, stk4, stk5,stk0)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")

##### Recruitment
dat1 <-
  do.call(rbind,
    lapply(1:length(mods), 
       function(i) {
        cbind(as.data.frame(rec(mods[[i]])), mod = i)
       })
)

cols <- brewer.pal(max(3, length(mods)), "Set1")
xyplot(data ~ year, group = mod, data = dat1, type = "l", 
       col = cols, lty = 1, lwd = 2)

p2<-ggplot(dat1)+ geom_line(aes(year,data,col=factor(mod)),size=1.3)+ theme(axis.title.y = element_blank(),axis.title.x = element_blank(),plot.title = element_text(lineheight=.8, face="bold"))+ ggtitle("Recruitment")+ scale_colour_manual(name="Sub-model",labels=c("fmodel1", "fmodel2", "fmodel3","fmodel4","fmodel5","fmodel0"),values=cbbPalette)
# + opts(panel.background = theme_rect(fill="white"))
p2

#### Fbar
dat2 <-
  do.call(rbind,
          lapply(1:length(mods), 
                 function(i) {
                   cbind(as.data.frame(fbar(mods[[i]])), mod = i)
                 })
  )

cols <- brewer.pal(max(3, length(mods)), "Set1")
xyplot(data ~ year, group = mod, data = dat2, type = "l", 
       col = cols, lty = 1, lwd = 2)

p4<-ggplot(dat2)+ geom_line(aes(year,data,col=factor(mod)),size=1.3)+ theme(axis.title.y = element_blank(),plot.title = element_text(lineheight=.8, face="bold"))+ ggtitle("Mean F")+ scale_colour_manual(name="Sub-model",labels=c("fmodel1", "fmodel2", "fmodel3","fmodel4","fmodel5","fmodel0"),values=cbbPalette)
p4

##### Total Catch
dat3 <-
  do.call(rbind,
          lapply(1:length(mods), 
                 function(i) {
                   cbind(as.data.frame(catch(mods[[i]])), mod = i)
                 })
  )

cols <- brewer.pal(max(3, length(mods)), "Set1")
xyplot(data ~ year, group = mod, data = dat3, type = "l", 
       col = cols, lty = 1, lwd = 2)

p3<-ggplot(dat3)+ geom_line(aes(year,data,col=factor(mod)),size=1.3)+ theme(axis.title.y = element_blank(),axis.title.x = element_blank(),plot.title = element_text(lineheight=.8, face="bold"))+ ggtitle("Total Catch")+ scale_colour_manual(name="Sub-model",labels=c("fmodel1", "fmodel2", "fmodel3","fmodel4","fmodel5","fmodel0"),values=cbbPalette)
p3

#### SSB
dat4 <-
  do.call(rbind,
          lapply(1:length(mods), 
                 function(i) {
                   cbind(as.data.frame(ssb(mods[[i]])), mod = i)
                 })
  )

cols <- brewer.pal(max(3, length(mods)), "Set1")
xyplot(data ~ year, group = mod, data = dat4, type = "l", 
       col = cols, lty = 1, lwd = 2)

p1<-ggplot(dat4)+ geom_line(aes(year,data,col=factor(mod)),size=1.3)+ theme(axis.title.y = element_blank(),axis.title.x = element_blank(),plot.title = element_text(lineheight=.8, face="bold"))+ ggtitle("SSB")+ scale_colour_manual(name="Sub-model",labels=c("fmodel1", "fmodel2", "fmodel3","fmodel4","fmodel5","fmodel0"),values=cbbPalette)
p1

######  GGplot multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# x11()  #### to open a new device for plotting

multiplot(p1,p3, p2, p4, cols=2)

######  Lattice multiplot

stks <- list(stk1, stk2, stk3, stk4, stk5, stk0)

FUNCs <- c("ssb", "catch", "fbar", "rec")

getSumm <- function(stks, FUNCs) {
  do.call(rbind,
          lapply(FUNCs, function(x)
          {
            FUN <- match.fun(x)
            out <- as.data.frame(FUN(stks))
            out $ age <- paste(out $ age)
            cbind(out, fun = x)
          })
  )
}

dats <-
  do.call(rbind,
          lapply(seq(stks), function(i) {
            out <- getSumm(stks[[i]], FUNCs = FUNCs)
            cbind(out, stks = i)
          })
  )

dats $ cfun <- factor(dats $ fun, levels = c("ssb","rec","catch","fbar"), labels = c("SSB","Recruitment","Total Catch","Mean F"))
xyplot(data ~ year | cfun, group = stks, data = dats,
       scales = list(y = list(relation = "free", rot = 0)),
       col = brewer.pal(length(stks), "Set1"),
       lty = 1, type = c("l","g"), lwd = 2,
       as.table = TRUE,
       ylab = "", xlab = "Year", between = list(x = 0.5, y = 0.5))


# Conclusions
# The a4a model, when compared to other stock assessment models, provides the user with both
# ease of use and sufficient flexibility, while obtaining robust results. The setup of the model
# permits a wide variety of sub-model implementations with just a few lines of code. Moreover, the
# analyst can then compare the results of the different stock assessments using a number of
# provided plots and diagnostics.


# References
# https://fishreg.jrc.ec.europa.eu/web/a4a/
# R Development Core Team (2013) R: A Language and Environment for Statistical Computing. R Foundation for Statistical Computing, Vienna, Austria. URL http://www.R-project.org/. ISBN
# 3-900051-07-0.
# Kell LT, Mosqueira I, Grosjean P, Fromentin JM, Garcia D, et al. (2007) FLR: an open-source framework for the evaluation and development of management strategies. ICES Journal of Marine
# Science: Journal du Conseil 64: 640–646.
# Fournier D, Skaug H, Ancheta J, Ianelli J, Magnusson A, et al. (2012) AD Model Builder: using automatic differentiation for statistical inference of highly parameterized complex non-linear
# models. Optimization Methods and Software 27: 233–249.
