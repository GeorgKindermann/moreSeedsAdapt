dat <- read.csv("../data/mast/mast.csv.bz2")

dat <- reshape(dat, direction="long", varying=5:NCOL(dat), timevar = "jahr")
dat$id <- NULL
dat <- na.omit(dat)

table(dat$art, dat$mast)

#Die entfernen die immer nur 4 bei der Mast angeben
t0 <- aggregate(mast ~ art + bfi + wg + hk, data=dat, FUN=min)
t0 <- t0[t0$mast < 4,]
t0$mast <- NULL
dat <- merge(dat, t0)

library(ggplot2)

pdf("/tmp/moreSeedsAdapt_overview.pdf")

t0 <- aggregate(mast ~ art + jahr, data=dat, FUN=length)
names(t0)[3] <- "nn"
t0 <- merge(expand.grid(art=unique(t0$art), jahr=unique(t0$jahr)), t0, all.x=T)
t0$nn[is.na(t0$nn)] <- 0
t1 <- aggregate(hk ~ art + jahr + mast, data=dat, FUN=length)
names(t1)[4] <- "n"
t1 <- merge(expand.grid(art=unique(t1$art), jahr=unique(t1$jahr), mast=unique(t1$mast)), t1, all.x=T)
t1$n[is.na(t1$n)] <- 0
t1 <- merge(t0, t1)
tba <- unique(t1$art)
for(i in 1:length(tba)) {
  sc <- max(t0$nn[t0$art==tba[i]])
  print(ggplot() + geom_area(data = t1[t1$art==tba[i],], aes(x=jahr,y=n,group=as.factor(mast),fill=as.factor(mast)), position="fill") + ggtitle(paste("Baumart:",tba[i])) + ylab("Anteil") + geom_line(data = t0[t0$art==tba[i],], aes(x = jahr, y=nn/max(nn))) + scale_y_continuous(sec.axis = sec_axis(~.*sc, name = "Beobachtungen")) + labs(fill = "Mast") )
}

t0 <- aggregate(mast ~ art + jahr + hk, data=dat, FUN=length)
names(t0)[4] <- "nn"
t0 <- merge(expand.grid(art=unique(t0$art), jahr=unique(t0$jahr), hk=unique(t0$hk)), t0, all.x=T)
t0$nn[is.na(t0$nn)] <- 0
t1 <- aggregate(bfi ~ art + jahr + hk + mast, data=dat, FUN=length)
names(t1)[5] <- "n"
t1 <- merge(expand.grid(art=unique(t1$art), jahr=unique(t1$jahr), hk=unique(t0$hk), mast=unique(t1$mast)), t1, all.x=T)
t1$n[is.na(t1$n)] <- 0
t1 <- merge(t0, t1)
thk <- unique(t1$hk)
tthk <- c("-900","900-1500",">1500")
for(i in 1:length(tba)) {
  for(j in 1:length(thk)) {
    sc <- max(t0$nn[t0$art==tba[i] & t0$hk==thk[j]])
    if(sc > 0) {
      print(ggplot() + geom_area(data = t1[t1$art==tba[i] & t1$hk==thk[j],], aes(x=jahr,y=n,group=as.factor(mast),fill=as.factor(mast)), position="fill") + ggtitle(paste("Baumart:",tba[i], "   Hoehenstufe:", tthk[j])) + ylab("Anteil") + geom_line(data = t0[t0$art==tba[i] & t0$hk==thk[j],], aes(x = jahr, y=nn/max(nn))) + scale_y_continuous(sec.axis = sec_axis(~.*sc, name = "Beobachtungen")) + labs(fill = "Mast") )
    }
  }
}

dev.off()





