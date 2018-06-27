dat <- read.csv("../data/mast/mast.csv.bz2")

dat <- reshape(dat, direction="long", varying=5:NCOL(dat), timevar = "jahr")
dat$id <- NULL
dat <- na.omit(dat)

#Die entfernen die immer nur 4 bei der Mast angeben
t0 <- aggregate(mast ~ art + bfi + wg + hk, data=dat, FUN=min)
t0 <- t0[t0$mast < 4,]
t0$mast <- NULL
dat <- merge(dat, t0)

datKlim <- read.csv("../data/wetter/wetterWgBfiHk.csv.bz2")

#Tage
t1 <- with(datKlim, as.POSIXlt(paste(jahr,monat,"1",sep="-")))
t2 <- t1
t2$mon <- t2$mon + 1
datKlim$tage <- as.numeric(t2 - t1)
rm(t1, t2)

#Jahresmitteltemperatur
t1 <- aggregate(tas*tage ~ wg + bfi + hk + jahr, data=datKlim, FUN=sum)
t2 <- aggregate(tage ~ wg + bfi + hk + jahr, data=datKlim, FUN=sum)
me <- merge(t1, t2)
me$t <- me[,5] / me[,6]
rm(t1, t2)
me <- merge(dat, me[,c("bfi", "wg", "hk", "jahr", "t")])
me <- me[with(me,order(art,bfi,wg,hk,jahr)),]
me$tPre <- with(me, ave(t, paste(art,bfi,wg,hk), FUN=function(x) {c(NA, x[-length(x)])}))
me$dt <- with(me, ave(jahr, paste(art,bfi,wg,hk), FUN=function(x) {x-c(NA, x[-length(x)])}))
me$tPre[me$dt!=1] <- NA
me$dt <- NULL

with(me[me$art=="fichte",], boxplot(t ~ mast))
with(me[me$art=="fichte",], boxplot(tPre ~ mast))
with(me[me$art=="fichte",], boxplot(tPre-t ~ mast))
#Das Vorjahr war kaelter als Heuer bzw. Heuer waermer als Vorjahr -> Mastjahr bei Fichte
with(me[me$art=="fichte" & me$hk==0,], boxplot(tPre-t ~ mast))
with(me[me$art=="fichte" & me$hk==1,], boxplot(tPre-t ~ mast))
with(me[me$art=="fichte" & me$hk==2,], boxplot(tPre-t ~ mast))
#Das trifft fuer alle Hoehenstufen zu

pdf("/tmp/mastDTemperatur.pdf")
for(sart in unique(me$art)) {
  try(with(me[me$art==sart,], boxplot(tPre-t ~ mast, main=sart)))
  try(with(me[me$art==sart & me$hk==0,], boxplot(tPre-t ~ mast, main=paste(sart, "-900"))))
  try(with(me[me$art==sart & me$hk==1,], boxplot(tPre-t ~ mast, main=paste(sart, "900-1500"))))
  try(with(me[me$art==sart & me$hk==2,], boxplot(tPre-t ~ mast, main=paste(sart, ">1500"))))
}
dev.off()

me$tAvg <- with(me, ave(t, paste(art,bfi,wg,hk), FUN=function(x) {mean(x)}))
with(me[me$art=="fichte",], boxplot(t-tAvg ~ mast))
with(me[me$art=="fichte",], boxplot(tPre-tAvg ~ mast))

pdf("/tmp/mastDTemp2.pdf")
for(sart in unique(me$art)) {
  with(me[me$art==sart,], boxplot(tPre-tAvg ~ mast, main=sart,ylab="tPre-tAvg")); abline(h=0)
  with(me[me$art==sart,], boxplot(t-tAvg ~ mast, main=sart,ylab="t-tAvg")); abline(h=0)
}
dev.off()





