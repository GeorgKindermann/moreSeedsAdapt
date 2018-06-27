dat <- read.csv("../data/mast/mast.csv.bz2")

dat <- reshape(dat, direction="long", varying=5:NCOL(dat), timevar = "jahr")
dat$id <- NULL
dat <- na.omit(dat)

#Die entfernen die immer nur 4 bei der Mast angeben
t0 <- aggregate(mast ~ art + bfi + wg + hk, data=dat, FUN=min)
t0 <- t0[t0$mast < 4,]
t0$mast <- NULL
dat <- merge(dat, t0)

#Nur Fichte
dat <- dat[dat$art == "fichte",]

datJr <- read.csv("../data/jahrringbreiten/jahrringFichteWgBfiHk.csv.bz2")
datJr$baum <- 1:NROW(datJr)
datJr <- reshape(datJr, direction="long", varying=4:(NCOL(datJr)-1), timevar = "jahr", idvar = "baum")
datJr <- datJr[!is.na(datJr$ir),]

datJr <- datJr[with(datJr,order(baum,jahr)),]
datJr$idxPre <- with(datJr, ave(ir, baum, FUN=function(x) {2*x/(x+c(NA, x[-length(x)]))}))
datJr$idxPost <- with(datJr, ave(ir, baum, FUN=function(x) {2*x/(x+c(x[-1],NA))}))

me <- merge(datJr[,c("bfi", "wg", "hk", "jahr", "idxPre", "idxPost")], dat[,c("bfi", "wg", "hk", "jahr", "mast")])

boxplot(me$idxPre ~ me$mast)
abline(1,0)
boxplot(me$idxPost ~ me$mast)
abline(1,0)

t1 <- aggregate(cbind(idxPre, idxPost) ~ mast, data=me, FUN=median)
plot(t1$mast, t1$idxPre, type="l")
lines(t1$mast, t1$idxPost, col=2)
abline(1,0)
legend("top", legend=c("Idx-Pre","Idx-Post"), col=1:2, lty=1)

