dat <- read.csv("../data/mast/mast.csv.bz2")

dat <- reshape(dat, direction="long", varying=5:NCOL(dat), timevar = "jahr")
dat$id <- NULL
dat <- na.omit(dat)

#Die entfernen die immer nur 4 bei der Mast angeben
t0 <- aggregate(mast ~ art + bfi + wg + hk, data=dat, FUN=min)
t0 <- t0[t0$mast < 4,]
t0$mast <- NULL
dat <- merge(dat, t0)

dat <- reshape(dat, idvar = c("art","bfi","wg","hk"), timevar = "jahr", direction = "wide")
dat <- dat[,c(names(dat)[1:4],sort(names(dat)[5:NCOL(dat)]))]
dat <- dat[with(dat,order(art,bfi,wg,hk)),]

farben <- c("#FF0000", "#FFFF00", "#00FF00", "#00FFFF", "#0000FF", "#FF00FF", "#640000", "#006400")

library(cluster)
d <- dist(dat[dat$art == "fichte", grep("^mast....", names(dat))], method = "manhattan")
d[is.na(d)] <- median(d, na.rm=T)
#
fit <- hclust(d, method="ward.D2")
plot(fit, labels = F, main="Fichte - hclust,Manhattan,Ward")
rect.hclust(fit, k=6, border=farben)
#
fit <- agnes(d, dis=T, method="ward")
plot(fit, which.plots=2, labels = F, main="Fichte - agnes,Manhattan,Ward")
rect.hclust(fit, k=6, border=farben)
#
fit <- diana(d, dis=T)
plot(fit, which.plots=2, labels = F, main="Fichte - diana,Manhattan")
rect.hclust(fit, k=6, border=farben)

dat$group <- NA
d <- dist(dat[dat$art == "fichte", grep("^mast....", names(dat))], method = "manhattan")
d[is.na(d)] <- median(d, na.rm=T)
fit <- hclust(d, method="ward.D2")
plot(fit, labels = F, main="Fichte - hclust,Manhattan,Ward")
rect.hclust(fit, k=3, border=farben)
dat$group[dat$art == "fichte"] <- cutree(fit, k=3)
#
d <- dist(dat[dat$art == "rotbuche", grep("^mast....", names(dat))], method = "manhattan")
d[is.na(d)] <- median(d, na.rm=T)
fit <- hclust(d, method="ward.D2")
plot(fit, labels = F, main="Buche - hclust,Manhattan,Ward")
rect.hclust(fit, k=4, border=farben)
dat$group[dat$art == "rotbuche"] <- cutree(fit, k=4)
#
tt <- dat[,c("art", "wg", "bfi", "hk", "group")]
tt <- tt[!is.na(tt$group),]
tt <- reshape(data=tt, v.names = "group", idvar = c("wg", "bfi", "hk"), timevar = "art", direction = "wide")
write.table(tt, "/tmp/cluster.txt", quote = F, row.names = F)

