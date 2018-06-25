#Alle Baumarten einlesen
for (file in list.files(path="../data/mast/", pattern="*.csv.bz2")) {
  tmp <- read.csv(paste("../data/mast/", file, sep=""), colClasses = "character")
  t1 <- cbind(art=strsplit(file, "\\.")[[1]][1],tmp[1:5], stack(tmp[,6:NCOL(tmp)]), stringsAsFactors = FALSE)
  t1$ind <- as.character(t1$ind)
  t1 <- t1[t1$values != "",]
  if(exists("dat")) {dat <- rbind(dat, t1)
  } else {dat <- t1}
}
names(dat)[7:8] <- c("mast","jahr")
dat$jahr <- as.integer(substring(dat$jahr,2))

rosetta <- data.frame(wg = c(4,5,7,13,15,16,17,25,27,28,33,39,41,42,50,52,55,59,66,72,86,87,90,94,95,100,103,105,109,110,115,121,123,126,129,131,135), Wuchsgebiet = c("VII-1","VII-3","VII-2","V-4","III-4","VII-5","VII-4","IIa-5","VII-6","IIb-3","I-6","IV-1","IV-2","IIa-1","I-1","I-2","IIa-2","IIa-3","I-3","I-4","I-5","IV-3","III-2","V-1","IIa-4","I-7","I-8","VI-3","V-2","III-1","V-3","III-3","VI-1","VI-2","I-9","IIb-2","IIb-1"))
dat <- merge(dat, rosetta, all.x=T)
table(dat$Wuchsgebiet[is.na(dat$wg)]) #Unmatched

rosetta <- data.frame(hk = 0:2, Höhenstufe = c("-900", "900-1500", "1500+"))
dat <- merge(dat, rosetta, all.x=T)
table(dat$Höhenstufe[is.na(dat$hk)]) #Unmatched

rosetta <- data.frame(bfi = c(102,101,203,206,208,209,201,204,207,302,304,305,307,308,310,312,313,316,306,301,314,413,401,402,403,404,406,408,409,411,412,405,410,501,503,504,505,502,601,602,603,605,606,609,611,612,608,614,604,703,706,801,802,803,804,901,610,613,202,205,710,717,701,716,713,705,704,702,303,315,707,311,719,718,709,708,714,715,309,607,615,407,711,712), Bezirk = c("Bgld. Süd","Bgld. Nord","Hermagor","Spittal a. d. Drau","Völkermarkt","Wolfsberg","Feldkirchen","Klagenfurt","Villach","Baden","Horn","Korneuburg","Lilienfeld","Melk","Neunkirchen","Scheibbs","Waidhofen / Thaya","Zwettl","Krems","Amstetten","Wiener Neustadt","Wels","Braunau am Inn","Freistadt","Gmunden","Kirchdorf an der Krems","Perg","Rohrbach","Schärding am Inn","Urfahr-Umgebung","Vöcklabruck","Linz","Steyr","Hallein","St.Johann i. Pongau","Tamsweg","Zell am See","Salzburg","Bruck a. d. Mur","Deutschlandsberg","Feldbach","Hartberg","Judenburg","Leoben","Mürzzuschlag","Murau","Leibnitz","Voitsberg","Graz","Innsbruck","Landeck","Bludenz","Bregenz","Dornbirn","Feldkirch","Wien","Liezen","Stainach","Friesach","St.Veit / Glan","Reutte","Telfs","Hall","Steinach","Schwaz","Kufstein","Kitzbühel","Imst","Gaenserndorf","Wien-Umgebung","Lechtal","St.Pölten","Zillertal","Wörgl","Matrei","Lienz","Sillian","Silz","Mistelbach","Knittelfeld","Weiz","Ried im Innkreis","Ried","St.Johann i. T. "))
dat <- merge(dat, rosetta, all.x=T)
table(dat$Bezirk[is.na(dat$bfi)]) #Unmatched

aggregate(art ~ bfi + HASC_2, data=dat, FUN=length)

#Redundante Daten loeschen
dat$Bezirk <- NULL
dat$Höhenstufe <- NULL
dat$Wuchsgebiet <- NULL
dat$Bundesland <- NULL
dat$HASC_2 <- NULL

#Mastangaben bereiningen
#1..Vollmast, 2..Halbmast, 3..Sprengmast, 4..Fehlernte
table(dat$mast)
dat$mast[dat$mast=="1-2"] <- 1.5
dat$mast[dat$mast=="1-2-3"] <- 2
dat$mast[dat$mast=="1-3"] <- 2
dat$mast[dat$mast=="1-4"] <- 2.5
dat$mast[dat$mast=="2-3"] <- 2.5
dat$mast[dat$mast=="2-4"] <- 3
dat$mast[dat$mast=="2(1)"] <- 2
dat$mast[dat$mast=="2(3)"] <- 2
dat$mast[dat$mast=="21-2"] <- 2
dat$mast[dat$mast=="3-2"] <- 3
dat$mast[dat$mast=="3-4"] <- 3.5
dat$mast[dat$mast=="3(2)"] <- 3
dat$mast[dat$mast=="4 (3)"] <- 4
dat$mast[dat$mast=="4-3"] <- 4
dat$mast[dat$mast=="4(3)"] <- 4
dat$mast <- as.numeric(dat$mast)

dat$art <- as.factor(dat$art)

dat <- na.omit(dat)

dat <- aggregate(mast ~ art + bfi + wg + hk + jahr, data=dat, FUN=mean)

dat <- reshape(dat, idvar = c("art","bfi","wg","hk"), timevar = "jahr", direction = "wide")
dat <- dat[,c(names(dat)[1:4],sort(names(dat)[5:NCOL(dat)]))]
dat <- dat[with(dat,order(art,bfi,wg,hk)),]
write.csv(dat, bzfile("/tmp/mast.csv.bz2", compression = 9), row.names = F)







