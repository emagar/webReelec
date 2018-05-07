rm(list = ls())


d <- read.csv(file = "/Volumes/DANIEL/sonora_12_15.csv", stringsAsFactors = FALSE)

head(d)
colnames(d)<-c("secc","dist_new","dist_old")
# dsi seen from offspring perspective
# new district's "father" and district similarity index, cf. Cox & Katz
d$father <- NA
d$dsi <- 0
for (i in 1:21){
  #i <- 16 # debug
  sel.n <- which(d$dist_new==i)                  # secciones in new district
  tmp <- table(d$dist_old[sel.n])
  target <- as.numeric(names(tmp)[tmp==max(tmp)]) 
  d$father[sel.n] <- target
  sel.f <- which(d$dist_old==target) # secciones in father district
  sel.c <- intersect(sel.n, sel.f)             # secciones common to father and new districts
  d$dsi[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
}

dsi <- d[duplicated(d$dist_new)==FALSE, c("dist_new","father","dsi")]
dsi <- dsi[order(dsi$dist_new),]
dsi$cab2017 <- c("Pánuco","Tantoyuca","Tuxpan","Álamo Temapache","Poza Rica","Papantla","Martínez de la Torre","Misantla","Perote","Xalapa","Xalapa","Coatepec","Emiliano Zapata","Veracruz","Veracruz","Boca del Río","Medellín de Bravo","Huatusco","Córdoba","Orizaba","Camerino Z. Mendoza","Zongolica","Cosamaloapan","Santiago Tuxtla","San Andrés Tuxtla","Cosoleacaque","Acayucan","Minatitlán","Coatzacoalcos","Coatzacoalcos")
#dsi$cab2017<-c("Victoria de Durango","Victoria de Durango","Victoria de Durango","Victoria de Durango","Victoria de Durango","El Salto","Santiago Papasquiaro","Santa María del Oro","Bermejilla","Gómez Palacio","Gómez Palacio","Gómez Palacio","Lerdo","Cuéncame de Ceniceros","Nombre de Dios")
#dsi$cab2017<-c("San José del Cabo","La Paz","La Paz","La Paz","La Paz","La Paz","San José del Cabo","Cabo San Lucas","Cabo San Lucas","Ciudad Constitución","Ciudad Constitución","San José del Cabo","Loreto","Guerrero Negro","La Paz","Cabo San Lucas")
dsi <- dsi[order(dsi$dsi),]

write.csv(dsi, file = "/Volumes/DANIEL/son_dsi.csv", row.names = FALSE)
write.csv(d,file="/Volumes/DANIEL/son.csv",row.names=F)


rm(list = ls())
Sys.setlocale("LC_ALL", "ES_ES.UTF-8")
#dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/simIndex"
directorio <- ("~/Volumes/DANIEL/mxDistritos/mapasComparados/loc/simIndex")
setwd("~/Volumes/DANIEL/mxDistritos/mapasComparados/loc/simIndex")
dir()

ags1 <- read.csv(file = "dist_ags_cab.csv", stringsAsFactors = FALSE)
ags2 <- read.csv(file = "dist_ags.csv", stringsAsFactors = FALSE)
summary(ags1$dsi)
summary(ags2$dsi) # son iguales
head(ags1) # tiene cabeceras
head(ags2) # el que manipula el código original
all <- merge(x = ags2, y = ags1[,c("disloc2016", "cab")], by = "disloc2016", all = TRUE)
head(all)
rm(ags1, ags2)
ags_median<-median(all$dsi,na.rm=T)
ags_75<-quantile(all$dsi,.75,na.rm=T)
ags_25<-quantile(all$dsi,.25,na.rm=T)
ags_num<-sum(all$edon==1,na.rm=T)
all$son <- all$disloc2016; all$disloc2016 <- NULL
all <- all[, c("edon","son","dsi","father","cab")] # ordena columnas

h<-hist(all$dsi, breaks=50, density=100, xlab="DSI",ylab="Frecuencia",main="Histograma del DSI")
xfit<-seq(min(all$dsi),max(all$dsi),length=200)
yfit<-dnorm(xfit,mean=mean(all$dsi),sd=sd(all$dsi))
yfit<-yfit*diff(h$mids[1:2])*length(all$dsi)
lines(xfit,yfit, col="black",lwd=2)



#

new <- read.csv(file = "dist_bc.csv", stringsAsFactors = FALSE)
head(new)
new$son <- new$disloc2016; new$disloc2016 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
bc_median<-median(new$dsi,na.rm=T)
bc_75<-quantile(new$dsi,.75,na.rm=T)
bc_25<-quantile(new$dsi,.25,na.rm=T)
bc_num<-sum(new$edon==2,na.rm=T)
rm(new)
#
new <- read.csv(file = "dist_bcs.csv", stringsAsFactors = FALSE)
head(new)
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
bcs_median<-median(new$dsi,na.rm=T)
bcs_75<-quantile(new$dsi,.75,na.rm=T)
bcs_25<-quantile(new$dsi,.25,na.rm=T)
bcs_num<-sum(new$edon==3,na.rm=T)
rm(new)
#
new <- read.csv(file = "dist_cam.csv", stringsAsFactors = FALSE)
head(new)
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
cam_median<-median(new$dsi,na.rm=T)
cam_75<-quantile(new$dsi,.75,na.rm=T)
cam_25<-quantile(new$dsi,.25,na.rm=T)
cam_num<-sum(new$edon==4,na.rm=T)
rm(new)
#
new <- read.csv(file = "dist_coa.csv", stringsAsFactors = FALSE)
head(new)
new$son <- new$disloc2017; new$disloc2017 <- NULL
#new$cab <- new$cab2017; new$cab2017 <- NULL
all <- rbind(all, new[names(all)])
tail(all)
coa_median<-median(new$dsi,na.rm=T)
coa_75<-quantile(new$dsi,.75,na.rm=T)
coa_25<-quantile(new$dsi,.25,na.rm=T)
coa_num<-sum(new$edon==5,na.rm=T)
rm(new)
#
new <- read.csv(file = "dist_col.csv", stringsAsFactors = FALSE)
head(new)
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
new <- new[,c("edon","son","dsi","father","cab")]
all <- rbind(all, new[names(all)])
tail(all)
col_median<-median(new$dsi,na.rm=T)
col_75<-quantile(new$dsi,.75,na.rm=T)
col_25<-quantile(new$dsi,.25,na.rm=T)
col_num<-sum(new$edon==6,na.rm=T)
rm(new)
#
new <- read.csv(file = "dist_cps.csv", stringsAsFactors = FALSE)
head(new)
new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
cps_median<-median(new$dsi,na.rm=T)
cps_75<-quantile(new$dsi,.75,na.rm=T)
cps_25<-quantile(new$dsi,.25,na.rm=T)
cps_num<-sum(new$edon==7,na.rm=T)
rm(new)
#
new <- read.csv(file = "dist_cua.csv", stringsAsFactors = FALSE)
head(new)
new$son <- new$disloc2016; new$disloc2016 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
cua_median<-median(new$dsi,na.rm=T)
cua_75<-quantile(new$dsi,.75,na.rm=T)
cua_25<-quantile(new$dsi,.25,na.rm=T)
cua_num<-sum(new$edon==8,na.rm=T)
rm(new)
#
new <- read.csv(file = "dist_df33.csv", stringsAsFactors = FALSE)
head(new)
#new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
df_median<-median(new$dsi,na.rm=T)
df_75<-quantile(new$dsi,.75,na.rm=T)
df_25<-quantile(new$dsi,.25,na.rm=T)
df_num<-sum(new$edon==9,na.rm=T)
rm(new)
#
#new <- read.csv(file = "dist_df40.csv", stringsAsFactors = FALSE)
#head(new)
#new <- new[-which(new$disloc2018==0),] # drop missing secciones
#new$son <- new$disloc2018; new$disloc2018 <- NULL
#new$cab <- NA
#all <- rbind(all, new[names(all)])
#tail(all)
#rm(new)
#
new <- read.csv(file = "dist_dgo.csv", stringsAsFactors = FALSE)
head(new)
#new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2016; new$disloc2016 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
dgo_median<-median(new$dsi,na.rm=T)
dgo_75<-quantile(new$dsi,.75,na.rm=T)
dgo_25<-quantile(new$dsi,.25,na.rm=T)
dgo_num<-sum(new$edon==10,na.rm=T)
rm(new)
#
new <- read.csv(file = "dist_gua.csv", stringsAsFactors = FALSE)
head(new)
new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
gua_median<-median(new$dsi,na.rm=T)
gua_75<-quantile(new$dsi,.75,na.rm=T)
gua_25<-quantile(new$dsi,.25,na.rm=T)
gua_num<-sum(new$edon==11,na.rm=T)
rm(new)
#
new <- read.csv(file = "dist_gue.csv", stringsAsFactors = FALSE)
head(new)
new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
gue_median<-median(new$dsi,na.rm=T)
gue_75<-quantile(new$dsi,.75,na.rm=T)
gue_25<-quantile(new$dsi,.25,na.rm=T)
gue_num<-sum(new$edon==12,na.rm=T)
rm(new)
#
new <- read.csv(file = "dist_hgo.csv", stringsAsFactors = FALSE)
head(new)
#new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2016; new$disloc2016 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
hgo_median<-median(new$dsi,na.rm=T)
hgo_75<-quantile(new$dsi,.75,na.rm=T)
hgo_25<-quantile(new$dsi,.25,na.rm=T)
hgo_num<-sum(new$edon==13,na.rm=T)
rm(new)
#
new <- read.csv(file = "dist_jal.csv", stringsAsFactors = FALSE)
head(new)
#new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
jal_median<-median(new$dsi,na.rm=T)
jal_75<-quantile(new$dsi,.75,na.rm=T)
jal_25<-quantile(new$dsi,.25,na.rm=T)
jal_num<-sum(new$edon==14,na.rm=T)
rm(new)
#
new <- read.csv(file = "dist_mex.csv", stringsAsFactors = FALSE)
head(new)
#new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
mex_median<-median(new$dsi,na.rm=T)
mex_75<-quantile(new$dsi,.75,na.rm=T)
mex_25<-quantile(new$dsi,.25,na.rm=T)
mex_num<-sum(new$edon==15,na.rm=T)
rm(new)
#
new <- read.csv(file = "dist_mic.csv", stringsAsFactors = FALSE)
head(new)
new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
mic_median<-median(new$dsi,na.rm=T)
mic_75<-quantile(new$dsi,.75,na.rm=T)
mic_25<-quantile(new$dsi,.25,na.rm=T)
mic_num<-sum(new$edon==16,na.rm=T)
rm(new)
#
new <- read.csv(file = "dist_mor12.csv", stringsAsFactors = FALSE)
head(new)
#new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
mor_median<-median(new$dsi,na.rm=T)
mor_75<-quantile(new$dsi,.75,na.rm=T)
mor_25<-quantile(new$dsi,.25,na.rm=T)
mor_num<-sum(new$edon==17,na.rm=T)
rm(new)
#
#new <- read.csv(file = "dist_mor18.csv", stringsAsFactors = FALSE)
#head(new)
#new <- new[-which(new$disloc2018==0),] # drop missing secciones
#new$son <- new$disloc2018; new$disloc2018 <- NULL
#new$cab <- NA
#all <- rbind(all, new[names(all)])
#tail(all)
#rm(new)
#
new <- read.csv(file = "dist_nay.csv", stringsAsFactors = FALSE)
head(new)
#new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2017; new$disloc2017 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
nay_median<-median(new$dsi,na.rm=T)
nay_75<-quantile(new$dsi,.75,na.rm=T)
nay_25<-quantile(new$dsi,.25,na.rm=T)
nay_num<-sum(new$edon==18,na.rm=F)
rm(new)
#
new <- read.csv(file = "dist_nl.csv", stringsAsFactors = FALSE)
head(new)
#new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
nl_median<-median(new$dsi,na.rm=T)
nl_75<-quantile(new$dsi,.75,na.rm=T)
nl_25<-quantile(new$dsi,.25,na.rm=T)
nl_num<-sum(new$edon==19,na.rm=T)
rm(new)
#
new <- read.csv(file = "dist_oax.csv", stringsAsFactors = FALSE)
head(new)
new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
oax_median<-median(new$dsi,na.rm=T)
oax_75<-quantile(new$dsi,.75,na.rm=T)
oax_25<-quantile(new$dsi,.25,na.rm=T)
oax_num<-sum(new$edon==20,na.rm=T)
rm(new)
#
new <- read.csv(file = "dist_pue.csv", stringsAsFactors = FALSE)
head(new)
#new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
pue_median<-median(new$dsi,na.rm=T)
pue_75<-quantile(new$dsi,.75,na.rm=T)
pue_25<-quantile(new$dsi,.25,na.rm=T)
pue_num<-sum(new$edon==21,na.rm=T)
rm(new)
#
new <- read.csv(file = "dist_que.csv", stringsAsFactors = FALSE)
head(new)
#new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
que_median<-median(new$dsi,na.rm=T)
que_75<-quantile(new$dsi,.75,na.rm=T)
que_25<-quantile(new$dsi,.25,na.rm=T)
que_num<-sum(new$edon==22,na.rm=T)
rm(new)

new <- read.csv(file = "dist_qui.csv", stringsAsFactors = FALSE)
head(new)
#new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2015; new$disloc2015 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
qui_median<-median(new$dsi,na.rm=T)
qui_75<-quantile(new$dsi,.75,na.rm=T)
qui_25<-quantile(new$dsi,.25,na.rm=T)
qui_num<-sum(new$edon==23,na.rm=T)
rm(new)
#
new <- read.csv(file = "dist_san.csv", stringsAsFactors = FALSE)
head(new)
#new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
san_median<-median(new$dsi,na.rm=T)
san_75<-quantile(new$dsi,.75,na.rm=T)
san_25<-quantile(new$dsi,.25,na.rm=T)
san_num<-sum(new$edon==24,na.rm=T)
rm(new)
#
new <- read.csv(file = "dist_sin.csv", stringsAsFactors = FALSE)
head(new)
#new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2017; new$disloc2017 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
sin_median<-median(new$dsi,na.rm=T)
sin_75<-quantile(new$dsi,.75,na.rm=T)
sin_25<-quantile(new$dsi,.25,na.rm=T)
sin_num<-sum(new$edon==25,na.rm=T)
rm(new)

new <- read.csv(file = "son_dsi.csv", stringsAsFactors = FALSE)
head(new)
#new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2012; new$disloc2012 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
son_median<-median(new$dsi,na.rm=T)
son_75<-quantile(new$dsi,.75,na.rm=T)
son_25<-quantile(new$dsi,.25,na.rm=T)
son_num<-sum(new$edon==26,na.rm=T)
rm(new)


new <- read.csv(file = "dist_tab.csv", stringsAsFactors = FALSE)
head(new)
new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
tab_median<-median(new$dsi,na.rm=T)
tab_75<-quantile(new$dsi,.75,na.rm=T)
tab_25<-quantile(new$dsi,.25,na.rm=T)
tab_num<-sum(new$edon==27,na.rm=T)
rm(new)

new <- read.csv(file = "dist_tam.csv", stringsAsFactors = FALSE)
head(new)
#new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2015; new$disloc2015 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
tam_median<-median(new$dsi,na.rm=T)
tam_75<-quantile(new$dsi,.75,na.rm=T)
tam_25<-quantile(new$dsi,.25,na.rm=T)
tam_num<-sum(new$edon==28,na.rm=T)
rm(new)
#
new <- read.csv(file = "dist_tla15.csv", stringsAsFactors = FALSE)
head(new)
#new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2016; new$disloc2016 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
tla_median<-median(new$dsi,na.rm=T)
tla_75<-quantile(new$dsi,.75,na.rm=T)
tla_25<-quantile(new$dsi,.25,na.rm=T)
tla_num<-sum(new$edon==29,na.rm=T)
rm(new)
#
#new <- read.csv(file = "dist_tla19.csv", stringsAsFactors = FALSE)
#head(new)
#new <- new[-which(new$disloc2018==0),] # drop missing secciones
#new$son <- new$disloc2018; new$disloc2018 <- NULL
#new$cab <- NA
#all <- rbind(all, new[names(all)])
#tail(all)
#rm(new)
#
new <- read.csv(file = "dist_ver.csv", stringsAsFactors = FALSE)
head(new)
#new <- new[-which(new$dist_new==0),] # drop missing secciones
new$son <- new$disloc2016; new$disloc2016 <- NULL
new$cab <- new$cab2017; new$cab2017<-NULL
new$father <-new$father14; new$father14<-NULL
all <- rbind(all, new[names(all)])
tail(all)
ver_median<-median(new$dsi,na.rm=T)
ver_75<-quantile(new$dsi,.75,na.rm=T)
ver_25<-quantile(new$dsi,.25,na.rm=T)
ver_num<-sum(new$edon==30,na.rm=T)
rm(new)
#

new <- read.csv(file = "dist_yuc.csv", stringsAsFactors = FALSE)
head(new)
#new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$dist_new; new$dist_new <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
yuc_median<-median(new$dsi,na.rm=T)
yuc_75<-quantile(new$dsi,.75,na.rm=T)
yuc_25<-quantile(new$dsi,.25,na.rm=T)
yuc_num<-sum(new$edon==31,na.rm=T)
rm(new)

new <- read.csv(file = "dist_zac.csv", stringsAsFactors = FALSE)
head(new)
#new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2016; new$disloc2016 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
zac_median<-median(new$dsi,na.rm=T)
zac_75<-quantile(new$dsi,.75,na.rm=T)
zac_25<-quantile(new$dsi,.25,na.rm=T)
zac_num<-sum(new$edon==32,na.rm=T)
rm(new)
#
#seguir con los demás
dir()

# add edo
all$edo <- NA;
all$edo[all$edon==1] <- "ags"
all$edo[all$edon==2] <- "bc"
all$edo[all$edon==3] <- "bcs"
all$edo[all$edon==4] <- "cam"
all$edo[all$edon==5] <- "coa"
all$edo[all$edon==6] <- "col"
all$edo[all$edon==7] <- "cps"
all$edo[all$edon==8] <- "cua"
all$edo[all$edon==9] <- "df"
all$edo[all$edon==10] <- "dgo"
all$edo[all$edon==11] <- "gua"
all$edo[all$edon==12] <- "gue"
all$edo[all$edon==13] <- "hgo"
all$edo[all$edon==14] <- "jal"
all$edo[all$edon==15] <- "mex"
all$edo[all$edon==16] <- "mic"
all$edo[all$edon==17] <- "mor"
all$edo[all$edon==18] <- "nay"
all$edo[all$edon==19] <- "nl"
all$edo[all$edon==20] <- "oax"
all$edo[all$edon==21] <- "pue"
all$edo[all$edon==22] <- "que"
all$edo[all$edon==23] <- "qui"
all$edo[all$edon==24] <- "san"
all$edo[all$edon==25] <- "sin"
all$edo[all$edon==26] <- "son"
all$edo[all$edon==27] <- "tab"
all$edo[all$edon==28] <- "tam"
all$edo[all$edon==29] <- "tla"
all$edo[all$edon==30] <- "ver"
all$edo[all$edon==31] <- "yuc"
all$edo[all$edon==32] <- "zac"
#
all$pct <- ecdf(all$dsi)(all$dsi) # add percentile value 
all <- all[order(all$dsi),]
all$dcrit8 <- as.numeric(all$edon==5 | all$edon==9 | all$edon==12 | all$edon==24 | all$edon==29 | all$edon==31 | all$edon==32)
head(all)

descrip<-summary(all)
summary(lm(dsi ~ dcrit8, data = all))

head(all)

Estado<-c("Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila","Colima","Chiapas","Chihuahua","Ciudad de México","Durango","Guanajuato","Guerrero","Hidalgo","Jalisco","México","Michoacán","Morelos","Nayarit","Nuevo León","Oaxaca","Puebla","Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatán","Zacatecas")
DSImin<-c()
DSI25<-c(ags_25,bc_25,bcs_25,cam_25,coa_25,col_25,cps_25,cua_25,df_25,dgo_25,gua_25,gue_25,hgo_25,jal_25,mex_25,mic_25,mor_25,nay_25,nl_25,oax_25,pue_25,que_25,qui_25,san_25,sin_25,son_25,tab_25,tam_25,tla_25,ver_25,yuc_25,zac_25)
DSImediana<-c(ags_median,bc_median,bcs_median,cam_median,coa_median,col_median,cps_median,cua_median,df_median,dgo_median,gua_median,gue_median,hgo_median,jal_median,mex_median,mic_median,mor_median,nay_median,nl_median,oax_median,pue_median,que_median,qui_median,san_median,sin_median,son_median,tab_median,tam_median,tla_median,ver_median,yuc_median,zac_median)
DSI75<-c(ags_75,bc_75,bcs_75,cam_75,coa_75,col_75,cps_75,cua_75,df_75,dgo_75,gua_75,gue_75,hgo_75,jal_75,mex_75,mic_75,mor_75,nay_75,nl_75,oax_75,pue_75,que_75,qui_75,san_75,sin_75,son_75,tab_75,tam_75,tla_75,ver_75,yuc_75,zac_75)
num_distritos<-c(ags_num,bc_num,bcs_num,cam_num,coa_num,col_num,cps_num,cua_num,df_num,dgo_num,gua_num,gue_num,hgo_num,jal_num,mex_num,mic_num,mor_num,nay_num,nl_num,oax_num,pue_num,que_num,qui_num,san_num,sin_num,son_num,tab_num,tam_num,tla_num,ver_num,yuc_num,zac_num)
datos<-cbind(Estado,DSI25,DSImediana,DSI75,num_distritos)

write.csv(datos,"/Volumes/DANIEL/tabla.csv")
descrip
summary(all$DSI)
summary(all$dsi)
