# scrap R script for testing things





# load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# (1) Load and organize data ----
# dat <- read_csv("cnb_merged_20220107.csv")
dat <- read_csv("data/cnb_merged_20220201.csv")    # digsym, GNG, AIM, + itemwise (iw) DDISC, iw RDISC, iw EDISC
dat$remote <- ifelse(dat$platform=="webcnp",0,1)
names(dat)[6] <- "bblid"
dat <- dat[,c(1:6,8:20,525,21:524)]
dat$test_sessions_v.dotest <- as.Date(dat$test_sessions_v.dotest,"%m/%d/%y")

demo <- dat[,1:20]    # demographics & non-test-specific things


# * separating out into tasks ----
notval <- c("N","F","V3")

mpraxis <- cbind(demo, dat[,grepl("mpraxis",colnames(dat))]) # RTCR
mpraxis <- mpraxis[!is.na(mpraxis$mpraxis_rtcr),]
mpraxis <- mpraxis[!(mpraxis$mpraxis_valid %in% notval),]

pcet <- cbind(demo, dat[,grepl("pcet",colnames(dat))])   # acc2, RTCR, and cat
pcet <- pcet[!is.na(pcet$pcet_rtcr),]
pcet <- pcet[!(pcet$pcet_valid %in% notval),]

cpt <- cbind(demo, dat[,grepl("cpt",colnames(dat))])     # PTP, PFP, TPRT
cpt <- cpt[!is.na(cpt$cpt_tprt),]
cpt <- cpt[!(cpt$cpt_valid %in% notval),]

lnb <- cbind(demo, dat[,grepl("lnb",colnames(dat))])     # MCR and MRTC
lnb <- lnb[!is.na(lnb$lnb_mrtc),]
lnb <- lnb[!(lnb$lnb_valid %in% notval),]

er40 <- cbind(demo, dat[,grepl("er40",colnames(dat))])   # CR and RTCR
er40 <- er40[!is.na(er40$er40_rtcr),]
er40 <- er40[!(er40$er40_valid %in% notval),]

pvrt <- cbind(demo, dat[,grepl("pvrt",colnames(dat))]) # CR, PC, and RTCR
pvrt <- pvrt[!is.na(pvrt$pvrt_rtcr),]
pvrt <- pvrt[!(pvrt$pvrt_valid %in% notval),]

pmat <- cbind(demo, dat[,grepl("pmat",colnames(dat))]) # PC and RTCR
pmat <- pmat[!is.na(pmat$pmat_rtcr),]
pmat <- pmat[!(pmat$pmat_valid %in% notval),]

volt <- cbind(demo, dat[,grepl("volt",colnames(dat))]) # CR and RTCR
volt <- volt[!is.na(volt$volt_rtcr),]
volt <- volt[!(volt$volt_valid %in% notval),]

cpf <- cbind(demo, dat[,grepl("cpf",colnames(dat))])   # CR and RTCR
cpf <- cpf[!is.na(cpf$cpf_rtcr),]
cpf <- cpf[!(cpf$cpf_valid %in% notval),]

medf <- cbind(demo, dat[,grepl("medf",colnames(dat))]) # PC and RTCR
medf <- medf[!is.na(medf$medf_rtcr),]
medf <- medf[!(medf$medf_valid  %in% notval),]

adt <- cbind(demo, dat[,grepl("adt",colnames(dat))])   # PC and RTCR
adt <- adt[!is.na(adt$adt_rtcr),]
adt <- adt[!(adt$adt_valid %in% notval),]

plot <- cbind(demo, dat[,grepl("plot",colnames(dat))]) # PC and RTCR
plot <- plot[!is.na(plot$plot_rtcr),]
plot <- plot[!(plot$plot_valid %in% notval),]

cpw <- cbind(demo, dat[,grepl("cpw",colnames(dat))])   # CR and RTCR
cpw <- cpw[!is.na(cpw$cpw_rtcr),]
cpw <- cpw[!(cpw$cpw_valid %in% notval),]

tap <- cbind(demo, dat[,grepl("tap",colnames(dat))])   # hand and TOT
tap <- tap[!is.na(tap$tap_tot),]
tap <- tap[!(tap$tap_valid %in% notval),]

digsym <- cbind(demo, dat[,grepl("digsym",colnames(dat))], dat[,grepl("ds",colnames(dat))])   # cor and corrt
digsym <- digsym[!is.na(digsym$dscor),-c(24,26)]
digsym <- digsym[!(digsym$digsym_valid %in% notval),]

dsm <- cbind(demo, dat[,grepl("digsym",colnames(dat))], dat[,grepl("ds",colnames(dat))])   # memcr and mcrrt
dsm <- dsm[!is.na(dsm$dsmemcr),-c(23,25)]   # memory component of digsym test
dsm <- dsm[!(dsm$digsym_valid %in% notval),]

gng <- cbind(demo, dat[,grepl("gng",colnames(dat))])   # CR and RTCR
gng <- gng[!is.na(gng$gng_cr),]
gng <- gng[!(gng$gng_valid %in% notval),]

aim <- cbind(demo, dat[,grepl("aim",colnames(dat))])   # TOT and totrt    empty for now
aim <- aim[!is.na(aim$aim_tot),]
aim <- aim[!(aim$tap_valid %in% notval),]


colors <- c("#6E4230", "#BF9660", "#065C50", "#48B0A2")



# making plots that are not made with the mean

temp <- adt
temp$finp <- ifelse(temp$test_sessions_v.gender=="F" & temp$remote == 0,temp$adt_pc,NA)
temp$frem <- ifelse(temp$test_sessions_v.gender=="F" & temp$remote == 1,temp$adt_pc,NA)
temp$minp <- ifelse(temp$test_sessions_v.gender=="M" & temp$remote == 0,temp$adt_pc,NA)
temp$mrem <- ifelse(temp$test_sessions_v.gender=="M" & temp$remote == 1,temp$adt_pc,NA)

nnn <- c(sum(!is.na(temp$finp)),
         sum(!is.na(temp$frem)),
         sum(!is.na(temp$minp)),
         sum(!is.na(temp$mrem)))
temp$

ggplot(temp,aes(x=test_sessions_v.age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=finp, color=paste("Female In-person, n =",nnn[1])),size=1) +
  geom_point(aes(y=frem, color=paste("Female Remote, n =",nnn[2])),size=1) +
  geom_point(aes(y=minp, color=paste("Male In-person, n =",nnn[3])),size=1) +
  geom_point(aes(y=mrem, color=paste("Male Remote, n =",nnn[4])),size=1) +
  geom_smooth(aes(y=finp, color=paste("Female In-person, n =",nnn[1])),se=F,size=1.5) +
  geom_smooth(aes(y=frem, color=paste("Female Remote, n =",nnn[2])),se=F,size=1.5) +
  geom_smooth(aes(y=minp, color=paste("Male In-person, n =",nnn[3])),se=F,size=1.5) +
  geom_smooth(aes(y=mrem, color=paste("Male Remote, n =",nnn[4])),se=F,size=1.5) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Percent Correct",
       title = "ADT Accuracy (as Percent Correct) across age, sex and platform") +
  scale_x_continuous(limits = c(5,105))












