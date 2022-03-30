# Comparing in-person vs remote test results on CNB

# Akira Di Sandro, 01.18.22


# load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ccoptimalmatch)
library(tidyverse)
library(ggdist)
library(lme4)
library(ggpubr)
library(lmerTest)
library(visreg)
library(ggh4x)
library(stats)
library(MatchIt)



# (1) Load and organize data ----
# dat <- read_csv("cnb_merged_20220107.csv")
x <- read_csv("data/cnb_merged_20220201.csv")    # digsym, GNG, AIM, + itemwise (iw) DDISC, iw RDISC, iw EDISC
dat <- x
dat$remote <- ifelse(dat$platform=="webcnp",0,1)
dat <- dat[,c(1:6,8:20,525,21:524)]
dat$test_sessions_v.dotest <- as.Date(dat$test_sessions_v.dotest,"%m/%d/%y")
dat$test_sessions_v.dob <- as.Date(dat$test_sessions_v.dob,"%m/%d/%y")
dat$test_sessions_v.dob <- as.Date(ifelse(dat$test_sessions_v.dob > Sys.Date(), 
                                          format(dat$test_sessions_v.dob, "19%y-%m-%d"), 
                                          format(dat$test_sessions_v.dob)))
names(dat)[c(6,7,9,10,13)] <- c("bblid","age","dob","dotest","sex")
ds_names <- names(dat[,grepl("ds",colnames(dat))]) 
ds_names <- sub("ds","ds_",ds_names)
colnames(dat)[grep("ds",colnames(dat))] <- ds_names

# get rid of BBLIDs < 5 digits
dat <- dat[-which(dat$bblid < 10000),]
dat <- dat[-which(dat$bblid > 1000000),]

# exclude the 103 year old for now
dat <- dat[-which(dat$age >100),]




# * separating out into tasks ----
notval <- c("N","F","V3")

mpraxis <- cbind(demo, dat[,grepl("mpraxis",colnames(dat))]) # RTCR
mpraxis <- mpraxis[!is.na(mpraxis$mpraxis_rtcr),]
mpraxis <- mpraxis[!(mpraxis$mpraxis_valid %in% notval),]
mpraxis <- select(mpraxis, "age","dotest","sex","remote","mpraxis_rtcr")

pcet <- cbind(demo, dat[,grepl("pcet",colnames(dat))])   # acc2, RTCR, and cat
pcet <- pcet[!is.na(pcet$pcet_rtcr),]
pcet <- pcet[!(pcet$pcet_valid %in% notval),]
pcet_cat <- select(pcet, "age","dotest","sex","remote","pcet_cat","pcet_rtcr") 
pcet_acc2 <- select(pcet, "age","dotest","sex","remote","pcet_acc2","pcet_rtcr") 

cpt <- cbind(demo, dat[,grepl("cpt",colnames(dat))])     # PTP, PFP, TPRT
cpt <- cpt[!is.na(cpt$cpt_tprt),]
cpt <- cpt[!(cpt$cpt_valid %in% notval),]
cpt <- select(cpt, "age","dotest","sex","remote","cpt_ptp","cpt_tprt")  

lnb <- cbind(demo, dat[,grepl("lnb",colnames(dat))])     # MCR and MRTC
lnb <- lnb[!is.na(lnb$lnb_mrtc),]
lnb <- lnb[!(lnb$lnb_valid %in% notval),]
lnb <- select(lnb, "age","dotest","sex","remote","lnb_mcr","lnb_mrtc")

er40 <- cbind(demo, dat[,grepl("er40",colnames(dat))])   # CR and RTCR
er40 <- er40[!is.na(er40$er40_rtcr),]
er40 <- er40[!(er40$er40_valid %in% notval),]
er40 <- select(er40, "age","dotest","sex","remote","er40_cr","er40_rtcr")    

pvrt <- cbind(demo, dat[,grepl("pvrt",colnames(dat))]) # CR, PC, and RTCR
pvrt <- pvrt[!is.na(pvrt$pvrt_rtcr),]
pvrt <- pvrt[!(pvrt$pvrt_valid %in% notval),]
pvrt <- select(pvrt, "age","dotest","sex","remote","pvrt_cr","pvrt_rtcr")

pmat <- cbind(demo, dat[,grepl("pmat",colnames(dat))]) # PC and RTCR
pmat <- pmat[!is.na(pmat$pmat_rtcr),]
pmat <- pmat[!(pmat$pmat_valid %in% notval),]
pmat <- select(pmat, "age","dotest","sex","remote","pmat_pc","pmat_rtcr")

volt <- cbind(demo, dat[,grepl("volt",colnames(dat))]) # CR and RTCR
volt <- volt[!is.na(volt$volt_rtcr),]
volt <- volt[!(volt$volt_valid %in% notval),]
volt <- select(volt, "age","dotest","sex","remote","volt_cr","volt_rtcr")

cpf <- cbind(demo, dat[,grepl("cpf",colnames(dat))])   # CR and RTCR
cpf <- cpf[!is.na(cpf$cpf_rtcr),]
cpf <- cpf[!(cpf$cpf_valid %in% notval),]
cpf <- select(cpf, "age","dotest","sex","remote","cpf_cr","cpf_rtcr")

medf <- cbind(demo, dat[,grepl("medf",colnames(dat))]) # PC and RTCR
medf <- medf[!is.na(medf$medf_rtcr),]
medf <- medf[!(medf$medf_valid  %in% notval),]
medf <- select(medf, "age","dotest","sex","remote","medf_pc","medf_rtcr")

adt <- cbind(demo, dat[,grepl("adt",colnames(dat))])   # PC and RTCR
adt <- adt[!is.na(adt$adt_rtcr),]
adt <- adt[!(adt$adt_valid %in% notval),]
adt <- select(adt, "age","dotest","sex","remote","adt_pc","adt_rtcr")

plot <- cbind(demo, dat[,grepl("plot",colnames(dat))]) # PC and RTCR
plot <- plot[!is.na(plot$plot_rtcr),]
plot <- plot[!(plot$plot_valid %in% notval),]
plot <- select(plot, "age","dotest","sex","remote","plot_pc","plot_rtcr")

cpw <- cbind(demo, dat[,grepl("cpw",colnames(dat))])   # CR and RTCR
cpw <- cpw[!is.na(cpw$cpw_rtcr),]
cpw <- cpw[!(cpw$cpw_valid %in% notval),]
cpw <- select(cpw, "age","dotest","sex","remote","cpw_cr","cpw_rtcr")

tap <- cbind(demo, dat[,grepl("tap",colnames(dat))])   # hand and TOT
tap <- tap[!is.na(tap$tap_tot),]
tap <- tap[!(tap$tap_valid %in% notval),]
# not sure how to look at TAP data

digsym <- cbind(demo, dat[,grepl("digsym",colnames(dat))], dat[,grepl("ds",colnames(dat))])   # cor and corrt
digsym <- digsym[!is.na(digsym$dscor),-c(24,26)]
digsym <- digsym[!(digsym$digsym_valid %in% notval),]
digsym <- select(digsym, "age","dotest","sex","remote","dscor","dscorrt")

dsm <- cbind(demo, dat[,grepl("digsym",colnames(dat))], dat[,grepl("ds",colnames(dat))])   # memcr and mcrrt
dsm <- dsm[!is.na(dsm$dsmemcr),-c(23,25)]   # memory component of digsym test
dsm <- dsm[!(dsm$digsym_valid %in% notval),]
dsm <- select(dsm, "age","dotest","sex","remote","dsmemcr","dsmcrrt")

gng <- cbind(demo, dat[,grepl("gng",colnames(dat))])   # CR and RTCR
gng <- gng[!is.na(gng$gng_cr),]
gng <- gng[!(gng$gng_valid %in% notval),]
gng <- select(gng, "age","dotest","sex","remote","gng_cr","gng_rtcr")

aim <- cbind(demo, dat[,grepl("aim",colnames(dat))])   # TOT and totrt    empty for now
aim <- aim[!is.na(aim$aim_tot),]
aim <- aim[!(aim$tap_valid %in% notval),]
aim <- select(aim, "age","dotest","sex","remote","adt_pc","adt_rtcr") # not sure what to use here

ddisc # all DISC tasks are missing data right now

rdisc

edisc


# (2) Plots comparing sex and platform with age on x-axis (noting n's) ----

# * (i) accuracy (adt,cpf,cpt,cpw,er40,lnb,medf,plot,pmat,pvrt,volt) ----
colors <- c("#6E4230", "#BF9660", "#065C50", "#48B0A2")

acc_texts <- c("adt","cpf","cpt","cpw","digsym","dsm","er40","gng","lnb","medf",
               "pcet_cat","pcet_acc2","plot","pmat","pvrt","volt")          # no aim or disc for now, no data, no tap bc not sure what to do, no mpraxis (only rt)
acc_tests <- mget(acc_texts)

plotlist <- list()
nnn <- list()
for (i in 1:length(acc_texts)) {        # still not satisfied about the nnn part
  test <- acc_tests[[i]]
  cr_pc <- ifelse(str_detect(names(test)[5],"cat"),"Categories Achieved",
                  ifelse(str_detect(names(test)[5],"acc2"),"Acc2 Score",
                         ifelse(str_detect(names(test)[5],"ptp"),"Percent True Positive",
                                ifelse(str_detect(names(test)[5],"pc"),"Percent Correct","Total Correct"))))
  names(test)[5] <- "acc"
  test$finp <- ifelse(test$sex=="F" & test$remote == 0,test$acc,NA)
  test$frem <- ifelse(test$sex=="F" & test$remote == 1,test$acc,NA)
  test$minp <- ifelse(test$sex=="M" & test$remote == 0,test$acc,NA)
  test$mrem <- ifelse(test$sex=="M" & test$remote == 1,test$acc,NA)
  
  nnn[[i]] <- c(sum(!is.na(test$finp)),
                sum(!is.na(test$frem)),
                sum(!is.na(test$minp)),
                sum(!is.na(test$mrem)))
  
  p <- ggplot(test,aes(x=age)) +
    scale_color_manual(values=colors) +
    geom_point(aes(y=finp, color=paste("Female In-person, n =",nnn[[i]][1])),size=.8) +
    geom_point(aes(y=frem, color=paste("Female Remote, n =",nnn[[i]][2])),size=.8) +
    geom_point(aes(y=minp, color=paste("Male In-person, n =",nnn[[i]][3])),size=.8) +
    geom_point(aes(y=mrem, color=paste("Male Remote, n =",nnn[[i]][4])),size=.8) +
    geom_smooth(aes(y=finp, color=paste("Female In-person, n =",nnn[[i]][1])),se=F,size=1) +
    geom_smooth(aes(y=frem, color=paste("Female Remote, n =",nnn[[i]][2])),se=F,size=1) +
    geom_smooth(aes(y=minp, color=paste("Male In-person, n =",nnn[[i]][3])),se=F,size=1) +
    geom_smooth(aes(y=mrem, color=paste("Male Remote, n =",nnn[[i]][4])),se=F,size=1) +
    theme_minimal() +
    theme(legend.title = element_blank()) +
    labs(x = "Age",
         y = "Percent Correct",
         title = paste0(str_to_upper(acc_texts[i]), " Accuracy (as ", cr_pc, ") across age, sex and platform")) +
    scale_x_continuous(limits = c(5,105))
  
  plotlist[[i]] <- p
}

pdf("plots/acc_plots.pdf")
for (i in 1:length(acc_texts)) {
  print(plotlist[[i]])
}
dev.off()








# * (ii) speed (adt,cpf,cpt,cpw,er40,lnb,medf,mpraxis,pcet,plot,pmat,pvrt,volt) ----

# * * (a) ADT speed ----
temp <- adt %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(adt_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

adt_sgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "ADT Speed (as Response time) across age, sex and platform") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/ADT_spe.pdf")


# * * (b) CPF speed ----
temp <- cpf %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(cpf_w_rtcr),n=n())    # Tyler said to use weighted response time measures here
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

cpf_sgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "CPF Speed (as Response time) across age, sex and platform") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/CPF_spe.pdf")


# * * (c) CPT speed ----

temp <- cpt %>%
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>%
  dplyr::summarise(mean=mean(cpt_tprt),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

cpt_sgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "CPT Speed (as Response time) across age, sex and platform") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/CPT_spe.pdf")



# * * (d) CPW speed ----

# no remote data

temp <- cpw %>%
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>%
  dplyr::summarise(mean=mean(cpw_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

cpw_sgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "CPW Speed (as Response time) across age, sex and platform") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/CPW_spe.pdf")



# * * (e) ER40 speed ----
temp <- er40 %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(er40_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

er40_sgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "ER40 Speed (as Response time) across age, sex and platform") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/ER40_spe.pdf")


# * * (f) LNB speed ----
temp <- lnb %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(lnb_mrtc),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

lnb_sgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "LNB Speed (as Response time) across age, sex and platform") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/LNB_spe.pdf")


# * * (g) MEDF speed ----
temp <- medf %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(medf_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

medf_sgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "MEDF Speed (as Response time) across age, sex and platform") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/MEDF_spe.pdf")


# * * (h) MPRAXIS speed ----
temp <- mpraxis %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(mpraxis_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

mpraxis_sgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "MPRAXIS Speed (as Response time) across age, sex and platform") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/MPRAXIS_spe.pdf")


# * * (i) PCET speed ----
temp <- pcet %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(pcet_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

pcet_sgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "PCET Speed (as Response time) across age, sex and platform") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/PCET_spe.pdf")


# * * (j) PLOT speed ----
temp <- plot %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(plot_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

plot_sgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "PLOT Speed (as Response time) across age, sex and platform") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/PLOT_spe.pdf")


# * * (k) PMAT speed ----
temp <- pmat %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(pmat_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

pmat_sgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "PMAT Speed (as Response time) across age, sex and platform") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/PMAT_spe.pdf")


# * * (l) PVRT speed ----

# no remote data

temp <- pvrt %>%
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>%
  dplyr::summarise(mean=mean(pvrt_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

pvrt_sgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "PVRT Speed (as Response time) across age, sex and platform") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/PVRT_spe.pdf")



# * * (m) VOLT speed ----
temp <- volt %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(volt_w_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

volt_sgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "VOLT Speed (as Response time) across age, sex and platform") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/individual/VOLT_spe.pdf")



# * * (n) AIM speed ----
temp <- aim %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(aim_totrt),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

aim_sgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "AIM Speed (as Response time) across age, sex and platform") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/individual/AIM_spe.pdf")



# * * (o) DIGSYM speed ----
temp <- digsym %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(dscorrt),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

digsym_sgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "DIGSYM Speed (as Response time) across age, sex and platform") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/individual/DIGSYM_spe.pdf")



# * * (p) DIGSYM (memory) speed ----
temp <- dsm %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(dsmcrrt),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

dsm_sgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "DIGSYM (memory) Speed (as Response time) across age, sex and platform") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/individual/DIGSYMmem_spe.pdf")



# * * (q) GNG speed ----
temp <- gng %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(gng_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

gng_sgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "GNG Speed (as Response time) across age, sex and platform") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/individual/GNG_spe.pdf")





# (3) 2020-present: Plots comparing sex and platform with age on x-axis (noting n's) ----

# * (i) getting rid of extra dates ----
adt_c <- adt[adt$dotest > as.Date("2019-12-31"),]    # "c" for COVID cause :P  (>u<)
aim_c <- aim[aim$dotest > as.Date("2019-12-31"),]
cpf_c <- cpf[cpf$dotest > as.Date("2019-12-31"),]
cpt_c <- cpt[cpt$dotest > as.Date("2019-12-31"),]
cpw_c <- cpw[cpw$dotest > as.Date("2019-12-31"),]
digsym_c <- digsym[digsym$dotest > as.Date("2019-12-31"),]
dsm_c <- dsm[dsm$dotest > as.Date("2019-12-31"),]
er40_c <- er40[er40$dotest > as.Date("2019-12-31"),]
gng_c <- gng[gng$dotest > as.Date("2019-12-31"),]
lnb_c <- lnb[lnb$dotest > as.Date("2019-12-31"),]
medf_c <- medf[medf$dotest > as.Date("2019-12-31"),]
mpraxis_c <- mpraxis[mpraxis$dotest > as.Date("2019-12-31"),]
pcet_cat_c <- pcet_cat[pcet_cat$dotest > as.Date("2019-12-31"),]
pcet_acc2_c <- pcet_acc2[pcet_acc2$dotest > as.Date("2019-12-31"),]
plot_c <- plot[plot$dotest > as.Date("2019-12-31"),]
pmat_c <- pmat[pmat$dotest > as.Date("2019-12-31"),]
pvrt_c <- pvrt[pvrt$dotest > as.Date("2019-12-31"),]
volt_c <- volt[volt$dotest > as.Date("2019-12-31"),]

# * (ii) accuracy (adt,cpf,cpt,cpw,er40,lnb,medf,plot,pmat,pvrt,volt) ----

# * * (a) ADT accuracy ----
temp <- adt_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(adt_pc),n=n())
names(temp) <- c("age","sex","remote","mpc","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mpc,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mpc,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mpc,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mpc,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

adt_cgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Percent Correct",
       title = "ADT Accuracy (as Percent Correct) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/ADT_c_acc.pdf")


# * * (b) CPF accuracy ----
temp <- cpf_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(cpf_cr),n=n())
names(temp) <- c("age","sex","remote","mtc","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mtc,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mtc,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mtc,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mtc,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

cpf_cgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Total Correct",
       title = "CPF Accuracy (as Total Correct) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/CPF_c_acc.pdf")


# * * (c) CPT accuracy ----

temp <- cpt_c %>%
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>%
  dplyr::summarise(mean=mean(cpt_ptp),n=n())
names(temp) <- c("age","sex","remote","mptp","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mptp,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mptp,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mptp,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mptp,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

cpt_cgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Percent True Positive",
       title = "CPT Accuracy (as Percent true Positive) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/CPT_c_acc.pdf")



# * * (d) CPW accuracy ----

temp <- cpw_c %>%
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>%
  dplyr::summarise(mean=mean(cpw_cr),n=n())
names(temp) <- c("age","sex","remote","mtc","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mtc,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mtc,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mtc,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mtc,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

cpw_cgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Total Correct",
       title = "CPW Accuracy (as Total Correct) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/CPW_c_acc.pdf")



# * * (e) ER40 accuracy ----
temp <- er40_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(er40_cr),n=n())
names(temp) <- c("age","sex","remote","mtc","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mtc,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mtc,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mtc,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mtc,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

er40_cgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Total Correct",
       title = "ER40 Accuracy (as Total Correct) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/ER40_c_acc.pdf")


# * * (f) LNB accuracy ----
temp <- lnb_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(lnb_mcr),n=n())
names(temp) <- c("age","sex","remote","mtc","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mtc,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mtc,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mtc,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mtc,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

lnb_cgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Total Correct",
       title = "LNB Accuracy (as Total Correct) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/LNB_c_acc.pdf")


# * * (g) MEDF accuracy ----
temp <- medf_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(medf_pc),n=n())
names(temp) <- c("age","sex","remote","mpc","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mpc,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mpc,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mpc,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mpc,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

medf_cgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Percent Correct",
       title = "MEDF Accuracy (as Percent Correct) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/MEDF_c_acc.pdf")


# * * (h) PCET accuracy (CAT) ----
temp <- pcet_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(pcet_cat),n=n())
names(temp) <- c("age","sex","remote","mtc","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mtc,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mtc,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mtc,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mtc,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

pcet_cgg1.1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Catergories Achieved",
       title = "PCET Accuracy (as Catergories Achieved) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/PCET_c_acc1.pdf")


# * * (i) PCET accuracy2 ----
temp <- pcet_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(pcet_acc2),n=n())
names(temp) <- c("age","sex","remote","mpc","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mpc,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mpc,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mpc,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mpc,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

pcet_cgg1.2 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Accuracy2",
       title = "PCET Accuracy (as Accuracy2) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/PCET_c_acc2.pdf")


# * * (j) PLOT accuracy ----
temp <- plot_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(plot_pc),n=n())
names(temp) <- c("age","sex","remote","mpc","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mpc,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mpc,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mpc,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mpc,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

plot_cgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Percent Correct",
       title = "PLOT Accuracy (as Percent Correct) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/PLOT_c_acc.pdf")


# * * (k) PMAT accuracy ----
temp <- pmat_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(pmat_pc),n=n())
names(temp) <- c("age","sex","remote","mpc","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mpc,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mpc,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mpc,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mpc,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

pmat_cgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Percent Correct",
       title = "PMAT Accuracy (as Percent Correct) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/PMAT_c_acc.pdf")


# * * (l) PVRT accuracy ----

temp <- pvrt %>%
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>%
  dplyr::summarise(mean=mean(pvrt_pc),n=n())
names(temp) <- c("age","sex","remote","mpc","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mpc,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mpc,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mpc,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mpc,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

pvrt_gg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Percent Correct",
       title = "PVRT Accuracy (as Percent Correct) across age, sex and platform") +
  scale_x_continuous(breaks = seq(5,105, by=20))

ggsave("plots/PVRT_c_acc.pdf")



# * * (m) VOLT accuracy ----
temp <- volt_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(volt_cr),n=n())
names(temp) <- c("age","sex","remote","mtc","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mtc,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mtc,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mtc,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mtc,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

volt_cgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Total Correct",
       title = "VOLT Accuracy (as Total Correct) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/VOLT_c_acc.pdf")



# * * (n) AIM accuracy ----
temp <- aim_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(aim_tot),n=n())
names(temp) <- c("age","sex","remote","mtc","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mtc,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mtc,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mtc,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mtc,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

aim_cgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Total Correct",
       title = "AIM Accuracy (as Total Correct) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/AIM_c_acc.pdf")



# * * (o) DIGSYM accuracy ----
temp <- digsym_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(dscor),n=n())
names(temp) <- c("age","sex","remote","mtc","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mtc,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mtc,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mtc,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mtc,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

digsym_cgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Total Correct",
       title = "DIGSYM Accuracy (as Total Correct) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/DIGSYM_c_acc.pdf")



# * * (p) DIGSYM memory accuracy ----
temp <- dsm_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(dsmemcr),n=n())
names(temp) <- c("age","sex","remote","mtc","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mtc,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mtc,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mtc,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mtc,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

dsm_cgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Total Correct",
       title = "DIGSYM (memory) Accuracy (as Total Correct) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/DIGSYMmem_c_acc.pdf")



# * * (q) GNG accuracy ----
temp <- gng_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(gng_cr),n=n())
names(temp) <- c("age","sex","remote","mtc","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mtc,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mtc,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mtc,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mtc,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

gng_cgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Total Correct",
       title = "GNG Accuracy (as Total Correct) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/GNG_c_acc.pdf")




# * (iii) speed (adt,cpf,cpt,cpw,er40,lnb,medf,mpraxis,pcet,plot,pmat,pvrt,volt) ----

# * * (a) ADT speed ----
temp <- adt_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(adt_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

adt_scgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "ADT Speed (as Response time) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/ADT_c_spe.pdf")


# * * (b) CPF speed ----
temp <- cpf_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(cpf_w_rtcr),n=n())    # Tyler said to use weighted response time measures here
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

cpf_scgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "CPF Speed (as Response time) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/CPF_c_spe.pdf")


# * * (c) CPT speed ----

temp <- cpt_c %>%
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>%
  dplyr::summarise(mean=mean(cpt_tprt),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

cpt_scgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "CPT Speed (as Response time) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/CPT_c_spe.pdf")



# * * (d) CPW speed ----

temp <- cpw_c %>%
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>%
  dplyr::summarise(mean=mean(cpw_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

cpw_scgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "CPW Speed (as Response time) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/CPW_c_spe.pdf")



# * * (e) ER40 speed ----
temp <- er40_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(er40_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

er40_scgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "ER40 Speed (as Response time) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/ER40_c_spe.pdf")


# * * (f) LNB speed ----
temp <- lnb_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(lnb_mrtc),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

lnb_scgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "LNB Speed (as Response time) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/LNB_c_spe.pdf")


# * * (g) MEDF speed ----
temp <- medf_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(medf_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

medf_scgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "MEDF Speed (as Response time) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/MEDF_c_spe.pdf")


# * * (h) MPRAXIS speed ----
temp <- mpraxis_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(mpraxis_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

mpraxis_scgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "MPRAXIS Speed (as Response time) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/MPRAXIS_c_spe.pdf")


# * * (i) PCET speed ----
temp <- pcet_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(pcet_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

pcet_scgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "PCET Speed (as Response time) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/PCET_c_spe.pdf")


# * * (j) PLOT speed ----
temp <- plot_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(plot_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

plot_scgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "PLOT Speed (as Response time) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/PLOT_c_spe.pdf")


# * * (k) PMAT speed ----
temp <- pmat_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(pmat_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

pmat_scgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "PMAT Speed (as Response time) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/PMAT_c_spe.pdf")


# * * (l) PVRT accuracy ----

temp <- pvrt_c %>%
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>%
  dplyr::summarise(mean=mean(pvrt_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

pvrt_sgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "PVRT Speed (as Response time) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/PVRT_c_spe.pdf")



# * * (m) VOLT accuracy ----
temp <- volt_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(volt_w_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

volt_scgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "VOLT Speed (as Response time) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/individual/VOLT_c_spe.pdf")



# * * (n) AIM accuracy ----
temp <- aim_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(aim_totrt),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

aim_scgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "AIM Speed (as Response time) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/individual/aim_c_spe.pdf")



# * * (o) DIGSYM accuracy ----
temp <- digsym_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(dscorrt),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

digsym_scgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "DIGSYM Speed (as Response time) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/individual/DIGSYM_c_spe.pdf")



# * * (m) VOLT accuracy ----
temp <- volt_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(volt_w_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

volt_scgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "VOLT Speed (as Response time) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/individual/VOLT_c_spe.pdf")



# * * (m) VOLT accuracy ----
temp <- volt_c %>% 
  group_by(test_sessions_v.age,test_sessions_v.gender,remote) %>% 
  dplyr::summarise(mean=mean(volt_w_rtcr),n=n())
names(temp) <- c("age","sex","remote","mrtcr","n")
temp$female_inp <- ifelse(temp$sex == "F" & temp$remote == 0, temp$mrtcr,NA)
temp$female_rem <- ifelse(temp$sex == "F" & temp$remote == 1, temp$mrtcr,NA)
temp$male_inp <- ifelse(temp$sex == "M" & temp$remote == 0, temp$mrtcr,NA)
temp$male_rem <- ifelse(temp$sex == "M" & temp$remote == 1, temp$mrtcr,NA)

finp <- temp[!is.na(temp$female_inp),c(1,6)]
frem <- temp[!is.na(temp$female_rem),c(1,7)]
minp <- temp[!is.na(temp$male_inp),c(1,8)]
mrem <- temp[!is.na(temp$male_rem),c(1,9)]

new <- merge(finp,frem,by="age",all=T)
new <- merge(new,minp,by="age",all=T)
new <- merge(new,mrem,by="age",all=T)

nnn <- c(sum(temp[temp$sex=="F" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="F" & temp$remote=="1","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="0","n"]),
         sum(temp[temp$sex=="M" & temp$remote=="1","n"]))

volt_scgg1 <- ggplot(new,aes(x=age)) +
  scale_color_manual(values=colors) +
  geom_point(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1]))) +
  geom_point(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2]))) +
  geom_point(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3]))) +
  geom_point(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4]))) +
  geom_smooth(aes(y=female_inp, color=paste("Female In-person, n =",nnn[1])),se=F) +
  geom_smooth(aes(y=female_rem, color=paste("Female Remote, n =",nnn[2])),se=F) +
  geom_smooth(aes(y=male_inp, color=paste("Male In-person, n =",nnn[3])),se=F) +
  geom_smooth(aes(y=male_rem, color=paste("Male Remote, n =",nnn[4])),se=F) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x = "Age",
       y = "Response Time (ms)",
       title = "VOLT Speed (as Response time) across age, sex and platform (2019 and on)") +
  scale_x_continuous(limits = c(5,105))

ggsave("plots/individual/VOLT_c_spe.pdf")






# (4) Statistical Analysis: regress out age and look for significant differences across platforms ----

# * (i) rename columns in tests so they are uniform ----
newadt <- adt_c[,c(2,3,6,7,10,13,20,23,24)]
newcpf <- cpf_c[,c(2,3,6,7,10,13,20,23,25)]
newcpt <- cpt_c[,c(2,3,6,7,10,13,20,23,25)]  
newcpw <- cpw_c[,c(2,3,6,7,10,13,20,23,25)]  
newer40 <- er40_c[,c(2,3,6,7,10,13,20,23,24)]
newlnb <- lnb_c[,c(2,3,6,7,10,13,20,23,24)]
newmedf <- medf_c[,c(2,3,6,7,10,13,20,23,24)]
newmpraxis <- mpraxis_c[,c(2,3,6,7,10,13,20,23)] # only RTCR
newpcet <- pcet_c[,c(2,3,6,7,10,13,20,25,23,24)]
newplot <- plot_c[,c(2,3,6,7,10,13,20,23,24)]
newpmat <- pmat_c[,c(2,3,6,7,10,13,20,23,24)]
newpvrt <- pvrt_c[,c(2,3,6,7,10,13,20,22,23)] 
newvolt <- volt_c[,c(2,3,6,7,10,13,20,23,25)]

newaim <- aim_c[]

# newtexts <- c("newadt","newcpf","newcpt","newcpw","newer40","newlnb","newmedf",
#               "newplot","newpmat","newpvrt","newvolt")
# newtests <- mget(newtexts)
# 
# for (i in 1:length(newtexts)) {
#   test <- newtests[[i]]
#   names(test) <- c("datasetID","site","bblid","age","dotest","sex","remote","acc","rt")
# }
# ++++++ i dont understand why the code above doesn't work:(

names(newadt) <- c("datasetID","site","bblid","age","dotest","sex","remote","acc","rt")
names(newcpf) <- c("datasetID","site","bblid","age","dotest","sex","remote","acc","rt")
names(newcpt) <- c("datasetID","site","bblid","age","dotest","sex","remote","acc","rt")  # no remote
names(newcpw) <- c("datasetID","site","bblid","age","dotest","sex","remote","acc","rt")  # no remote
names(newer40) <- c("datasetID","site","bblid","age","dotest","sex","remote","acc","rt")
names(newlnb) <- c("datasetID","site","bblid","age","dotest","sex","remote","acc","rt")
names(newmedf) <- c("datasetID","site","bblid","age","dotest","sex","remote","acc","rt")
names(newplot) <- c("datasetID","site","bblid","age","dotest","sex","remote","acc","rt")
names(newpmat) <- c("datasetID","site","bblid","age","dotest","sex","remote","acc","rt")
names(newpvrt) <- c("datasetID","site","bblid","age","dotest","sex","remote","acc","rt")  # no remote
names(newvolt) <- c("datasetID","site","bblid","age","dotest","sex","remote","acc","rt")

names(newmpraxis) <- c("datasetID","site","bblid","age","dotest","sex","remote","rt")
names(newpcet) <- c("datasetID","site","bblid","age","dotest","sex","remote","acc1","acc2","rt")


# * (ii) comparing in-person vs remote results ----
acctexts <- c("newadt","newcpf","newer40","newlnb","newmedf","newpcet","newplot","newpmat","newvolt")
acctests <- mget(acctexts)

for (i in 1:length(acctexts)) {
  test <- acctests[i]
  fit <- gam(acc ~ s(age,k=3) + sex, data = test)
  fitAD <- summary(fit)
  visreg(fit,"age", by="sex")
  
  test$res <- scale(resid(fit))   
  
  ttest <- t.test(test$res~test$remote)
  sumAcc <- c(sumAcc,"ttestAD")
}

# code from flash vs non-flash analysis (edited)
test <- newadt

fit <- gam(acc ~ s(age,k=3) + sex, data = test)
fitAD <- summary(fit)
visreg(fit,"age", by="sex")

test$res <- scale(resid(fit))   

ttest <- t.test(test$res~test$remote)
sumAcc <- c(sumAcc,"ttestAD")

# effect sizes
AD0 <- ttest$estimate[[1]]
AD1 <- ttest$estimate[[2]]
effsizeAD <- abs(AD0-AD1)
sumAcc <- c(sumAcc,"effsizeAD")

# box plot
boxAD <- ggplot(test, aes(x=factor(remote),y=res,group=remote)) +
  geom_boxplot() +
  labs(title = paste("Accuracy Difference in",textsAcc[i],"(All dates)")) +
  ylab("Residuals") +
  scale_x_discrete("Flash",breaks=0:1,labels=c("Non-Flash", "Flash")) +
  ylim(-5,5)
sumAcc <- c(sumAcc,"boxAD")
nAD <- res %>%
  group_by(flash) %>%
  summarise(mean=mean(residuals),median=median(residuals),n=n())
sumAcc <- c(sumAcc,"nAD")




# old code
test <- testsAcc[[i]]
name <- paste0(textsAcc[i],"sumAcc")
sumAcc <- c(textsAcc[i])

# alldates
fit <- gam(Accuracy ~ s(age,k=3) + gender, data = test)

# save summary of this model as a variable
fitAD <- summary(fit)
sumAcc <- c(sumAcc,"fitAD")

visreg(fit,"age", by="gender", main=paste(textsAcc[i],"AD"))

res <- scale(resid(fit))   # scaled residuals
newtest <- test[!is.na(test$Accuracy) & !is.na(test$age) & !is.na(test$gender),]
nflash <- newtest[newtest$flash==0,]
nfrownames <- row.names(nflash)

res <- as.data.frame(res)
rownames(res) <- rownames(newtest)
names(res) <- "residuals"
res$flash <- 1
for (j in 1:nrow(res)) {
  if (row.names(res[j,]) %in% nfrownames) {
    res[j,]$flash <- 0
  } else{}
}
res <- res[abs(res$residuals) <= 5,]


ttestAD <- t.test(res$residuals~res$flash)
sumAcc <- c(sumAcc,"ttestAD")

# effect sizes
AD0 <- ttestAD$estimate[[1]]
AD1 <- ttestAD$estimate[[2]]
effsizeAD <- abs(AD0-AD1)
sumAcc <- c(sumAcc,"effsizeAD")

# box plot
boxAD <- ggplot(res, aes(x=factor(flash),y=residuals,group=flash)) +
  geom_boxplot() +
  labs(title = paste("Accuracy Difference in",textsAcc[i],"(All dates)")) +
  ylab("Residuals") +
  scale_x_discrete("Flash",breaks=0:1,labels=c("Non-Flash", "Flash")) +
  ylim(-5,5)
sumAcc <- c(sumAcc,"boxAD")
nAD <- res %>%
  group_by(flash) %>%
  summarise(mean=mean(residuals),median=median(residuals),n=n())
sumAcc <- c(sumAcc,"nAD")














