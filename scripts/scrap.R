# scrap R things for inperson_v_remote.R


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


# ignore for age match ----
demo <- dat[,1:20]    # demographics & non-test-specific things

dat_bbl <- dat[!is.na(dat$bblid),]         # all cols of rows with BBLIDs
dat_bbl <- dat_bbl[order(dat_bbl$bblid),]
dat_rem <- dat_bbl[dat_bbl$remote==1,]
dat_inp <- dat_bbl[dat_bbl$remote==0,]
dat_t1 <- dat_bbl[]   # from dat_bbl, get the most recent timepoint data for each 


# * separating out into tasks ----
notval <- c("N")
colors <- c("#6E4230", "#BF9660", "#065C50", "#48B0A2")

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
digsym <- select(digsym, "age","dotest","sex","remote","ds_cor","ds_corrt")

dsm <- cbind(demo, dat[,grepl("digsym",colnames(dat))], dat[,grepl("ds",colnames(dat))])   # memcr and mcrrt
dsm <- dsm[!is.na(dsm$ds_memcr),-c(23,25)]   # memory component of digsym test
dsm <- dsm[!(dsm$digsym_valid %in% notval),]
dsm <- select(dsm, "age","dotest","sex","remote","ds_memcr","ds_mcrrt")

gng <- cbind(demo, dat[,grepl("gng",colnames(dat))])   # CR and RTCR
gng <- gng[!is.na(gng$gng_cr),]
gng <- gng[!(gng$gng_valid %in% notval),]
gng <- select(gng, "age","dotest","sex","remote","gng_cr","gng_rtcr")

aim <- cbind(demo, dat[,grepl("aim",colnames(dat))])   # TOT and totrt    empty for now
aim <- aim[!is.na(aim$aim_tot),]
aim <- aim[!(aim$tap_valid %in% notval),]
aim <- select(aim, "age","dotest","sex","remote","adt_pc","adt_rtcr") # not sure what to use here
aim <- NA

ddisc <- NA # all DISC tasks are missing data right now

rdisc <- NA

edisc <- NA


# this doesn't seem as relevant anymore lol
test_index <- data.frame(matrix(nrow = 22,ncol=4))
test_index[,1] <- sort(c("adt","aim","cpf","cpt","cpw","digsym","dsm","er40","gng","lnb",
                         "medf","pcet_cat","pcet_acc2","plot","pmat","pvrt","volt","ddisc",
                         "edisc","rdisc","mpraxis","tap"))

names(test_index) <- c("test_names","acc","speed","weird_acc")





acc_texts <- c("adt","cpf","cpt","cpw","digsym","dsm","er40","gng","lnb","medf",
               "pcet_cat","pcet_acc2","plot","pmat","pvrt","tap","volt")          # no aim or disc for now, no ddataa
acc_tests <- mget(acc_texts)



# most up to date -- 02/22/22
plotlist <- list()
nnn <- list()
for (i in 1:4) {        
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

pdf("plots/test_acc_plots17Feb22.pdf")
for (i in 1:4) {
  print(plotlist[[i]])
}
dev.off()









# using the loop of accuracy plots for all (speed, 2020 and on acc/speed)  -- still working on these 02/22/22
all_texts <- test_index[,1]
all_tests <- mget(all_texts)

accplotlist <- list()
accCplotlist <- list()
speplotlist <- list()
speCplotlist <- list()
nnn <- list()
for (i in 1:length(all_texts)) {        # still not satisfied about the nnn part
  test <- all_tests[[i]]
  # acc
  if (all_texts[i]!="mpraxis"){
    cr_pc <- ifelse(str_detect(names(test)[5],"cat"),"Categories Achieved",
                    ifelse(str_detect(names(test)[5],"acc2"),"Acc2 Score",
                           ifelse(str_detect(names(test)[5],"ptp"),"Percent True Positive",
                                  ifelse(str_detect(names(test)[5],"pc"),"Percent Correct","Total Correct"))))
    names(test)[5] <- "acc"
    
    # all dates
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
           title = paste0(str_to_upper(all_texts[i]), " Accuracy (as ", cr_pc, ") across age, sex and platform")) +
      scale_x_continuous(limits = c(5,105))
    
    accplotlist[[i]] <- p
    
    # 2020 and on
    test <- test[test$dotest > as.Date("2019-12-31"),]
    test$finp <- ifelse(test$sex=="F" & test$remote == 0,test$acc,NA)
    test$frem <- ifelse(test$sex=="F" & test$remote == 1,test$acc,NA)
    test$minp <- ifelse(test$sex=="M" & test$remote == 0,test$acc,NA)
    test$mrem <- ifelse(test$sex=="M" & test$remote == 1,test$acc,NA)
    
    j <- length(all_texts)+i-1
    nnn[[j]] <- c(sum(!is.na(test$finp)),
                  sum(!is.na(test$frem)),
                  sum(!is.na(test$minp)),
                  sum(!is.na(test$mrem)))
    
    c <- ggplot(test,aes(x=age)) +
      scale_color_manual(values=colors) +
      geom_point(aes(y=finp, color=paste("Female In-person, n =",nnn[[j]][1])),size=.8) +
      geom_point(aes(y=frem, color=paste("Female Remote, n =",nnn[[j]][2])),size=.8) +
      geom_point(aes(y=minp, color=paste("Male In-person, n =",nnn[[j]][3])),size=.8) +
      geom_point(aes(y=mrem, color=paste("Male Remote, n =",nnn[[j]][4])),size=.8) +
      geom_smooth(aes(y=finp, color=paste("Female In-person, n =",nnn[[j]][1])),se=F,size=1) +
      geom_smooth(aes(y=frem, color=paste("Female Remote, n =",nnn[[j]][2])),se=F,size=1) +
      geom_smooth(aes(y=minp, color=paste("Male In-person, n =",nnn[[j]][3])),se=F,size=1) +
      geom_smooth(aes(y=mrem, color=paste("Male Remote, n =",nnn[[j]][4])),se=F,size=1) +
      theme_minimal() +
      theme(legend.title = element_blank()) +
      labs(x = "Age",
           y = "Percent Correct",
           title = paste0(str_to_upper(all_texts[i]), " Accuracy (as ", cr_pc, ") across age, sex and platform (2020 and on)")) +
      scale_x_continuous(limits = c(5,105))
    
    accCplotlist[[i]] <- c
  }
  
  # spe
  if (all_texts[i]!="tap"){
    names(test)[5] <- "acc"
    
    # all dates
    test$finp <- ifelse(test$sex=="F" & test$remote == 0,test$acc,NA)
    test$frem <- ifelse(test$sex=="F" & test$remote == 1,test$acc,NA)
    test$minp <- ifelse(test$sex=="M" & test$remote == 0,test$acc,NA)
    test$mrem <- ifelse(test$sex=="M" & test$remote == 1,test$acc,NA)
    
    nnn[[i]] <- c(sum(!is.na(test$finp)),
                  sum(!is.na(test$frem)),
                  sum(!is.na(test$minp)),
                  sum(!is.na(test$mrem)))
    
    sp <- ggplot(test,aes(x=age)) +
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
           title = paste0(str_to_upper(all_texts[i]), " Speed (as Response time) across age, sex and platform")) +
      scale_x_continuous(limits = c(5,105))
    
    speplotlist[[i]] <- sp
    
    # 2020 and on
    test <- test[test$dotest > as.Date("2019-12-31"),]
    test$finp <- ifelse(test$sex=="F" & test$remote == 0,test$acc,NA)
    test$frem <- ifelse(test$sex=="F" & test$remote == 1,test$acc,NA)
    test$minp <- ifelse(test$sex=="M" & test$remote == 0,test$acc,NA)
    test$mrem <- ifelse(test$sex=="M" & test$remote == 1,test$acc,NA)
    
    j <- length(all_texts)+i-1
    nnn[[j]] <- c(sum(!is.na(test$finp)),
                  sum(!is.na(test$frem)),
                  sum(!is.na(test$minp)),
                  sum(!is.na(test$mrem)))
    
    sc <- ggplot(test,aes(x=age)) +
      scale_color_manual(values=colors) +
      geom_point(aes(y=finp, color=paste("Female In-person, n =",nnn[[j]][1])),size=.8) +
      geom_point(aes(y=frem, color=paste("Female Remote, n =",nnn[[j]][2])),size=.8) +
      geom_point(aes(y=minp, color=paste("Male In-person, n =",nnn[[j]][3])),size=.8) +
      geom_point(aes(y=mrem, color=paste("Male Remote, n =",nnn[[j]][4])),size=.8) +
      geom_smooth(aes(y=finp, color=paste("Female In-person, n =",nnn[[j]][1])),se=F,size=1) +
      geom_smooth(aes(y=frem, color=paste("Female Remote, n =",nnn[[j]][2])),se=F,size=1) +
      geom_smooth(aes(y=minp, color=paste("Male In-person, n =",nnn[[j]][3])),se=F,size=1) +
      geom_smooth(aes(y=mrem, color=paste("Male Remote, n =",nnn[[j]][4])),se=F,size=1) +
      theme_minimal() +
      theme(legend.title = element_blank()) +
      labs(x = "Age",
           y = "Percent Correct",
           title = paste0(str_to_upper(all_texts[i]), " Speed (as Response time) across age, sex and platform (2020 and on)")) +
      scale_x_continuous(limits = c(5,105))
    
    speCplotlist[[i]] <- sc
  }
  
  
}

pdf("plots/acc_plots.pdf")
for (i in 1:length(all_texts)) {
  print(accplotlist[[i]])
}
dev.off()






# for cross-sectional analysis: look at most recent tp for rem and inp for each unique bblid
# separate by site, 
# longitudinal analysis: spaghetti plots







# using Noah's code for my data ----

dat2 <- dat %>% 
  dplyr::select(bblid,test_sessions.datasetid,test_sessions.siteid, 
         test_sessions.famid, test_sessions.subid, age, test_sessions_v.battery, 
         dob, dotest, test_sessions_v.education, test_sessions_v.feducation,
         sex, test_sessions_v.handedness, test_sessions_v.meducation, platform, remote,
         adt_valid, adt_pc, adt_rtcr, 
         aim_valid, aim_tot, aim_totrt,
         cpf_valid, cpf_cr, cpf_w_rtcr, 
         cpt_valid, cpt_ptp, cpt_tprt,
         cpw_valid, cpw_cr, cpw_w_rtcr,
         digsym_valid, ds_cor, ds_corrt, ds_memcr, ds_mcrrt,
         er40_valid, er40_cr, er40_rtcr, 
         gng_valid, gng_cr, gng_rtcr,
         lnb_valid, lnb_mcr, lnb_mrtc,
         medf_valid, medf_pc, medf_rtcr, 
         mpraxis_valid, mpraxis_rtcr, 
         pcet_valid, pcet_cat, pcet_acc2, pcet_rtcr, 
         plot_valid, plot_pc, plot_rtcr,
         pmat_valid, pmat_pc, pmat_rtcr,
         pvrt_valid, pvrt_cr, pvrt_rtcr,
         tap_valid, tap_tot, tap_valid, 
         volt_valid, volt_cr, volt_w_rtcr) %>%
  mutate(sex = ifelse(sex == "F","Female","Male")) %>% 
  mutate(remote = ifelse(platform == "webcnp","In-person","Remote")) %>% 
  mutate(sex_remote = paste(sex,remote))

colnames(dat2)[grep("digsym",colnames(dat2))] <- "ds_valid"

repeats_list <- dat2 %>% 
  group_by(bblid) %>% 
  filter(n() > 1) %>%
  arrange(dotest) %>% 
  ungroup() %>% 
  group_split(bblid)

find_last_test <- function(df_by_bblid){      # if a bblid's last test is remote, take the most recent remote test data
  if(any(df_by_bblid$remote == "Remote")){
    last_test <- df_by_bblid %>% 
      arrange(dotest) %>% 
      filter(remote == "Remote") %>% 
      slice_tail(n = 1)
  } else{
    last_test <- df_by_bblid %>%              # if a bblid's last tests is in-person, take last in-person data
      arrange(dotest) %>% 
      slice_tail(n = 1)
  }
  return(last_test)
}                                             # each bblid will have either remote OR in-person test data, but not both

Last_tests <- map_dfr(repeats_list,find_last_test) # 1712 x 69, 2/22/22

cnb_cross <- dat2 %>%    # combining all info from bblid's that only have one row of data
  group_by(bblid) %>% 
  filter(n() == 1) %>%
  arrange(dotest) %>% 
  ungroup() %>% 
  bind_rows(Last_tests)   # attaching all data from last_tests at the end

# get rid of outliers

tests <- cnb_cross %>%   # bblid, age, dob, dotest, sex, _valid
  dplyr::select(!(matches("bblid") | matches("^test") | "age" | "dob" | "dotest" | "sex" | "platform" | "remote" | matches("_valid$") | matches("^aim") | "sex_remote")) %>%    # ^[this]: anything beginning with [this]; [this]$: anything ending with [this]
  colnames()
for(test in tests){
  cnb_cross[[test]] <- ifelse(cnb_cross[[test]] > mean(cnb_cross[[test]],na.rm = TRUE) + 6*sd(cnb_cross[[test]],na.rm = TRUE),mean(cnb_cross[[test]],na.rm = TRUE) + 6*sd(cnb_cross[[test]],na.rm = TRUE),cnb_cross[[test]])
  cnb_cross[[test]] <- ifelse(cnb_cross[[test]] < mean(cnb_cross[[test]],na.rm = TRUE) - 6*sd(cnb_cross[[test]],na.rm = TRUE),mean(cnb_cross[[test]],na.rm = TRUE) - 6*sd(cnb_cross[[test]],na.rm = TRUE),cnb_cross[[test]])
}

response_vars <- cnb_cross %>% 
  dplyr::select(!(contains("valid"))) %>% 
  dplyr::select(adt_pc:volt_w_rtcr) %>% 
  colnames()

# exclude aim for now (DISC tasks excluded already)
response_vars <- setdiff(response_vars,c("aim_tot", "aim_totrt"))

Test_map <- data.frame('Prefix' = c("er40","pvrt","volt","cpf","cpw","gng","mpraxis","pcet","pmat","medf","adt","plot","tap","cpt","lnb","volt","plot","ds"),
                       "Test_name" = c("Penn Emotion Recognition Test","Penn Verbal Reasoning Test","Visual Object Learning Test","Penn Face Memory Test",'Penn Word Memory Test',
                                       "Go-No-Go Test","Motor Praxis Test","Penn Conditional Exclusion Test","Penn Matrix Analysis Test","Measured Emotion Differentiation Test",
                                       "Age Differentiation Test","Penn Line Orientation Test","Penn Computerized Finger Tapping Test","Penn Continuous Performance Test",
                                       "Letter-N-Back","Visual Object Learning Test","Penn Line Orientation Test","Digit Symbol Search"))

Metric_map <- data.frame("Suffix" = c("_cor","_cr","_corrt","_rtcr","_tot","_acc2","_tprt","_ptp","_mcr","_mrtc","_pc","_mp2rtcr","_cat","_mrtc","_tp","_memcr","_mcrrt"),
                         "Label" = c("Correct Responses","Correct Responses","Median Response Time \n Correct Responses (ms)","Median Response Time \n Correct Responses (ms)","Average Taps \n (Dominant and Non-dominant hand added together)",
                                     "Accuracy","Median Response Time \n True Positives (ms)","True Positive (%)","Total True Positive Responses","Median Response Time \n Correct Responses",
                                     "Correct Responses (%)","Median Response Time \n Correct Responses (ms)","Categories Achieved","Median Response Time \n Correct Responses (ms)",
                                     "True Positive Responses","Correct Responses","Median Response Time \n Correct Responses (ms)"))

# (Noah's) old plot making script ----
# LongitudinalPlots <- list()
# cntr <- 1
# theme_set(theme_minimal())
# for(test in response_vars){
#   test_split <- str_split(test,pattern = "_")[[1]]
#   test_prefix <- test_split[1]
#   test_suffix <- paste0("_",test_split[length(test_split)])
#   
#   Plot_title <- Test_map %>% 
#     filter(Prefix == test_prefix) %>% 
#     pull(Test_name)
#   
#   ylabel <- Metric_map %>% 
#     filter(Suffix == test_suffix) %>% 
#     pull(Label)
#   
#   N.df <- cnb_cross[,c(test,"sex_remote","age")] %>% 
#     filter(if_all(everything(), ~ !is.na(.))) %>% 
#     group_by(sex_remote) %>% 
#     dplyr::summarize(n = n()) %>% 
#     mutate(sex_remote_N = factor(paste0(sex_remote,": ","N = ",n))) %>% 
#     arrange(sex_remote_N)
#   
#   if(nrow(N.df) == 2){
#     LongitudinalPlots[[cntr]] <- cnb_cross %>% 
#       left_join(N.df) %>% 
#       filter(!is.na(sex_remote_N)) %>% 
#       ggplot(aes_string(x = "age",y = test,color = "sex_remote_N")) + 
#       geom_point(size = .6) + geom_smooth(se = FALSE) + labs(x = "Age",y = ylabel,title = Plot_title,color = "") + 
#       scale_color_manual(values = c("#ca0020","#0571b0")) + theme(legend.position = "bottom")
#     cntr <- cntr + 1
#   } else{
#     LongitudinalPlots[[cntr]] <- cnb_cross %>% 
#       left_join(N.df) %>% 
#       filter(!is.na(sex_remote_N)) %>% 
#       ggplot(aes_string(x = "age",y = test,color = "sex_remote_N")) + 
#       geom_point(size = .6) + geom_smooth(se = FALSE) + labs(x = "Age",y = ylabel,title = Plot_title,color = "") + 
#       scale_color_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de")) + theme(legend.position = "bottom")
#     cntr <- cntr + 1
#   }
# }

# use this to make all graphs

# * my version of Noah's plot making code ----
LPall_rec <- list()                                         # [L]ongitudinal [P]lots for [all] and [rec]ent dates
cntr <- 1
for(test in response_vars){
  allcnt <- 2*cntr-1
  reccnt <- 2*cntr
  
  test_split <- str_split(test,pattern = "_")[[1]]
  test_prefix <- test_split[1]
  test_suffix <- paste0("_",test_split[length(test_split)])
  
  Plot_title <- Test_map %>% 
    filter(Prefix == test_prefix) %>% 
    pull(Test_name)
  
  if (test %in% c("ds_memcr", "ds_mcrrt")) {
    Plot_title <- paste(Plot_title, "(Memory)")
  }
  
  ylabel <- Metric_map %>% 
    filter(Suffix == test_suffix) %>% 
    pull(Label)
  
  N.df <- cnb_cross[,c(test,"sex_remote","age")] %>% 
    filter(if_all(everything(), ~ !is.na(.))) %>% 
    group_by(sex_remote) %>% 
    dplyr::summarize(n = n()) %>% 
    mutate(sex_remote_N = factor(paste0(sex_remote,": ","N = ",n))) %>% 
    arrange(sex_remote_N)
  
  if(nrow(N.df) == 2){
    LPall_rec[[allcnt]] <- cnb_cross %>% 
      left_join(N.df) %>% 
      filter(!is.na(sex_remote_N)) %>% 
      ggplot(aes_string(x = "age",y = test,color = "sex_remote_N")) + 
      geom_point(size = .6) + geom_smooth(se = FALSE) + labs(x = "Age",y = ylabel,title = Plot_title,color = "") +
      scale_x_continuous(limits = c(5,90)) +
      scale_color_manual(values = c("#ca0020","#0571b0")) + 
      theme(legend.position = "bottom",
            legend.text = element_text(size=5),
            plot.margin = margin(1,1,1,1,unit = "cm"))
  } else{
    LPall_rec[[allcnt]] <- cnb_cross %>% 
      left_join(N.df) %>% 
      filter(!is.na(sex_remote_N)) %>% 
      ggplot(aes_string(x = "age",y = test,color = "sex_remote_N")) + 
      geom_point(size = .6) + geom_smooth(se = FALSE) + labs(x = "Age",y = ylabel,title = Plot_title,color = "") +
      scale_x_continuous(limits = c(5,90)) +
      scale_color_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de")) + 
      theme(legend.position = "bottom",
            legend.text = element_text(size=5),
            plot.margin = margin(1,1,1,1,unit = "cm"))
  }
  
  # recent data (2016 and on)
  
  pt2 <- Test_map %>%                       # [p]lot [t]itle 2
    filter(Prefix == test_prefix) %>% 
    pull(Test_name)
  
  pt2 <- ifelse(test %in% c("ds_memcr", "ds_mcrrt"),paste(pt2, "(Memory, 2016 and on)"),paste(pt2,"(2016 and on)"))
  
  ylabel2 <- Metric_map %>% 
    filter(Suffix == test_suffix) %>% 
    pull(Label)
  
  N.df <- cnb_cross[,c(test,"sex_remote","age","dotest")] %>% 
    filter(if_all(everything(), ~ !is.na(.))) %>% 
    filter(dotest > as.Date("2015-12-31")) %>% 
    group_by(sex_remote) %>% 
    dplyr::summarize(n = n()) %>% 
    mutate(sex_remote_N = factor(paste0(sex_remote,": ","N = ",n))) %>% 
    arrange(sex_remote_N)
  
  if(nrow(N.df) == 2){
    LPall_rec[[reccnt]] <- cnb_cross %>% 
      left_join(N.df) %>% 
      filter(!is.na(sex_remote_N)) %>% 
      filter(dotest > as.Date("2015-12-31")) %>% 
      ggplot(aes_string(x = "age",y = test,color = "sex_remote_N")) + 
      geom_point(size = .6) + geom_smooth(se = FALSE) + labs(x = "Age",y = ylabel,title = pt2,color = "") + 
      scale_x_continuous(limits = c(5,90)) +
      scale_color_manual(values = c("#ca0020","#0571b0")) + 
      theme(legend.position = "bottom",
            legend.text = element_text(size=5),
            plot.margin = margin(1,1,1,1,unit = "cm"))
  } else{
    LPall_rec[[reccnt]] <- cnb_cross %>% 
      left_join(N.df) %>% 
      filter(!is.na(sex_remote_N)) %>% 
      filter(dotest > as.Date("2015-12-31")) %>%  
      ggplot(aes_string(x = "age",y = test,color = "sex_remote_N")) + 
      geom_point(size = .6) + geom_smooth(se = FALSE) + labs(x = "Age",y = ylabel,title = pt2,color = "") + 
      scale_x_continuous(limits = c(5,90)) +
      scale_color_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de")) + 
      theme(legend.position = "bottom",
            legend.text = element_text(size=5),
            plot.margin = margin(1,1,1,1,unit = "cm"))
  }
  cntr <- cntr + 1
}

pdf("plots/fromNoahscode/cross_plots24Feb22.pdf")
for (i in 1:length(LPall_rec)){
  print(LPall_rec[[i]])
}
dev.off()





# Noah's longitudinal plots ----

# Create a longitudinal data set using individuals who were members of the PNC 9498 cohort


cnb_long_bblids <- dat %>% 
  group_by(bblid) %>% 
  dplyr::summarize(n = n()) %>% 
  ungroup() %>% 
  filter(n > 1) %>% 
  pull(bblid)

cnb_long <- dat %>% 
  filter(bblid %in% cnb_long_bblids) %>% 
  select(!(matches("^test") | matches("^do") | matches("plat") | matches("_valid$") | matches("genus$") | matches("DISC") | matches("aim") | matches("^tap") | matches("pfp") | matches("tprt.1") | matches("volt_rtcr") | matches("cpf_rtcr") | matches("cpw_rtcr")))  # for now, remove all aim, tap, and DISC

# Create Longitudinal Plots for each test 

response_cols <- cnb_long %>% 
  select(mpraxis_rtcr:last_col()) %>% 
  colnames()
cnb_long_plots <- list()
cntr <- 1

for(test in response_cols){
  test_quo <- quo(!!sym(test))
  test_split <- str_split(test,pattern = "_")[[1]]
  test_prefix <- test_split[1]
  test_suffix <- paste0("_",test_split[length(test_split)])
  
  Plot_title <- Test_map %>% 
    filter(Prefix == test_prefix) %>% 
    pull(Test_name)
  
  ylabel <- Metric_map %>% 
    filter(Suffix == test_suffix) %>% 
    pull(Label)
  
  # Cap values at 6sd above/below the norm
  cnb_long[[test]] <- ifelse(cnb_long[[test]] > mean(cnb_long[[test]],na.rm = T) + 6*sd(cnb_long[[test]],na.rm = T),mean(cnb_long[[test]],na.rm = T) + 6*sd(cnb_long[[test]],na.rm = T),cnb_long[[test]])
  cnb_long[[test]] <- ifelse(cnb_long[[test]] < mean(cnb_long[[test]],na.rm = T) - 6*sd(cnb_long[[test]],na.rm = T),mean(cnb_long[[test]],na.rm = T) - 6*sd(cnb_long[[test]],na.rm = T),cnb_long[[test]])
  
  # Find N for each test
  
  Sex_N_timepoints <- cnb_long %>% 
    filter(!is.na(!!test_quo),!is.na(sex)) %>% 
    nrow()
  
  Sex_N_subj <-  cnb_long %>% 
    filter(!is.na(!!test_quo),!is.na(sex)) %>% 
    with(length(unique(bblid)))
  
  remote_N_timepoints <- cnb_long %>% 
    filter(!is.na(!!test_quo),!is.na(remote)) %>% 
    nrow()
  
  remote_N_subj <-  cnb_long %>% 
    filter(!is.na(!!test_quo),!is.na(remote)) %>% 
    with(length(unique(bblid)))
  
  Sex_rem_N_timepoints <- cnb_long %>% 
    filter(!is.na(!!test_quo),!is.na(remote),!is.na(sex)) %>% 
    nrow()
  
  Sex_rem_N_subj <-  cnb_long %>% 
    filter(!is.na(!!test_quo),!is.na(remote),!is.na(sex)) %>% 
    with(length(unique(bblid)))
  
  
  # Create three plots for sex, PS, and sex by PS effects
  
  df_for_plot <- cnb_long %>% 
    rename(Sex = sex) %>% 
    rename(Age = age) %>% 
    mutate(Sex = ifelse(Sex == "M","Male","Female")) %>% 
    mutate(remote = factor(remote,levels = c("Remote","In person"))) %>% 
    filter(!is.na(Sex),!is.na(remote)) %>% 
    mutate(Sex_rem = paste(Sex,remote)) %>% 
    mutate(Sex_rem = factor(Sex_rem,levels = c("Male Remmote","Male In person","Female Remmote","Female In person")))
  
  Plot_sep_sex <- df_for_plot %>% 
    ggplot(aes(x = Age,y = !!test_quo,color = Sex,group = bblid)) + 
    geom_smooth(aes(x = Age,y = !!test_quo,color = Sex,group = Sex)) + 
    geom_point(size = .1) + geom_line(alpha = .15) + 
    labs(x = "Age",y = ylabel,title = "",caption = paste0("N = ",Sex_N_subj,", Timepoints = ",Sex_N_timepoints)) + 
    scale_color_manual(values = c("#d7191c","#2b83ba")) 
  
  Plot_sep_PS <- df_for_plot %>% 
    ggplot(aes(x = Age,y = !!test_quo,color = PS,group = bblid)) + 
    geom_smooth(aes(x = Age,y = !!test_quo,color = PS,group = PS),se = F) + 
    geom_point(size = .1) + geom_line(alpha = .15) + 
    labs(x = "Age",y = ylabel,title = "",caption = paste0("N = ",PS_N_subj,", Timepoints = ",PS_N_timepoints),color = "") + 
    theme(legend.position = "bottom") + scale_color_brewer(palette = "Dark2") 
  
  Plot_sep_sex_PS <- df_for_plot %>% 
    ggplot(aes(x = Age,y = !!test_quo,color = Sex_PS,group = bblid)) + 
    geom_smooth(aes(x = Age,y = !!test_quo,color = Sex_PS,group = Sex_PS),se = F,alpha = 2) + 
    geom_point(size = .1) + geom_line(alpha = .15) + labs(x = "Age",y = ylabel,title = "",caption = paste0("N = ",Sex_PS_N_subj,", Timepoints = ",Sex_PS_N_timepoints),color = "") + 
    theme(legend.position = "bottom") + scale_color_manual(values = c("#6baed6","#2171b5","#08306b","#fc9272","#cb181d","#67000d")) + guides(color = guide_legend(nrow = 1))
  
  
  cnb_long_plots[[cntr]] <- annotate_figure(ggarrange(Plot_sep_sex,Plot_sep_PS,Plot_sep_sex_PS,labels = c("A","B","C")),top = text_grob(Plot_title,size = 16,face = "bold"))
  
  cntr <- cntr + 1
}







# age-matching code from Tyler ----
# i have to do some age-matching per test

demos <- cnb_cross %>% 
  dplyr::select(bblid:remote)

test_names <- sort(unique(sapply(str_split(response_vars,pattern = "_"),"[[",1)))

for (test in test_names) {
  templist <- c()
  
  test_dat <- cbind(demos, cnb_cross[,grepl(test, colnames(cnb_cross))])
  last_col <- tail(colnames(test_dat),1)
  test_dat <- test_dat %>%
    drop_na(last_col) %>% 
    drop_na(age)
  
  test_rem <- test_dat %>% 
    filter(remote == "Remote") %>% 
    mutate(rem = 1)
  n_rem <- dim(test_rem)[1]
  
  test_inp <- test_dat %>% 
    filter(remote == "In-person") %>% 
    mutate(rem = 0)
  
  test_dat2 <- rbind(test_rem,test_inp)       # test_dat recombined after adding var, rem = c(0,1)
  
  set.seed(2)
  mod <- matchit(rem~age+sex,data=test_dat2,ratio=1)    # matching by age and sex for now
  test_inperson <- test_dat2[mod$match.matrix,]
  n_inp <- dim(test_inperson)[1]
  
  test_dat <- rbind(test_rem,test_inperson)
  
  templist <- c(templist,"n_rem","n_inp","test_dat")
  
  if (test %in% c("tap","mpraxis")) {
    t_test1 <- t.test(test_dat[,18]~rem,data=test_dat)
    templist <- c(templist,"t_test1")
  }  else if (test == "pcet") {
    t_test1 <- t.test(test_dat[,18]~rem,data=test_dat)
    t_test2 <- t.test(test_dat[,19]~rem,data=test_dat)
    t_test3 <- t.test(test_dat[,20]~rem,data=test_dat)
    templist <- c(templist,"t_test1")
    templist <- c(templist,"t_test2")
    templist <- c(templist,"t_test3")
  } else if (test == "ds") {
    t_test1 <- t.test(test_dat[,18]~rem,data=test_dat)
    t_test2 <- t.test(test_dat[,19]~rem,data=test_dat)
    t_test3 <- t.test(test_dat[,20]~rem,data=test_dat)
    t_test4 <- t.test(test_dat[,21]~rem,data=test_dat)
    templist <- c(templist,"t_test1")
    templist <- c(templist,"t_test2")
    templist <- c(templist,"t_test3")
    templist <- c(templist,"t_test4")
  } else {
    t_test1 <- t.test(test_dat[,18]~rem,data=test_dat)
    t_test2 <- t.test(test_dat[,19]~rem,data=test_dat)
    templist <- c(templist,"t_test1")
    templist <- c(templist,"t_test2")
  }
  
  templist <- mget(templist)
  assign(paste(test,"list",sep="_"),templist)
}




# using the same code from above but incorporating sex and education regression before age-matching ----

# new column for avg parent edu.
cnb_cross$ParentEdu <- rowMeans(cnb_cross[,c("test_sessions_v.feducation","test_sessions_v.meducation")],na.rm=T)
cnb_cross <- cnb_cross[,c(1:16,70,17:69)]
cnb_cross$ParentEdu[is.nan(cnb_cross$ParentEdu)] <- NA   # not sure about using this because there are a lot of obs that have ParentEdu as 0

for (test in test_names) {
  templist <- c()
  
  test_dat <- cbind(demos, cnb_cross[,grepl(test, colnames(cnb_cross))]) %>% 
    dplyr::select(!matches("valid"))
  last_col <- tail(colnames(test_dat),1)
  test_dat <- test_dat %>%
    drop_na(last_col) %>% 
    drop_na(age)
  if (test=="plot"){
    test_dat <- test_dat %>%
      drop_na(colnames(test_dat)[ncol(test_dat)-1])
  }
  
  # regressing age out (and possibly avg parent edu as well?) - not yet 3/10/22
  if (test %in% c("tap","mpraxis")) {
    test_dat$measure_speed <- names(test_dat)[ncol(test_dat)]
    
    names(test_dat)[ncol(test_dat)-1] <- "speed"
    
    spe_fit <- glm(speed ~ sex, data = test_dat)
    test_dat$spe_res <- scale(resid(spe_fit))
    
  }  else if (test == "pcet") {
    test_dat$measure_cat <- names(test_dat)[ncol(test_dat)-2]
    test_dat$measure_acc2 <- names(test_dat)[ncol(test_dat)-2]
    test_dat$measure_speed <- names(test_dat)[ncol(test_dat)-2]
    
    names(test_dat)[ncol(test_dat)-5] <- "cat"
    names(test_dat)[ncol(test_dat)-4] <- "acc2"
    names(test_dat)[ncol(test_dat)-3] <- "speed"
    
    cat_fit <- glm(cat ~ sex, data = test_dat)
    test_dat$cat_res <- scale(resid(cat_fit))
    
    acc2_fit <- glm(acc2 ~ sex, data = test_dat)
    test_dat$acc2_res <- scale(resid(acc2_fit))
    
    spe_fit <- glm(speed ~ sex, data = test_dat)
    test_dat$spe_res <- scale(resid(spe_fit))
    
  } else if (test == "ds") {
    test_dat$measure_acc <- names(test_dat)[ncol(test_dat)-3]
    test_dat$measure_speed <- names(test_dat)[ncol(test_dat)-3]
    test_dat$measure_mem_acc <- names(test_dat)[ncol(test_dat)-3]
    test_dat$measure_mem_speed <- names(test_dat)[ncol(test_dat)-3]
    
    names(test_dat)[ncol(test_dat)-7] <- "acc"
    names(test_dat)[ncol(test_dat)-6] <- "speed"
    names(test_dat)[ncol(test_dat)-5] <- "mem_acc"
    names(test_dat)[ncol(test_dat)-4] <- "mem_speed"
    
    acc_fit <- glm(acc ~ sex, data = test_dat)
    test_dat$acc_res <- scale(resid(acc_fit))
    
    spe_fit <- glm(speed ~ sex, data = test_dat)
    test_dat$spe_res <- scale(resid(spe_fit))
    
    macc_fit <- glm(mem_acc ~ sex, data = test_dat)
    test_dat$macc_res <- scale(resid(macc_fit))
    
    mspe_fit <- glm(mem_speed ~ sex, data = test_dat)
    test_dat$mspe_res <- scale(resid(mspe_fit))
    
  } else {
    test_dat$measure_acc <- names(test_dat)[ncol(test_dat)-1]
    test_dat$measure_speed <- names(test_dat)[ncol(test_dat)-1]
    
    names(test_dat)[ncol(test_dat)-3] <- "acc"
    names(test_dat)[ncol(test_dat)-2] <- "speed"
    
    acc_fit <- glm(acc ~ sex, data = test_dat)
    test_dat$acc_res <- scale(resid(acc_fit))
    
    spe_fit <- glm(speed ~ sex, data = test_dat)
    test_dat$spe_res <- scale(resid(spe_fit))
  }
  
  test_rem <- test_dat %>% 
    filter(remote == "Remote") %>% 
    mutate(rem = 1)
  n_rem <- dim(test_rem)[1]
  
  test_inp <- test_dat %>% 
    filter(remote == "In-person") %>% 
    mutate(rem = 0)
  
  test_dat2 <- rbind(test_rem,test_inp)       # test_dat recombined after adding var, rem = c(0,1)
  
  set.seed(2)
  mod <- matchit(rem~age,data=test_dat2,ratio=1)    # matching by age
  test_inperson <- test_dat2[mod$match.matrix,]
  n_inp <- dim(test_inperson)[1]
  
  test_dat <- rbind(test_rem,test_inperson)
  
  templist <- c(templist,"n_rem","n_inp","test_dat")
  
  if (test %in% c("tap","mpraxis")) {
    t_test1 <- t.test(spe_res~rem,data=test_dat)
    templist <- c(templist,"t_test1")
  }  else if (test == "pcet") {
    t_test1 <- t.test(cat_res~rem,data=test_dat)
    t_test1 <- t.test(acc2_res~rem,data=test_dat)
    t_test2 <- t.test(spe_res~rem,data=test_dat)
    templist <- c(templist,"t_test1")
    templist <- c(templist,"t_test2")
    templist <- c(templist,"t_test3")
  } else if (test == "ds") {
    t_test1 <- t.test(acc_res~rem,data=test_dat)
    t_test2 <- t.test(spe_res~rem,data=test_dat)
    t_test3 <- t.test(macc_res~rem,data=test_dat)
    t_test4 <- t.test(mspe_res~rem,data=test_dat)
    templist <- c(templist,"t_test1")
    templist <- c(templist,"t_test2")
    templist <- c(templist,"t_test3")
    templist <- c(templist,"t_test4")
  } else {
    t_test1 <- t.test(acc_res~rem,data=test_dat)
    t_test2 <- t.test(spe_res~rem,data=test_dat)
    templist <- c(templist,"t_test1")
    templist <- c(templist,"t_test2")
  }
  
  templist <- mget(templist)
  assign(paste(test,"list2",sep="_"),templist)
}


# plots: age-matched samples ----
# loop for making trajectory plots but only with data that is in the age-matched samples

new_texts <- paste0(test_names,"_list2")
new_texts <- setdiff(new_texts,paste0(c("tap","mpraxis","ds","pcet"),"_list2"))   # keep these out of the loop because they have dif cols than the rest of the tests
new_tests <- mget(new_texts)

crossPlots_age <- list()                                         # [cross]sectional [Plots] for [age]-matched samples
ct <- 1
for (i in 1:length(new_tests)) {
  acc_ct <- 2*ct-1
  spe_ct <- 2*ct
  
  test <- new_tests[[i]][[3]] %>% 
    mutate(sex_remote = paste(sex,remote))
  
  # retrieve test name  -- then use Noah's code to get plot and axis titles
  test_name <- names(unlist(new_tests[i]))[1]
  test_split <- str_split(test_name,pattern = "_")[[1]]
  test_prefix <- test_split[1]
  
  Plot_title <- Test_map %>% 
    filter(Prefix == test_prefix) %>% 
    pull(Test_name)
  
  N.df <- test %>% 
    group_by(sex_remote) %>% 
    dplyr::summarize(n = n()) %>% 
    mutate(sex_remote_N = factor(paste0(sex_remote,": ","N = ",n))) %>% 
    arrange(sex_remote_N)
  
  # acc plot
  measure_names <- test[1,grepl("measure",colnames(test))]
  
  ylabel1 <- measure_names[1,1]
  ylabel1 <- str_split(ylabel1,pattern = "_")[[1]][2]
  ylabel1 <- Metric_map %>%
    filter(Suffix == paste0("_",ylabel1)) %>%
    pull(Label)
  
  crossPlots_age[[acc_ct]] <- test %>% 
    left_join(N.df) %>% 
    filter(!is.na(sex_remote_N)) %>% 
    ggplot(aes_string(x = "age",y = names(test)[21],color = "sex_remote_N")) + 
    geom_point(size = .6) + geom_smooth(aes(fill=sex_remote_N),alpha=0.2) + labs(x = "Age",y = ylabel1,title = Plot_title,color = "") +
    scale_x_continuous(limits = c(5,90)) +
    scale_color_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de"),name="") + 
    scale_fill_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de"),name="") + 
    theme(legend.position = "bottom",
          legend.text = element_text(size=5),
          plot.margin = margin(1,1,1,1,unit = "cm"))
  
  
  # spe plot 
  ylabel2 <- measure_names[1,2]
  ylabel2 <- str_split(ylabel2,pattern = "_")[[1]][2]
  ylabel2 <- Metric_map %>%
    filter(Suffix == paste0("_",ylabel2)) %>%
    pull(Label)
  
  crossPlots_age[[spe_ct]] <- test %>%
    left_join(N.df) %>%
    filter(!is.na(sex_remote_N)) %>% 
    ggplot(aes_string(x = "age",y = names(test)[22],color = "sex_remote_N")) + 
    geom_point(size = .6) + geom_smooth(aes(fill=sex_remote_N),alpha=0.2) + labs(x = "Age",y = ylabel2,title = Plot_title,color = "") +
    scale_x_continuous(limits = c(5,90)) +
    scale_color_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de"),name="") + 
    scale_fill_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de"),name="") + 
    theme(legend.position = "bottom",
          legend.text = element_text(size=5),
          plot.margin = margin(1,1,1,1,unit = "cm"))
  
  ct <- ct + 1
}

# * loop but for tap and mpraxis ----
speed_texts <- c("tap_list2","mpraxis_list2")
speed_tests <- mget(speed_texts)

ct <- 25
for (i in 1:length(speed_tests)) {
  test <- speed_tests[[i]][[3]] %>% 
    mutate(sex_remote = paste(sex,remote))
  
  # retrieve test name  -- then use Noah's code to get plot and axis titles
  test_name <- names(unlist(speed_tests[i]))[1]
  test_split <- str_split(test_name,pattern = "_")[[1]]
  test_prefix <- test_split[1]
  
  Plot_title <- Test_map %>% 
    filter(Prefix == test_prefix) %>% 
    pull(Test_name)
  
  N.df <- test %>% 
    group_by(sex_remote) %>% 
    dplyr::summarize(n = n()) %>% 
    mutate(sex_remote_N = factor(paste0(sex_remote,": ","N = ",n))) %>% 
    arrange(sex_remote_N)
  
  ylabel <- test[1,grepl("measure",colnames(test))]
  
  # spe plot 
  ylabel <- str_split(ylabel,pattern = "_")[[1]][2]
  ylabel <- Metric_map %>%
    filter(Suffix == paste0("_",ylabel)) %>%
    pull(Label)
  
  crossPlots_age[[ct]] <- test %>%
    left_join(N.df) %>%
    filter(!is.na(sex_remote_N)) %>% 
    ggplot(aes_string(x = "age",y = names(test)[19],color = "sex_remote_N")) + 
    geom_point(size = .6) + geom_smooth(aes(fill=sex_remote_N),alpha=0.2) + labs(x = "Age",y = ylabel,title = Plot_title,color = "") +
    scale_x_continuous(limits = c(5,90)) +
    scale_color_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de"),name="") + 
    scale_fill_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de"),name="") + 
    theme(legend.position = "bottom",
          legend.text = element_text(size=5),
          plot.margin = margin(1,1,1,1,unit = "cm"))
  
  ct <- ct + 1
}


# * ds and pcet graphs ----

test <- ds_list2[[3]] %>% 
  mutate(sex_remote = paste(sex,remote))

Plot_title <- Test_map %>% 
  filter(Prefix == "ds") %>% 
  pull(Test_name)

N.df <- test %>% 
  group_by(sex_remote) %>% 
  dplyr::summarize(n = n()) %>% 
  mutate(sex_remote_N = factor(paste0(sex_remote,": ","N = ",n))) %>% 
  arrange(sex_remote_N)

# acc plot
measure_names <- test[1,grepl("measure",colnames(test))]

ylabel1 <- measure_names[1,1]
ylabel1 <- str_split(ylabel1,pattern = "_")[[1]][2]
ylabel1 <- Metric_map %>%
  filter(Suffix == paste0("_",ylabel1)) %>%
  pull(Label)

crossPlots_age[[ct]] <- test %>% 
  left_join(N.df) %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes_string(x = "age",y = names(test)[25],color = "sex_remote_N")) + 
  geom_point(size = .6) + geom_smooth(aes(fill=sex_remote_N),alpha=0.2) + labs(x = "Age",y = ylabel1,title = Plot_title,color = "") +
  scale_x_continuous(limits = c(5,90)) +
  scale_color_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de"),name="") + 
  scale_fill_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de"),name="") + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=5),
        plot.margin = margin(1,1,1,1,unit = "cm"))

ct <- ct + 1


# spe plot 
ylabel2 <- measure_names[1,2]
ylabel2 <- str_split(ylabel2,pattern = "_")[[1]][2]
ylabel2 <- Metric_map %>%
  filter(Suffix == paste0("_",ylabel2)) %>%
  pull(Label)

crossPlots_age[[ct]] <- test %>%
  left_join(N.df) %>%
  filter(!is.na(sex)) %>% 
  ggplot(aes_string(x = "age",y = names(test)[26],color = "sex_remote_N")) + 
  geom_point(size = .6) + geom_smooth(aes(fill=sex_remote_N),alpha=0.2) + labs(x = "Age",y = ylabel2,title = Plot_title,color = "") +
  scale_x_continuous(limits = c(5,90)) +
  scale_color_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de"),name="") + 
  scale_fill_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de"),name="") + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=5),
        plot.margin = margin(1,1,1,1,unit = "cm"))

ct <- ct + 1

# mem acc plot
Plot_title <- paste(Plot_title,"(Memory)")

crossPlots_age[[ct]] <- test %>% 
  left_join(N.df) %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes_string(x = "age",y = names(test)[27],color = "sex_remote_N")) + 
  geom_point(size = .6) + geom_smooth(aes(fill=sex_remote_N),alpha=0.2) + labs(x = "Age",y = ylabel1,title = Plot_title,color = "") +
  scale_x_continuous(limits = c(5,90)) +
  scale_color_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de"),name="") + 
  scale_fill_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de"),name="") + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=5),
        plot.margin = margin(1,1,1,1,unit = "cm"))

ct <- ct + 1


# mem spe plot 

crossPlots_age[[ct]] <- test %>%
  left_join(N.df) %>%
  filter(!is.na(sex)) %>% 
  ggplot(aes_string(x = "age",y = names(test)[28],color = "sex_remote_N")) + 
  geom_point(size = .6) + geom_smooth(aes(fill=sex_remote_N),alpha=0.2) + labs(x = "Age",y = ylabel2,title = Plot_title,color = "") +
  scale_x_continuous(limits = c(5,90)) +
  scale_color_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de"),name="") + 
  scale_fill_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de"),name="") + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=5),
        plot.margin = margin(1,1,1,1,unit = "cm"))

ct <- ct + 1


# pcet

test <- pcet_list2[[3]] %>% 
  mutate(sex_remote = paste(sex,remote))

Plot_title <- Test_map %>% 
  filter(Prefix == "pcet") %>% 
  pull(Test_name)

N.df <- test %>% 
  group_by(sex_remote) %>% 
  dplyr::summarize(n = n()) %>% 
  mutate(sex_remote_N = factor(paste0(sex_remote,": ","N = ",n))) %>% 
  arrange(sex_remote_N)

# cat plot
measure_names <- test[1,grepl("measure",colnames(test))]

ylabel1 <- measure_names[1,1]
ylabel1 <- str_split(ylabel1,pattern = "_")[[1]][2]
ylabel1 <- Metric_map %>%
  filter(Suffix == paste0("_",ylabel1)) %>%
  pull(Label)

crossPlots_age[[ct]] <- test %>% 
  left_join(N.df) %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes_string(x = "age",y = names(test)[23],color = "sex_remote_N")) + 
  geom_point(size = .6) + geom_smooth(aes(fill=sex_remote_N),alpha=0.2) + labs(x = "Age",y = ylabel1,title = Plot_title,color = "") +
  scale_x_continuous(limits = c(5,90)) +
  scale_color_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de"),name="") + 
  scale_fill_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de"),name="") + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=5),
        plot.margin = margin(1,1,1,1,unit = "cm"))

ct <- ct + 1


# acc2 plot
ylabel2 <- measure_names[1,2]
ylabel2 <- str_split(ylabel2,pattern = "_")[[1]][2]
ylabel2 <- Metric_map %>%
  filter(Suffix == paste0("_",ylabel2)) %>%
  pull(Label)

crossPlots_age[[ct]] <- test %>% 
  left_join(N.df) %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes_string(x = "age",y = names(test)[24],color = "sex_remote_N")) + 
  geom_point(size = .6) + geom_smooth(aes(fill=sex_remote_N),alpha=0.2) + labs(x = "Age",y = ylabel2,title = Plot_title,color = "") +
  scale_x_continuous(limits = c(5,90)) +
  scale_color_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de"),name="") + 
  scale_fill_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de"),name="") + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=5),
        plot.margin = margin(1,1,1,1,unit = "cm"))

ct <- ct + 1


# spe plot 
ylabel3 <- measure_names[1,3]
ylabel3 <- str_split(ylabel3,pattern = "_")[[1]][2]
ylabel3 <- Metric_map %>%
  filter(Suffix == paste0("_",ylabel3)) %>%
  pull(Label)

crossPlots_age[[ct]] <- test %>%
  left_join(N.df) %>%
  filter(!is.na(sex)) %>% 
  ggplot(aes_string(x = "age",y = names(test)[25],color = "sex_remote_N")) + 
  geom_point(size = .6) + geom_smooth(aes(fill=sex_remote_N),alpha=0.2) + labs(x = "Age",y = ylabel3,title = Plot_title,color = "") +
  scale_x_continuous(limits = c(5,90)) +
  scale_color_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de"),name="") + 
  scale_fill_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de"),name="") + 
  theme(legend.position = "bottom",
        legend.text = element_text(size=5),
        plot.margin = margin(1,1,1,1,unit = "cm"))

ct <- ct + 1


pdf("plots/age_matched/cross_plots10Mar22.pdf")
for (i in 1:length(crossPlots_age)){
  print(crossPlots_age[[i]])
}
dev.off()

# adt -- acc   :)
# cpf -- acc   :)
# cpt -- spe   :)
# ds -- spe
# er40 -- acc, spe   :)
# gng -- acc, spe   :)
# lnb -- speed
# plot -- acc
# pmat -- acc
# pvrt -- acc, spe   :)
# tap -- spe
# volt -- acc



