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

notval <- c("N")
colors <- c("#6E4230", "#BF9660", "#065C50", "#48B0A2")

datbbl <- dat[!is.na(dat$bblid),]


# * separating out into tasks ----

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

# using Noah's code for my data

dat2 <- datbbl %>% 
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
  bind_rows(Last_tests) %>%    # attaching all data from last_tests at the end
  filter(!is.na(bblid))

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






Pall_rec <- list()                                         # [L]ongitudinal [P]lots for [all] and [rec]ent dates
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
  
  N.df <- cnb_cross[,c(test,"remote","age")] %>% 
    filter(if_all(everything(), ~ !is.na(.))) %>% 
    group_by(remote) %>% 
    dplyr::summarize(n = n()) %>% 
    mutate(remote_N = factor(paste0(remote,": ","N = ",n))) %>% 
    arrange(remote_N)
  

  Pall_rec[[allcnt]] <- cnb_cross %>% 
    left_join(N.df) %>% 
    filter(!is.na(remote_N)) %>% 
    ggplot(aes_string(x = "age",y = test,color = "remote_N")) + 
    geom_point(size = .6) + geom_smooth(se = FALSE) + labs(x = "Age",y = ylabel,title = Plot_title,color = "") +
    scale_x_continuous(limits = c(5,90)) +
    scale_color_manual(values = c("#EB6746","#4ED3ED")) + 
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.text = element_text(size=5),
          plot.margin = margin(1,1,1,1,unit = "cm"))
  
  
  # recent data (2016 and on)
  
  pt2 <- Test_map %>%                       # [p]lot [t]itle 2
    filter(Prefix == test_prefix) %>% 
    pull(Test_name)
  
  pt2 <- ifelse(test %in% c("ds_memcr", "ds_mcrrt"),paste(pt2, "(Memory, 2016 and on)"),paste(pt2,"(2016 and on)"))
  
  ylabel2 <- Metric_map %>% 
    filter(Suffix == test_suffix) %>% 
    pull(Label)
  
  N.df <- cnb_cross[,c(test,"remote","age","dotest")] %>% 
    filter(if_all(everything(), ~ !is.na(.))) %>% 
    filter(dotest > as.Date("2015-12-31")) %>% 
    group_by(remote) %>% 
    dplyr::summarize(n = n()) %>% 
    mutate(remote_N = factor(paste0(remote,": ","N = ",n))) %>% 
    arrange(remote_N)
  

  Pall_rec[[reccnt]] <- cnb_cross %>% 
    left_join(N.df) %>% 
    filter(!is.na(remote_N)) %>% 
    filter(dotest > as.Date("2015-12-31")) %>% 
    ggplot(aes_string(x = "age",y = test,color = "remote_N")) + 
    geom_point(size = .6) + geom_smooth(se = FALSE) + labs(x = "Age",y = ylabel,title = pt2,color = "") + 
    scale_x_continuous(limits = c(5,90)) +
    scale_color_manual(values = c("#EB6746","#4ED3ED")) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.text = element_text(size=5),
          plot.margin = margin(1,1,1,1,unit = "cm"))
  
  cntr <- cntr + 1
}

pdf("plots/fromNoahscode/cross_plots_onlyBBL_4April22.pdf",height = 7,width = 10)
for (i in 1:length(Pall_rec)){
  print(Pall_rec[[i]])
}
dev.off()
























# (4) Statistical Analysis: regress out age and look for significant differences across platforms ----

# age-matching code from Tyler ----
# i have to do some age-matching per test

demos <- cnb_cross %>% 
  dplyr::select(bblid:remote)

test_names <- sort(unique(sapply(str_split(response_vars,pattern = "_"),"[[",1)))

t_tests <- list()
cntr <- 1
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
  t_tests[[cntr]] <- templist
  cntr <- cntr+1
}













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














