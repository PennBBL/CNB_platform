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
library(TOSTER)
library(mgcv)
library(matrixStats)
library(kableExtra)


# (1) Load and organize data ----
# dat <- read_csv("cnb_merged_20220107.csv")
x <- read_csv("data/cnb_merged_20220201.csv")    # digsym, GNG, AIM, + itemwise (iw) DDISC, iw RDISC, iw EDISC
x2 <- read_csv("data/cnb_merged_webcnp_surveys_allbblprjcts_longform20220406.csv")
x2 <- x2 %>% dplyr::select(!(names(x2)[grep('^KRDISC.*\\.1$',colnames(x2))]))
dat <- x2
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

# get rid of BBLIDs < 5 digits and > 6 digits
dat <- dat[-which(dat$bblid < 10000),]
dat <- dat[-which(dat$bblid > 1000000),]

# exclude the 103 year old for now
dat <- dat[-which(dat$age >100),]

notval <- c("N")
colors <- c("#6E4230", "#BF9660", "#065C50", "#48B0A2")

datbbl <- dat[!is.na(dat$bblid),]
dat35 <- datbbl[which(datbbl$age<36),]      # 14611 x 524


# DISC task sum scores
ddisc_qs <- dat35 %>% dplyr::select((names(dat35)[grep('DDISC.q',colnames(dat35))]))
dat35$ddisc_sum <- rowSums(ddisc_qs-1)
ddisc_ttrs <- dat35 %>% dplyr::select((names(dat35)[grep('DDISC.trr',colnames(dat35))]))
dat35$ddisc_mcr <- rowMedians(as.matrix(ddisc_ttrs))

rdisc_qs <- dat35 %>% dplyr::select((names(dat35)[grep('RDISC.q',colnames(dat35))]))
dat35$rdisc_sum <- rowSums(rdisc_qs-1)
rdisc_ttrs <- dat35 %>% dplyr::select((names(dat35)[grep('RDISC.trr',colnames(dat35))]))
dat35$rdisc_mcr <- rowMedians(as.matrix(rdisc_ttrs))

edisc_qs <- dat35 %>% dplyr::select((names(dat35)[grep('EDISC',colnames(dat35))])) %>% 
  dplyr::select((names(dat35)[grep('_resp',colnames(dat35))])) %>% 
  dplyr::select(EDISC.q_101_resp:EDISC.q_134_resp)
dat35$edisc_sum <- rowSums(edisc_qs-1)
edisc_ttrs <- dat35 %>% dplyr::select((names(dat35)[grep('EDISC',colnames(dat35))])) %>% 
  dplyr::select((names(dat35)[grep('_ttr',colnames(dat35))])) %>% 
  dplyr::select(EDISC.q_101_ttr:EDISC.q_134_ttr)
dat35$edisc_mcr <- rowMedians(as.matrix(edisc_ttrs))



# using Noah's code for my data, including DISC data using sum scores for now (4.11.22)

dat2 <- dat35 %>% 
  dplyr::select(bblid,test_sessions.datasetid,test_sessions.siteid, 
                test_sessions.famid, test_sessions.subid, age, test_sessions_v.battery, 
                dob, dotest, test_sessions_v.education, test_sessions_v.feducation,
                sex, test_sessions_v.handedness, test_sessions_v.meducation, platform, remote,
                adt_valid, adt_pc, adt_rtcr, 
                aim_valid, aim_tot, aim_totrt,
                cpf_valid, cpf_cr, cpf_w_rtcr, 
                cpt_valid, cpt_ptp, cpt_tprt,
                cpw_valid, cpw_cr, cpw_w_rtcr,
                ddisc_sum, ddisc_mcr,
                digsym_valid, ds_cor, ds_corrt, ds_memcr, ds_mcrrt,
                edisc_sum, edisc_mcr,
                er40_valid, er40_cr, er40_rtcr, 
                gng_valid, gng_cr, gng_rtcr,
                lnb_valid, lnb_mcr, lnb_mrtc,
                medf_valid, medf_pc, medf_rtcr, 
                mpraxis_valid, mpraxis_rtcr, 
                pcet_valid, pcet_cat, pcet_acc2, pcet_rtcr, 
                plot_valid, plot_pc, plot_rtcr,
                pmat_valid, pmat_pc, pmat_rtcr,
                pvrt_valid, pvrt_pc, pvrt_rtcr,
                rdisc_sum, rdisc_mcr,
                tap_valid, tap_tot, tap_valid, 
                volt_valid, volt_cr, volt_w_rtcr) %>%
  mutate(sex = ifelse(sex == "F","Female","Male")) %>% 
  mutate(remote = ifelse(platform == "webcnp","In-person","Remote")) %>% 
  mutate(sex_remote = paste(sex,remote))      # 14611 x 75 (including DISC data), 4.13.22

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

Last_tests <- map_dfr(repeats_list,find_last_test) # 1506 x 69, 4/7/22, x 75 4.13.22 (including DISC)

cnb_cross <- dat2 %>%    # combining all info from bblid's that only have one row of data
  group_by(bblid) %>% 
  filter(n() == 1) %>%
  arrange(dotest) %>% 
  ungroup() %>% 
  bind_rows(Last_tests) %>%    # attaching all data from last_tests at the end
  filter(!is.na(bblid)) %>% 
  mutate(age_bin = ntile(age, n=3))

demos <- cnb_cross %>% 
  dplyr::select(bblid:remote, age_bin)

# get rid of outliers

tests <- cnb_cross %>%   # bblid, age, dob, dotest, sex, _valid
  dplyr::select(!(matches("bblid") | matches("^test") | "age" | "dob" | "dotest" | "sex" | "platform" | "remote" | matches("_valid$") | matches("^aim") | "sex_remote" | "age_bin")) %>%    # ^[this]: anything beginning with [this]; [this]$: anything ending with [this]
  colnames()
for(test in tests){
  cnb_cross[[test]] <- ifelse(cnb_cross[[test]] > mean(cnb_cross[[test]],na.rm = TRUE) + 6*sd(cnb_cross[[test]],na.rm = TRUE),mean(cnb_cross[[test]],na.rm = TRUE) + 6*sd(cnb_cross[[test]],na.rm = TRUE),cnb_cross[[test]])
  cnb_cross[[test]] <- ifelse(cnb_cross[[test]] < mean(cnb_cross[[test]],na.rm = TRUE) - 6*sd(cnb_cross[[test]],na.rm = TRUE),mean(cnb_cross[[test]],na.rm = TRUE) - 6*sd(cnb_cross[[test]],na.rm = TRUE),cnb_cross[[test]])
}

response_vars <- cnb_cross %>% 
  dplyr::select(!(contains("valid"))) %>% 
  dplyr::select(adt_pc:volt_w_rtcr) %>% 
  colnames()

# exclude DISC tasks for now
# response_vars <- setdiff(response_vars,c("aim_tot", "aim_totrt"))

Test_map <- data.frame('Prefix' = c("er40","pvrt","cpf","cpw","gng","mpraxis","pcet","pmat","medf","adt","plot","tap","cpt","lnb","volt","plot","ds","aim","ddisc","edisc","rdisc"),
                       "Test_name" = c("Penn Emotion Recognition Test","Penn Verbal Reasoning Test","Penn Face Memory Test","Penn Word Memory Test",
                                       "Go-No-Go Test","Motor Praxis Test","Penn Conditional Exclusion Test","Penn Matrix Analysis Test","Measured Emotion Differentiation Test",
                                       "Age Differentiation Test","Penn Line Orientation Test","Penn Computerized Finger Tapping Test","Penn Continuous Performance Test",
                                       "Letter-N-Back","Visual Object Learning Test","Penn Line Orientation Test","Digit Symbol Search","Abstraction, Inhibition, Working Memory Test",
                                       "Delay Discounting Task","Effort Discounting Task","Risk Discounting Task"))

Metric_map <- data.frame("Suffix" = c("_cor","_cr","_corrt","_rtcr","_tot","_acc2","_tprt","_ptp","_mcr","_mrtc","_pc","_mp2rtcr","_cat","_mrtc","_tp","_memcr","_mcrrt","_sum","_totrt"),
                         "Label" = c("Correct Responses","Correct Responses","Median Response Time \n Correct Responses (ms)","Median Response Time \n Correct Responses (ms)","Average Taps \n (Dominant and Non-dominant hand added together)",
                                     "Accuracy","Median Response Time \n True Positives (ms)","True Positive (%)","Total True Positive Responses","Median Response Time \n Correct Responses",
                                     "Correct Responses (%)","Median Response Time \n Correct Responses (ms)","Categories Achieved","Median Response Time \n Correct Responses (ms)",
                                     "True Positive Responses","Correct Responses","Median Response Time \n Correct Responses (ms)", "Sum Score", "Median Response Time \n Correct Responses (ms)"))








# (2) Plots comparing sex and platform with age on x-axis (noting n's) ----


# run this after running t-test code

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
    pull(Label) %>% 
    paste0(", Sex-regressed Residuals")
  
  test_dat <- cbind(demos, cnb_cross[,test])
  last_col <- tail(colnames(test_dat),1)
  test_dat <- test_dat %>%
    drop_na(last_col) %>% 
    drop_na(age)
  
  # regressing sex out 
  fit <- glm(test_dat[,ncol(test_dat)] ~ sex, data = test_dat)
  test_dat$res <- scale(resid(fit))
  test_dat$res <- ifelse(test_dat$res>6,6,test_dat$res)
  test_dat$res <- ifelse(test_dat$res<(-6),-6,test_dat$res)
  
  test_dat_keep <- test_dat
  
  # age-match
  test_rem <- test_dat %>% 
    filter(remote == "Remote") %>% 
    mutate(rem = 1)
  n_rem <- dim(test_rem)[1]
  
  test_inp <- test_dat %>% 
    filter(remote == "In-person") %>% 
    mutate(rem = 0)
  
  test_dat2 <- rbind(test_rem,test_inp)       # test_dat recombined after adding var, rem = c(0,1)
  
  set.seed(2)
  mod <- matchit(rem~age+sex,data=test_dat2,ratio=1)    # matching by age and sex for now, is there a reason to match by sex if sex is already regressed out?
  test_inperson <- test_dat2[mod$match.matrix,]
  n_inp <- dim(test_inperson)[1]
  
  test_dat <- rbind(test_rem,test_inperson)
  
  N.df <- test_dat %>%    
    # filter(if_all(everything(), ~ !is.na(.))) %>% 
    group_by(remote) %>% 
    dplyr::summarize(n = n()) %>% 
    mutate(remote_N = factor(paste0(remote,": ","N = ",n))) %>% 
    arrange(remote_N)

  Pall_rec[[allcnt]] <- test_dat %>% 
    left_join(N.df) %>% 
    filter(!is.na(remote_N)) %>% 
    ggplot(aes_string(x = "age",y = "res",color = "remote_N")) + 
    geom_point(size = .6) + geom_smooth(aes(fill=remote_N),method="gam",alpha=0.2) + labs(x = "Age",y = ylabel,title = Plot_title,color = "") + # add method = "gam"
    geom_vline(xintercept=c(12,17), linetype="dashed",color="#545454") +
    scale_x_continuous(limits = c(5,40)) +
    scale_color_manual(values = c("#EB6746","#4ED3ED"),name="") + 
    scale_fill_manual(values = c("#EB6746","#4ED3ED"),name="") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.text = element_text(size=5),
          plot.margin = margin(1,1,1,1,unit = "cm"))
  
  # fit <- gam(test ~ s(age,k=3) + sex, data = test_dat) # possibly by = "gender"
  # visreg(fit,"age", by="gender", main=paste(textsAcc[i],"LY"))
  # see if gam provides p-value for interaction, if not, use linear models with squared and cubed terms 
  
  # recent data (2016 and on)
  test_dat <- test_dat_keep %>% 
    filter(dotest > as.Date("2015-12-31"))
  
  # age-match
  test_rem <- test_dat %>% 
    filter(remote == "Remote") %>% 
    mutate(rem = 1)
  n_rem <- dim(test_rem)[1]
  
  test_inp <- test_dat %>% 
    filter(remote == "In-person") %>% 
    mutate(rem = 0)
  
  test_dat2 <- rbind(test_rem,test_inp)       # test_dat recombined after adding var, rem = c(0,1)
  
  set.seed(2)
  mod <- matchit(rem~age+sex,data=test_dat2,ratio=1)    # matching by age and sex for now, is there a reason to match by sex if sex is already regressed out?
  test_inperson <- test_dat2[mod$match.matrix,]
  n_inp <- dim(test_inperson)[1]
  
  test_dat <- rbind(test_rem,test_inperson)
  
  pt2 <- Test_map %>%                       # [p]lot [t]itle 2
    filter(Prefix == test_prefix) %>% 
    pull(Test_name)
  
  pt2 <- ifelse(test %in% c("ds_memcr", "ds_mcrrt"),paste(pt2, "(Memory, 2016 and on)"),paste(pt2,"(2016 and on)"))
  
  ylabel2 <- Metric_map %>% 
    filter(Suffix == test_suffix) %>% 
    pull(Label) %>% 
    paste0(", Sex-regressed Residuals")
  
  N.df <- test_dat %>% 
    filter(!is.na(bblid)) %>% 
    group_by(remote) %>% 
    dplyr::summarize(n = n()) %>% 
    mutate(remote_N = factor(paste0(remote,": ","N = ",n))) %>% 
    arrange(remote_N)
  
  Pall_rec[[reccnt]] <- test_dat %>% 
    left_join(N.df) %>% 
    filter(!is.na(remote_N)) %>% 
    ggplot(aes_string(x = "age",y = "res",color = "remote_N")) + 
    geom_point(size = .6) + geom_smooth(aes(fill=remote_N),method="gam",alpha=0.2) + labs(x = "Age",y = ylabel,title = pt2,color = "") + 
    geom_vline(xintercept=c(12,17), linetype="dashed",color="#545454") +
    scale_x_continuous(limits = c(5,40)) +
    scale_color_manual(values = c("#EB6746","#4ED3ED"),name="") +
    scale_fill_manual(values = c("#EB6746","#4ED3ED"),name="") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.text = element_text(size=5),
          plot.margin = margin(1,1,1,1,unit = "cm"))
  
  cntr <- cntr + 1
}

pdf("plots/cross_plots_3May22.pdf",height = 7,width = 10)
for (i in 1:length(Pall_rec)){
  print(Pall_rec[[i]])
}
dev.off()






# (3) Statistical Analysis: regress out age and look for significant differences across platforms ----

# * t-tests ----
# i have to do some age-matching per test

demos <- cnb_cross %>% 
  dplyr::select(bblid:remote,age_bin)

test_names <- sort(unique(sapply(str_split(response_vars,pattern = "_"),"[[",1)))

t_tests <- list()
cntr <- 1
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
  
  # regressing sex out 
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
  
  test_dat_res <- test_dat                    # test_dat [res]erve, keeping this version so i can use it in other loops
  templist <- c(templist,"test_dat_res")    
  
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
  
  templist <- c(templist,"n_rem","n_inp")
  
  notest <- Test_map %>% 
    filter(Prefix == test) %>% 
    pull(Test_name)
  
  if (test %in% c("tap","mpraxis")) {
    t_test1 <- t.test(spe_res~rem,data=test_dat)
    
    AB_p <- data.frame("age_7_12" = rep(NA,1),"age_12_17" = rep(NA,1),"age_17_35" = rep(NA,1))
    rownames(AB_p) <- "spe"
    
    AB_p[1,1] <- ifelse(dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==1),])$p.value,5),NA)
    AB_p[1,2] <- ifelse(dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 0),])[1] > 5,
                        round(round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==2),])$p.value,5),5),NA)
    AB_p[1,3] <- ifelse(dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 1),])[1] > 5 &
                       dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 0),])[1] > 5,
                     round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==3),])$p.value,5),NA)
    
    tAB_p <- AB_p %>%
      kbl(caption = paste0("Age-binned T-Tests for ", notest), align = rep("c", 8),
          col.names = c("7-12","12-17","17-35")) %>%
      kable_classic(full_width = F, html_font = "Cambria")
    
    templist <- c(templist,"t_test1","tAB_p")
  }  else if (test == "pcet") {
    t_test1 <- t.test(cat_res~rem,data=test_dat)
    t_test1 <- t.test(acc2_res~rem,data=test_dat)
    t_test2 <- t.test(spe_res~rem,data=test_dat)
    
    AB_p <- data.frame("age_7_12" = rep(NA,3),"age_12_17" = rep(NA,3),"age_17_35" = rep(NA,3))
    rownames(AB_p) <- c("cat","acc2","spe")
    
    AB_p[1,1] <- ifelse(dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 1),])[1] > 5 &
                           dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 0),])[1] > 5,
                         round(t.test(cat_res~rem,data=test_dat[which(test_dat$age_bin==1),])$p.value,5),NA)
    AB_p[1,2] <- ifelse(dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 1),])[1] > 5 &
                           dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 0),])[1] > 5,
                         round(t.test(cat_res~rem,data=test_dat[which(test_dat$age_bin==2),])$p.value,5),NA)
    AB_p[1,3] <- ifelse(dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 1),])[1] > 5 &
                           dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 0),])[1] > 5,
                         round(t.test(cat_res~rem,data=test_dat[which(test_dat$age_bin==3),])$p.value,5),NA)
    
    AB_p[2,1] <- ifelse(dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(acc2_res~rem,data=test_dat[which(test_dat$age_bin==1),])$p.value,5),NA)
    AB_p[2,2] <- ifelse(dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(acc2_res~rem,data=test_dat[which(test_dat$age_bin==2),])$p.value,5),NA)
    AB_p[2,3] <- ifelse(dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(acc2_res~rem,data=test_dat[which(test_dat$age_bin==3),])$p.value,5),NA)
    
    AB_p[3,1] <- ifelse(dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==1),])$p.value,5),NA)
    AB_p[3,2] <- ifelse(dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==2),])$p.value,5),NA)
    AB_p[3,3] <- ifelse(dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 1),])[1] > 5 &
                       dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 0),])[1] > 5,
                     round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==3),])$p.value,5),NA)
    
    tAB_p <- AB_p %>%
      kbl(caption = paste0("Age-binned T-Tests for ", notest), align = rep("c", 8),
          col.names = c("7-12","12-17","17-35")) %>%
      kable_classic(full_width = F, html_font = "Cambria")
    
    templist <- c(templist,"t_test1","t_test2","t_test3","tAB_p")
  } else if (test == "ds") {
    t_test1 <- t.test(acc_res~rem,data=test_dat)
    t_test2 <- t.test(spe_res~rem,data=test_dat)
    t_test3 <- t.test(macc_res~rem,data=test_dat)
    t_test4 <- t.test(mspe_res~rem,data=test_dat)
    
    AB_p <- data.frame("age_7_12" = rep(NA,4),"age_12_17" = rep(NA,4),"age_17_35" = rep(NA,4))
    rownames(AB_p) <- c("acc","spe","macc","mspe")
    
    AB_p[1,1] <- ifelse(dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 1),])[1] > 5 &
                       dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 0),])[1] > 5,
                     round(t.test(acc_res~rem,data=test_dat[which(test_dat$age_bin==1),])$p.value,5),NA)
    AB_p[1,2] <- ifelse(dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 1),])[1] > 5 &
                       dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 0),])[1] > 5,
                     round(t.test(acc_res~rem,data=test_dat[which(test_dat$age_bin==2),])$p.value,5),NA)
    AB_p[1,3] <- ifelse(dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 1),])[1] > 5 &
                       dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 0),])[1] > 5,
                     round(t.test(acc_res~rem,data=test_dat[which(test_dat$age_bin==3),])$p.value,5),NA)
    
    AB_p[2,1] <- ifelse(dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 1),])[1] > 5 &
                       dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 0),])[1] > 5,
                     round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==1),])$p.value,5),NA)
    AB_p[2,2] <- ifelse(dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 1),])[1] > 5 &
                       dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 0),])[1] > 5,
                     round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==2),])$p.value,5),NA)
    AB_p[2,3] <- ifelse(dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 1),])[1] > 5 &
                       dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 0),])[1] > 5,
                     round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==3),])$p.value,5),NA)
    
    AB_p[3,1] <- ifelse(dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 1),])[1] > 5 &
                         dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 0),])[1] > 5,
                       round(t.test(macc_res~rem,data=test_dat[which(test_dat$age_bin==1),])$p.value,5),NA)
    AB_p[3,2] <- ifelse(dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 1),])[1] > 5 &
                         dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 0),])[1] > 5,
                       round(t.test(macc_res~rem,data=test_dat[which(test_dat$age_bin==2),])$p.value,5),NA)
    AB_p[3,3] <- ifelse(dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 1),])[1] > 5 &
                       dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 0),])[1] > 5,
                     round(t.test(macc_res~rem,data=test_dat[which(test_dat$age_bin==3),])$p.value,5),NA)
    
    AB_p[4,1] <- ifelse(dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 1),])[1] > 5 &
                        dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 0),])[1] > 5,
                      round(t.test(mspe_res~rem,data=test_dat[which(test_dat$age_bin==1),])$p.value,5),NA)
    AB_p[4,2] <- ifelse(dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 1),])[1] > 5 &
                        dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 0),])[1] > 5,
                      round(t.test(mspe_res~rem,data=test_dat[which(test_dat$age_bin==2),])$p.value,5),NA)
    AB_p[4,3] <- ifelse(dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 1),])[1] > 5 &
                       dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 0),])[1] > 5,
                     round(t.test(mspe_res~rem,data=test_dat[which(test_dat$age_bin==3),])$p.value,5),NA)
    
    tAB_p <- AB_p %>%
      kbl(caption = paste0("Age-binned T-Tests for ", notest), align = rep("c", 8),
          col.names = c("7-12","12-17","17-35")) %>%
      kable_classic(full_width = F, html_font = "Cambria")
    
    templist <- c(templist,"t_test1","t_test2","t_test3","t_test4","tAB_p")
  } else {
    t_test1 <- t.test(acc_res~rem,data=test_dat)
    t_test2 <- t.test(spe_res~rem,data=test_dat)
    AB_p <- data.frame("age_7_12" = rep(NA,2),"age_12_17" = rep(NA,2),"age_17_35" = rep(NA,2))
    rownames(AB_p) <- c("acc","spe")
    AB_p[1,1] <- ifelse(dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 1),])[1] > 5 &
                     dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 0),])[1] > 5,
                   round(t.test(acc_res~rem,data=test_dat[which(test_dat$age_bin==1),])$p.value,5),NA)
    AB_p[1,2] <- ifelse(dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 1),])[1] > 5 &
                       dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 0),])[1] > 5,
                     round(t.test(acc_res~rem,data=test_dat[which(test_dat$age_bin==2),])$p.value,5),NA)
    AB_p[1,3] <- ifelse(dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 1),])[1] > 5 &
                       dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 0),])[1] > 5,
                     round(t.test(acc_res~rem,data=test_dat[which(test_dat$age_bin==3),])$p.value,5),NA)
    
    AB_p[2,1] <- ifelse(dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 1),])[1] > 5 &
                    dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 0),])[1] > 5,
                  round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==1),])$p.value,5),NA)
    AB_p[2,2] <- ifelse(dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 1),])[1] > 5 &
                    dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 0),])[1] > 5,
                  round(round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==2),])$p.value,5),5),NA)
    AB_p[2,3] <- ifelse(dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 1),])[1] > 5 &
                       dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 0),])[1] > 5,
                     round(round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==3),])$p.value,5),5),NA)
    
    tAB_p <- AB_p %>%
      kbl(caption = paste0("Age-binned T-Tests for ", notest), align = rep("c", 8),
          col.names = c("7-12","12-17","17-35")) %>%
      kable_classic(full_width = F, html_font = "Cambria")
    
    templist <- c(templist,"t_test1","t_test2","tAB_p")
  }
  
  templist <- mget(templist)
  assign(paste(test,"list",sep="_"),templist)
  t_tests[[cntr]] <- templist
  cntr <- cntr+1
}





# * t-tests for more recent data only (2016 ~) ----
t_tests_rec <- list()
cntr <- 1
for (test in test_names) {
  templist <- c()
  
  test_list <- paste(test,"list",sep="_")
  test_list <- mget(test_list)[[1]]
  test_dat <- test_list$test_dat_res %>% 
    filter(dotest > as.Date("2015-12-31"))
  
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
  
  test_dat <- rbind(test_rem,test_inperson) %>% 
    filter(!is.na(rem))
  
  templist <- c(templist,"n_rem","n_inp","test_dat")
  
  notest <- Test_map %>% 
    filter(Prefix == test) %>% 
    pull(Test_name)
  
  if (test %in% c("tap","mpraxis")) {
    t_test1 <- t.test(spe_res~rem,data=test_dat)
    
    AB_p <- data.frame("age_7_12" = rep(NA,1),"age_12_17" = rep(NA,1),"age_17_35" = rep(NA,1))
    rownames(AB_p) <- "spe"
    
    AB_p[1,1] <- ifelse(dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==1),])$p.value,5),NA)
    AB_p[1,2] <- ifelse(dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 0),])[1] > 5,
                        round(round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==2),])$p.value,5),5),NA)
    AB_p[1,3] <- ifelse(dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==3),])$p.value,5),NA)
    
    tAB_p <- AB_p %>%
      kbl(caption = paste0("Age-binned T-Tests for ", notest, " (2016 and on)"), align = rep("c", 8),
          col.names = c("7-12","12-17","17-35")) %>%
      kable_classic(full_width = F, html_font = "Cambria")
    
    templist <- c(templist,"t_test1","tAB_p")
  }  else if (test == "pcet") {
    t_test1 <- t.test(cat_res~rem,data=test_dat)
    t_test1 <- t.test(acc2_res~rem,data=test_dat)
    t_test2 <- t.test(spe_res~rem,data=test_dat)
    
    AB_p <- data.frame("age_7_12" = rep(NA,3),"age_12_17" = rep(NA,3),"age_17_35" = rep(NA,3))
    rownames(AB_p) <- c("cat","acc2","spe")
    
    AB_p[1,1] <- ifelse(dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(cat_res~rem,data=test_dat[which(test_dat$age_bin==1),])$p.value,5),NA)
    AB_p[1,2] <- ifelse(dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(cat_res~rem,data=test_dat[which(test_dat$age_bin==2),])$p.value,5),NA)
    AB_p[1,3] <- ifelse(dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(cat_res~rem,data=test_dat[which(test_dat$age_bin==3),])$p.value,5),NA)
    
    AB_p[2,1] <- ifelse(dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(acc2_res~rem,data=test_dat[which(test_dat$age_bin==1),])$p.value,5),NA)
    AB_p[2,2] <- ifelse(dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(acc2_res~rem,data=test_dat[which(test_dat$age_bin==2),])$p.value,5),NA)
    AB_p[2,3] <- ifelse(dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(acc2_res~rem,data=test_dat[which(test_dat$age_bin==3),])$p.value,5),NA)
    
    AB_p[3,1] <- ifelse(dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==1),])$p.value,5),NA)
    AB_p[3,2] <- ifelse(dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==2),])$p.value,5),NA)
    AB_p[3,3] <- ifelse(dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==3),])$p.value,5),NA)
    
    tAB_p <- AB_p %>%
      kbl(caption = paste0("Age-binned T-Tests for ", notest, " (2016 and on)"), align = rep("c", 8),
          col.names = c("7-12","12-17","17-35")) %>%
      kable_classic(full_width = F, html_font = "Cambria")
    
    templist <- c(templist,"t_test1","t_test2","t_test3","tAB_p")
  } else if (test == "ds") {
    t_test1 <- t.test(acc_res~rem,data=test_dat)
    t_test2 <- t.test(spe_res~rem,data=test_dat)
    t_test3 <- t.test(macc_res~rem,data=test_dat)
    t_test4 <- t.test(mspe_res~rem,data=test_dat)
    
    AB_p <- data.frame("age_7_12" = rep(NA,4),"age_12_17" = rep(NA,4),"age_17_35" = rep(NA,4))
    rownames(AB_p) <- c("acc","spe","macc","mspe")
    
    AB_p[1,1] <- ifelse(dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(acc_res~rem,data=test_dat[which(test_dat$age_bin==1),])$p.value,5),NA)
    AB_p[1,2] <- ifelse(dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(acc_res~rem,data=test_dat[which(test_dat$age_bin==2),])$p.value,5),NA)
    AB_p[1,3] <- ifelse(dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(acc_res~rem,data=test_dat[which(test_dat$age_bin==3),])$p.value,5),NA)
    
    AB_p[2,1] <- ifelse(dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==1),])$p.value,5),NA)
    AB_p[2,2] <- ifelse(dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==2),])$p.value,5),NA)
    AB_p[2,3] <- ifelse(dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==3),])$p.value,5),NA)
    
    AB_p[3,1] <- ifelse(dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(macc_res~rem,data=test_dat[which(test_dat$age_bin==1),])$p.value,5),NA)
    AB_p[3,2] <- ifelse(dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(macc_res~rem,data=test_dat[which(test_dat$age_bin==2),])$p.value,5),NA)
    AB_p[3,3] <- ifelse(dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(macc_res~rem,data=test_dat[which(test_dat$age_bin==3),])$p.value,5),NA)
    
    AB_p[4,1] <- ifelse(dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(mspe_res~rem,data=test_dat[which(test_dat$age_bin==1),])$p.value,5),NA)
    AB_p[4,2] <- ifelse(dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(mspe_res~rem,data=test_dat[which(test_dat$age_bin==2),])$p.value,5),NA)
    AB_p[4,3] <- ifelse(dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(mspe_res~rem,data=test_dat[which(test_dat$age_bin==3),])$p.value,5),NA)
    
    tAB_p <- AB_p %>%
      kbl(caption = paste0("Age-binned T-Tests for ", notest, " (2016 and on)"), align = rep("c", 8),
          col.names = c("7-12","12-17","17-35")) %>%
      kable_classic(full_width = F, html_font = "Cambria")
    
    templist <- c(templist,"t_test1","t_test2","t_test3","t_test4","tAB_p")
  } else {
    t_test1 <- t.test(acc_res~rem,data=test_dat)
    t_test2 <- t.test(spe_res~rem,data=test_dat)
    AB_p <- data.frame("age_7_12" = rep(NA,2),"age_12_17" = rep(NA,2),"age_17_35" = rep(NA,2))
    rownames(AB_p) <- c("acc","spe")
    AB_p[1,1] <- ifelse(dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(acc_res~rem,data=test_dat[which(test_dat$age_bin==1),])$p.value,5),NA)
    AB_p[1,2] <- ifelse(dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(acc_res~rem,data=test_dat[which(test_dat$age_bin==2),])$p.value,5),NA)
    AB_p[1,3] <- ifelse(dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(acc_res~rem,data=test_dat[which(test_dat$age_bin==3),])$p.value,5),NA)
    
    AB_p[2,1] <- ifelse(dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==1 & test_dat$rem == 0),])[1] > 5,
                        round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==1),])$p.value,5),NA)
    AB_p[2,2] <- ifelse(dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==2 & test_dat$rem == 0),])[1] > 5,
                        round(round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==2),])$p.value,5),5),NA)
    AB_p[2,3] <- ifelse(dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 1),])[1] > 5 &
                          dim(test_dat[which(test_dat$age_bin==3 & test_dat$rem == 0),])[1] > 5,
                        round(round(t.test(spe_res~rem,data=test_dat[which(test_dat$age_bin==3),])$p.value,5),5),NA)
    
    tAB_p <- AB_p %>%
      kbl(caption = paste0("Age-binned T-Tests for ", notest, " (2016 and on)"), align = rep("c", 8),
          col.names = c("7-12","12-17","17-35")) %>%
      kable_classic(full_width = F, html_font = "Cambria")
    
    templist <- c(templist,"t_test1","t_test2","tAB_p")
  }
  
  templist <- mget(templist)
  assign(paste(test,"list_rec",sep="_"),templist)
  t_tests_rec[[cntr]] <- templist
  cntr <- cntr+1
}




# * equivalence testing ----

e_tests <- list()
cntr <- 1
for (test in test_names) {
  templist <- c()
  
  test_list <- paste(test,"list",sep="_")
  test_list <- mget(test_list)[[1]]
  test_dat <- test_list$test_dat_res

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
  
  notest <- Test_map %>% 
    filter(Prefix == test) %>% 
    pull(Test_name)
  
  if (test %in% c("tap","mpraxis")) {
    m1 <- mean(test_dat %>% filter(rem==0) %>% pull(spe_res))
    m2 <- mean(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    sd1 <- sd(test_dat %>% filter(rem==0) %>% pull(spe_res))
    sd2 <- sd(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    n1 <- length(test_dat %>% filter(rem==0) %>% pull(spe_res))
    n2 <- length(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    
    e_test1 <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
    
    AB_p <- data.frame("age_7_12" = rep(NA,1),"age_12_17" = rep(NA,1),"age_17_35" = rep(NA,1))
    rownames(AB_p) <- "spe"
    
    for (num in 1:3){
      if (dim(temp_dat[which(temp_dat$rem == 1),])[1] > 5 &
          dim(temp_dat[which(temp_dat$rem == 0),])[1] > 5) {
        temp_dat <- test_dat %>% filter(age_bin == num)
        m1 <- mean(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        m2 <- mean(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        sd1 <- sd(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        sd2 <- sd(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        n1 <- length(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        n2 <- length(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        etest <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
        eq_p <- round(max(etest$TOST$p.value[-1]),5)
        
        AB_p[1,num] <- eq_p
      } else {
        AB_p[1,num] <- NA
      }
      
    }
    
    tAB_p <- AB_p %>%
      kbl(caption = paste0("Age-binned Equivalence Tests for ", notest), align = rep("c", 8),
          col.names = c("7-12","12-17","17-35")) %>%
      kable_classic(full_width = F, html_font = "Cambria")
    
    templist <- c(templist,"e_test1","tAB_p")
  }  else if (test == "pcet") {
    m1 <- mean(test_dat %>% filter(rem==0) %>% pull(cat_res))
    m2 <- mean(test_dat %>% filter(rem!=0) %>% pull(cat_res))
    sd1 <- sd(test_dat %>% filter(rem==0) %>% pull(cat_res))
    sd2 <- sd(test_dat %>% filter(rem!=0) %>% pull(cat_res))
    n1 <- length(test_dat %>% filter(rem==0) %>% pull(cat_res))
    n2 <- length(test_dat %>% filter(rem!=0) %>% pull(cat_res))
    
    e_test1 <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
    
    m1 <- mean(test_dat %>% filter(rem==0) %>% pull(acc2_res))
    m2 <- mean(test_dat %>% filter(rem!=0) %>% pull(acc2_res))
    sd1 <- sd(test_dat %>% filter(rem==0) %>% pull(acc2_res))
    sd2 <- sd(test_dat %>% filter(rem!=0) %>% pull(acc2_res))
    n1 <- length(test_dat %>% filter(rem==0) %>% pull(acc2_res))
    n2 <- length(test_dat %>% filter(rem!=0) %>% pull(acc2_res))
    
    e_test2 <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
    
    m1 <- mean(test_dat %>% filter(rem==0) %>% pull(spe_res))
    m2 <- mean(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    sd1 <- sd(test_dat %>% filter(rem==0) %>% pull(spe_res))
    sd2 <- sd(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    n1 <- length(test_dat %>% filter(rem==0) %>% pull(spe_res))
    n2 <- length(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    
    e_test3 <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
    
    AB_p <- data.frame("age_7_12" = rep(NA,3),"age_12_17" = rep(NA,3),"age_17_35" = rep(NA,3))
    rownames(AB_p) <- c("cat","acc2","spe")
    
    for (num in 1:3){
      temp_dat <- test_dat %>% filter(age_bin == num)
      if (dim(temp_dat[which(temp_dat$rem == 1),])[1] > 5 &
          dim(temp_dat[which(temp_dat$rem == 0),])[1] > 5) {
        m1 <- mean(temp_dat %>% filter(rem==0) %>% pull(cat_res))
        m2 <- mean(temp_dat %>% filter(rem!=0) %>% pull(cat_res))
        sd1 <- sd(temp_dat %>% filter(rem==0) %>% pull(cat_res))
        sd2 <- sd(temp_dat %>% filter(rem!=0) %>% pull(cat_res))
        n1 <- length(temp_dat %>% filter(rem==0) %>% pull(cat_res))
        n2 <- length(temp_dat %>% filter(rem!=0) %>% pull(cat_res))
        etest <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
        eq_p <- round(max(etest$TOST$p.value[-1]),5)
        
        AB_p[1,num] <- eq_p
        
        m1 <- mean(temp_dat %>% filter(rem==0) %>% pull(acc2_res))
        m2 <- mean(temp_dat %>% filter(rem!=0) %>% pull(acc2_res))
        sd1 <- sd(temp_dat %>% filter(rem==0) %>% pull(acc2_res))
        sd2 <- sd(temp_dat %>% filter(rem!=0) %>% pull(acc2_res))
        n1 <- length(temp_dat %>% filter(rem==0) %>% pull(acc2_res))
        n2 <- length(temp_dat %>% filter(rem!=0) %>% pull(acc2_res))
        etest <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
        eq_p <- round(max(etest$TOST$p.value[-1]),5)
        
        AB_p[2,num] <- eq_p
        
        m1 <- mean(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        m2 <- mean(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        sd1 <- sd(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        sd2 <- sd(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        n1 <- length(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        n2 <- length(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        etest <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
        eq_p <- round(max(etest$TOST$p.value[-1]),5)
        
        AB_p[3,num] <- eq_p
      } else {
        AB_p[1,num] <- NA
        AB_p[2,num] <- NA
        AB_p[3,num] <- NA
      }
    }
    
    tAB_p <- AB_p %>%
      kbl(caption = paste0("Age-binned Equivalence Tests for ", notest), align = rep("c", 8),
          col.names = c("7-12","12-17","17-35")) %>%
      kable_classic(full_width = F, html_font = "Cambria")
    
    templist <- c(templist,"e_test1","e_test2","e_test3","tAB_p")
  } else if (test == "ds") {
    m1 <- mean(test_dat %>% filter(rem==0) %>% pull(acc_res))
    m2 <- mean(test_dat %>% filter(rem!=0) %>% pull(acc_res))
    sd1 <- sd(test_dat %>% filter(rem==0) %>% pull(acc_res))
    sd2 <- sd(test_dat %>% filter(rem!=0) %>% pull(acc_res))
    n1 <- length(test_dat %>% filter(rem==0) %>% pull(acc_res))
    n2 <- length(test_dat %>% filter(rem!=0) %>% pull(acc_res))
    
    e_test1 <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
    
    m1 <- mean(test_dat %>% filter(rem==0) %>% pull(spe_res))
    m2 <- mean(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    sd1 <- sd(test_dat %>% filter(rem==0) %>% pull(spe_res))
    sd2 <- sd(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    n1 <- length(test_dat %>% filter(rem==0) %>% pull(spe_res))
    n2 <- length(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    
    e_test2 <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
    
    m1 <- mean(test_dat %>% filter(rem==0) %>% pull(macc_res))
    m2 <- mean(test_dat %>% filter(rem!=0) %>% pull(macc_res))
    sd1 <- sd(test_dat %>% filter(rem==0) %>% pull(macc_res))
    sd2 <- sd(test_dat %>% filter(rem!=0) %>% pull(macc_res))
    n1 <- length(test_dat %>% filter(rem==0) %>% pull(macc_res))
    n2 <- length(test_dat %>% filter(rem!=0) %>% pull(macc_res))
    
    e_test3 <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
    
    m1 <- mean(test_dat %>% filter(rem==0) %>% pull(mspe_res))
    m2 <- mean(test_dat %>% filter(rem!=0) %>% pull(mspe_res))
    sd1 <- sd(test_dat %>% filter(rem==0) %>% pull(mspe_res))
    sd2 <- sd(test_dat %>% filter(rem!=0) %>% pull(mspe_res))
    n1 <- length(test_dat %>% filter(rem==0) %>% pull(mspe_res))
    n2 <- length(test_dat %>% filter(rem!=0) %>% pull(mspe_res))
    
    e_test4 <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
    
    AB_p <- data.frame("age_7_12" = rep(NA,4),"age_12_17" = rep(NA,4),"age_17_35" = rep(NA,4))
    rownames(AB_p) <- c("acc","spe","macc","mspe")
    
    for (num in 1:3){
      temp_dat <- test_dat %>% filter(age_bin == num)
      if (dim(temp_dat[which(temp_dat$rem == 1),])[1] > 5 &
          dim(temp_dat[which(temp_dat$rem == 0),])[1] > 5) {
        m1 <- mean(temp_dat %>% filter(rem==0) %>% pull(acc_res))
        m2 <- mean(temp_dat %>% filter(rem!=0) %>% pull(acc_res))
        sd1 <- sd(temp_dat %>% filter(rem==0) %>% pull(acc_res))
        sd2 <- sd(temp_dat %>% filter(rem!=0) %>% pull(acc_res))
        n1 <- length(temp_dat %>% filter(rem==0) %>% pull(acc_res))
        n2 <- length(temp_dat %>% filter(rem!=0) %>% pull(acc_res))
        etest <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
        eq_p <- round(max(etest$TOST$p.value[-1]),5)
        
        AB_p[1,num] <- eq_p
        
        m1 <- mean(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        m2 <- mean(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        sd1 <- sd(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        sd2 <- sd(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        n1 <- length(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        n2 <- length(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        etest <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
        eq_p <- round(max(etest$TOST$p.value[-1]),5)
        
        AB_p[2,num] <- eq_p
        
        m1 <- mean(temp_dat %>% filter(rem==0) %>% pull(macc_res))
        m2 <- mean(temp_dat %>% filter(rem!=0) %>% pull(macc_res))
        sd1 <- sd(temp_dat %>% filter(rem==0) %>% pull(macc_res))
        sd2 <- sd(temp_dat %>% filter(rem!=0) %>% pull(macc_res))
        n1 <- length(temp_dat %>% filter(rem==0) %>% pull(macc_res))
        n2 <- length(temp_dat %>% filter(rem!=0) %>% pull(macc_res))
        etest <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
        eq_p <- round(max(etest$TOST$p.value[-1]),5)
        
        AB_p[3,num] <- eq_p
        
        m1 <- mean(temp_dat %>% filter(rem==0) %>% pull(mspe_res))
        m2 <- mean(temp_dat %>% filter(rem!=0) %>% pull(mspe_res))
        sd1 <- sd(temp_dat %>% filter(rem==0) %>% pull(mspe_res))
        sd2 <- sd(temp_dat %>% filter(rem!=0) %>% pull(mspe_res))
        n1 <- length(temp_dat %>% filter(rem==0) %>% pull(mspe_res))
        n2 <- length(temp_dat %>% filter(rem!=0) %>% pull(mspe_res))
        etest <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
        eq_p <- round(max(etest$TOST$p.value[-1]),5)
        
        AB_p[4,num] <- eq_p
        
      } else {
        AB_p[1,num] <- NA
        AB_p[2,num] <- NA
        AB_p[3,num] <- NA
        AB_p[4,num] <- NA
      }
    }
    
    tAB_p <- AB_p %>%
      kbl(caption = paste0("Age-binned Equivalence Tests for ", notest), align = rep("c", 8),
          col.names = c("7-12","12-17","17-35")) %>%
      kable_classic(full_width = F, html_font = "Cambria")
    
    templist <- c(templist,"e_test1","e_test2","e_test3","e_test4","tAB_p")
  } else {
    m1 <- mean(test_dat %>% filter(rem==0) %>% pull(acc_res))
    m2 <- mean(test_dat %>% filter(rem!=0) %>% pull(acc_res))
    sd1 <- sd(test_dat %>% filter(rem==0) %>% pull(acc_res))
    sd2 <- sd(test_dat %>% filter(rem!=0) %>% pull(acc_res))
    n1 <- length(test_dat %>% filter(rem==0) %>% pull(acc_res))
    n2 <- length(test_dat %>% filter(rem!=0) %>% pull(acc_res))
    
    e_test1 <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
    
    m1 <- mean(test_dat %>% filter(rem==0) %>% pull(spe_res))
    m2 <- mean(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    sd1 <- sd(test_dat %>% filter(rem==0) %>% pull(spe_res))
    sd2 <- sd(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    n1 <- length(test_dat %>% filter(rem==0) %>% pull(spe_res))
    n2 <- length(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    
    e_test2 <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
    
    AB_p <- data.frame("age_7_12" = rep(NA,2),"age_12_17" = rep(NA,2),"age_17_35" = rep(NA,2))
    rownames(AB_p) <- c("acc","spe")
    
    for (num in 1:3){
      temp_dat <- test_dat %>% filter(age_bin == num)
      if (dim(temp_dat[which(temp_dat$rem == 1),])[1] > 5 &
          dim(temp_dat[which(temp_dat$rem == 0),])[1] > 5) {
        m1 <- mean(temp_dat %>% filter(rem==0) %>% pull(acc_res))
        m2 <- mean(temp_dat %>% filter(rem!=0) %>% pull(acc_res))
        sd1 <- sd(temp_dat %>% filter(rem==0) %>% pull(acc_res))
        sd2 <- sd(temp_dat %>% filter(rem!=0) %>% pull(acc_res))
        n1 <- length(temp_dat %>% filter(rem==0) %>% pull(acc_res))
        n2 <- length(temp_dat %>% filter(rem!=0) %>% pull(acc_res))
        etest <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
        eq_p <- round(max(etest$TOST$p.value[-1]),5)

        AB_p[1,num] <- eq_p

        m1 <- mean(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        m2 <- mean(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        sd1 <- sd(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        sd2 <- sd(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        n1 <- length(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        n2 <- length(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        etest <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
        eq_p <- round(max(etest$TOST$p.value[-1]),5)

        AB_p[2,num] <- eq_p
      } else {
        AB_p[1,num] <- NA
        AB_p[2,num] <- NA
      }
    }
    
    tAB_p <- AB_p %>%
      kbl(caption = paste0("Age-binned Equivalence Tests for ", notest), align = rep("c", 8),
          col.names = c("7-12","12-17","17-35")) %>%
      kable_classic(full_width = F, html_font = "Cambria")
    templist <- c(templist,"e_test1","e_test2","tAB_p")
  }
  
  templist <- mget(templist)
  assign(paste(test,"eqlist",sep="_"),templist)
  e_tests[[cntr]] <- templist
  cntr <- cntr+1
}






# * equivalence testing for more recent data only (2016 ~) ----

e_tests_rec <- list()
cntr <- 1
for (test in test_names) {
  templist <- c()
  
  test_list <- paste(test,"list",sep="_")
  test_list <- mget(test_list)[[1]]
  test_dat <- test_list$test_dat_res %>% 
    filter(dotest > as.Date("2015-12-31"))
  
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
  
  test_dat <- rbind(test_rem,test_inperson) %>% 
    filter(!is.na(rem))
  
  templist <- c(templist,"n_rem","n_inp","test_dat")
  
  notest <- Test_map %>% 
    filter(Prefix == test) %>% 
    pull(Test_name)
  
  if (test %in% c("tap","mpraxis")) {
    m1 <- mean(test_dat %>% filter(rem==0) %>% pull(spe_res))
    m2 <- mean(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    sd1 <- sd(test_dat %>% filter(rem==0) %>% pull(spe_res))
    sd2 <- sd(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    n1 <- length(test_dat %>% filter(rem==0) %>% pull(spe_res))
    n2 <- length(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    
    e_test1 <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
    
    AB_p <- data.frame("age_7_12" = rep(NA,1),"age_12_17" = rep(NA,1),"age_17_35" = rep(NA,1))
    rownames(AB_p) <- "spe"
    
    for (num in 1:3){
      if (dim(temp_dat[which(temp_dat$rem == 1),])[1] > 5 &
          dim(temp_dat[which(temp_dat$rem == 0),])[1] > 5) {
        temp_dat <- test_dat %>% filter(age_bin == num)
        m1 <- mean(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        m2 <- mean(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        sd1 <- sd(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        sd2 <- sd(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        n1 <- length(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        n2 <- length(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        etest <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
        eq_p <- round(max(etest$TOST$p.value[-1]),5)
        
        AB_p[1,num] <- eq_p
      } else {
        AB_p[1,num] <- NA
      }
    }
    
    tAB_p <- AB_p %>%
      kbl(caption = paste0("Age-binned Equivalence Tests for ", notest), align = rep("c", 8),
          col.names = c("7-12","12-17","17-35")) %>%
      kable_classic(full_width = F, html_font = "Cambria")
    
    templist <- c(templist,"e_test1","tAB_p")
  }  else if (test == "pcet") {
    m1 <- mean(test_dat %>% filter(rem==0) %>% pull(cat_res))
    m2 <- mean(test_dat %>% filter(rem!=0) %>% pull(cat_res))
    sd1 <- sd(test_dat %>% filter(rem==0) %>% pull(cat_res))
    sd2 <- sd(test_dat %>% filter(rem!=0) %>% pull(cat_res))
    n1 <- length(test_dat %>% filter(rem==0) %>% pull(cat_res))
    n2 <- length(test_dat %>% filter(rem!=0) %>% pull(cat_res))
    
    e_test1 <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
    
    m1 <- mean(test_dat %>% filter(rem==0) %>% pull(acc2_res))
    m2 <- mean(test_dat %>% filter(rem!=0) %>% pull(acc2_res))
    sd1 <- sd(test_dat %>% filter(rem==0) %>% pull(acc2_res))
    sd2 <- sd(test_dat %>% filter(rem!=0) %>% pull(acc2_res))
    n1 <- length(test_dat %>% filter(rem==0) %>% pull(acc2_res))
    n2 <- length(test_dat %>% filter(rem!=0) %>% pull(acc2_res))
    
    e_test2 <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
    
    m1 <- mean(test_dat %>% filter(rem==0) %>% pull(spe_res))
    m2 <- mean(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    sd1 <- sd(test_dat %>% filter(rem==0) %>% pull(spe_res))
    sd2 <- sd(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    n1 <- length(test_dat %>% filter(rem==0) %>% pull(spe_res))
    n2 <- length(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    
    e_test3 <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
    
    AB_p <- data.frame("age_7_12" = rep(NA,3),"age_12_17" = rep(NA,3),"age_17_35" = rep(NA,3))
    rownames(AB_p) <- c("cat","acc2","spe")
    
    for (num in 1:3){
      temp_dat <- test_dat %>% filter(age_bin == num)
      if (dim(temp_dat[which(temp_dat$rem == 1),])[1] > 5 &
          dim(temp_dat[which(temp_dat$rem == 0),])[1] > 5) {
        m1 <- mean(temp_dat %>% filter(rem==0) %>% pull(cat_res))
        m2 <- mean(temp_dat %>% filter(rem!=0) %>% pull(cat_res))
        sd1 <- sd(temp_dat %>% filter(rem==0) %>% pull(cat_res))
        sd2 <- sd(temp_dat %>% filter(rem!=0) %>% pull(cat_res))
        n1 <- length(temp_dat %>% filter(rem==0) %>% pull(cat_res))
        n2 <- length(temp_dat %>% filter(rem!=0) %>% pull(cat_res))
        etest <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
        eq_p <- round(max(etest$TOST$p.value[-1]),5)
        
        AB_p[1,num] <- eq_p
        
        m1 <- mean(temp_dat %>% filter(rem==0) %>% pull(acc2_res))
        m2 <- mean(temp_dat %>% filter(rem!=0) %>% pull(acc2_res))
        sd1 <- sd(temp_dat %>% filter(rem==0) %>% pull(acc2_res))
        sd2 <- sd(temp_dat %>% filter(rem!=0) %>% pull(acc2_res))
        n1 <- length(temp_dat %>% filter(rem==0) %>% pull(acc2_res))
        n2 <- length(temp_dat %>% filter(rem!=0) %>% pull(acc2_res))
        etest <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
        eq_p <- round(max(etest$TOST$p.value[-1]),5)
        
        AB_p[2,num] <- eq_p
        
        m1 <- mean(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        m2 <- mean(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        sd1 <- sd(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        sd2 <- sd(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        n1 <- length(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        n2 <- length(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        etest <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
        eq_p <- round(max(etest$TOST$p.value[-1]),5)
        
        AB_p[3,num] <- eq_p
      } else {
        AB_p[1,num] <- NA
        AB_p[2,num] <- NA
        AB_p[3,num] <- NA
      }
    }
    
    tAB_p <- AB_p %>%
      kbl(caption = paste0("Age-binned Equivalence Tests for ", notest), align = rep("c", 8),
          col.names = c("7-12","12-17","17-35")) %>%
      kable_classic(full_width = F, html_font = "Cambria")
    
    templist <- c(templist,"e_test1","e_test2","e_test3","tAB_p")
  } else if (test == "ds") {
    m1 <- mean(test_dat %>% filter(rem==0) %>% pull(acc_res))
    m2 <- mean(test_dat %>% filter(rem!=0) %>% pull(acc_res))
    sd1 <- sd(test_dat %>% filter(rem==0) %>% pull(acc_res))
    sd2 <- sd(test_dat %>% filter(rem!=0) %>% pull(acc_res))
    n1 <- length(test_dat %>% filter(rem==0) %>% pull(acc_res))
    n2 <- length(test_dat %>% filter(rem!=0) %>% pull(acc_res))
    
    e_test1 <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
    
    m1 <- mean(test_dat %>% filter(rem==0) %>% pull(spe_res))
    m2 <- mean(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    sd1 <- sd(test_dat %>% filter(rem==0) %>% pull(spe_res))
    sd2 <- sd(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    n1 <- length(test_dat %>% filter(rem==0) %>% pull(spe_res))
    n2 <- length(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    
    e_test2 <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
    
    m1 <- mean(test_dat %>% filter(rem==0) %>% pull(macc_res))
    m2 <- mean(test_dat %>% filter(rem!=0) %>% pull(macc_res))
    sd1 <- sd(test_dat %>% filter(rem==0) %>% pull(macc_res))
    sd2 <- sd(test_dat %>% filter(rem!=0) %>% pull(macc_res))
    n1 <- length(test_dat %>% filter(rem==0) %>% pull(macc_res))
    n2 <- length(test_dat %>% filter(rem!=0) %>% pull(macc_res))
    
    e_test3 <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
    
    m1 <- mean(test_dat %>% filter(rem==0) %>% pull(mspe_res))
    m2 <- mean(test_dat %>% filter(rem!=0) %>% pull(mspe_res))
    sd1 <- sd(test_dat %>% filter(rem==0) %>% pull(mspe_res))
    sd2 <- sd(test_dat %>% filter(rem!=0) %>% pull(mspe_res))
    n1 <- length(test_dat %>% filter(rem==0) %>% pull(mspe_res))
    n2 <- length(test_dat %>% filter(rem!=0) %>% pull(mspe_res))
    
    e_test4 <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
    
    AB_p <- data.frame("age_7_12" = rep(NA,4),"age_12_17" = rep(NA,4),"age_17_35" = rep(NA,4))
    rownames(AB_p) <- c("acc","spe","macc","mspe")
    
    for (num in 1:3){
      temp_dat <- test_dat %>% filter(age_bin == num)
      if (dim(temp_dat[which(temp_dat$rem == 1),])[1] > 5 &
          dim(temp_dat[which(temp_dat$rem == 0),])[1] > 5) {
        m1 <- mean(temp_dat %>% filter(rem==0) %>% pull(acc_res))
        m2 <- mean(temp_dat %>% filter(rem!=0) %>% pull(acc_res))
        sd1 <- sd(temp_dat %>% filter(rem==0) %>% pull(acc_res))
        sd2 <- sd(temp_dat %>% filter(rem!=0) %>% pull(acc_res))
        n1 <- length(temp_dat %>% filter(rem==0) %>% pull(acc_res))
        n2 <- length(temp_dat %>% filter(rem!=0) %>% pull(acc_res))
        etest <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
        eq_p <- round(max(etest$TOST$p.value[-1]),5)
        
        AB_p[1,num] <- eq_p
        
        m1 <- mean(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        m2 <- mean(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        sd1 <- sd(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        sd2 <- sd(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        n1 <- length(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        n2 <- length(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        etest <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
        eq_p <- round(max(etest$TOST$p.value[-1]),5)
        
        AB_p[2,num] <- eq_p
        
        m1 <- mean(temp_dat %>% filter(rem==0) %>% pull(macc_res))
        m2 <- mean(temp_dat %>% filter(rem!=0) %>% pull(macc_res))
        sd1 <- sd(temp_dat %>% filter(rem==0) %>% pull(macc_res))
        sd2 <- sd(temp_dat %>% filter(rem!=0) %>% pull(macc_res))
        n1 <- length(temp_dat %>% filter(rem==0) %>% pull(macc_res))
        n2 <- length(temp_dat %>% filter(rem!=0) %>% pull(macc_res))
        etest <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
        eq_p <- round(max(etest$TOST$p.value[-1]),5)
        
        AB_p[3,num] <- eq_p
        
        m1 <- mean(temp_dat %>% filter(rem==0) %>% pull(mspe_res))
        m2 <- mean(temp_dat %>% filter(rem!=0) %>% pull(mspe_res))
        sd1 <- sd(temp_dat %>% filter(rem==0) %>% pull(mspe_res))
        sd2 <- sd(temp_dat %>% filter(rem!=0) %>% pull(mspe_res))
        n1 <- length(temp_dat %>% filter(rem==0) %>% pull(mspe_res))
        n2 <- length(temp_dat %>% filter(rem!=0) %>% pull(mspe_res))
        etest <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
        eq_p <- round(max(etest$TOST$p.value[-1]),5)
        
        AB_p[4,num] <- eq_p
        
      } else {
        AB_p[1,num] <- NA
        AB_p[2,num] <- NA
        AB_p[3,num] <- NA
        AB_p[4,num] <- NA
      }
    }
    
    tAB_p <- AB_p %>%
      kbl(caption = paste0("Age-binned Equivalence Tests for ", notest), align = rep("c", 8),
          col.names = c("7-12","12-17","17-35")) %>%
      kable_classic(full_width = F, html_font = "Cambria")
    
    templist <- c(templist,"e_test1","e_test2","e_test3","e_test4","tAB_p")
  } else {
    m1 <- mean(test_dat %>% filter(rem==0) %>% pull(acc_res))
    m2 <- mean(test_dat %>% filter(rem!=0) %>% pull(acc_res))
    sd1 <- sd(test_dat %>% filter(rem==0) %>% pull(acc_res))
    sd2 <- sd(test_dat %>% filter(rem!=0) %>% pull(acc_res))
    n1 <- length(test_dat %>% filter(rem==0) %>% pull(acc_res))
    n2 <- length(test_dat %>% filter(rem!=0) %>% pull(acc_res))
    
    e_test1 <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
    
    m1 <- mean(test_dat %>% filter(rem==0) %>% pull(spe_res))
    m2 <- mean(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    sd1 <- sd(test_dat %>% filter(rem==0) %>% pull(spe_res))
    sd2 <- sd(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    n1 <- length(test_dat %>% filter(rem==0) %>% pull(spe_res))
    n2 <- length(test_dat %>% filter(rem!=0) %>% pull(spe_res))
    
    e_test2 <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
    
    AB_p <- data.frame("age_7_12" = rep(NA,2),"age_12_17" = rep(NA,2),"age_17_35" = rep(NA,2))
    rownames(AB_p) <- c("acc","spe")
    
    for (num in 1:3){
      temp_dat <- test_dat %>% filter(age_bin == num)
      if (dim(temp_dat[which(temp_dat$rem == 1),])[1] > 5 &
          dim(temp_dat[which(temp_dat$rem == 0),])[1] > 5) {
        m1 <- mean(temp_dat %>% filter(rem==0) %>% pull(acc_res))
        m2 <- mean(temp_dat %>% filter(rem!=0) %>% pull(acc_res))
        sd1 <- sd(temp_dat %>% filter(rem==0) %>% pull(acc_res))
        sd2 <- sd(temp_dat %>% filter(rem!=0) %>% pull(acc_res))
        n1 <- length(temp_dat %>% filter(rem==0) %>% pull(acc_res))
        n2 <- length(temp_dat %>% filter(rem!=0) %>% pull(acc_res))
        etest <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
        eq_p <- round(max(etest$TOST$p.value[-1]),5)
        
        AB_p[1,num] <- eq_p
        
        m1 <- mean(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        m2 <- mean(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        sd1 <- sd(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        sd2 <- sd(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        n1 <- length(temp_dat %>% filter(rem==0) %>% pull(spe_res))
        n2 <- length(temp_dat %>% filter(rem!=0) %>% pull(spe_res))
        etest <- tsum_TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,low_eqbound=-0.2,high_eqbound=0.2,alpha = 0.05,var.equal = F,eqbound_type="SMD")
        eq_p <- round(max(etest$TOST$p.value[-1]),5)
        
        AB_p[2,num] <- eq_p
      } else {
        AB_p[1,num] <- NA
        AB_p[2,num] <- NA
      }
    }
    
    tAB_p <- AB_p %>%
      kbl(caption = paste0("Age-binned Equivalence Tests for ", notest), align = rep("c", 8),
          col.names = c("7-12","12-17","17-35")) %>%
      kable_classic(full_width = F, html_font = "Cambria")
    templist <- c(templist,"e_test1","e_test2","tAB_p")
  }
  
  templist <- mget(templist)
  assign(paste(test,"eqlist_rec",sep="_"),templist)
  e_tests_rec[[cntr]] <- templist
  cntr <- cntr+1
}













