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

demo <- dat[,1:20]    # demographics & non-test-specific things

dat_bbl <- dat[!is.na(dat$bblid),]         # all cols of rows with BBLIDs
dat_bbl <- dat_bbl[order(dat_bbl$bblid),]
dat_rem <- dat_bbl[dat_bbl$remote==1,]
dat_inp <- dat_bbl[dat_bbl$remote==0,]
dat_t1 <- dat_bbl[]   # from dat_bbl, get the most recent timepoint data for each 


# * separating out into tasks ----
notval <- c("N")

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

colors <- c("#6E4230", "#BF9660", "#065C50", "#48B0A2")



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




# trying age-matching code ----
#      this ended up taking a long time, might need to figure out different age-matching process

library(ccoptimalmatch)


# example code from site

# practice code
create_subset <- not_processed %>%                             # take not_processed data
  filter(case_control =="case") %>%                            # select only the "case", so in my case all the "rem"
  arrange(Practice_Id, Gender, JCG) %>%                        # order by practice_id first, then by gender, and then JCG
  distinct(Gender, JCG, Practice_Id, .keep_all = TRUE) %>%     # keeps all distinct combinations of gender, jcg and practice_id
  dplyr::mutate(subset = 1:n()) %>%                            # create new variable, subset that is just 1-n
  select(Gender, JCG, Practice_Id, subset)                     # only select columns Gender, JCG, practice_id, subset

case_with_subset <- not_processed %>%                               # take not_processed data
  filter(case_control =="case") %>%                                 # select only the "case"
  full_join(create_subset, by = c("Gender", "JCG", "Practice_Id"))  # 


control_with_subset <- not_processed %>% 
  filter(case_control =="control") %>%
  right_join(create_subset, by = c("Gender", "JCG", "Practice_Id"))

not_processed1 <- rbind(case_with_subset,control_with_subset)

bdd_controls <- not_processed1[not_processed1$case_control=="control",]
bdd_controls$cluster_case <- 0
bdd_cases <- not_processed1[not_processed1$case_control=="case",]
bdd_cases$cluster_case <- paste("case",1:nrow(bdd_cases),sep = "_")

not_processed1 <- rbind(bdd_cases,bdd_controls)
not_processed1$age <- not_processed1$JCG-not_processed1$Birth_Year 

bdd_cases <- not_processed1[not_processed1$case_control=="case",]
bdd_control <- not_processed1[not_processed1$case_control=="control",]

bdd_temp <- data.frame()
list_p <- unique(bdd_cases$cluster_case)

for(i in 1:length(list_p)){
  temp <- bdd_cases[bdd_cases$cluster_case==list_p[i],]
  subset_identified <- temp$subset
  temp0 <- bdd_control[bdd_control$subset==temp$subset,]
  temp_final <- rbind(temp,temp0)
  temp_final$cluster_case <- list_p[i]
  temp_final=temp_final %>%
    group_by(cluster_case) %>%
    mutate(age_diff = abs(age - age[case_control=="case"]),
           fup_diff = foll_up - foll_up[case_control=="case"])
  temp_final$age_fup <- ifelse(temp_final$age_diff<=2&temp_final$fup_diff==0,"accept","delete")
  temp_final <- temp_final[temp_final$age_fup=="accept",]
  temp_final$age_fup <- NULL
  bdd_temp <- rbind(bdd_temp,temp_final)
}


bdd_temp = bdd_temp %>% group_by(cluster_case) %>% mutate(total_control_per_case = n()-1)
bdd_temp$case_ind <- ifelse(bdd_temp$case_control=="case",1,0)
bdd_temp <- subset(bdd_temp, select=c(cluster_case, Patient_Id, case_control, case_ind,
                                      JCG, entry_year, CI, age_diff, fup_diff, total_control_per_case))

bdd_temp = bdd_temp %>% group_by(Patient_Id) %>% mutate(freq_of_controls = n())


bdd_temp<-bdd_temp[order(bdd_temp$cluster_case,bdd_temp$case_control,bdd_temp$fup_diff,
                         bdd_temp$age_diff,bdd_temp$freq_of_controls),]

final_data <- optimal_matching(bdd_temp, n_con=4, cluster_case, Patient_Id, 
                               total_control_per_case, case_control, with_replacement = FALSE)

final_data = final_data %>% group_by(cluster_case) %>% mutate(total_control_matched = n()-1)
table(final_data$case_control,final_data$total_control_matched)



# trying example code with my data

my_data <- dat_bbl[,c(6,10,9,8,13,20)]
my_data <- my_data[!is.na(my_data$test_sessions_v.dob),]

# test specific -- trying it out with adt (since it would have to vary by test anyways)
my_adt <- cbind(demo, dat[,grepl("adt",colnames(dat))])   # PC and RTCR
my_adt <- my_adt[!is.na(my_adt$adt_rtcr),]
my_adt <- my_adt[!(my_adt$adt_valid %in% notval) & !is.na(my_adt$age),]
my_adt <- select(my_adt, "bblid","dotest","dob","age","test_sessions_v.battery","sex","remote","adt_pc","adt_rtcr")


my_create_subset <- my_adt %>%                             
  filter(remote == 1) %>%                            
  arrange(sex, age) %>%                        
  distinct(sex, age, .keep_all = TRUE) %>%     
  dplyr::mutate(subset = 1:n()) %>%                           
  select(sex, age, subset)

my_case_with_subset <- my_adt %>%                               
  filter(remote == 1) %>%                                 
  full_join(my_create_subset, by = c("sex", "age"))  

my_control_with_subset <- my_adt %>% 
  filter(remote == 0) %>%
  right_join(my_create_subset, by = c("sex", "age"))

my_not_processed <- rbind(my_case_with_subset,my_control_with_subset)

my_controls <- my_not_processed[my_not_processed$remote == 0,]
my_controls$cluster_case <- 0
my_cases <- my_not_processed[my_not_processed$remote == 1,]
my_cases <- my_cases[rowSums(is.na(my_cases))<ncol(my_cases),]
my_cases$cluster_case <- paste("case",1:nrow(my_cases),sep = "_")

my_not_processed <- rbind(my_cases,my_controls)

my_cases <- my_not_processed[my_not_processed$remote == 1,]
my_cases <- my_cases[rowSums(is.na(my_cases))<ncol(my_cases),]
my_control <- my_not_processed[my_not_processed$remote == 0,]

my_temp <- data.frame()
list_p <- unique(my_cases$cluster_case)

for(i in 1:length(list_p)){
  temp <- my_cases[my_cases$cluster_case==list_p[i],]
  subset_identified <- temp$subset
  temp0 <- my_control[my_control$subset==temp$subset,]
  temp0 <- temp0[rowSums(is.na(temp0))<ncol(temp0),]
  temp_final <- rbind(temp,temp0)
  temp_final$cluster_case <- list_p[i]
  temp_final=temp_final %>%
    group_by(cluster_case) %>%
    mutate(age_diff = abs(age - age[remote==1]))
  temp_final$age_fup <- ifelse(temp_final$age_diff==0,"accept","delete")   # age difference of 0
  temp_final <- temp_final[temp_final$age_fup=="accept",]
  temp_final$age_fup <- NULL
  my_temp <- rbind(my_temp,temp_final)
}

my_temp = my_temp %>% group_by(cluster_case) %>% mutate(total_control_per_case = n()-1)
my_temp <- subset(my_temp, select=c(cluster_case, bblid, test_sessions_v.battery, remote, dob, age, 
                                    dotest, sex, adt_pc,adt_rtcr,age_diff,total_control_per_case))

my_temp = my_temp %>% group_by(bblid) %>% mutate(freq_of_controls = n())

my_temp <- my_temp[order(my_temp$cluster_case,my_temp$remote,
                         my_temp$age_diff,my_temp$freq_of_controls),]
my_temp$case_control <- ifelse(my_temp$remote==1,"case","control")

my_final_data <- optimal_matching(my_temp, n_con=1, cluster_case, bblid, 
                               total_control_per_case, case_control, with_replacement = FALSE) # tried this but it takes way too long (let it run for about an hour already)








final_data = final_data %>% group_by(cluster_case) %>% mutate(total_control_matched = n()-1)
table(final_data$case_control,final_data$total_control_matched)




# for cross-sectional analysis: look at most recent tp for rem and inp for each unique bblid
# separate by site, 
# longitudinal analysis: spaghetti plots







# using Noah's code for my data ----


dat2 <- dat %>% 
  select(bblid,test_sessions.datasetid,test_sessions.siteid, 
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
  select(!(matches("bblid") | matches("^test") | "age" | "dob" | "dotest" | "sex" | "platform" | "remote" | matches("_valid$") | matches("^aim") | "sex_remote")) %>%    # ^[this]: anything beginning with [this]; [this]$: anything ending with [this]
  colnames()
for(test in tests){
  cnb_cross[[test]] <- ifelse(cnb_cross[[test]] > mean(cnb_cross[[test]],na.rm = TRUE) + 6*sd(cnb_cross[[test]],na.rm = TRUE),mean(cnb_cross[[test]],na.rm = TRUE) + 6*sd(cnb_cross[[test]],na.rm = TRUE),cnb_cross[[test]])
  cnb_cross[[test]] <- ifelse(cnb_cross[[test]] < mean(cnb_cross[[test]],na.rm = TRUE) - 6*sd(cnb_cross[[test]],na.rm = TRUE),mean(cnb_cross[[test]],na.rm = TRUE) - 6*sd(cnb_cross[[test]],na.rm = TRUE),cnb_cross[[test]])
}

response_vars <- cnb_cross %>% 
  select(!(contains("valid"))) %>% 
  select(adt_pc:volt_w_rtcr) %>% 
  colnames()

# exclude aim for now (DISC tasks excluded already)
response_vars <- setdiff(response_vars,c("aim_tot", "aim_totrt"))

Test_map <- data.frame('Prefix' = c("er40","pvrt","volt","cpf","cpw","gng","mpraxis","pcet","pmat","medf","adt","plot","tap","cpt","tap","lnb","volt","plot","ds"),
                       "Test_name" = c("Penn Emotion Recognition Test","Penn Verbal Reasoning Test","Visual Object Learning Test","Penn Face Memory Test",'Penn Word Memory Test',
                                       "Go-No-Go Test","Motor Praxis Test","Penn Conditional Exclusion Test","Penn Matrix Analysis Test","Measured Emotion Differentiation Test",
                                       "Age Differentiation Test","Penn Line Orientation Test","Penn Computerized Finger Tapping Test","Penn Continuous Performance Test",
                                       "Penn Computerized Finger Tapping Test","Letter-N-Back","Visual Object Learning Test","Penn Line Orientation Test","Digit Symbol Search"))

Metric_map <- data.frame("Suffix" = c("_cr","_rtcr","_tot","_acc2","_tprt","_ptp","_mcr","_mrtc","_pc","_mp2rtcr","_cat","_mrtc","_tp","_memcr","_mcrrt"),
                         "Label" = c("Correct Responses","Median Response Time \n Correct Responses (ms)","Average Taps \n (Dominant and Non-dominant hand added together)",
                                     "Accuracy","Median Response Time \n True Positives (ms)","True Positive (%)","Total True Positive Responses","Median Response Time \n Correct Responses",
                                     "Correct Responses (%)","Median Response Time \n Correct Responses (ms)","Categories Achieved","Median Response Time \n Correct Responses (ms)",
                                     "True Positive Responses","Correct Responses","Median Response Time \n Correct Responses (ms)"))

LongitudinalPlots <- list()
cntr <- 1
theme_set(theme_minimal())
for(test in response_vars){
  test_split <- str_split(test,pattern = "_")[[1]]
  test_prefix <- test_split[1]
  test_suffix <- paste0("_",test_split[length(test_split)])
  
  Plot_title <- Test_map %>% 
    filter(Prefix == test_prefix) %>% 
    pull(Test_name)
  
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
    LongitudinalPlots[[cntr]] <- cnb_cross %>% 
      left_join(N.df) %>% 
      filter(!is.na(sex_remote_N)) %>% 
      ggplot(aes_string(x = "age",y = test,color = "sex_remote_N")) + 
      geom_point(size = .6) + geom_smooth(se = FALSE) + labs(x = "Age",y = ylabel,title = Plot_title,color = "") + 
      scale_color_manual(values = c("#ca0020","#0571b0")) + theme(legend.position = "bottom")
    cntr <- cntr + 1
  } else{
    LongitudinalPlots[[cntr]] <- cnb_cross %>% 
      left_join(N.df) %>% 
      filter(!is.na(sex_remote_N)) %>% 
      ggplot(aes_string(x = "age",y = test,color = "sex_remote_N")) + 
      geom_point(size = .6) + geom_smooth(se = FALSE) + labs(x = "Age",y = ylabel,title = Plot_title,color = "") + 
      scale_color_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de")) + theme(legend.position = "bottom")
    cntr <- cntr + 1
  }
}

# use this to make all graphs


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











