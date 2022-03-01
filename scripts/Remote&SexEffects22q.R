# Aki's copy of Noah's script (using Noah's spaghetti plot script) -- turned out to be cross-sectional analysis plots, not longitudinal
# Import the necessary functions and data

library(tidyverse)
library(ggdist)
library(lme4)
library(ggpubr)
library(lmerTest)
library(visreg)
library(ggh4x)
CNB <- read_csv("~/Projects/22q/Data/cnb_all_202109.csv")
codebook <- read_csv("~/Downloads/bbl_cross_battery_codebook.csv")

# Filter to include only 22q subjects, select only necessary columns

CNB <- CNB %>% 
  select(test_sessions.bblid,test_sessions.datasetid,test_sessions.siteid, 
         test_sessions.famid, test_sessions.subid, test_sessions_v.age, test_sessions_v.battery, 
         test_sessions_v.dob, test_sessions_v.dotest, test_sessions_v.education, test_sessions_v.feducation,
         test_sessions_v.gender, test_sessions_v.handedness, test_sessions_v.meducation, deleted_sample, cnbagegrp, 
         platform, ADT36_A.valid_code, ADT36_A.ADT36A_CR, ADT36_A.ADT36A_PC, ADT36_A.ADT36A_RTCR, CPF_B.valid_code, 
         CPF_B.CPF_CR, CPF_B.CPF_RTCR, 
         CPF_B.CPF_W_RTCR, ER40_D.valid_code, ER40_D.ER40D_CR, ER40_D.ER40D_RTCR, MEDF36_A.valid_code, 
         MEDF36_A.MEDF36A_CR, MEDF36_A.MEDF36A_RTCR, MPRACT.valid_code, MPRACT.MP2RTCR, PCET_A.valid_code, PCET_A.PCET_RTCR, 
         PCET_A.PCET_CAT, PCET_A.PCET_ACC2, PMAT24_A.valid_code, PMAT24_A.PMAT24_A_CR, 
         PMAT24_A.PMAT24_A_RTCR, SCTAP.valid_code, SCTAP.SCTAP_TOT, SLNB2_90.valid_code, 
         SLNB2_90.SLNB2_MCR, SLNB2_90.SLNB2_MRTC, SPCPTN90.valid_code, SPCPTN90.SCPN90_TP,
         SPCPTN90.SCPN90_TPRT, SPCPTNL.valid_code, SPCPTNL.SCPN_TPRT, SPCPTNL.SCPN_TP,
         SVOLT_A.SVOLT_RTCR, VSPLOT15.valid_code, VSPLOT15.VSPLOT15_CR, VSPLOT15.VSPLOT15_RTCR) %>% 
  filter(deleted_sample == 1) %>% 
  mutate(test_sessions_v.gender = ifelse(test_sessions_v.gender == "F","Female","Male")) %>% 
  mutate(remote = ifelse(platform == "webcnp","In-person","Remote")) %>% 
  mutate(gender_remote = paste(test_sessions_v.gender,remote))

# First, create cross-sectional data set by taking only the last test from repeat subjects

CNB_repeats_list <- CNB %>% 
  group_by(test_sessions.bblid) %>% 
  filter(n() > 1) %>% 
  mutate(test_sessions_v.dotest = str_replace_all(test_sessions_v.dotest,pattern = "([[:digit:]][[:digit:]])$",replacement = "20\\1")) %>% 
  mutate(test_sessions_v.dotest = as.Date(test_sessions_v.dotest,format = "%m/%d/%Y")) %>% 
  arrange(test_sessions_v.dotest) %>% 
  ungroup() %>% 
  group_split(test_sessions.bblid)

find_last_test <- function(df_by_bblid){
  if(any(df_by_bblid$remote == "Remote")){
   last_test <- df_by_bblid %>% 
      arrange(test_sessions_v.dotest) %>% 
      filter(remote == "Remote") %>% 
      slice_tail(n = 1)
  } else{
   last_test <- df_by_bblid %>% 
      arrange(test_sessions_v.dotest) %>% 
      slice_tail(n = 1)
  }
  return(last_test)
}
  
Last_tests <- map_dfr(CNB_repeats_list,find_last_test)

CNB_cross <- CNB %>% 
  group_by(test_sessions.bblid) %>% 
  filter(n() == 1) %>% 
  mutate(test_sessions_v.dotest = str_replace_all(test_sessions_v.dotest,pattern = "([[:digit:]][[:digit:]])$",replacement = "20\\1")) %>% 
  mutate(test_sessions_v.dotest = as.Date(test_sessions_v.dotest,format = "%m/%d/%Y")) %>% 
  arrange(test_sessions_v.dotest) %>% 
  ungroup() %>% 
  bind_rows(Last_tests) 

# Cap values at 6 sd 

tests <- CNB_cross %>% 
     select(!(matches("^test") | matches("valid_code") | matches("_AR$") | "remote" | "gender_remote" | "deleted_sample" | "cnbagegrp" | "platform")) %>% 
     colnames()
for(test in tests){
  CNB_cross[[test]] <- ifelse(CNB_cross[[test]] > mean(CNB_cross[[test]],na.rm = TRUE) + 6*sd(CNB_cross[[test]],na.rm = TRUE),mean(CNB_cross[[test]],na.rm = TRUE) + 6*sd(CNB_cross[[test]],na.rm = TRUE),CNB_cross[[test]])
  CNB_cross[[test]] <- ifelse(CNB_cross[[test]] < mean(CNB_cross[[test]],na.rm = TRUE) - 6*sd(CNB_cross[[test]],na.rm = TRUE),mean(CNB_cross[[test]],na.rm = TRUE) - 6*sd(CNB_cross[[test]],na.rm = TRUE),CNB_cross[[test]])
}

# Use codebook to build data frame which maps test acronyms to test names
Test_map <- data.frame('Prefix' = c("er40","pvrt","volt","cpf","cpw","gng","mpract","pcet","pmat24","medf36","adt36","plot","tap","cpt","lnb","sctap","slnb2","SPCPTN90","SPCPTNL","svolt","vsplot15"),
                       "Test_name" = c("Penn Emotion Recognition Test","Penn Verbal Reasoning Test","Visual Object Learning Test","Penn Face Memory Test",'Penn Word Memory Test',"Go-No-Go Test","Motor Praxis Test","Penn Conditional Exclusion Test","Penn Matrix Analysis Test","Measured Emotion Differentiation Test","Age Differentiation Test","Penn Line Orientation Test","Penn Computerized Finger Tapping Test",
                                       "Penn Continuous Performance Test","Letter-N-Back Test","Penn Computerized Finger Tapping Test","Letter-N-Back","Penn Continuous Performance Test - Numbers","Penn Continuous Performance Test - Letters","Visual Object Learning Test","Penn Line Orientation Test"))
Test_map$Prefix <- str_to_upper(Test_map$Prefix)

# Use codebook to map measurements to longer names
Metric_map <- data.frame("Suffix" = c("_cr","_rtcr","_tot","_acc2","_tprt","_ptp","_mcr","_mrtc","_pc","_mp2rtcr","_cat","_mrtc","_tp"),
                         "Label" = c("Correct Responses","Median Reaction Time \n Correct Responses (ms)","Average Taps \n (Dominant and Non-dominant hand added together)","Accuracy",
                                     "Median Response Time \n True Positives (ms)","True Positive (%)","Total True Positive Responses","Median Response Time \n Correct Responses","Correct Responses (%)","Median Reaction Time \n Correct Responses (ms)","Categories Achieved","Median Reaction Time \n Correct Responses (ms)","True Positive Responses"))
Metric_map$Suffix <- str_to_upper(Metric_map$Suffix)
# Create plots of cognitive test performance by sex and remote
LongitudinalPlots <- list()
response_vars <- CNB_cross %>% 
  select(!(contains("valid_code"))) %>% 
  select(ADT36_A.ADT36A_CR:VSPLOT15.VSPLOT15_RTCR) %>% 
  colnames()

cntr <- 1
theme_set(theme_minimal())

for(test in response_vars){
  
test_noPeriod <- str_replace_all(test,pattern = "\\.",replacement = "_")
test_split <- str_split(test_noPeriod,pattern = "_")[[1]]
test_prefix <- test_split[1]
test_suffix <- paste0("_",test_split[length(test_split)])
  
Plot_title <- Test_map %>% 
    filter(Prefix == test_prefix) %>% 
    pull(Test_name)
  
ylabel <- Metric_map %>% 
    filter(Suffix == test_suffix) %>% 
    pull(Label)
  
N.df <- CNB_cross[,c(test,"gender_remote","test_sessions_v.age")] %>% 
  filter(if_all(everything(), ~ !is.na(.))) %>% 
  group_by(gender_remote) %>% 
  summarize(n = n()) %>% 
  mutate(gender_remote_N = factor(paste0(gender_remote,": ","N = ",n))) %>% 
  arrange(gender_remote_N)

if(nrow(N.df) == 2){
  LongitudinalPlots[[cntr]] <- CNB_cross %>% 
    left_join(N.df) %>% 
    filter(!is.na(gender_remote_N)) %>% 
    ggplot(aes_string(x = "test_sessions_v.age",y = test,color = "gender_remote_N")) + geom_point(size = .6) + geom_smooth(se = FALSE) + labs(x = "Age",y = ylabel,title = Plot_title,color = "") + scale_color_manual(values = c("#ca0020","#0571b0")) + theme(legend.position = "bottom")
  cntr <- cntr + 1
} else{
  LongitudinalPlots[[cntr]] <- CNB_cross %>% 
    left_join(N.df) %>% 
    filter(!is.na(gender_remote_N)) %>% 
    ggplot(aes_string(x = "test_sessions_v.age",y = test,color = "gender_remote_N")) + geom_point(size = .6) + geom_smooth(se = FALSE) + labs(x = "Age",y = ylabel,title = Plot_title,color = "") + scale_color_manual(values = c("#ca0020","#f4a582","#0571b0","#92c5de")) + theme(legend.position = "bottom")
  cntr <- cntr + 1
}
}


# IGNORE everything below (Aki) ----
  
# Create plots of cognitive test performance by siteid
SiteIDPlots <- list()

response_vars <- CNB_cross %>% 
  select(!(contains("valid_code"))) %>% 
  select(ADT36_A.ADT36A_CR:VSPLOT15.VSPLOT15_RTCR) %>% 
  colnames()

cntr <- 1
theme_set(theme_minimal())
for(test in response_vars){
  
test_noPeriod <- str_replace_all(test,pattern = "\\.",replacement = "_")
test_split <- str_split(test_noPeriod,pattern = "_")[[1]]
test_prefix <- test_split[1]
test_suffix <- paste0("_",test_split[length(test_split)])
  
Plot_title <- Test_map %>% 
    filter(Prefix == test_prefix) %>% 
    pull(Test_name)
  
ylabel <- Metric_map %>% 
    filter(Suffix == test_suffix) %>% 
    pull(Label)

SiteIDPlots[[cntr]] <- CNB_cross %>% 
    ggplot(aes(x = test_sessions.siteid,y = .data[[test]],color = test_sessions.siteid,fill = test_sessions.siteid)) + geom_point(size = 1.3,alpha = .4,position = position_jitter(seed = 1,width = .1)) + 
    geom_boxplot(alpha = .1,width = .3,outlier.shape = NA) + labs(x = "Site",y = ylabel,title = Plot_title) + 
    scale_color_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2") + theme(legend.position = "none") + 
    ggdist::stat_halfeye(justification = 1.25,.width = 0,point_colour = NA,side = "left",adjust = .6) 
  cntr <- cntr + 1
}

# Look at repeated measures to examine effect of being remote on test scores

# Start by age regressing scores 

# for(test in response_vars){
# 
# f <- as.formula(paste0(test,"~test_sessions_v.age+I(test_sessions_v.age^2)"))
# mod <- lm(f,data = CNB)
# CNB[as.numeric(names(mod$residuals)),paste0(test,"_AR")] <- mod$residuals
# 
# }
# 
# Longitudinal_bblids <- CNB %>% 
#   group_by(test_sessions.bblid) %>% 
#   summarize(n = n()) %>% 
#   filter(n != 1) %>% 
#   pull(test_sessions.bblid)

# Run mixed-effects models to see if being remote is a significant predictor of test score
# coef.mem <- data.frame("Test" = rep(NA,length = length(response_vars)),"Coefficient" = rep(NA,length = length(response_vars)),"t_value" = rep(NA,length = length(response_vars)),"N_remote" = rep(NA,length = length(response_vars)))
# cntr <- 1
# for(test in response_vars){
#   N_remote <- CNB[which(!is.na(CNB[,test])),] %>% 
#     with(sum(remote == "Remote"))
#   if(N_remote == 0){
#     coef.mem$N_remote[cntr] <- N_remote
#     coef.mem$Test[cntr] <- test
#     cntr <- cntr + 1
#     next
#   } else{
#     f <- as.formula(paste0(test,"~test_sessions_v.age + I(test_sessions_v.age^2) + I(test_sessions_v.age^3) + remote + (1 | test_sessions.bblid)"))
#     mem <- lmer(f,data = CNB)
#     coef.mem$Test[cntr] <- test
#     coef.mem$Coefficient[cntr] <- round(as.numeric(coef(summary(mem))["remoteRemote","Estimate"]),2)
#     coef.mem$t_value[cntr] <- round(as.numeric(coef(summary(mem))["remoteRemote","t value"]),2)
#     coef.mem$N_remote[cntr] <- N_remote
#     cntr <- cntr + 1
#   }
# }

# Plot differences between Remote and In-person scores for those who have them

# response_vars_AR <- paste0(response_vars,"_AR")
# Remote_Longitudinal_Plots <- list()
# cntr <- 1
# for(test in response_vars_AR){
#   unique_locations <- CNB[,c("remote",test)] %>% 
#     filter(across(.cols = everything(),.fns = ~ !is.na(.))) %>% 
#     with(unique(remote))
#   if(length(unique_locations) == 1){
#     next
#   } else{
#     test_var <- quo(!!sym(test))
#     Score_Change_summary <- CNB %>% 
#       filter(test_sessions.bblid %in% Longitudinal_bblids) %>% 
#       select(test_sessions.bblid,test_sessions_v.age,remote,contains("_AR")) %>% 
#       group_by(remote,test_sessions.bblid) %>% 
#       summarise(Avg = mean(!!test_var,na.rm = TRUE)) %>% 
#       ungroup() %>% 
#       filter(!is.na(Avg)) %>%
#       group_by(test_sessions.bblid) %>% 
#       filter(n() > 1) %>% 
#       ungroup() %>%
#       group_by(remote) %>% 
#       summarize(Mean = mean(Avg,na.rm = TRUE)) %>% 
#       ungroup()
#     
#     Remote_Longitudinal_Plots[[cntr]] <-  CNB %>% 
#       filter(test_sessions.bblid %in% Longitudinal_bblids) %>% 
#       select(test_sessions.bblid,test_sessions_v.age,remote,contains("_AR")) %>% 
#       group_by(remote,test_sessions.bblid) %>% 
#       summarize(Avg = mean(!!test_var,na.rm = TRUE)) %>% 
#       ungroup() %>% 
#       filter(!is.na(Avg)) %>% 
#       group_by(test_sessions.bblid) %>% 
#       filter(n() > 1) %>% 
#       ungroup() %>% 
#       ggplot(aes(x = remote,y = Avg)) + geom_point(size = .25,color = "darkred") + geom_line(aes(group = test_sessions.bblid),color = "darkred",alpha = .2) + labs(x = "Test Location",y = "Average Test Score \n (age-regressed)",title = test) + geom_point(data = Score_Change_summary,aes(x = remote,y = Mean),size = 1.5) + geom_line(data = Score_Change_summary,aes(x = remote,y = Mean,group = 1))
#     
#     cntr <- cntr + 1
#   }
# }

#ggarrange(Remote_Longitudinal_Plots[[3]],Remote_Longitudinal_Plots[[6]],Remote_Longitudinal_Plots[[10]],Remote_Longitudinal_Plots[[11]],labels = c("A","B","C","D"))


# Run  mixed effects models on the cross-sectional data 

# Start by examining reaction time

CNB_cross_RTCR <- CNB_cross %>% 
  select(test_sessions.bblid,test_sessions_v.age,test_sessions_v.gender,remote,matches("RTCR$"),matches("MRTC"),matches("TPRT")) %>% 
  select(!(matches("_AR$")|CPF_B.CPF_W_RTCR)) %>% 
  pivot_longer(cols = ADT36_A.ADT36A_RTCR:last_col(),names_to = "Test",values_to = "Reaction_time") %>% 
  rename(age = test_sessions_v.age) %>% 
  rename(gender = test_sessions_v.gender) %>% 
  mutate(age = as.numeric(scale(age,center = TRUE,scale = FALSE))) %>% 
  group_by(Test) %>% 
  mutate(Reaction_time = as.numeric(scale(Reaction_time))) %>% 
  ungroup() %>% 
  mutate(Test = str_remove_all(Test,pattern = "_.*")) %>% 
  mutate(Test = str_remove_all(Test,pattern = "\\..*")) 

RTCR_mod <- lmer(Reaction_time ~ gender + age + I(age^2) + remote + gender:(age + I(age^2)) + gender:remote + remote:(age + I(age^2)) + gender:remote:(age + I(age^2)) + (1|Test),data = CNB_cross_RTCR)
summary(RTCR_mod)

visreg(RTCR_mod,"age",by = "remote",cond = list(gender = "Female"), gg = T) + theme_minimal() + labs(x = "Age (centered at age = 18)",y = "Reaction Time",title = "Females") 
visreg(RTCR_mod,"age",by = "remote",cond = list(gender = "Male"), gg = T) + theme_minimal() + labs(x = "Age (centered at age = 18)",y = "Reaction Time",title = "Males") 

# Similar analysis for Correct responses

CNB_cross_CR <- CNB_cross %>% 
  select(test_sessions.bblid,test_sessions_v.age,test_sessions_v.gender,remote,matches("_CR$"),matches("_TP$"),matches("_MCR")) %>% 
  select(!(matches("_AR$"))) %>% 
  pivot_longer(cols = ADT36_A.ADT36A_CR:last_col(),names_to = "Test",values_to = "Correct_responses") %>% 
  rename(age = test_sessions_v.age) %>% 
  rename(gender = test_sessions_v.gender) %>% 
  mutate(age = as.numeric(scale(age,center = TRUE,scale = FALSE))) %>% 
  group_by(Test) %>% 
  mutate(Correct_responses = as.numeric(scale(Correct_responses))) %>% 
  ungroup() %>% 
  mutate(Test = str_remove_all(Test,pattern = "_.*")) %>% 
  mutate(Test = str_remove_all(Test,pattern = "\\..*")) 

CR_mod <- lmer(Correct_responses ~ gender + age + I(age^2) + remote + gender:(age + I(age^2)) + gender:remote + remote:(age + I(age^2)) + gender:remote:(age + I(age^2)) + (1|Test),data = CNB_cross_CR)
summary(CR_mod)


visreg(CR_mod,"age",by = "remote",cond = list(gender = "Female"), gg = T) + theme_minimal() + labs(x = "Age (centered at age = 18)",y = "Correct Responses",title = "Females") 
visreg(CR_mod,"age",by = "remote",cond = list(gender = "Male"), gg = T) + theme_minimal() + labs(x = "Age (centered at age = 18)",y = "Correct Responses",title = "Males") 


# Run Mixed Effect Models on the Longitudinal Data

CNB_long_RTCR <- CNB %>% 
  group_by(test_sessions.bblid) %>% 
  filter(n() > 1,sum(remote == "Remote") > 0,sum(remote == "In-person") > 0) %>% 
  ungroup() %>% 
  select(test_sessions.bblid,test_sessions_v.age,test_sessions_v.gender,remote,matches("RTCR$"),matches("MRTC"),matches("TPRT")) %>% 
  select(!(matches("_AR$")|CPF_B.CPF_W_RTCR)) %>% 
  pivot_longer(cols = ADT36_A.ADT36A_RTCR:last_col(),names_to = "Test",values_to = "Reaction_time") %>% 
  rename(age = test_sessions_v.age) %>% 
  rename(gender = test_sessions_v.gender) %>% 
  mutate(age = as.numeric(scale(age,center = TRUE,scale = FALSE))) %>% 
  group_by(Test) %>% 
  mutate(Reaction_time = as.numeric(scale(Reaction_time))) %>% 
  ungroup() %>% 
  mutate(Test = str_remove_all(Test,pattern = "_.*")) %>% 
  mutate(Test = str_remove_all(Test,pattern = "\\..*")) 

RTCR_mod_long <- lmer(Reaction_time ~ gender + age + I(age^2) + remote + gender:(age + I(age^2)) + gender:remote + remote:(age + I(age^2)) + gender:remote:(age + I(age^2)) + (1|Test) + (1|test_sessions.bblid),data = CNB_long_RTCR)
summary(RTCR_mod_long)

visreg(RTCR_mod_long,"age",by = "remote",cond = list(gender = "Female"), gg = T) + theme_minimal() + labs(x = "Age",y = "Reaction Time",title = "Females") 
visreg(RTCR_mod_long,"age",by = "remote",cond = list(gender = "Male"), gg = T) + theme_minimal() + labs(x = "Age",y = "Reaction Time",title = "Males") 

# Similar analysis for Correct responses

CNB_long_CR <- CNB %>% 
  group_by(test_sessions.bblid) %>% 
  filter(n() > 1,sum(remote == "Remote") > 0,sum(remote == "In-person") > 0) %>% 
  ungroup() %>% 
  select(test_sessions.bblid,test_sessions_v.age,test_sessions_v.gender,remote,matches("_CR$"),matches("_TP$"),matches("_MCR")) %>% 
  select(!matches("_AR$")) %>% 
  pivot_longer(cols = ADT36_A.ADT36A_CR:last_col(),names_to = "Test",values_to = "Correct_responses") %>% 
  rename(age = test_sessions_v.age) %>% 
  rename(gender = test_sessions_v.gender) %>% 
  mutate(age = as.numeric(scale(age,center = TRUE,scale = FALSE))) %>% 
  group_by(Test) %>% 
  mutate(Correct_responses = as.numeric(scale(Correct_responses))) %>% 
  ungroup() %>% 
  mutate(Test = str_remove_all(Test,pattern = "_.*")) %>% 
  mutate(Test = str_remove_all(Test,pattern = "\\..*"))

CR_mod_long <- lmer(Correct_responses ~ gender + age + I(age^2) + remote + gender:(age + I(age^2)) + gender:remote + remote:(age + I(age^2)) + gender:remote:(age + I(age^2)) + (1|Test) + (1|test_sessions.bblid),data = CNB_long_CR)
summary(CR_mod_long)

visreg(CR_mod_long,"age",by = "remote",cond = list(gender = "Female"), gg = T) + theme_minimal() + labs(x = "Age",y = "Correct Responses",title = "Females") 
visreg(CR_mod_long,"age",by = "remote",cond = list(gender = "Male"), gg = T) + theme_minimal() + labs(x = "Age",y = "Correct Responses",title = "Males") 

CNB_long_CR %>% 
  
  

pdf(file = "/Users/hillmann/Projects/22q/Results/Sex&remoteEffects22qplots.pdf",width = 14,height = 8)  

LongitudinalPlots[[1]]
LongitudinalPlots[[2]]
LongitudinalPlots[[3]]
LongitudinalPlots[[4]]
LongitudinalPlots[[5]]
LongitudinalPlots[[6]]
LongitudinalPlots[[7]]
LongitudinalPlots[[8]]
LongitudinalPlots[[9]]
LongitudinalPlots[[10]]
LongitudinalPlots[[11]]
LongitudinalPlots[[12]]
LongitudinalPlots[[13]]
LongitudinalPlots[[14]]
LongitudinalPlots[[15]]
LongitudinalPlots[[16]]
LongitudinalPlots[[17]]
LongitudinalPlots[[18]]
LongitudinalPlots[[19]]
LongitudinalPlots[[20]]
LongitudinalPlots[[21]]
LongitudinalPlots[[22]]
LongitudinalPlots[[23]]
LongitudinalPlots[[24]]
LongitudinalPlots[[25]]
LongitudinalPlots[[26]]

SiteIDPlots[[1]]
SiteIDPlots[[2]]
SiteIDPlots[[3]]
SiteIDPlots[[4]]
SiteIDPlots[[5]]
SiteIDPlots[[6]]
SiteIDPlots[[7]]
SiteIDPlots[[8]]
SiteIDPlots[[9]]
SiteIDPlots[[10]]
SiteIDPlots[[11]]
SiteIDPlots[[12]]
SiteIDPlots[[13]]
SiteIDPlots[[14]]
SiteIDPlots[[15]]
SiteIDPlots[[16]]
SiteIDPlots[[17]]
SiteIDPlots[[18]]
SiteIDPlots[[19]]
SiteIDPlots[[20]]
SiteIDPlots[[21]]
SiteIDPlots[[22]]
SiteIDPlots[[23]]
SiteIDPlots[[24]]
SiteIDPlots[[25]]
SiteIDPlots[[26]]
dev.off()



