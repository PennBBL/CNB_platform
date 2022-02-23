# Aki's copy of Noah's code for longitudinal analysis spaghetti plots
# Load in necessary data and scripts

library(tidyverse)
CNB_mega <- read_csv("~/Projects/CNB_Longitudinal/Data/cnb_merged_20220201.csv")
CNB_9498 <- read_csv("~/Projects/CNB_Longitudinal/Data/n9498_cnb_zscores_fr_20170202.csv")
PS <- read_csv("~/Projects/CNB_Longitudinal/Data/n9498_diagnosis_dxpmr7_20170509.csv")
codebook <- read_csv("~/Projects/CNB_Longitudinal/Data/bbl_cross_battery_codebook.csv")

# Create a longitudinal data set using individuals who were members of the PNC 9498 cohort
CNB_9498_bblids <- CNB_9498 %>% 
  with(unique(bblid))

CNB_long_bblids <- CNB_mega %>% 
  filter(test_sessions.bblid.clean %in% CNB_9498_bblids) %>% 
  group_by(test_sessions.bblid.clean) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  filter(n > 1) %>% 
  pull(test_sessions.bblid.clean)

CNB_long <- CNB_mega %>% 
  filter(test_sessions.bblid.clean %in% CNB_long_bblids) %>% 
  left_join(PS,by = c("test_sessions.bblid.clean" = "bblid")) %>% 
  rename(PS = goassessDxpmr7) %>% 
  select(test_sessions.bblid.clean,test_sessions_v.age,test_sessions_v.gender,PS,matches("_cr$"),matches("_rtcr$"),tap_tot,pcet_acc2,cpt_tprt,cpt_ptp,lnb_mcr,lnb_mrtc,pmat_pc,medf_pc,adt_pc,plot_pc) %>% 
  select(!(matches("_w_rtcr$")))

# Use codebook to build data frame which maps test acronyms to test names
Test_map <- data.frame('Prefix' = c("er40","pvrt","volt","cpf","cpw","gng","mpraxis","pcet","pmat","medf","adt","plot","tap","cpt","lnb"),
"Test_name" = c("Penn Emotion Recognition Test","Penn Verbal Reasoning Test","Visual Object Learning Test","Penn Face Memory Test",'Penn Word Memory Test',"Go-No-Go Test","Motor Praxis Test","Penn Conditional Exclusion Test","Penn Matrix Analysis Test","Measured Emotion Differentiation Test","Age Differentiation Test","Penn Line Orientation Test","Penn Computerized Finger Tapping Test",
                "Penn Continuous Performance Test","Letter-N-Back Test"))

# Use codebook to map measurements to longer names
Metric_map <- data.frame("Suffix" = c("_cr","_rtcr","_tot","_acc2","_tprt","_ptp","_mcr","_mrtc","_pc"),
"Label" = c("Correct Responses","Median Reaction Time \n Correct Responses (ms)","Total Taps","Accuracy",
            "Median Response Time \n True Positives (ms)","True Positive (%)","Total True Positive Responses","Median Response Time \n Correct Responses","Correct Responses (%)"))


# Create Longitudinal Plots for each test 

response_cols <- CNB_long %>% 
  select(er40_cr:last_col()) %>% 
  colnames()
CNB_long_plots <- list()
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

# Cap values at 6sd above the norm
CNB_long[[test]] <- ifelse(CNB_long[[test]] > mean(CNB_long[[test]],na.rm = T) + 6*sd(CNB_long[[test]],na.rm = T),mean(CNB_long[[test]],na.rm = T) + 6*sd(CNB_long[[test]],na.rm = T),CNB_long[[test]])

# Find N for each test

Sex_N_timepoints <- CNB_long %>% 
  filter(!is.na(!!test_quo),!is.na(test_sessions_v.gender)) %>% 
  nrow()

Sex_N_subj <-  CNB_long %>% 
  filter(!is.na(!!test_quo),!is.na(test_sessions_v.gender)) %>% 
  with(length(unique(test_sessions.bblid.clean)))

PS_N_timepoints <- CNB_long %>% 
  filter(!is.na(!!test_quo),!is.na(PS)) %>% 
  nrow()

PS_N_subj <-  CNB_long %>% 
  filter(!is.na(!!test_quo),!is.na(PS)) %>% 
  with(length(unique(test_sessions.bblid.clean)))
  
Sex_PS_N_timepoints <- CNB_long %>% 
  filter(!is.na(!!test_quo),!is.na(PS),!is.na(test_sessions_v.gender)) %>% 
  nrow()

Sex_PS_N_subj <-  CNB_long %>% 
  filter(!is.na(!!test_quo),!is.na(PS),!is.na(test_sessions_v.gender)) %>% 
  with(length(unique(test_sessions.bblid.clean)))


# Create three plots for sex, PS, and sex by PS effects

df_for_plot <- CNB_long %>% 
  rename(Sex = test_sessions_v.gender) %>% 
  rename(Age = test_sessions_v.age) %>% 
  rename(bblid = test_sessions.bblid.clean) %>% 
  mutate(Sex = ifelse(Sex == "M","Male","Female")) %>% 
  mutate(PS = factor(PS,levels = c("TD","OP","PS"))) %>% 
  filter(!is.na(Sex),!is.na(PS)) %>% 
  mutate(Sex_PS = paste(Sex,PS)) %>% 
  mutate(Sex_PS = factor(Sex_PS,levels = c("Male TD","Male OP","Male PS",'Female TD',"Female OP","Female PS")))

theme_set(theme_minimal())

Plot_sep_sex <- df_for_plot %>% 
  ggplot(aes(x = Age,y = !!test_quo,color = Sex,group = bblid)) + geom_smooth(aes(x = Age,y = !!test_quo,color = Sex,group = Sex)) + geom_point(size = .1) + geom_line(alpha = .15) + labs(x = "Age",y = ylabel,title = "",caption = paste0("N = ",Sex_N_subj,", Timepoints = ",Sex_N_timepoints)) + scale_color_manual(values = c("#d7191c","#2b83ba")) 

Plot_sep_PS <- df_for_plot %>% 
  ggplot(aes(x = Age,y = !!test_quo,color = PS,group = bblid)) + geom_smooth(aes(x = Age,y = !!test_quo,color = PS,group = PS),se = F) + geom_point(size = .1) + geom_line(alpha = .15) + labs(x = "Age",y = ylabel,title = "",caption = paste0("N = ",PS_N_subj,", Timepoints = ",PS_N_timepoints),color = "") + theme(legend.position = "bottom") + scale_color_brewer(palette = "Dark2") 
  
Plot_sep_sex_PS <- df_for_plot %>% 
  ggplot(aes(x = Age,y = !!test_quo,color = Sex_PS,group = bblid)) + geom_smooth(aes(x = Age,y = !!test_quo,color = Sex_PS,group = Sex_PS),se = F,alpha = 2) + geom_point(size = .1) + geom_line(alpha = .15) + labs(x = "Age",y = ylabel,title = "",caption = paste0("N = ",Sex_PS_N_subj,", Timepoints = ",Sex_PS_N_timepoints),color = "") + 
  theme(legend.position = "bottom") + scale_color_manual(values = c("#6baed6","#2171b5","#08306b","#fc9272","#cb181d","#67000d")) + guides(color = guide_legend(nrow = 1))


CNB_long_plots[[cntr]] <- annotate_figure(ggarrange(Plot_sep_sex,Plot_sep_PS,Plot_sep_sex_PS,labels = c("A","B","C")),top = text_grob(Plot_title,size = 16,face = "bold"))

cntr <- cntr + 1
}

# Plots for Presentation & pdf

pdf(file = "/Users/hillmann/Projects/CNB_Longitudinal/Results/SexByAgeByPSPlots.pdf",width = 14,height = 8)
CNB_long_plots[[1]]
CNB_long_plots[[2]]
CNB_long_plots[[3]]
CNB_long_plots[[4]]
CNB_long_plots[[5]]
CNB_long_plots[[6]]
CNB_long_plots[[7]]
CNB_long_plots[[8]]
CNB_long_plots[[9]]
CNB_long_plots[[10]]
CNB_long_plots[[11]]
CNB_long_plots[[12]]
CNB_long_plots[[13]]
CNB_long_plots[[14]]
CNB_long_plots[[15]]
CNB_long_plots[[16]]
CNB_long_plots[[17]]
CNB_long_plots[[18]]
CNB_long_plots[[19]]
CNB_long_plots[[20]]
CNB_long_plots[[21]]
CNB_long_plots[[22]]
CNB_long_plots[[23]]
CNB_long_plots[[24]]
CNB_long_plots[[25]]
CNB_long_plots[[26]]
CNB_long_plots[[27]]
CNB_long_plots[[28]]
dev.off()
