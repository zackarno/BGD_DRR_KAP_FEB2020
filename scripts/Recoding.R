
# library -----------------------------------------------------------------

library(dplyr)
library(anytime)
library(lubridate)
library(AMR)
library(forcats) 


# read_data ---------------------------------------------------------------


drr_data<- read.csv("Input/24022020_DRR_KAP_Clean_data.csv",stringsAsFactors = FALSE,header = T ,na.strings = c(""," ", "n/a",NA)) 

data<- drr_data %>% select(matches("X_14|X_17|X_18|X_39|X_39a|X_40|X_40a|X_41|X_41a")) %>% colnames()

drr_data[data]<- as.data.frame(sapply(drr_data[data], as.numeric))



# variables ---------------------------------------------------------------


cyclone_data_all<- drr_data %>% select(starts_with("X_14")) %>% select(-c("X_14_Can_you_tell_us_ccur_during_the_year" ,"X_14_Can_you_tell_us_ccur_during_the_year.don_t_know"))%>% 
  colnames()%>% dput()


cyclone_data<- drr_data %>% dplyr::select(matches("year.april|year.may|year.october|year.november")) %>% colnames() %>% dput()

danger_and_risk_colnames<-drr_data %>% select(starts_with("X_17")) %>% select(-c( "X_17_What_dangers_and_risks_are",
                                                                                  "X_17_What_dangers_and_risks_are.don_t_know")) %>% colnames() %>% dput()


problem_after_cyclone<- drr_data %>% select(starts_with("X_18_What_dangers_problems")) %>% select(-c( "X_18_What_dangers_problems_can_",
                                                                                                      "X_18_What_dangers_problems_can_.don_t_know")) %>%  colnames() %>% dput()



actions_before_cyclone_hc<- drr_data %>% select(starts_with("X_18_What_actions")) %>% select(-c("X_18_What_actions_would_you_ta",
                                                                                                "X_18_What_actions_would_you_ta.don_t_know")) %>% colnames() %>% dput 

actions_before_cyclone_rc<- drr_data %>% select(starts_with("X_18_What_actions")) %>% select(-c("X_18_What_actions_would_you_ta",
                                                                                                "X_18_What_actions_would_you_ta.don_t_know","X_18_What_actions_would_you_ta.go_to_a_cyclone_shelter")) %>% colnames() %>% dput

actions_before_cyclone_rc_withot_dntknw_go <- c("X_18_What_actions_would_you_ta.don_t_know",
                                                "X_18_What_actions_would_you_ta.go_to_a_cyclone_shelter")


one_flag_meaning_right<- c("X_39_What_does_it_mean_when_on.a_storm_is_near__it_may_not_re",
                           "X_39_What_does_it_mean_when_on.if_the_storm_moves_away_or_wea")

one_flag_meaning_wrong<- drr_data %>% select(starts_with("X_39_What_does_it_mean")) %>% select(-c("X_39_What_does_it_mean_when_on.a_storm_is_near__it_may_not_re",
                                                                                                  "X_39_What_does_it_mean_when_on.if_the_storm_moves_away_or_wea",
                                                                                                  "X_39_What_does_it_mean_when_on")) %>% colnames() %>% dput()


action_one_flag_hc<- drr_data %>% select(starts_with("X_39a")) %>% select(-c("X_39a_What_are_you_supposed_to",
                                                                             "X_39a_What_are_you_supposed_to.don_t_know")) %>%  colnames() %>% dput()

action_one_flag_rc<- drr_data %>% select(starts_with("X_39a")) %>% select(-c("X_39a_What_are_you_supposed_to",
                                                                             "X_39a_What_are_you_supposed_to.go_to_a_cyclone_shelter",
                                                                             "X_39a_What_are_you_supposed_to.don_t_know")) %>%  colnames() %>% dput()
actions_one_flag_rc_withot_dntknw_go <- c("X_39a_What_are_you_supposed_to.go_to_a_cyclone_shelter",
                                          "X_39a_What_are_you_supposed_to.don_t_know")

two_flag_meaning_right<- c( "X_40_What_does_it_mean_when_tw.a_storm_is_almost_here__it_wil","X_40_What_does_it_mean_when_tw.there_may_be_heavy_rain_and_st")

two_flag_meaning_wrong<- drr_data %>% select(starts_with("X_40_What_does_it_mean")) %>% select(-c( "X_40_What_does_it_mean_when_tw.a_storm_is_almost_here__it_wil",
                                                                                                   "X_40_What_does_it_mean_when_tw.there_may_be_heavy_rain_and_st",
                                                                                                   "X_40_What_does_it_mean_when_tw")) %>%  colnames() %>% dput()

action_two_flag_hc<-drr_data %>% select(starts_with("X_40a")) %>% select(-c("X_40a_What_are_you_supposed_to",
                                                                            "X_40a_What_are_you_supposed_to.don_t_know")) %>%  colnames() %>% dput()
action_two_flag_rc<- drr_data %>% select(starts_with("X_40a")) %>% select(-c("X_40a_What_are_you_supposed_to",
                                                                             "X_40a_What_are_you_supposed_to.don_t_know",
                                                                             "X_40a_What_are_you_supposed_to.go_to_a_cyclone_shelter")) %>%  colnames() %>% dput()
actions_two_flag_rc_withot_dntknw_go <- c("X_40a_What_are_you_supposed_to.don_t_know",
                                          "X_40a_What_are_you_supposed_to.go_to_a_cyclone_shelter")

three_flag_meaning_right<- c("X_41_What_does_it_mean_when_th.you_will_experience_a_severe_w","X_41_What_does_it_mean_when_th.sirens_will_sound_to_give_warn")

three_flag_meaning_wrong<- drr_data %>% select(starts_with("X_41_What_does_it_mean")) %>% select(-c("X_41_What_does_it_mean_when_th.you_will_experience_a_severe_w",
                                                                                                    "X_41_What_does_it_mean_when_th.sirens_will_sound_to_give_warn",
                                                                                                    "X_41_What_does_it_mean_when_th")) %>% colnames() %>% dput()


action_three_flag_hc<- drr_data %>% select(starts_with("X_41a")) %>% select(-c("X_41a_What_are_you_supposed_to" ,
                                                                               "X_41a_What_are_you_supposed_to.don_t_know")) %>%  colnames() %>% dput()

action_three_flag_rc<- drr_data %>% select(starts_with("X_41a")) %>% select(-c("X_41a_What_are_you_supposed_to" ,
                                                                               "X_41a_What_are_you_supposed_to.don_t_know",
                                                                               "X_41a_What_are_you_supposed_to.go_to_a_cyclone_shelter")) %>%  colnames() %>% dput()
actions_three_flag_rc_withot_dntknw_go <- c("X_41a_What_are_you_supposed_to.don_t_know",
                                            "X_41a_What_are_you_supposed_to.go_to_a_cyclone_shelter")

# recoding ----------------------------------------------------------------


final_data<- drr_data %>% mutate(
  cyclone_data_all_rs= rowSums(drr_data[,cyclone_data_all],na.rm = T),
  cyclone_data_rs= rowSums(drr_data[,cyclone_data],na.rm = T),
  i.time_of_cyclones= if_else(cyclone_data_all_rs==4 & cyclone_data_rs== 4,"yes","no",NULL),
  danger_and_risk_rs= rowSums(drr_data[,danger_and_risk_colnames],na.rm = T),
  i.danger_risk_during_cyclone= if_else(danger_and_risk_rs>0 & X_17_What_dangers_and_risks_are.don_t_know==0  ,"yes","no",NULL),
  problem_after_cyclone_rs= rowSums(drr_data[,problem_after_cyclone],na.rm = T),
  i.danger_problem_after_cyclone= if_else(problem_after_cyclone_rs>0 & X_18_What_dangers_problems_can_.don_t_know==0  ,"yes","no",NULL),
  actions_before_cyclone_hc_rs=  rowSums(drr_data[,actions_before_cyclone_hc],na.rm = T),
  i.HC_action_taken_pre_cyclone= if_else(drr_data$X_3_Survey_category=="refugee_commun","",
                                         if_else(actions_before_cyclone_hc_rs>0 & X_18_What_actions_would_you_ta.don_t_know==0, "yes","no",NULL)),
  actions_before_cyclone_rc_rs=  rowSums(drr_data[,actions_before_cyclone_rc],na.rm = T),
  actions_before_cyclone_rc_withot_dntknw_go_rs= rowSums(drr_data[,actions_before_cyclone_rc_withot_dntknw_go],na.rm = T),
  i.refugee_action_taken_pre_cyclone= if_else(drr_data$X_3_Survey_category=="host_community","",
                                              if_else(actions_before_cyclone_rc_rs>0 & actions_before_cyclone_rc_withot_dntknw_go_rs==0, "yes","no",NULL)),
  i.action_dnt_know_pre_cyclone= if_else(X_18_What_actions_would_you_ta.don_t_know==1, "yes","no",NULL),
  i.action_go_to_shelter_pre_cyclone= if_else( X_18_What_actions_would_you_ta.go_to_a_cyclone_shelter==1 , "yes","no",NULL),
  one_flag_meaning_right_rs= rowSums(drr_data[,one_flag_meaning_right],na.rm = T),
  one_flag_meaning_wrong_rs= rowSums(drr_data[,one_flag_meaning_wrong],na.rm = T),
  i.one_flag_definition= if_else(one_flag_meaning_right_rs>0 & one_flag_meaning_wrong_rs==0, "yes","no",NULL),
  action_one_flag_hc_rs= rowSums(drr_data[,action_one_flag_hc],na.rm = T),
  i.HC_action_one_flag= if_else(drr_data$X_3_Survey_category=="refugee_commun","",
                                if_else(action_one_flag_hc_rs>0 & X_39a_What_are_you_supposed_to.don_t_know==0, "yes","no",NULL)),
  action_one_flag_rc_rs= rowSums(drr_data[,action_one_flag_rc],na.rm = T),
  actions_one_flag_rc_withot_dntknw_go_rs= rowSums(drr_data[,actions_one_flag_rc_withot_dntknw_go],na.rm = T),
  i.refugee_action_one_flag = if_else(drr_data$X_3_Survey_category=="host_community","",
                                      if_else(action_one_flag_rc_rs>0 & actions_one_flag_rc_withot_dntknw_go_rs==0, "yes","no",NULL)),
  i.action_dnt_know_one_flag= if_else(X_39a_What_are_you_supposed_to.don_t_know==1, "yes","no",NULL),
  i.action_go_to_shelter_one_flag= if_else( X_39a_What_are_you_supposed_to.go_to_a_cyclone_shelter==1, "yes","no",NULL),
  two_flag_meaning_right_rs= rowSums(drr_data[,two_flag_meaning_right],na.rm = T),
  two_flag_meaning_wrong_rs= rowSums(drr_data[,two_flag_meaning_wrong],na.rm = T),
  i.two_flag_definition= if_else(two_flag_meaning_right_rs>0 & two_flag_meaning_wrong_rs==0, "yes","no",NULL),
  action_two_flag_hc_rs=  rowSums(drr_data[,action_two_flag_hc],na.rm = T),
  i.HC_action_two_flag= if_else(drr_data$X_3_Survey_category=="refugee_commun","",
                                if_else(action_two_flag_hc_rs>0 & X_40a_What_are_you_supposed_to.don_t_know==0, "yes","no",NULL)),
  action_two_flag_rc_rs=  rowSums(drr_data[,action_two_flag_rc],na.rm = T),
  actions_two_flag_rc_withot_dntknw_go_rs= rowSums(drr_data[,actions_two_flag_rc_withot_dntknw_go],na.rm = T),
  i.refugee_action_two_flag= if_else(drr_data$X_3_Survey_category=="host_community","",
                                     if_else(action_two_flag_rc_rs>0 & actions_two_flag_rc_withot_dntknw_go_rs==0, "yes","no",NULL)),
  i.action_dnt_know_two_flag= if_else(X_40a_What_are_you_supposed_to.don_t_know==1, "yes","no",NULL),
  i.action_go_to_shelter_two_flag= if_else( X_40a_What_are_you_supposed_to.go_to_a_cyclone_shelter==1, "yes","no",NULL),
  three_flag_meaning_right_rs= rowSums(drr_data[,three_flag_meaning_right],na.rm = T),
  three_flag_meaning_wrong_rs= rowSums(drr_data[,three_flag_meaning_wrong],na.rm = T),
  i.three_flag_definition= if_else(three_flag_meaning_right_rs>0 & three_flag_meaning_wrong_rs==0, "yes","no",NULL),
  
  action_three_flag_hc_rs=  rowSums(drr_data[,action_three_flag_hc],na.rm = T),
  i.HC_action_three_flag= if_else(drr_data$X_3_Survey_category=="refugee_commun","",
                                  if_else(action_three_flag_hc_rs>0 &X_41a_What_are_you_supposed_to.don_t_know==0, "yes","no",NULL)),
  action_three_flag_rc_rs=  rowSums(drr_data[,action_three_flag_rc],na.rm = T),
  actions_three_flag_rc_withot_dntknw_go_rs= rowSums(drr_data[,actions_three_flag_rc_withot_dntknw_go],na.rm = T),
  i.refugee_action_three_flag= if_else(drr_data$X_3_Survey_category=="host_community","",
                                       if_else(action_three_flag_rc_rs>0 & actions_three_flag_rc_withot_dntknw_go_rs==0, "yes","no",NULL)),
  i.action_dnt_know_three_flag = if_else(X_41a_What_are_you_supposed_to.don_t_know==1, "yes","no",NULL),
  i.action_go_to_shelter_three_flag= if_else(X_41a_What_are_you_supposed_to.go_to_a_cyclone_shelter==1, "yes","no",NULL)
  
)

