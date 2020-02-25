
# library -----------------------------------------------------------------

library(dplyr)
library(AMR)
library(readr)

# Read_data ---------------------------------------------------------------

data_path <- "Input/24022020_DRR_KAP_Clean_data.csv"
data <- read.csv(data_path)


# precondition ----------------------------------------------------------------

sum_all_cyclone <- data %>% select(starts_with("X_14_Can_you_tell_us_ccur_during_the_year.")) %>% select (-starts_with("X_14_Can_you_tell_us_ccur_during_the_year.don_t")) %>% colnames() %>% dput
sum_april_may_oct_nov <- data %>% select(matches("year.april|year.may|year.october|year.november")) %>% colnames() %>% dput

sum_dngr_rsk_during_all_without_dnt_knw <- data %>% select(starts_with("X_17_What_dangers_and_risks_are.")) %>% select (-matches("X_17_What_dangers_and_risks_are.don_t_know")) %>% colnames() %>% dput
sum_dngr_rsk_during_dnt_knw <- data %>% select (matches("X_17_What_dangers_and_risks_are.don_t_know")) %>% colnames() %>% dput

sum_what_dangr_after_cyclone_without_dnt_knw <- data %>% select(starts_with("X_18_What_dangers_problems_can_.")) %>% select (-matches("X_18_What_dangers_problems_can_.don_t_know")) %>% colnames() %>% dput


sum_hc_what_action_pre_cyclone_without_dont_knw <- data %>% select(contains("X_18_What_actions_would_you_ta.")) %>% 
  select(-matches("X_18_What_actions_would_you_ta.don_t_know")) %>% colnames() %>% dput

sum_ref_what_action_pre_cyclone_without_dont_knw_and_go_to_cyln <- data %>% select(contains("X_18_What_actions_would_you_ta.")) %>% 
  select(-c("X_18_What_actions_would_you_ta.don_t_know","X_18_What_actions_would_you_ta.go_to_a_cyclone_shelter")) %>% colnames() %>% dput


one_flag_correct <- c("X_39_What_does_it_mean_when_on.a_storm_is_near__it_may_not_re", 
                      "X_39_What_does_it_mean_when_on.if_the_storm_moves_away_or_wea")
one_flag_uncorrect<- data %>% select(contains("X_39_What_does_it_mean_when_on.")) %>%
  select(-one_flag_correct) %>% colnames() %>% dput

two_flag_correct <- c("X_40_What_does_it_mean_when_tw.a_storm_is_almost_here__it_wil", 
                      "X_40_What_does_it_mean_when_tw.there_may_be_heavy_rain_and_st")
two_flag_uncorrect<- data %>% select(contains("X_40_What_does_it_mean_when_tw.")) %>%
  select(-two_flag_correct) %>% colnames() %>% dput

three_flag_correct <- c("X_41_What_does_it_mean_when_th.you_will_experience_a_severe_w", 
                        "X_41_What_does_it_mean_when_th.sirens_will_sound_to_give_warn")
three_flag_uncorrect<- data %>% select(contains("X_41_What_does_it_mean_when_th.")) %>%
  select(-three_flag_correct) %>% colnames() %>% dput


sum_hc_supp_to_do_one_flg_without_dont_knw <- data %>% select(contains("X_39a_What_are_you_supposed_to.")) %>% 
  select(-matches("X_39a_What_are_you_supposed_to.don_t_know")) %>% colnames() %>% dput

sum_ref_supp_to_do_one_flg_without_dont_knw_and_go_to_cyln <- data %>% select(contains("X_39a_What_are_you_supposed_to.")) %>% 
  select(-c("X_39a_What_are_you_supposed_to.don_t_know","X_39a_What_are_you_supposed_to.go_to_a_cyclone_shelter")) %>% colnames() %>% dput


sum_hc_supp_to_do_two_flg_without_dont_knw <- data %>% select(contains("X_40a_What_are_you_supposed_to.")) %>% 
  select(-matches("X_40a_What_are_you_supposed_to.don_t_know")) %>% colnames() %>% dput

sum_ref_supp_to_do_two_flg_without_dont_knw_and_go_to_cyln <- data %>% select(contains("X_40a_What_are_you_supposed_to.")) %>% 
  select(-c("X_40a_What_are_you_supposed_to.don_t_know","X_40a_What_are_you_supposed_to.go_to_a_cyclone_shelter")) %>% colnames() %>% dput


sum_hc_supp_to_do_three_flg_without_dont_knw <- data %>% select(contains("X_41a_What_are_you_supposed_to.")) %>% 
  select(-matches("X_41a_What_are_you_supposed_to.don_t_know")) %>% colnames() %>% dput

sum_ref_supp_to_do_three_flg_without_dont_knw_and_go_to_cyln <- data %>% select(contains("X_41a_What_are_you_supposed_to.")) %>% 
  select(-c("X_41a_What_are_you_supposed_to.don_t_know","X_41a_What_are_you_supposed_to.go_to_a_cyclone_shelter")) %>% colnames() %>% dput



# Recoding ----------------------------------------------------------------

final_data <- data %>% mutate(
  cyclone_all_month_rowsum = rowSums(data[sum_all_cyclone],na.rm = T),
  cyclone_period_correct_rowsum = rowSums(data[sum_april_may_oct_nov],na.rm = T),
  i.time_of_cyclones = if_else(cyclone_all_month_rowsum == 4 & cyclone_period_correct_rowsum ==4 , "yes","no",NULL),
  
  dan_rsk_drng_cycln_all_rowsum= rowSums(data[sum_dngr_rsk_during_all_without_dnt_knw],na.rm = T),
  dan_rsk_drng_cycln_dnt_knw_rowsum = rowSums(data[sum_dngr_rsk_during_dnt_knw],na.rm = T),
  i.danger_risk_during_cyclone = if_else(dan_rsk_drng_cycln_all_rowsum >0,"yes","no",NULL),
  
  what_dngr_aftr_cycln_without_dnt_knw_rowsum = rowSums(data[sum_what_dangr_after_cyclone_without_dnt_knw],na.rm = T),
  i.danger_problem_after_cyclone = if_else(what_dngr_aftr_cycln_without_dnt_knw_rowsum > 0,"yes","no",NULL),
  
  hc.what_action_pre_cyclone_without_dont_knw_rowsum = rowSums(data[sum_hc_what_action_pre_cyclone_without_dont_knw],na.rm = T),
  ref.what_action_pre_cyclone_without_dont_knw_and_go_cycln_center_rowsum = rowSums(data[sum_ref_what_action_pre_cyclone_without_dont_knw_and_go_to_cyln],na.rm = T),
  i.HC_action_taken_pre_cyclone = if_else(X_3_Survey_category == "refugee_commun", "",
                                          if_else(hc.what_action_pre_cyclone_without_dont_knw_rowsum > 0,"yes","no",NULL)),
  i.refugee_action_taken_pre_cyclone = if_else(X_3_Survey_category == "host_community", "",
                                               if_else(ref.what_action_pre_cyclone_without_dont_knw_and_go_cycln_center_rowsum > 0,"yes","no",NULL)),
  i.action_dnt_know_pre_cyclone = if_else(X_18_What_actions_would_you_ta.don_t_know == 1,"yes","No",NULL),
  i.action_go_to_shelter_pre_cyclone = if_else(X_18_What_actions_would_you_ta.go_to_a_cyclone_shelter == 1,"yes","No",NULL),
  
  one_flag_uncorrect_rowsum = rowSums(data[one_flag_uncorrect],na.rm = T), 
  one_flag_correct_rowsum = rowSums(data[one_flag_correct],na.rm = T),
  i.one_flag_definition = if_else(one_flag_correct_rowsum > 0 & one_flag_uncorrect_rowsum == 0, "yes","no",NULL),
  
  two_flag_uncorrect_rowsum = rowSums(data[two_flag_uncorrect],na.rm = T), 
  two_flag_correct_rowsum = rowSums(data[two_flag_correct],na.rm = T),
  i.two_flag_definition = if_else(two_flag_correct_rowsum > 0 & two_flag_uncorrect_rowsum == 0, "yes","no",NULL),
  
  
  three_flag_uncorrect_rowsum = rowSums(data[three_flag_uncorrect],na.rm = T), 
  three_flag_correct_rowsum = rowSums(data[three_flag_correct],na.rm = T),
  i.three_flag_definition = if_else(three_flag_correct_rowsum > 0 & three_flag_uncorrect_rowsum == 0, "yes","no",NULL),
  
  
  hc.supp_to_do_one_flg_without_dont_knw_rowsum = rowSums(data[sum_hc_supp_to_do_one_flg_without_dont_knw],na.rm = T),
  ref.supp_to_do_one_flg_without_dont_knw_and_go_cycln_center_rowsum = rowSums(data[sum_ref_supp_to_do_one_flg_without_dont_knw_and_go_to_cyln],na.rm = T),
  i.HC_action_one_flag = if_else(X_3_Survey_category == "refugee_commun", "",
                                          if_else(hc.supp_to_do_one_flg_without_dont_knw_rowsum > 0,"yes","no",NULL)),
  i.refugee_action_one_flag = if_else(X_3_Survey_category == "host_community", "",
                                               if_else(ref.supp_to_do_one_flg_without_dont_knw_and_go_cycln_center_rowsum > 0,"yes","no",NULL)),
  i.action_dnt_know_one_flag = if_else(X_39a_What_are_you_supposed_to.don_t_know == 1,"yes","No",NULL),
  i.action_go_to_shelter_one_flag = if_else(X_39a_What_are_you_supposed_to.go_to_a_cyclone_shelter == 1,"yes","No",NULL),
  
  
  hc.supp_to_do_two_flg_without_dont_knw_rowsum = rowSums(data[sum_hc_supp_to_do_two_flg_without_dont_knw],na.rm = T),
  ref.supp_to_do_two_flg_without_dont_knw_and_go_cycln_center_rowsum = rowSums(data[sum_ref_supp_to_do_two_flg_without_dont_knw_and_go_to_cyln],na.rm = T),
  i.HC_action_two_flag = if_else(X_3_Survey_category == "refugee_commun", "",
                                 if_else(hc.supp_to_do_two_flg_without_dont_knw_rowsum > 0,"yes","no",NULL)),
  i.refugee_action_two_flag = if_else(X_3_Survey_category == "host_community", "",
                                 if_else(ref.supp_to_do_two_flg_without_dont_knw_and_go_cycln_center_rowsum > 0,"yes","no",NULL)),
  i.action_dnt_know_two_flag = if_else(X_40a_What_are_you_supposed_to.don_t_know == 1,"yes","No",NULL),
  i.action_go_to_shelter_two_flag = if_else(X_40a_What_are_you_supposed_to.go_to_a_cyclone_shelter == 1,"yes","No",NULL),
  
  
  hc.supp_to_do_three_flg_without_dont_knw_rowsum = rowSums(data[sum_hc_supp_to_do_three_flg_without_dont_knw],na.rm = T),
  ref.supp_to_do_three_flg_without_dont_knw_and_go_cycln_center_rowsum = rowSums(data[sum_ref_supp_to_do_three_flg_without_dont_knw_and_go_to_cyln],na.rm = T),
  i.HC_action_three_flag = if_else(X_3_Survey_category == "refugee_commun", "",
                                 if_else(hc.supp_to_do_three_flg_without_dont_knw_rowsum > 0,"yes","no",NULL)),
  i.refugee_action_three_flag = if_else(X_3_Survey_category == "host_community", "",
                                 if_else(ref.supp_to_do_three_flg_without_dont_knw_and_go_cycln_center_rowsum > 0,"yes","no",NULL)),
  i.action_dnt_know_three_flag = if_else(X_41a_What_are_you_supposed_to.don_t_know == 1,"yes","No",NULL),
  i.action_go_to_shelter_three_flag = if_else(X_41a_What_are_you_supposed_to.go_to_a_cyclone_shelter == 1,"yes","No",NULL),
  ) %>% arrange(X_index)


                                  