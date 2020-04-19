library(tidyverse)
library(butteR)
library(survey)
library(srvyr)
library(dplyr)
# library(koboquest)

population<- c("host_community","refugee_commun")[2]

assess_survey<- readxl::read_xls("Input/tool/KAP_XLS.XLS",sheet = "survey")
assess_choices<-readxl::read_xls("Input/tool/KAP_XLS.XLS",sheet = "choices")

df<-read.csv("Input/data/25022020_DRR_KAP_Recoded_forButter.csv", stringsAsFactors = F, na.strings = c(""," ", "NA",NA),strip.white = T)

dfs<-split(df, df$X_3_Survey_category)

df<-dfs[[population]]


if(population=="host_community"){
  df_strata<-"Area_village_street"
  sf_strata<-"Union"
  sf_pop<- "HH_pop"
  analysis_strata<-"Thana"
  pop<-read.csv("Input/Population_Figures_2011_Census_HostCommunity.csv", stringsAsFactors = F, na.strings=c(""," "))
  pop<- pop %>% 
    mutate(!!sf_strata:=stringr::str_replace_all(!!sym(sf_strata),"Teknaf Sadar", "Teknaf"))
  
  (df$Area_village_street %>% unique())[!df$Area_village_street %>% unique() %in% pop$Union]
  df<-df %>% filter(Area_village_street%in% pop[[sf_strata]] )
  }
if(population== "refugee_commun") {
  pop<- read.csv("Input/201909_UNHCR_Pop.csv", stringsAsFactors = F, na.strings=c(""," "))
  southern_camps<-c("Camp 23", "Camp 24", "Camp 25", "Camp 26", "Camp 27", 
                    "Nayapara RC")
  analysis_strata<-"regional_strata"
  sf_strata<-"Camp"
  sf_pop<- "Total.Families"
  df_strata<- "Camp_Number"
  df<-df %>% filter(Camp_Number!="HC")
  df<-df %>% 
    mutate(
      regional_strata= ifelse(Camp_Number %in% southern_camps,"southern_camps", "northern_camps")
    )
  pop<-pop %>% 
    filter(!is.na(Camp),is.na(Block)) %>% 
    filter(Camp!="Kutupalong RC") %>% 
    mutate(
      !!(sf_strata):=stringr::str_replace(Camp, "Total","") %>% trimws(),
      !!(sf_strata):= stringr::str_replace_all(Camp,"Extension","Ext"),
      Total.Families=readr::parse_number(Total.Families %>% stringr::str_replace_all(",","")),
      Total.Individuals= readr::parse_number(Total.Individuals %>% stringr::str_replace_all(",",""))
    ) %>% 
    filter(!!sym(sf_strata)!="Kutupalong RC") 
    # ) %>% 
    # select(Camp_Number=!!sym(sf_strata), Total.Families,Total.Individuals) %>% 
    # 
    # mutate(
    #   Freq= Total.Families/sum(Total.Families)*nrow(.)
    #   
    # )
  
}

sf_with_weights<-df %>% 
  group_by(!!sym(df_strata)) %>% 
  summarise(sample_strata_num=n()) %>% 
  right_join(pop, by=setNames(sf_strata,df_strata)) %>% 
  mutate(sample_global=sum(sample_strata_num),
         pop_global=sum(!!sym(sf_pop)),
         survey_weight= (!!sym(sf_pop)/pop_global)/(sample_strata_num/sample_global)
  )


df2<-df %>% left_join(sf_with_weights)

# DEFINE SURVEY OBJECT
dfsvy<-svydesign(ids = ~1,strata = formula(paste0("~",df_strata)),data = df2,weights = formula(paste0("~", "survey_weight")))
#THIS ADDS FACTOR LEVELS FROM TOOL
# debugonce(butteR::questionnaire_factorize_categorical)
# dfsvy$variables<- butteR::questionnaire_factorize_categorical(data = dfsvy$variables,questionnaire = assessment,return_full_data = T)
#GET RID OF EMPTY COLUMNS AND OTHER COLUMNS
colnames(df2) %>% dput()
dont_analyze<-c("X_uuid", "start", "end", "X__1_Record_your_current_location_latitude", 
   "X__1_Record_your_current_location_longitude", "X_3_Survey_category", 
   "X_5_Age", "X_6_Gender", "Thana", "Area_village_street", "Camp_Number",
   "sample_strata_num", "Upazila", "HH_pop", "sample_global", "pop_global", "survey_weight",
   "Total.Families", "Total.Individuals")

dont_analyze_in_data<-dont_analyze[dont_analyze %in% colnames(df2)]
is_not_empty<-function(x){ all(is.na(x))==FALSE}


cols_to_analyze<-df2 %>% select(-starts_with("Other"), -ends_with(".other")) %>%
  select_if(.,is_not_empty) %>% select(-dont_analyze_in_data) %>% colnames() 



# the following variable will break butteR if the factors aren't expanded.
# butteRs butteR::questionnaire_factorize_categorical() should  automatically solve this problem for all questions, but
# this tool is out of control... so will just do it manually.

dfsvy$variables$i.time_of_cyclones<- forcats::fct_expand(dfsvy$variables$i.time_of_cyclones,c( "no", "yes"))
dfsvy$variables$X_20_Just_before_a_cyclone_arri.local_leader__m<- forcats::fct_expand(dfsvy$variables$X_20_Just_before_a_cyclone_arri.local_leader__m,c( "0", "1"))
dfsvy$variables$X_42a_If_YES_how_can_you_reco.don_t_know_what_they_look_like<- forcats::fct_expand(dfsvy$variables$X_42a_If_YES_how_can_you_reco.don_t_know_what_they_look_like,c( "0", "1"))
dfsvy$variables$X_20_Just_before_a_cyclone_arri.cic<- forcats::fct_expand(dfsvy$variables$X_20_Just_before_a_cyclone_arri.cic,c( "0", "1"))
dfsvy$variables$X_26a_IF_YES_Where_was_it<-forcats::fct_expand(dfsvy$variables$X_20_Just_before_a_cyclone_arri.cic,c( "bangladesh", "myanmar"))

basic_analysis_overall<-butteR::mean_proportion_table(design = dfsvy,list_of_variables = cols_to_analyze)

basic_analysis_strata<-butteR::mean_proportion_table(design = dfsvy,list_of_variables = cols_to_analyze,aggregation_level = analysis_strata)

basic_analysis_by_gender<-butteR::mean_proportion_table(design = dfsvy,list_of_variables = cols_to_analyze,aggregation_level = c("X_6_Gender"))


if (population == "host_community") {
output_folder <- "output/host/"
write.csv(basic_analysis_overall,paste0(output_folder,"basic_analysis_overall_host.csv"))
  write.csv(basic_analysis_strata,paste0(output_folder,"basic_analysis_strata_host.csv"))
  write.csv(basic_analysis_by_gender,paste0(output_folder,"basic_analysis_by_gender_host.csv"))
}

if (population == "refugee_commun") {
  output_folder <- "output/refugee/"
  write.csv(basic_analysis_overall,paste0(output_folder,"basic_analysis_overall_ref.csv"))
  write.csv(basic_analysis_strata,paste0(output_folder,"basic_analysis_strata_ref.csv"))
  write.csv(basic_analysis_by_gender,paste0(output_folder,"basic_analysis_by_gender_ref.csv"))
}
