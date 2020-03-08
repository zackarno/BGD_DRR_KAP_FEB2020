library(dplyr)
library(butteR)
library(tidyr)
library(AMR)
library(stringr)
library(srvyr)

data_for_butter <- read.csv("Input/25022020_DRR_KAP_Recoded_forButter.csv",na.strings = c("", " "), stringsAsFactors = F)


# test --------------------------------------------------------------------

 data_for_butter <- data_for_butter %>% filter(Thana %in% c("Ukhia","Teknaf"))

df_srvyr <- as_survey(data_for_butter, strata = Thana)

colnames <- data_for_butter %>% dplyr::select(starts_with("i."),"Thana") %>% select(-"i.time_of_cyclones" ) %>% colnames() %>% dput


mean_proportion_table_df <- butteR::mean_proportion_table(design = df_srvyr, 
                              list_of_variables = colnames,
                              aggregation_level = "Thana",
                              round_to = 2,
                              return_confidence = FALSE,
                              na_replace = FALSE)



avg_proportion_table_df <- butteR::mean_proportion_table(design = df_srvyr, 
                                                          list_of_variables = colnames,
                                                          round_to = 2,
                                                          return_confidence = FALSE,
                                                          na_replace = FALSE)
