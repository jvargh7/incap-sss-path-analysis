source("../.Rprofile")


source(paste0(path_incap_social_mobility,"/functions/clean_glm_result.R"))
source(paste0(path_sss_repo,"/functions/clean_pathanalysis.R"))
source(paste0(path_incap_social_mobility,"/functions/clean_mi_conditionalregression.R"))
library(compareGroups)
library(lavaan)


# Reading data ---------
source(paste0(path_sss_repo,"/preprocessing/analysis7_df.R"))



model_df = analysis7_df %>% 
  dplyr::filter(!is.na(gtadladdercommunity2018), !is.na(gtadladdereconomic2018)) %>% 
  mutate(sexM = case_when(chsex == "female" ~ 0,
                          TRUE ~ 1),
         byear = gtchbyear - 62
  ) %>% 
  dplyr::select(-gtvillage,-chsex,-gtchbyear)  %>% 
  mutate_at(vars(pcall6775_1,pcall1987_1,
                 pcall2002_1,pcall1618_1,
                 moscho_imputed,
                 moage_imputed,byear,  moscho_sib,
                 gtadeduyr1618,
                 
                 gtadladdereconomic2018,gtadladdercommunity2018),function(x) as.numeric(scale(x))) %>% 
  arrange(d_id_unim)

y_vars <- c("adbmi","adravenstotscore","adsrq","adhappy")

source(paste0(path_sss_repo,"/functions/multiple imputation with aux.R"))

source(paste0(path_sss_repo,"/functions/mi_reg_coef.R"))
source(paste0(path_sss_repo,"/functions/temp_mr.R"))


# Table 3 ------------

result_path_mr <- data.frame(
  variable = character(),
  model1c = character(),
  model2c = character(),
  model3c = character(),
  
  model1e = character(),
  model2e = character(),
  model3e = character()
)



for (y in y_vars) {
  
  temp_path_mr <- temp_mr(y,df = miaux_dfs)
  
  result_path_mr <- bind_rows(result_path_mr,
                              temp_path_mr
  )
  
}

result_path_mr <- result_path_mr %>% 
  mutate(variable = as.character(variable)) %>% 
  mutate(variable = case_when(variable %in% c("adht","ht") ~ "Height in 2016",
                              variable == "adbmi" ~ "BMI in 2016",
                              variable == "fmi" ~ "Fat Mass Index in 2016",
                              variable == "ffmi" ~ "Fat Free Mass Index in 2016",
                              variable == "adwc" ~ "Waist Circumference in 2016",
                              variable == "adsbp" ~ "SBP in 2016",
                              variable == "addbp" ~ "DBP in 2016",
                              
                              variable == "adglucose" ~ "Glucose in 2016",
                              variable == "adhdl" ~ "HDL in 2016",
                              variable == "gtadldlc2016" ~ "LDL in 2016",
                              variable == "adtgl" ~ "TGL in 2016",
                              
                              variable == "adsrq" ~ "SRQ-20 in 2018",
                              variable == "adlifesat" ~ "Life Satisfaction in 2018",
                              variable %in% c("adhappy","gtadhappytot2018") ~ "Happiness in 2018",
                              
                              
                              variable == "adravenstotscore" ~ "Ravens in 2015-18",
                              variable == "adlsrawscore" ~ "List Sort in 2017-18",
                              variable == "addccsnihcomputedscore" ~ "DCCS in 2017-18",
                              variable == "adflankernihcomputedscore" ~ "Flanker in 2017-18",
                              variable == "adpcrawscore" ~ "Pattern Comparison in 2015-18",
                              
                              variable == "gtadladdercommunity2018" ~ "Community Ladder in 2017-18",
                              variable == "gtadladdereconomic2018" ~ "Economic Ladder in 2017-18",
                              
                              variable == "pcall6775_1" ~ "Wealth Index 1967-75",
                              variable %in% c("scale(adeduyr)","scale(gtadeduyr1618)","eduyr1618z") ~ "COMPLETED YEARS OF SCHOOLING (relative z-scores)",
                              variable %in% c("adeduyr","gtadeduyr1618") ~ "COMPLETED YEARS OF SCHOOLING",
                              variable == "pcall1987_1" ~ "Wealth Index 1987",
                              variable == "pcall1996_1" ~ "Wealth Index 1996",
                              variable == "pcall2002_1" ~ "Wealth Index 2002",
                              variable == "pcall2016_1" ~ "Wealth Index 2016",
                              variable == "pcall2018_1" ~ "Wealth Index 2018",
                              variable == "pcall1618_1" ~ "Wealth Index 2015-18",
                              variable == "rural" ~ "RURAL RESIDENT",
                              TRUE ~ variable
  ))


