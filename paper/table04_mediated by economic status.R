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
  dplyr::select(-gtvillage,-chsex,-gtchbyear) %>% 
  mutate_at(vars(pcall6775_1,pcall1987_1,
                 pcall2002_1,pcall1618_1,
                 moscho_imputed,moage_imputed,byear,
                 gtadeduyr1618,gtadladdereconomic2018,gtadladdercommunity2018),~scale(.))


order_of_columns = c("Wealth Index 1967-75","COMPLETED YEARS OF SCHOOLING","Wealth Index 1987","Wealth Index 2002","RURAL RESIDENT","EMPLOYMENT = YES","Wealth Index 2015-18",
                     # "Community Ladder in 2017-18",
                     "Economic Ladder in 2017-18",
                     "BMI in 2016",
                     "SRQ-20 in 2018",
                     "Life Satisfaction in 2018",
                     "Ravens in 2015-18",
                     "Happiness in 2018"
                     
)
order_of_rows = c("ATOLE VILLAGES",
                  "EXPOSURE 1000d",
                  "ATOLE 1000d",
                  "MATERNAL AGE (IMPUTED)",
                  "MATERNAL SCHOOLING (IMPUTED)",
                  "BIRTH YEAR - 1962",
                  "Sex = MALE",
                  "COMPLETED YEARS OF SCHOOLING",
                  
                  "Wealth Index 1967-75","Wealth Index 1987","Wealth Index 2002",
                  "RURAL RESIDENT",
                  "EMPLOYMENT = YES",
                  "Wealth Index 2015-18",
                  "Community Ladder in 2017-18",
                  "Economic Ladder in 2017-18")

# BMI ---------------

model_path_bmi_gtml <- '
adbmi ~  pcall6775_1 + pcall1987_1 + pcall2002_1 + pcall1618_1 + employmentyn + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear + gtadeduyr1618 + rural + gtadladdereconomic2018
gtadladdereconomic2018 ~ pcall6775_1 + pcall1987_1 + pcall2002_1 + pcall1618_1 + employmentyn + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear + gtadeduyr1618 + rural
pcall1618_1 ~ gtadeduyr1618 + pcall6775_1 + pcall1987_1 + pcall2002_1 + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear + rural + employmentyn
employmentyn ~ pcall6775_1 + pcall1987_1 + pcall2002_1 + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear + gtadeduyr1618 + rural
rural ~ pcall6775_1 + pcall1987_1 + pcall2002_1 + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear + gtadeduyr1618
pcall2002_1 ~ gtadeduyr1618 + pcall6775_1 + pcall1987_1 + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear
pcall1987_1 ~ gtadeduyr1618 + pcall6775_1 + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear
pcall6775_1 ~ moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + byear
gtadeduyr1618 ~ pcall6775_1 + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear
'

result_e_path_bmi <- sem(model_path_bmi_gtml,
                         data = model_df,
                         std.ov = FALSE,
                         cluster = "d_id_unim",
                         estimator = "mlr",
                         # group="chsex",
                         # Using FIML for "imputation"
                         missing = "fiml"
)


display_result_path(result_e_path_bmi) %>% 
  dplyr::select(iv,one_of(order_of_columns)) %>% 
  mutate(iv = factor(iv,levels=order_of_rows)) %>% 
  arrange(iv) %>% 
  knitr::kable(format="markdown")



# SRQ-20 -------------

model_path_srq_gtml <- '
adsrq ~  pcall6775_1 + pcall1987_1 + pcall2002_1 + pcall1618_1 + employmentyn + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear + gtadeduyr1618 + rural + gtadladdereconomic2018
gtadladdereconomic2018 ~ pcall6775_1 + pcall1987_1 + pcall2002_1 + pcall1618_1 + employmentyn + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear + gtadeduyr1618 + rural
pcall1618_1 ~ gtadeduyr1618 + pcall6775_1 + pcall1987_1 + pcall2002_1 + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear + rural + employmentyn
employmentyn ~ pcall6775_1 + pcall1987_1 + pcall2002_1 + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear + gtadeduyr1618 + rural
rural ~ pcall6775_1 + pcall1987_1 + pcall2002_1 + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear + gtadeduyr1618
pcall2002_1 ~ gtadeduyr1618 + pcall6775_1 + pcall1987_1 + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear
pcall1987_1 ~ gtadeduyr1618 + pcall6775_1 + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear
pcall6775_1 ~ moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + byear
gtadeduyr1618 ~ pcall6775_1 + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear
'

result_e_path_srq <- sem(model_path_srq_gtml,
                         data = model_df,
                         std.ov = FALSE,
                         cluster = "d_id_unim",
                         estimator = "mlr",
                         # group="chsex",
                         # Using FIML for "imputation"
                         missing = "fiml"
)



display_result_path(result_e_path_srq) %>% 
  dplyr::select(iv,one_of(order_of_columns)) %>% 
  mutate(iv = factor(iv,levels=order_of_rows)) %>% 
  arrange(iv) %>% 
  knitr::kable(format="markdown") 



# HAPPINESS ------------

model_path_happy_gtml <- '
adhappy ~  pcall6775_1 + pcall1987_1 + pcall2002_1 + pcall1618_1 + employmentyn + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear + gtadeduyr1618 + rural + gtadladdereconomic2018
gtadladdereconomic2018 ~ pcall6775_1 + pcall1987_1 + pcall2002_1 + pcall1618_1 + employmentyn + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear + gtadeduyr1618 + rural
pcall1618_1 ~ gtadeduyr1618 + pcall6775_1 + pcall1987_1 + pcall2002_1 + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear + rural + employmentyn
employmentyn ~ pcall6775_1 + pcall1987_1 + pcall2002_1 + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear + gtadeduyr1618 + rural
rural ~ pcall6775_1 + pcall1987_1 + pcall2002_1 + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear + gtadeduyr1618
pcall2002_1 ~ gtadeduyr1618 + pcall6775_1 + pcall1987_1 + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear
pcall1987_1 ~ gtadeduyr1618 + pcall6775_1 + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear
pcall6775_1 ~ moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + byear
gtadeduyr1618 ~ pcall6775_1 + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear
'
result_e_path_happy <- sem(model_path_happy_gtml,
                           data = model_df,
                           std.ov = FALSE,
                           cluster = "d_id_unim",
                           estimator = "mlr",
                           # group="chsex",
                           # Using FIML for "imputation"
                           missing = "fiml"
)



display_result_path(result_e_path_happy) %>% 
  dplyr::select(iv,one_of(order_of_columns)) %>% 
  mutate(iv = factor(iv,levels=order_of_rows)) %>% 
  arrange(iv) %>% 
  knitr::kable(format="markdown") 



# CONSOLIDATED ---------

tab5_cols = c("Wealth Index 1967-75",
              "COMPLETED YEARS OF SCHOOLING",
              "Wealth Index 1987",
              "Wealth Index 1996",
              "Wealth Index 2002",
              "Wealth Index 2016",
              "Wealth Index 2018",
              "RURAL RESIDENT",
              "EMPLOYMENT = YES",
              "Wealth Index 2015-18",
              "Community Ladder in 2017-18",
              "Economic Ladder in 2017-18"
)

consolidated <- bind_rows(
  
  # display_result_path(result_c_path_ht,wider=FALSE),
  display_result_path(result_e_path_bmi,wider=FALSE),
  display_result_path(result_e_path_ravens,wider=FALSE),
  display_result_path(result_e_path_srq,wider=FALSE),
  display_result_path(result_e_path_happy,wider=FALSE)
  
  
) %>% 
  dplyr::filter(iv %in% tab5_cols) %>% 
  dplyr::filter(dv %in% c("Height in 2016",
                          "BMI in 2016",
                          "Fat Mass Index in 2016",
                          "Fat Free Mass Index in 2016",
                          "Waist Circumference in 2016",
                          "SBP in 2016",
                          "DBP in 2016",
                          
                          "Glucose in 2016",
                          "HDL in 2016",
                          "LDL in 2016",
                          "TGL in 2016",
                          
                          "SRQ-20 in 2018",
                          "Life Satisfaction in 2018",
                          "Happiness in 2018",
                          
                          # "Ravens in 2015-18",
                          "List Sort in 2017-18",
                          "DCCS in 2017-18",
                          "Flanker in 2017-18",
                          "Pattern Comparison in 2015-18"
                          
                          # "Community Ladder in 2017-18",
                          # "Economic Ladder in 2017-18"
  )) %>% 
  pivot_wider(names_from="iv",values_from = "display") %>% 
  dplyr::select(dv,one_of(tab5_cols))

consolidated %>% 
  knitr::kable(format="markdown") 


# PREDICTING LADDER ------------

consolidated <- bind_rows(
  
  # display_result_path(result_c_path_ht,wider=FALSE),
  display_result_path(result_e_path_bmi,wider=FALSE),
  # display_result_path(result_c_path_fmi,wider=FALSE),
  # display_result_path(result_c_path_ffmi,wider=FALSE),
  # display_result_path(result_c_path_srq,wider=FALSE),
  # display_result_path(result_c_path_ravens,wider=FALSE),
  # display_result_path(result_c_path_listsort,wider=FALSE),
  # display_result_path(result_c_path_dccs,wider=FALSE),
  # display_result_path(result_c_path_flanker,wider=FALSE),
  # display_result_path(result_c_path_pc,wider=FALSE),
  # display_result_path(result_c_path_lifesat,wider=FALSE),
  # display_result_path(result_c_path_happy,wider=FALSE),
  # display_result_path(result_c_path_community,wider=FALSE),
  # display_result_path(result_c_path_economic,wider=FALSE)
  
) %>%
  dplyr::filter(iv %in% c("Wealth Index 1967-75",
                          "COMPLETED YEARS OF SCHOOLING",
                          "Wealth Index 1987",
                          "Wealth Index 1996",
                          "Wealth Index 2002",
                          "Wealth Index 2016",
                          "Wealth Index 2018",
                          "RURAL RESIDENT",
                          "EMPLOYMENT = YES",
                          "Wealth Index 2015-18",
                          "Community Ladder in 2017-18",
                          "Economic Ladder in 2017-18"
  )) %>% 
  dplyr::filter(dv %in% c(
    "Community Ladder in 2017-18",
    "Economic Ladder in 2017-18"
  )) %>%
  pivot_wider(names_from="dv",values_from = "display")

consolidated %>%
  knitr::kable(format="markdown")