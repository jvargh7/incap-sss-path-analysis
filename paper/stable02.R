
# Data from table03.R -----------

source(paste0(path_incap_social_mobility,"/functions/gmethods_functions.R"))


## Alive in 2015-16 or 2017-18 ----------

source(paste0(path_incap_social_mobility,"/relative wealth/rw04_ipaw for alive.R"))

alive_df <- alive_df %>% 
  mutate(na_sss = case_when(id_uni %in% model_df$id_uni ~ 1,
                            TRUE ~ 0))

alive_df$c_sss_6 = censoring_weights(c_formula = paste0("na_sss", 
                                                        "~ chsex + moscho_imputed + moage_imputed + gtatole*exposure1000 + gtchbyear + pcall6775_1"),
                                     df = alive_df,
                                     type = "glm")


## Outcome reporting ------------

ipw_df <- model_df %>% 
  left_join(alive_df %>% 
              dplyr::select(id_uni,c_alive2018,c_sss_6),
            by = "id_uni") %>% 
  mutate(c_w = c_alive2018*c_sss_6)

rhs_formula_o = "~ pcall6775_1 + pcall1618_1 + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + sexM + byear + gtadeduyr1618 + rural"

mi_reg_coef_ipw <- function(formula_reg,mi_dfs,coef_name,coef_type = "Coefficient",c_w=numeric()){
  
  
  models_list <- list()
  
  for (i in 1:mi_iter){
    glm_c <- geepack::geeglm(formula_reg,
                             id = d_id_unim,
                             corstr = "unstructured",
                             weights = c_w,
                             # family = gaussian(),
                             # glmControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                             # control=glmControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                             # control=glmControl(optimizer ="nloptwrap2"),
                             data = complete(mi_dfs,i))
    
    models_list[[i]] <- glm_c
  }
  
  
  
  out_coef = extract_mi_coef_type(models_list,coef_name,coef_type)
  
  
  
  return(out_coef)
  
}

# function to consolidate results -------

temp_st3 <- function(o,df,type="mi",coef_type = "Coefficient",c_w=numeric()){
  
  formula1c_y <- paste0(o,"~ gtadladdercommunity2018") %>% as.formula()
  formula2c_y <- paste0(o,"~ gtadladdercommunity2018 + moscho_sib + gtatole + exposure1000 + atole1000 + moage_imputed + byear + sexM + pcall6775_1")%>% as.formula()
  formula3c_y <- paste0(o,"~ gtadladdercommunity2018 + moscho_sib + gtatole + exposure1000 + atole1000 + moage_imputed + byear + sexM + pcall6775_1 + pcall1618_1 + gtadeduyr1618 + rural + employmentyn") %>% as.formula()
  
  formula1e_y <- paste0(o,"~ gtadladdereconomic2018") %>% as.formula()
  formula2e_y <- paste0(o,"~ gtadladdereconomic2018 + moscho_sib + gtatole + exposure1000 + atole1000 + moage_imputed + byear + sexM + pcall6775_1") %>% as.formula()
  formula3e_y <- paste0(o,"~ gtadladdereconomic2018 + moscho_sib + gtatole + exposure1000 + atole1000 + moage_imputed + byear + sexM + pcall6775_1 + pcall1618_1 + gtadeduyr1618 + rural + employmentyn") %>% as.formula()
  
  
  formula4c_y <- paste0(o,"~ gtadladdercommunity2018 + moscho_sib + gtatole + exposure1000 + atole1000 + moage_imputed + byear + sexM + pcall6775_1 + pcall1618_1 + gtadeduyr1618 + rural + employmentyn + adlifesat") %>% as.formula()
  formula4e_y <- paste0(o,"~ gtadladdereconomic2018 + moscho_sib + gtatole + exposure1000 + atole1000 + moage_imputed + byear + sexM + pcall6775_1 + pcall1618_1 + gtadeduyr1618 + rural + employmentyn + adlifesat") %>% as.formula()
  
  if(type == "mi"){
    mr_df <- data.frame(variable = o,
                        model1c = mi_reg_coef_ipw(formula1c_y,mi_dfs = df,coef_name = "gtadladdercommunity2018",c_w=c_w),
                        model2c = mi_reg_coef_ipw(formula2c_y,mi_dfs = df,coef_name = "gtadladdercommunity2018",c_w=c_w),
                        model3c = mi_reg_coef_ipw(formula3c_y,mi_dfs = df,coef_name = "gtadladdercommunity2018",c_w=c_w),
                        model1e = mi_reg_coef_ipw(formula1e_y,mi_dfs = df,coef_name = "gtadladdereconomic2018",c_w=c_w),
                        model2e = mi_reg_coef_ipw(formula2e_y,mi_dfs = df,coef_name = "gtadladdereconomic2018",c_w=c_w),
                        model3e = mi_reg_coef_ipw(formula3e_y,mi_dfs = df,coef_name = "gtadladdereconomic2018",c_w=c_w),
                        model4c = mi_reg_coef_ipw(formula4c_y,mi_dfs = df,coef_name = "gtadladdercommunity2018",c_w=c_w),
                        model4e = mi_reg_coef_ipw(formula4e_y,mi_dfs = df,coef_name = "gtadladdereconomic2018",c_w=c_w)
                        
    )
  }
  
  
  
  
  return(mr_df)
  
  
  
}


# RUN ------------

result_path_st3 <- data.frame(
  variable = character(),
  model1c = character(),
  model2c = character(),
  model3c = character(),
  
  model1e = character(),
  model2e = character(),
  model3e = character()
)




for (y in y_vars) {
  
  temp_path_st3 <- temp_st3(y,df = miaux_dfs,c_w=ipw_df$c_w)
  
  result_path_st3 <- bind_rows(result_path_st3,
                               temp_path_st3
  )
  
}

result_path_st3 <- result_path_st3 %>% 
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


result_path_st3 %>% 
  knitr::kable(format="markdown") 