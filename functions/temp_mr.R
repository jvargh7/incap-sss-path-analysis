temp_mr <- function(o,df,type="mi",coef_type = "Coefficient"){
  
  formula1c_y <- paste0(o,"~ gtadladdercommunity2018") %>% as.formula()
  formula2c_y <- paste0(o,"~ gtadladdercommunity2018 + moscho_sib + gtatole + exposure1000 + atole1000 + moage_imputed + byear + sexM + pcall6775_1")%>% as.formula()
  formula3c_y <- paste0(o,"~ gtadladdercommunity2018 + moscho_sib + gtatole + exposure1000 + atole1000 + moage_imputed + byear + sexM + pcall6775_1 + pcall1618_1 + gtadeduyr1618 + rural + employmentyn") %>% as.formula()
  
  formula1e_y <- paste0(o,"~ gtadladdereconomic2018") %>% as.formula()
  formula2e_y <- paste0(o,"~ gtadladdereconomic2018 + moscho_sib + gtatole + exposure1000 + atole1000 + moage_imputed + byear + sexM + pcall6775_1") %>% as.formula()
  formula3e_y <- paste0(o,"~ gtadladdereconomic2018 + moscho_sib + gtatole + exposure1000 + atole1000 + moage_imputed + byear + sexM + pcall6775_1 + pcall1618_1 + gtadeduyr1618 + rural + employmentyn") %>% as.formula()
  
  
  formula4c_y <- paste0(o,"~ gtadladdercommunity2018 + moscho_sib + gtatole + exposure1000 + atole1000 + moage_imputed + byear + sexM + pcall6775_1 + pcall1618_1 + gtadeduyr1618 + rural + employmentyn + adlifesat") %>% as.formula()
  formula4e_y <- paste0(o,"~ gtadladdereconomic2018 + moscho_sib + gtatole + exposure1000 + atole1000 + moage_imputed + byear + sexM + pcall6775_1 + pcall1618_1 + gtadeduyr1618 + rural + employmentyn + adlifesat") %>% as.formula()
  
  
  
  if(type == "sem"){
    mr_df <- data.frame(variable = o,
                        model1c = sem_coef(formula1c_y,sem_df = df,coef_name = "gtadladdercommunity2018"),
                        model2c = sem_coef(formula2c_y,sem_df = df,coef_name = "gtadladdercommunity2018"),
                        model3c = sem_coef(formula3c_y,sem_df = df,coef_name = "gtadladdercommunity2018"),
                        model1e = sem_coef(formula1e_y,sem_df = df,coef_name = "gtadladdereconomic2018"),
                        model2e = sem_coef(formula2e_y,sem_df = df,coef_name = "gtadladdereconomic2018"),
                        model3e = sem_coef(formula3e_y,sem_df = df,coef_name = "gtadladdereconomic2018"),
                        model4c = sem_coef(formula4c_y,sem_df = df,coef_name = "gtadladdercommunity2018"),
                        model4e = sem_coef(formula4e_y,sem_df = df,coef_name = "gtadladdereconomic2018")
                        
    )
  }
  
  if(type == "mi"){
    mr_df <- data.frame(variable = o,
                        model1c = mi_reg_coef(formula1c_y,mi_dfs = df,coef_name = "gtadladdercommunity2018"),
                        model2c = mi_reg_coef(formula2c_y,mi_dfs = df,coef_name = "gtadladdercommunity2018"),
                        model3c = mi_reg_coef(formula3c_y,mi_dfs = df,coef_name = "gtadladdercommunity2018"),
                        model1e = mi_reg_coef(formula1e_y,mi_dfs = df,coef_name = "gtadladdereconomic2018"),
                        model2e = mi_reg_coef(formula2e_y,mi_dfs = df,coef_name = "gtadladdereconomic2018"),
                        model3e = mi_reg_coef(formula3e_y,mi_dfs = df,coef_name = "gtadladdereconomic2018"),
                        model4c = mi_reg_coef(formula4c_y,mi_dfs = df,coef_name = "gtadladdercommunity2018"),
                        model4e = mi_reg_coef(formula4e_y,mi_dfs = df,coef_name = "gtadladdereconomic2018")
                        
    )
  }
  
  
  
  
  return(mr_df)
  
  
  
}