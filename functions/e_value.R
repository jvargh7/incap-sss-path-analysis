
e_value_ci = function(beta,se,sd_y){
  
  d = beta/sd_y
  se_d = se/sd_y
  
  rr_beta = exp(0.91*d)
  rr_lci = exp(0.91*d - 1.78*se_d)
  rr_uci = exp(0.91*d + 1.78*se_d)
  
  if(rr_beta < 1){
    rr_est = 1/rr_beta
    e_val_est = rr_est + sqrt(rr_est*(rr_est - 1))
    rr_uci_est = ifelse(rr_uci >= 1, rr_uci,1/rr_uci)
    # If UL >= 1, then E-value = 1
    # If UL < 1, then let UL* = 1/UL and E-value = UL* + sqrt{UL* × (UL* - 1)}
    e_val_ci = ifelse(rr_uci >= 1,1,rr_uci_est + sqrt(rr_uci_est*(rr_uci_est-1)))
  }
  
  
  if(rr_beta > 1){
    rr_est = rr_beta
    e_val_est = rr_est + sqrt(rr_est*(rr_est - 1))
    
    e_val_ci = ifelse(rr_lci <= 1,1,rr_lci + sqrt(rr_lci*(rr_lci-1)))
    # If LL <= 1, then E-value = 1
    # If LL > 1, then E-value = LL + sqrt{LL × (LL - 1)}
  }
  
  return(paste0(round(e_val_est,2),"; CI: ",round(e_val_ci,2)))
  
}


evalue_to_beta <- function(e_value,sd_y){
  
  beta_evalue = log(e_value)/0.91
  
  return(beta_evalue)
}


temp_ev <- function(o,df,type="mi",sd = numeric(),coef_type = "beta + se"){
  
  formula3c_y <- paste0(o,"~ gtadladdercommunity2018 + moscho_sib + gtatole + exposure1000 + atole1000 + moage_imputed + byear + sexM + pcall6775_1 + pcall1618_1 + gtadeduyr1618 + rural + employmentyn") %>% as.formula()
  
  formula3e_y <- paste0(o,"~ gtadladdereconomic2018 + moscho_sib + gtatole + exposure1000 + atole1000 + moage_imputed + byear + sexM + pcall6775_1 + pcall1618_1 + gtadeduyr1618 + rural + employmentyn") %>% as.formula()
  
  if(type == "mi"){
    
    out_3c = mi_reg_coef(formula3c_y,mi_dfs = df,coef_name = "gtadladdercommunity2018",coef_type = "beta + se")
    out_3e = mi_reg_coef(formula3e_y,mi_dfs = df,coef_name = "gtadladdereconomic2018",coef_type = "beta + se")
    
    y_e_val_c = e_value_ci(out_3c[[1]],out_3c[[2]],sd)
    y_e_val_e = e_value_ci(out_3e[[1]],out_3e[[2]],sd)
    
    ev_df <- data.frame(variable = o,
                        model3c_beta = out_3c[[1]],
                        model3c_se = out_3c[[2]],
                        model3e_beta = out_3e[[1]],
                        model3e_se = out_3e[[2]],
                        e_value_c = y_e_val_c,
                        e_value_e = y_e_val_e
    )
  }
  
  
  
  
  return(ev_df)
  
  
  
}
