source(paste0(path_sss_repo,"/functions/e_value.R"))



result_path_ev <- data.frame(
  variable = character(),
  model3c_beta = numeric(),
  model3c_se = numeric(),
  model3e_beta = numeric(),
  model3e_se = numeric(),
  e_value_c = character(),
  e_value_e = character()
)



for (y in y_vars) {
  
  sd_y = sd(complete(miaux_dfs,1)[,y])
  
  temp_path_ev <- temp_ev(y,df = miaux_dfs,sd = sd_y)
  
  result_path_ev <- bind_rows(result_path_ev,
                              temp_path_ev
  )
  
}

result_path_ev %>% 
  knitr::kable()



sd_bmi = sd(model_df$adbmi)
sd_srq = sd(model_df$adsrq)
sd_happy = sd(model_df$gtadhappytot2018)


evalue_to_beta(1/1.27,sd_srq)
evalue_to_beta(1.36,sd_happy)

