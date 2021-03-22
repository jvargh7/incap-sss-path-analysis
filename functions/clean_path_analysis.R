display_result_path <- function(result_path, wider=TRUE,display_type = "coef_ci"){
  
  display_result <- result_path %>% 
    broom::tidy(.) %>% 
    
    dplyr::filter(op == "~",!is.na(statistic)) %>% 
    mutate(conf.low = estimate - 1.96*std.error,
           conf.high = estimate + 1.96*std.error) %>% 
    dplyr::select(term,
                  # group,
                  estimate,conf.low,conf.high) %>% 
    separate(.,col=term,into = c("dv","iv"),sep = " ~ ") %>% 
    mutate(dv = case_when(dv %in% c("adht","ht") ~ "Height in 2016",
                          dv == "adbmi" ~ "BMI in 2016",
                          dv == "fmi" ~ "Fat Mass Index in 2016",
                          dv == "ffmi" ~ "Fat Free Mass Index in 2016",
                          dv == "adwc" ~ "Waist Circumference in 2016",
                          dv == "adsbp" ~ "SBP in 2016",
                          dv == "addbp" ~ "DBP in 2016",
                          
                          dv == "adglucose" ~ "Glucose in 2016",
                          dv == "adhdl" ~ "HDL in 2016",
                          dv == "gtadldlc2016" ~ "LDL in 2016",
                          dv == "adtgl" ~ "TGL in 2016",
                          
                          dv == "adsrq" ~ "SRQ-20 in 2018",
                          dv == "adlifesat" ~ "Life Satisfaction in 2018",
                          dv %in% c("adhappy","gtadhappytot2018") ~ "Happiness in 2018",
                          
                          
                          dv == "adravenstotscore" ~ "Ravens in 2015-18",
                          dv == "adlsrawscore" ~ "List Sort in 2017-18",
                          dv == "addccsnihcomputedscore" ~ "DCCS in 2017-18",
                          dv == "adflankernihcomputedscore" ~ "Flanker in 2017-18",
                          dv == "adpcrawscore" ~ "Pattern Comparison in 2015-18",
                          
                          dv == "gtadladdercommunity2018" ~ "Community Ladder in 2017-18",
                          dv == "gtadladdereconomic2018" ~ "Economic Ladder in 2017-18",
                          
                          dv == "pcall6775_1" ~ "Wealth Index 1967-75",
                          dv %in% c("scale(adeduyr)","scale(gtadeduyr1618)","eduyr1618z") ~ "COMPLETED YEARS OF SCHOOLING (relative z-scores)",
                          dv %in% c("adeduyr","gtadeduyr1618") ~ "COMPLETED YEARS OF SCHOOLING",
                          
                          dv %in% c("ademploymentformal","employment_formal") ~ "EMPLOYMENT = FORMAL",
                          dv %in% c("ademploymentinformal","employment_informal") ~ "EMPLOYMENT = INFORMAL",
                          dv %in% c("employmentyn") ~ "EMPLOYMENT = YES",
                          dv == "rural" ~ "RURAL RESIDENT",
                          dv == "pcall1987_1" ~ "Wealth Index 1987",
                          dv == "pcall1996_1" ~ "Wealth Index 1996",
                          dv == "pcall2002_1" ~ "Wealth Index 2002",
                          dv == "pcall2016_1" ~ "Wealth Index 2016",
                          dv == "pcall2018_1" ~ "Wealth Index 2018",
                          dv == "pcall1618_1" ~ "Wealth Index 2015-18",
                          TRUE ~ dv
    ),
    
    iv = case_when(iv == "pcall6775_1" ~ "Wealth Index 1967-75",
                   iv == "pcall1987_1" ~ "Wealth Index 1987",
                   iv == "pcall1996_1" ~ "Wealth Index 1996",
                   iv == "pcall2002_1" ~ "Wealth Index 2002",
                   iv == "pcall2016_1" ~ "Wealth Index 2016",
                   iv == "pcall2018_1" ~ "Wealth Index 2018",
                   iv == "pcall1618_1" ~ "Wealth Index 2015-18",
                   
                   iv == "gtadladdercommunity2018" ~ "Community Ladder in 2017-18",
                   iv == "gtadladdereconomic2018" ~ "Economic Ladder in 2017-18",
                   
                   
                   iv == "gtatole" ~ "ATOLE VILLAGES",
                   iv == "exposure1000" ~ "EXPOSURE 1000d",
                   iv == "atole1000" ~ "ATOLE 1000d",
                   iv == "rural" ~ "RURAL RESIDENT",
                   iv == "byear" ~ "BIRTH YEAR - 1962",
                   iv == "gtchatoleexposurestatuspartial" ~ "PERIOD = PARTIAL (FRESCO)",
                   iv == "gtchatoleexposurestatusfull" ~ "PERIOD = FULL (FRESCO)",
                   iv == "gtatole:gtchatoleexposurestatuspartial" ~ "ATOLE X PARTIAL",
                   iv == "gtatole:gtchatoleexposurestatusfull" ~ "ATOLE X FULL",
                   iv %in% c("chsexfemale","sexF") ~ "Sex = FEMALE",
                   iv %in% c("chsexmale","sexM") ~ "Sex = MALE",
                   
                   iv == "gtvillageAT_CO" ~ "VILLAGE = CONACASTE (AT)",
                   iv == "gtvillageAT_SJ" ~ "VILLAGE = SAN JUAN (AT)",
                   iv == "gtvillageFR_SD" ~ "VILLAGE = SANTO DOMINGO (FR)",
                   iv == "gtvillageFR_ES" ~ "VILLAGE = ESPIRITU SANTO (FR)",
                   
                   
                   iv == "scale(moht_imputed)" ~ "MATERNAL HEIGHT (IMPUTED, relative z-scores)",
                   iv == "moscho_imputed" ~ "MATERNAL SCHOOLING (IMPUTED)",
                   iv == "moscho_sib" ~ "MATERNAL SCHOOLING (SIBLING)",
                   iv == "moage_imputed" ~ "MATERNAL AGE (IMPUTED)",
                   
                   iv == "score_change_ge300yes" ~ "(HOLMS RAHE STRESS >= 300)",
                   iv %in% c("scale(adeduyr)","scale(gtadeduyr1618)","eduyr1618z") ~ "COMPLETED YEARS OF SCHOOLING (relative z-scores)",
                   iv %in% c("adeduyr","gtadeduyr1618") ~ "COMPLETED YEARS OF SCHOOLING",
                   
                   iv == "gtadwealthindex2018" ~ "ADULT SOCIAL CLASS (z-scores)",
                   
                   iv %in% c("ademploymentformal","employment_formal") ~ "EMPLOYMENT = FORMAL",
                   iv %in% c("ademploymentinformal","employment_informal") ~ "EMPLOYMENT = INFORMAL",
                   iv %in% c("employmentyn") ~ "EMPLOYMENT = YES",
                   
                   iv == "(Intercept)" ~ "INTERCEPT",
                   TRUE ~ iv),         
    # group = case_when(group == 1 ~ "Male",
    #                   group == 2 ~ "Female"),
    
    display = case_when(display_type == "coef_ci" ~ paste0(round_d(estimate,2)," \t(",
                                                           round_d(conf.low,2),", ",
                                                           round_d(conf.high,2),")"),
                        display_type == "coef" ~ round_d(estimate,2) %>% as.character(.),
                        TRUE ~ paste0(round_d(estimate,2)," \t(",
                                      round_d(conf.low,2),", ",
                                      round_d(conf.high,2),")"))
    
    ) 
  
  
  if(wider == TRUE|is.null(wider)){
    out <- display_result %>% 
      dplyr::select(dv,iv,display) %>%
      pivot_wider(names_from="dv",values_from = "display")
    
  }
  
  if(wider == FALSE){
    out <- display_result %>% 
      dplyr::select(dv,iv,display)
    
  }
  
  return(out)
  
}



sem_coef <- function(formula_y=formula(), sem_df=data.frame(),coef_name="gtadladdercommunity2018"){
  
  result_sem <- sem(formula_y,
                    data=sem_df,
                    std.ov = FALSE,
                    cluster = "d_id_unim",
                    estimator = "mlr",
                    # group="chsex",
                    # Using FIML for "imputation"
                    missing = "fiml")
  
  print(nobs(result_sem))
  
  display_result <- result_sem %>% 
    broom::tidy(.) %>% 
    dplyr::filter(op == "~",!is.na(statistic)) %>% 
    mutate(conf.low = estimate - 1.96*std.error,
           conf.high = estimate + 1.96*std.error) %>% 
    dplyr::select(term,
                  # group,
                  estimate,conf.low,conf.high) %>% 
    separate(.,col=term,into = c("dv","iv"),sep = " ~ ") %>%
    dplyr::filter(iv %in% coef_name) %>% 
    mutate(display_type = paste0(round_d(estimate,2)," \t(",
                                 round_d(conf.low,2),", ",
                                 round_d(conf.high,2),")")) %>% 
    dplyr::select(display_type) %>% 
    pull()
  
  return(display_result)
  
}
