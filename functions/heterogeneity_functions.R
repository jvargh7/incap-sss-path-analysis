no_sex_heterogeneity <- function(y,miaux_dfs,sexref="sexM"){
  formula3c_y <- paste0(y,"~ gtadladdercommunity2018 +", sexref,
                        " + moscho_sib + gtatole + exposure1000 + atole1000 + moage_imputed + byear + pcall6775_1 + pcall1618_1 + gtadeduyr1618 + rural + employmentyn") %>% as.formula()
  formula3e_y <- paste0(y,"~ gtadladdereconomic2018 +", sexref,
                        " + moscho_sib + gtatole + exposure1000 + atole1000 + moage_imputed + byear + pcall6775_1 + pcall1618_1 + gtadeduyr1618 + rural + employmentyn") %>% as.formula()
  
  
  models_list_c <- list()
  models_list_e <- list()
  
  for (i in 1:mi_iter){
    glm_3c <- geepack::geeglm(formula3c_y,
                              id = d_id_unim,
                              corstr = "unstructured",
                              # family = gaussian(),
                              # glmControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                              # control=glmControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                              # control=glmControl(optimizer ="nloptwrap2"),
                              data = complete(miaux_dfs,i))
    glm_3e <- geepack::geeglm(formula3e_y,
                              id = d_id_unim,
                              corstr = "unstructured",
                              # family = gaussian(),
                              # glmControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                              # control=glmControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                              # control=glmControl(optimizer ="nloptwrap2"),
                              data = complete(miaux_dfs,i))
    
    models_list_c[[i]] <- glm_3c
    models_list_e[[i]] <- glm_3e
  }
  return(list(models_list_c,
              models_list_e))
  
}






sex_heterogeneity <- function(y,miaux_dfs,sexref="sexM"){
  formula5c_y <- paste0(y,"~ gtadladdercommunity2018*",sexref," + moscho_sib + gtatole + exposure1000 + atole1000 + moage_imputed + byear + pcall6775_1 + pcall1618_1 + gtadeduyr1618 + rural + employmentyn") %>% as.formula()
  formula5e_y <- paste0(y,"~ gtadladdereconomic2018*",sexref," + moscho_sib + gtatole + exposure1000 + atole1000 + moage_imputed + byear + pcall6775_1 + pcall1618_1 + gtadeduyr1618 + rural + employmentyn") %>% as.formula()
  
  
  models_list_c <- list()
  models_list_e <- list()
  
  for (i in 1:mi_iter){
    glm_5c <- geepack::geeglm(formula5c_y,
                              id = d_id_unim,
                              corstr = "unstructured",
                              # family = gaussian(),
                              # glmControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                              # control=glmControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                              # control=glmControl(optimizer ="nloptwrap2"),
                              data = complete(miaux_dfs,i))
    glm_5e <- geepack::geeglm(formula5e_y,
                              id = d_id_unim,
                              corstr = "unstructured",
                              # family = gaussian(),
                              # glmControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                              # control=glmControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                              # control=glmControl(optimizer ="nloptwrap2"),
                              data = complete(miaux_dfs,i))
    
    models_list_c[[i]] <- glm_5c
    models_list_e[[i]] <- glm_5e
  }
  return(list(models_list_c,
              models_list_e))
  
}

adses_heterogeneity <- function(y,miaux_dfs,adsesref=""){
  
  formula6c_y <- paste0(y,"~ gtadladdercommunity2018 + gtadladdercommunity2018:",adsesref," + sexM + moscho_sib + gtatole + exposure1000 + atole1000 + moage_imputed + byear + pcall6775_1 + pcall1618_1 + gtadeduyr1618 + rural + employmentyn") %>% as.formula()
  formula6e_y <- paste0(y,"~ gtadladdereconomic2018 + gtadladdereconomic2018:",adsesref," + sexM + moscho_sib + gtatole + exposure1000 + atole1000 + moage_imputed + byear + pcall6775_1 + pcall1618_1 + gtadeduyr1618 + rural + employmentyn") %>% as.formula()
  
  
  models_list_c <- list()
  models_list_e <- list()
  
  for (i in 1:mi_iter){
    glm_6c <- geepack::geeglm(formula6c_y,
                              id = d_id_unim,
                              corstr = "unstructured",
                              # family = gaussian(),
                              # glmControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                              # control=glmControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                              # control=glmControl(optimizer ="nloptwrap2"),
                              data = complete(miaux_dfs,i))
    glm_6e <- geepack::geeglm(formula6e_y,
                              id = d_id_unim,
                              corstr = "unstructured",
                              # family = gaussian(),
                              # glmControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                              # control=glmControl(optimizer ='optimx', optCtrl=list(method='nlminb')),
                              # control=glmControl(optimizer ="nloptwrap2"),
                              data = complete(miaux_dfs,i))
    
    models_list_c[[i]] <- glm_6c
    models_list_e[[i]] <- glm_6e
  }
  return(list(models_list_c,
              models_list_e))
  
}