mi_reg_coef <- function(formula_reg,mi_dfs,coef_name,coef_type = "Coefficient"){
  
  
  models_list <- list()
  
  for (i in 1:mi_iter){
    glm_c <- geepack::geeglm(formula_reg,
                             id = d_id_unim,
                             corstr = "unstructured",
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