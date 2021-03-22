# source(paste0(path_replication_repo,"/preprocessing/analysis7_df.R"))

library(mice)
# library(lme4)

mi_iter = 10

source(paste0(path_replication_repo,"/package/residuals_mi.R"))
source(paste0(path_replication_repo,"/package/rsquared_mi.R"))
# gtadladdercommunity2018 + moscho_imputed + gtatole + exposure1000 + atole1000 + moage_imputed + 
# byear + sexM + pcall6775_1 + pcall1618_1 + gtadeduyr1618 + rural + adlifesat

var_list <- c(y_vars,
              "pcall6775_1","pcall1618_1",
              "gtadladdercommunity2018","gtadladdereconomic2018",
              "moscho_sib","sexM","byear","gtadeduyr1618","moage_imputed",
              "gtatole","exposure1000","atole1000","rural","adlifesat",
              "employmentyn",
              "d_id_unim")

mi_null <- mice(model_df %>% 
                  dplyr::select(one_of(var_list)) %>% 
                  mutate(employmentyn = factor(employmentyn)),
                maxit = 0)

method = mi_null$method
pred = mi_null$predictorMatrix

method["employmentyn"] <- "logreg"
# pred[,y_vars] <- 0
# method[2:3] <- "rf"
# set.seed(2020)
miaux_dfs <- mice(model_df %>% 
                    dplyr::select(one_of(var_list)) %>% 
                    mutate(employmentyn = factor(employmentyn)),
                  method = method,
                  pred = pred,
                  m=mi_iter,maxit=50,seed=501)

plot(miaux_dfs)
