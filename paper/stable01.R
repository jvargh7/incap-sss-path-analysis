# Pull data from table03.R ------------

source(paste0(path_sss_repo,"/functions/heterogeneity_functions.R"))

# CONTRAST MATRIX ---------

contrast_matrix_nohet = matrix(c(
  rep(c(0),each=14),
  rep(c(0),each=14)
),
nrow=2,
byrow=TRUE
)
contrast_matrix_nohet[,3] <- 1
contrast_matrix_nohet[2,2] <- 1


contrast_matrix_het = matrix(c(
  rep(c(0),each=15),
  rep(c(0),each=15)
),
nrow=2,
byrow=TRUE
)
contrast_matrix_het[,2] <- 1
contrast_matrix_het[2,15] <- 1


## Sex heterogeneity for BMI --------------

y = "adbmi"

## No heterogeneity
models_list_nohet_bmi <- no_sex_heterogeneity(y, miaux_dfs)
clean_mi_conditionalregression(models_list_nohet_bmi[[1]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient) %>% 
  knitr::kable(format="markdown")
clean_mi_conditionalregression(models_list_nohet_bmi[[2]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient)  %>% 
  knitr::kable(format="markdown")

## Heterogeneity
models_list_het_bmi <- sex_heterogeneity(y, miaux_dfs)
clean_mi_conditionalregression(models_list_het_bmi[[1]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient) %>% 
  knitr::kable(format="markdown")
clean_mi_conditionalregression(models_list_het_bmi[[2]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient)  %>% 
  knitr::kable(format="markdown")

## Contrasts for BMI 

clean_mi_contrasts(models_list_nohet_bmi[[1]],model_matrix = contrast_matrix_nohet,vcov_type="robust")

clean_mi_contrasts(models_list_het_bmi[[1]],model_matrix = contrast_matrix_het,vcov_type="robust")
clean_mi_contrasts(models_list_het_bmi[[2]],model_matrix = contrast_matrix_het,vcov_type="robust")



# SRQ-20 -----------

y = "adsrq"


### No heterogeneity

models_list_nohet_srq <- no_sex_heterogeneity(y, miaux_dfs)
clean_mi_conditionalregression(models_list_nohet_srq[[1]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient) %>% 
  knitr::kable(format="markdown")
clean_mi_conditionalregression(models_list_nohet_srq[[2]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient)  %>% 
  knitr::kable(format="markdown")


### Sex heterogeneity

models_list_het_srq <- sex_heterogeneity(y, miaux_dfs)
clean_mi_conditionalregression(models_list_het_srq[[1]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient) %>% 
  knitr::kable(format="markdown")
clean_mi_conditionalregression(models_list_het_srq[[2]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient)  %>% 
  knitr::kable(format="markdown")


### Contrasts

clean_mi_contrasts(models_list_nohet_srq[[1]],model_matrix = contrast_matrix_nohet,vcov_type="robust")
clean_mi_contrasts(models_list_nohet_srq[[2]],model_matrix = contrast_matrix_nohet,vcov_type="robust")

clean_mi_contrasts(models_list_het_srq[[1]],model_matrix = contrast_matrix_het,vcov_type="robust")
clean_mi_contrasts(models_list_het_srq[[2]],model_matrix = contrast_matrix_het,vcov_type="robust")


# Happiness -------------

y = "adhappy"


### No heterogeneity

models_list_nohet_happy <- no_sex_heterogeneity(y, miaux_dfs)
clean_mi_conditionalregression(models_list_nohet_happy[[1]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient) %>% 
  knitr::kable(format="markdown")
clean_mi_conditionalregression(models_list_nohet_happy[[2]],link = "geeglm identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient)  %>% 
  knitr::kable(format="markdown")

### Sex heterogeneity

models_list_het_happy <- sex_heterogeneity(y, miaux_dfs)
clean_mi_conditionalregression(models_list_het_happy[[1]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient) %>% 
  knitr::kable(format="markdown")
clean_mi_conditionalregression(models_list_het_happy[[2]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient)  %>% 
  knitr::kable(format="markdown")


### Contrasts

clean_mi_contrasts(models_list_nohet_happy[[1]],model_matrix = contrast_matrix_nohet,vcov_type="robust")
clean_mi_contrasts(models_list_nohet_happy[[2]],model_matrix = contrast_matrix_nohet,vcov_type="robust")

clean_mi_contrasts(models_list_het_happy[[1]],model_matrix = contrast_matrix_het,vcov_type="robust")
clean_mi_contrasts(models_list_het_happy[[2]],model_matrix = contrast_matrix_het,vcov_type="robust")








