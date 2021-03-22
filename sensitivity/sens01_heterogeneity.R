

# Pull data from table03.R ------------

source(paste0(path_sss_repo,"/functions/heterogeneity_functions.R"))


# BMI -------------
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

## Wealth heterogeneity

models_list_wealth_bmi <- adses_heterogeneity(y, miaux_dfs,adsesref = "pcall1618_1")
clean_mi_conditionalregression(models_list_wealth_bmi[[1]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient) %>% 
  knitr::kable(format="markdown")
clean_mi_conditionalregression(models_list_wealth_bmi[[2]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient)  %>% 
  knitr::kable(format="markdown")




## SRQ-20 ------------

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


### Wealth heterogeneity

models_list_wealth_srq <- adses_heterogeneity(y, miaux_dfs,adsesref = "pcall1618_1")
clean_mi_conditionalregression(models_list_wealth_srq[[1]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient) %>% 
  knitr::kable(format="markdown")
clean_mi_conditionalregression(models_list_wealth_srq[[2]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient)  %>% 
  knitr::kable(format="markdown")


### Birth village heterogeneity

models_list_wealth_srq <- adses_heterogeneity(y, miaux_dfs,adsesref = "gtatole")
clean_mi_conditionalregression(models_list_wealth_srq[[1]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient) %>% 
  knitr::kable(format="markdown")
clean_mi_conditionalregression(models_list_wealth_srq[[2]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient)  %>% 
  knitr::kable(format="markdown")




### Urban-Rural heterogeneity

models_list_wealth_srq <- adses_heterogeneity(y, miaux_dfs,adsesref = "rural")
clean_mi_conditionalregression(models_list_wealth_srq[[1]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient) %>% 
  knitr::kable(format="markdown")
clean_mi_conditionalregression(models_list_wealth_srq[[2]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient)  %>% 
  knitr::kable(format="markdown")





## Happiness --------------


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



### Wealth heterogeneity

models_list_wealth_happy <- adses_heterogeneity(y, miaux_dfs,adsesref = "pcall1618_1")
clean_mi_conditionalregression(models_list_wealth_happy[[1]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient) %>% 
  knitr::kable(format="markdown")
clean_mi_conditionalregression(models_list_wealth_happy[[2]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient)  %>% 
  knitr::kable(format="markdown")


### Birth village heterogeneity

models_list_atole_happy <- adses_heterogeneity(y, miaux_dfs,adsesref = "gtatole")
clean_mi_conditionalregression(models_list_atole_happy[[1]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient) %>% 
  knitr::kable(format="markdown")
clean_mi_conditionalregression(models_list_atole_happy[[2]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient)  %>% 
  knitr::kable(format="markdown")


### Urban-Rural heterogeneity

models_list_rural_happy <- adses_heterogeneity(y, miaux_dfs,adsesref = "rural")
clean_mi_conditionalregression(models_list_rural_happy[[1]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient) %>% 
  knitr::kable(format="markdown")
clean_mi_conditionalregression(models_list_rural_happy[[2]],link = "lmer identity") %>% 
  mutate(sex = "Combined") %>% 
  dplyr::select(iv, Coefficient)  %>% 
  knitr::kable(format="markdown")


