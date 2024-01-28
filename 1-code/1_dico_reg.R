######### install packages 
# install.packages("name_of_package") 

######### load packages 
library(tidyverse)
library(dplyr)
library(haven) 
library(plm)
library("labelled")
library("miceadds")
library(plm)
library(estimatr)
library(stargazer)

######### load data

data <- read_dta("/Users/willylin/Desktop/ensae/S1/statapp/wsdata_cleaned_signalling.dta")

####### pour balance test 

######### premiere technique qui ne fonctionne pas : il faut ajouter le clusters
install.packages("rstatix")
install.packages("ggpubr")
library(rstatix)
library(ggpubr)

mean_age <- data
mean_age$treatment = as.factor(data$treatment) #il faut mettre en factor, sinon treatment est de categorie dbl+lbl : empeche l'anova

mean_age %>%  group_by(treatment) %>% 
  summarise(
    count = n(),
    mean = mean(bl_age, na.rm = TRUE),
    sd = sd(bl_age, na.rm = TRUE)
  )

res.aov <- mean_age %>% anova_test(bl_age ~ treatment,   white.adjust = TRUE
)
res.aov

######### deuxieme technique 
install.packages("Hmisc")
library(Hmisc)

t.test.cluster(data$bl_age, data$bl_block, data$treatment, conf.int = 0.95)


# ANOVA Table (type II tests)
# 
# Effect DFn  DFd     F    p p<.05     ges
# 1 treatment   1 6889 0.031 0.86       4.5e-06

#########


######### sommaire 

#1. exploration rapide de la base 
#2. exploration plus detaillée de la base : dictionnaire de variable // Table D.1 + D.2 stat desc. // Table D.6 + D.7 attrition, non reponse 
#3. regressions + traitements supplementaires 
  #3. D.8 comme benchmark // bon cluster groupe + effets fixes
  #3.
  #3.
  #3.
#4. randomisation et son interet : les tests (voir code1.R)
# statdesc aussi pour voir les distributions des variables 

#########################################################################  
################## 1. outils pour explorer rapidement ################### 
#########################################################################  

View(data) #open visualiser

ls(data) #list of variables
var_label(data) #list of labels, to know which var is which according to the paper

#analyse rapide de la non reponse // ici, toute la base de donnée semble totalement populée (pas d'attrition)
table(!is.na(data$anonid)) #6891 // no NA in ID 
table(!unique(data$anonid)) #6891 // so no duplicate ID

#voir rapidement une variable categorielle est populée dans chacune de ses categories
table(data$treatment)
# 0    1    2    3 
# 2275 2114 2248  254 
count(data, treatment) #nathan soluce : en prime, on a aussi les labels par categorie


###########################################################################################################################################################
#### 2. Phase d'exploration : recreer un dictionnaire de variable (voir où il y a de l'attrition (NA), et trouver les uniques identifiants (pour la var de cluster + trouver les ecarts d'obs))
##########################################################################################################################################################


########### Notes
# Note sur les dimensions sur R studio
# dim(data)[2] // c'est la dimension de la colonne de la df 'data'
# dim(data)[1] // _________ de la ligne de la df 'data'

#   # boucle allant de la 1ere var à la dernière var : R m'imprime le nom de la variable et son nombre de NA
# for (i in 1:dim(data)[2]){
#     print(paste(colnames(data)[i],sum(is.na(data[i]))))
# }

#   #_________________________  : R m'imprime soit le label/soit le nom de la var et son nb de NA
# for (i in 1:dim(data)[2]){
#   print(paste(ifelse(var_label((data)[i])=="NULL",colnames(data)[i],var_label((data)[i])), sum(is.na(data[i]))))
# }
########### 

########### ########### ########### ########### ########### ########### ########### ########### ########### ########### ########### ########### 
# 2.a. Base de donnée initiale : matrice des variables avec NA // dictionnaire de variable et documenter l'attrition
########### ########### ########### ########### ########### ########### ########### ########### ########### ########### ########### ########### 
# lien utile: https://stackoverflow.com/questions/13442461/populating-a-data-frame-in-r-in-a-loop

#1. initialisation de listes vides pour construire les vecteurs constitutifs de la matrice des variables
list_of_na <- list() 
list_of_na2 <- list() 
list_of_na3 <- list() 
varname <- list()

#2.1 boucle imbriquée pour obtenir les vecteurs constitutifs de la matrice des variables avec leur NA
#j'ai surcomplexifie sans faire expres, je vais faire un croisement selon les differents groupes de traitements - donc je ferai des boucles plus efficaces

#boucle i
# voir à la fin
#boucle j
# à chaque variable de la df data (les colonnes de df data, indicées i), on assigne à chaque ligne de la colonne numerotee col2 (indicee j): soit le label, soit le nom de la variable
#boucle m
# _________, assigner le nom de la variable
#boucle k
# assigner la somme des NA de la variable
#boucle l
# assigner la somme des non NA de la variable
#boucle u
# assigner la somme des uniques valeurs à chaque variable (pour trouver les groupes de cluster et l'unique identificateur des traités)
#boucle i
# à nos listes vides (à chacun de leur indice i), on assigne les resultats des boucles j,m,k,l (chacun de leur indices correspondants)

#finalement en fin de boucle, on obtient 4 listes, qu'on transforme ensuite en vecteur pour pouvoir les concaténer => on obtient alors la matrice

for (i in 1:dim(data)[2]){
  col1 <- numeric(dim(data)[2])
  for (j in 1:dim(data)[2]){
    col1[j] <-ifelse(var_label((data)[i])=="NULL",colnames(data)[i],var_label((data)[i]))
  } #boucle j
  varnames <- numeric(dim(data)[2])
  for (m in 1:dim(data)[2]){
    varnames[m] <- colnames(data)[i]
  } #boucle m
  col2 <- numeric(dim(data)[2])
  for (k in 1:dim(data)[2]){
    col2[k] <- sum(is.na(data[i]))
  } #boucle k
  col3 <- numeric(dim(data)[2])
  for (l in 1:dim(data)[2]){
    col3[l] <- sum(!is.na(data[i]))
  } #boucle l
  list_of_na[i] <- col1
  list_of_na2[i] <- col2
  list_of_na3[i] <- col3
  varname[i]<-varnames
}

##### 2.2 une boucle pour les valeurs uniques par variables
table(sum(!is.na(unique(data$bl_date_baseline)))) #84 groupes de cluster ! ca doit etre la variable pour le groupe de cluster

# # note pour moi meme : pour faire des boucles facilement
# # le faire avec l'indice i de la sequence, sans la liste à assigner les informations + print
# for (i in 1:dim(data)[2]){
#   print(sum(!is.na(unique(data[,i])))) #pour mimiquer la structure data$nom_de_la_variable
# }
# # ensuite tu crees des objets vides pour stocker l'info // listes ou bien df

result_df <- data.frame(variable_name = character(0), unique_count = numeric(0))
for (i in 1:dim(data)[2]){
  variable_name <- colnames(data)[i]
  unique_count <- sum(!is.na(unique(data[, i])))
  result_df <- rbind(result_df, data.frame(variable_name = variable_name, unique_count = unique_count))
}

#3. concatener la liste sous forme de vecteur (pour chacune des listes de la boucle 2.1)
#pas besoin pour la boucle 2.2

list_of_na <- do.call("rbind",list_of_na) # meme chose d'utiliser la commande rbind()
list_of_na2 <- do.call("rbind",list_of_na2)
list_of_na3 <- do.call("rbind",list_of_na3)
varname <- do.call("rbind",varname)

#4.1 rassembler les vecteurs en matrice (boucle 2.1)
na <- cbind(list_of_na,list_of_na2,list_of_na3,varname)
na <- as.data.frame(na) #pour pouvoir manipuler la matrice 
na[,2]<- as.numeric(na[,2]) #put in num to sum
na[,3]<- as.numeric(na[,3]) #put in num so you can sum them both
na <- na %>% group_by(V1) %>% mutate(total = V2+V3) #total d'obs 

#4.2 merger boucle2.1 et boucle2.2
na <- na %>% left_join(result_df, by=c('V4'='variable_name'))


########### ########### ########### ########### ########### ########### ########### ########### ########### ########### ########### ########### 
##### 2.b. Selon les groupes de traitements : documenter l'attrition dans le papier
########### ########### ########### ########### ########### ########### ########### ########### ########### ########### ########### ########### 

################ 1. fais la boucle à l'intérieur de la fonction d'abord, puis la fonction (2.)

#####
# 1.1 df vide

# rm(result_df3)
# rm(result_df2)
# rm(col1)
# rm(list_of_na)
# result_df2 <- data.frame(variable_name = character(0), 
#                          na_count = numeric(0), 
#                          not_na_count =numeric(0), 
#                          unique_count = numeric(0))
# 
# # 1.2 on remplit la df ligne par ligne
# 
# for (i in 1:dim(data)[2]){
#   # # col1 <- numeric(dim(data)[2])
#   # # col1[i] <- ifelse(is.logical(var_label(data[i])), colnames(data)[i], var_label(data[i]))
#   # col1 <- ifelse(is.logical(var_label((data)[i])), colnames(data)[i], var_label((data)[i]))
#   varnames <- colnames(data)[i]
#   col2 <- sum(is.na(data[[i]]))
#   col3 <- sum(!is.na(data[[i]]))
#   unique_count <- sum(!is.na(unique(data[[i]])))
#   
#   result_df2 <- rbind(result_df2, data.frame(variable_name = varnames,
#                                             na_count = col2,
#                                             not_na_count = col3,
#                                             unique_count = unique_count))
#   # result_df3 <- cbind(result_df2,col1) #marche pas, tant pis, sans les labels
# }
# 
# #1.3 on va append les labels // on construit le vecteur de labels
# #ca ne marche pas sans une double boucle, un mystere... // note pour plus tard : s'entrainter sur (etwas)[[qqch]], (etwas)[qqch] 
# 
# for (i in 1:dim(data)[2]){
#   col1 <- numeric(dim(data)[2])
#   for (j in 1:dim(data)[2]){
#     col1[j] <-ifelse(var_label((data)[i])=="NULL",colnames(data)[i],var_label((data)[i]))
#   } #boucle j
#   list_of_na[i] <- col1
# }
# col1 <- rbind(list_of_na)
# col1 <- as.data.frame(col1)
# col1 <- t(col1)
# 
# #1.4 append 1.3 et 1.2 
# result_df3 <- cbind(result_df2, col1)
# 
# #1.5 total des obs (na + no_na)
# result_df3 <- result_df3 %>% mutate(total_count = na_count + not_na_count)
#####

################ 2. la fonction + dictionnaire de variable (table D.1)

dico <- function(dataset_to_prompt){
  
  # 1.1 df vide
  list_of_na <- list() 
  mean_var<-list()
  std_var<-list()
  first_pctile<-list()
  nine_pctile<-list()
  
  result_df2 <- data.frame(variable_name = character(0), 
                           na_count = numeric(0), 
                           not_na_count =numeric(0), 
                           unique_count = numeric(0))
  
  # 1.2 on remplit la df ligne par ligne
  for (i in 1:dim(dataset_to_prompt)[2]){
    varnames <- colnames(dataset_to_prompt)[i]
    col2 <- sum(is.na(dataset_to_prompt[[i]]))
    col3 <- sum(!is.na(dataset_to_prompt[[i]]))
    unique_count <- sum(!is.na(unique(dataset_to_prompt[[i]])))
    
    result_df2 <- rbind(result_df2, data.frame(variable_name = varnames,
                                               na_count = col2,
                                               not_na_count = col3,
                                               unique_count = unique_count))
  }
  
  #1.3 on va append les labels // on construit le vecteur de labels
  #ca ne marche pas sans une double boucle, un mystere... je crois parce que ifelse ne suffit pas à assigner, donc il faut etre redondant // note pour plus tard : s'entrainter sur (etwas)[[qqch]], (etwas)[qqch] 
  for (i in 1:dim(dataset_to_prompt)[2]){
    col1 <- numeric(dim(dataset_to_prompt)[2])
    for (j in 1:dim(dataset_to_prompt)[2]){
      col1[j] <-ifelse(var_label((dataset_to_prompt)[i])=="NULL",colnames(dataset_to_prompt)[i],var_label((dataset_to_prompt)[i]))
    } #boucle j, labels des vars
    col5 <- numeric(dim(dataset_to_prompt)[2])
    for (k in 1:dim(dataset_to_prompt)[2]){
      col5[k] <- ifelse(is.Date(dataset_to_prompt[[i]]),NA,mean(dataset_to_prompt[[i]],na.rm = TRUE)
      ) #moyenne
    }
    col6 <- numeric(dim(dataset_to_prompt)[2])
    for (t in 1:dim(dataset_to_prompt)[2]){
      col6[t] <- ifelse(is.Date(dataset_to_prompt[[i]]),NA,sd(dataset_to_prompt[[i]],na.rm = TRUE)
      ) #std_var
    }
    col7 <- numeric(dim(dataset_to_prompt)[2])
    for (y in 1:dim(dataset_to_prompt)[2]){
      col7[y] <- ifelse(is.Date(dataset_to_prompt[[i]]),NA,quantile(dataset_to_prompt[[i]],c(0.1),na.rm = TRUE)
      ) #premier percentile
    }
    col8 <- numeric(dim(dataset_to_prompt)[2])
    for (w in 1:dim(dataset_to_prompt)[2]){
      col8[w] <- ifelse(is.Date(dataset_to_prompt[[i]]),NA,quantile(dataset_to_prompt[[i]],c(0.9),na.rm = TRUE)
      ) #pour mimiquer la structure data$nom_de_la_variable
    }
    
    list_of_na[i] <- col1
    
    mean_var[i] <- col5
    std_var[i] <- col6
    first_pctile[i] <- col7 
    nine_pctile[i] <- col8
    
  }
  col1 <- rbind(list_of_na,mean_var,
                std_var,
                first_pctile,
                nine_pctile)
  col1 <- as.data.frame(col1)
  col1 <- t(col1)
  
  #1.4 append 1.3 et 1.2 
  result_df3 <- cbind(result_df2, col1)
  
  #1.5 total des obs (na   no_na)
  result_df3 <- result_df3 %>% mutate(total_count = na_count  + not_na_count,
                                      obs_restantes = total_count-na_count) 
  
  dictionnaire_data <- result_df3
}

dico <- dico(data) #sans croisement 

################ 3. quelques dico de variables selon le traitement

data_treatment1 <- data %>% filter(treatment==1)
data_treatment2 <- data %>% filter(treatment==2)
data_treatment3 <- data %>% filter(treatment==3)
data_treatment0 <- data %>% filter(treatment==0)

dico_data1 <- dico(data_treatment1)
dico_data2 <- dico(data_treatment2)
dico_data3 <- dico(data_treatment3)
dico_data0 <- dico(data_treatment0)

## conclusion : l'attrition ne vient pas des differents traitements, mais des var d'interets y 
## conclusion 2 : on est en mesure de selectionner les variables et le groupe cluster (voir labels et nb d'obs)

############################################################            
############################################################            
############### 3. regressions (tables 1, 2, )
############################################################            
############################################################   

datatest <- data %>% filter(treatment!=3)
estimates <- lm.cluster(el_emp_7d~public+private+factor(bl_block), cluster='bl_date_baseline',data=datatest)
summary(estimates)
#il ne faut pas supprimer les placebos ! sinon on perd des groupes de cluster
# effectivement : 
table(!is.na(unique(datatest$bl_date_baseline)))
# TRUE 
# 81 

#il faut faire comme sur stata : on retrouve les coefficients des estimateurs par categories : 
estimates <- lm.cluster(el_emp_7d~factor(treatment)+factor(bl_block), cluster='bl_date_baseline',data=data)
summary(estimates)
table(!is.na(unique(data$bl_date_baseline)))
# TRUE 
# 84 
# on garde tous les groupes de cluster ainsi

##############################      
##### table 1 
############################## 

##############################      
##### emploi // stepwise regression
##############################      

data_endline <- data %>% filter(!is.na(el_emp_7d))
# table(!is.na(data_endline$anonid))
# 6607
# table(!is.na(unique(data_endline$bl_date_baseline)))
# TRUE 
# 84 
# c'est logique: on prend en covariates que des baseline covs, parce qu'ils sont pas affectes par le traitement

#avoir une liste de toutes les vars commencant par 'bl_'
bl_cov <- paste(grep("bl_", names(data_endline), value = TRUE), collapse = " + ")

data_endline <- data_endline %>% mutate(full_public = ifelse(treatment==2 | treatment==3,1,0))

#reg 0 // en suivant les covariates dans table d.7 de online appendix
estimates <- lm.cluster(el_emp_7d~full_public #vs public: 0.45 (std: .112 // intercept: 0.13, std" 0.072)
                        +factor(bl_block)
                        +bl_ed_gr12
                        +bl_score_num
                        +bl_score_lit
                        +bl_score_cft
                        +bl_score_grit
                        +noncog_score #n'est pas un bl_cov
                        +bl_belsco_num
                        +bl_belsco_lit_ter #tercile, not score
                        +bl_belsco_cft_ter #same
                        +bl_belest_index
                        +bl_age
                        +bl_male
                        +bl_emp_7d
                        +bl_time_med
                        +bl_time_presentbias
                        +bl_risk_high,
                        cluster='bl_date_baseline',data=data_endline)

summary(estimates)

#donc il ne faut pas traiter les placebos comme des publics, faut les traiter a part :
#reg1 : correct , on garde treatment, parce que c'est le vecteur de traitement // par contre, mystere de comment ils trouvent leur intercept
estimates <- lm.cluster(el_emp_7d~factor(treatment)
                        +bl_ed_gr12
                        # +bl_ed_postsec # if you add, 0.51 instead
                        +bl_score_num
                        +bl_score_lit
                        +bl_score_cft
                        +bl_score_grit
                        +noncog_score #n'est pas un bl_cov
                        +bl_belsco_num
                        +bl_belsco_lit_ter #tercile, not score
                        +bl_belsco_cft_ter #same
                        +bl_belest_index
                        +bl_age
                        +bl_male
                        +bl_emp_7d
                        +bl_time_med
                        +bl_time_presentbias
                        +bl_risk_high
                        +factor(bl_block), #randomblock
                        cluster='bl_date_baseline',data=data_endline)

summary(estimates)

# reg2 : corrige des na : tu obtiens des coeffs differents 
estimates <- lm.cluster(el_emp_7d~factor(treatment)
                        +bl_ed_gr12
                        +bl_score_num
                        +bl_score_lit
                        +bl_score_cft
                        +bl_score_grit
                        +noncog_score_nm #corrige
                        +bl_belsco_num_nm #corrige
                        +bl_belsco_lit_ter_nm #corrige
                        +bl_belsco_cft_ter_nm #corrige
                        +bl_belest_index
                        +bl_age
                        +bl_male
                        +bl_emp_7d
                        +bl_time_med
                        +bl_time_presentbias
                        +bl_risk_high
                        +factor(bl_block), #randomblock
                        cluster='bl_date_baseline',data=data_endline)

summary(estimates)

# on retient reg 1 pour les covariates 

#faire une fonction pour les regressions sans les traitements sur la var de traitement

###################################################
###################################################
#### salaire // transformation a la var de treatment
###################################################
###################################################


data_endline_wage <- data %>% filter(!is.na(el_emp_earn_all_uncd_i))
table(is.na(data_endline_wage$anonid)) #bon nombre d'obs

#traitement sur la variable 'treatment' : les salaires sont log transformes, il faut appliquer la meme transfo pour notre var de traitement (sinon on interprete mal les coeffs)
data_endline_wage <- data_endline_wage %>% mutate(treat_c = log(treatment+sqrt((treatment^2) +1))) #seulement public : 0.337
data_endline_wage %>% count(treatment) #pas de perte d'obs

##### reg 1 : 0.337, oui // mais std un poil au dessus

estimates <- lm.cluster(el_emp_earn_all_uncd_i~factor(treat_c)
                        +bl_ed_gr12
                        # +bl_ed_postsec # if you add, 0.335390220 instead
                        +bl_score_num
                        +bl_score_lit
                        +bl_score_cft
                        +bl_score_grit
                        +noncog_score #n'est pas un bl_cov
                        +bl_belsco_num
                        +bl_belsco_lit_ter #tercile, not score
                        +bl_belsco_cft_ter #same
                        +bl_belest_index
                        +bl_age
                        +bl_male
                        +bl_emp_7d
                        +bl_time_med
                        +bl_time_presentbias
                        +bl_risk_high
                        +factor(bl_block), #randomblock
                        cluster='bl_date_baseline',data=data_endline_wage)

summary(estimates)


############# reg 2 avec d'autres covariates ? 
# _nm pour eviter les na, c'est aussi pour ca qu'on transforme le y, et donc on transforme aussi la var de traitement
# 0.3158105518 (0.071876794) meilleur std

estimates <- lm.cluster(el_emp_earn_all_uncd_i~factor(treat_c)
                        +bl_ed_gr12
                        # +bl_ed_postsec # if you add, 0.339 (0.073516554) instead #####
                        # + bl_ed_degree
                        +bl_score_num
                        +bl_score_lit
                        +bl_score_cft
                        +bl_score_grit
                        +noncog_score_nm # on ajoute aussi nm ######
                        +bl_belsco_num_nm #corrige
                        +bl_belsco_lit_ter_nm #on met sans les NA
                        +bl_belsco_cft_ter_nm #same
                        +bl_belest_index
                        +bl_age
                        +bl_male
                        +bl_emp_7d
                        +bl_time_med_nm #desagrege
                        +bl_time_presentbias_nm #desagrege
                        +bl_risk_high_nm #desagrege
                        +factor(bl_block), #randomblock
                        cluster='bl_date_baseline',data=data_endline_wage)

summary(estimates)

#ici aussi, on garde reg 1 

#####################################
########## fonction de regression
#####################################

# sans transfo a la var de treatment
reg_bl_cov <- function(var_to_prompt,data_to_prompt){
  estimates <- lm.cluster(var_to_prompt~factor(treatment)
                          +bl_ed_gr12
                          +bl_score_num
                          +bl_score_lit
                          +bl_score_cft
                          +bl_score_grit
                          +noncog_score 
                          +bl_belsco_num
                          +bl_belsco_lit_ter 
                          +bl_belsco_cft_ter 
                          +bl_belest_index
                          +bl_age
                          +bl_male
                          +bl_emp_7d
                          +bl_time_med
                          +bl_time_presentbias
                          +bl_risk_high
                          +factor(bl_block), #randomblock
                          cluster='bl_date_baseline',data=data_to_prompt)
  
  summary(estimates)
  rm(estimates)}

# transfo var de treatment
reg_transfo <- function(var_to_prompt,data_to_prompt){
  
  data_to_prompt <- data_to_prompt %>% mutate(treat_c = log(treatment+sqrt((treatment^2) +1))) #appliquer log transfo sur la var traitement
  
  estimates <- lm.cluster(var_to_prompt~factor(treat_c)
                          +bl_ed_gr12
                          +bl_score_num
                          +bl_score_lit
                          +bl_score_cft
                          +bl_score_grit
                          +noncog_score 
                          +bl_belsco_num
                          +bl_belsco_lit_ter 
                          +bl_belsco_cft_ter 
                          +bl_belest_index
                          +bl_age
                          +bl_male
                          +bl_emp_7d
                          +bl_time_med
                          +bl_time_presentbias
                          +bl_risk_high
                          +factor(bl_block), #randomblock
                          cluster='bl_date_baseline',data=data_to_prompt)
  
  summary(estimates)
  rm(estimates)}

reg_bl_cov(data$el_emp_7d, data) #emploi // 0.51
reg_transfo(data$el_emp_earn_all_uncd_i, data) #salaire // 0.337
reg_transfo(data$el_emp_hour_all_uncd_i,data) #hours // 0.2016
reg_transfo(data$el_emp_wage_all_uncd_i,data) #hourly wage // 0.196 



# essayer de corriger le trop plein de std dev 
# https://stackoverflow.com/questions/44027482/cluster-robust-standard-errors-in-stargazer 
#0. commande integree pour lm.cluster 

#1. ols + effets fixes // + cluster // #est-ce que le probleme des ecarts-types vient des commandes ? 

#reg avec traitement log sur treatment
reg_via_ols_cluster_c <- function(var_to_prompt,data_to_prompt){
  data_to_prompt <- data_to_prompt %>% mutate(treat_c = log(treatment+sqrt((treatment^2) +1))) #appliquer log transfo sur la var traitement
  
  lm(var_to_prompt~factor(treat_c)
           +bl_ed_gr12
           +bl_score_num
           +bl_score_lit
           +bl_score_cft
           +bl_score_grit
           +noncog_score 
           +bl_belsco_num
           +bl_belsco_lit_ter 
           +bl_belsco_cft_ter 
           +bl_belest_index
           +bl_age
           +bl_male
           +bl_emp_7d
           +bl_time_med
           +bl_time_presentbias
           +bl_risk_high
           +factor(bl_block), 
           data=data_to_prompt)
} 

#reg sans traitement avec log 
reg_via_ols_cluster <- function(var_to_prompt,data_to_prompt){lm(var_to_prompt~factor(treatment)
                                                                   +bl_ed_gr12
                                                                   +bl_score_num
                                                                   +bl_score_lit
                                                                   +bl_score_cft
                                                                   +bl_score_grit
                                                                   +noncog_score 
                                                                   +bl_belsco_num
                                                                   +bl_belsco_lit_ter 
                                                                   +bl_belsco_cft_ter 
                                                                   +bl_belest_index
                                                                   +bl_age
                                                                   +bl_male
                                                                   +bl_emp_7d
                                                                   +bl_time_med
                                                                   +bl_time_presentbias
                                                                   +bl_risk_high
                                                                   +factor(bl_block), 
                                                                   data=data_to_prompt)
} 

ols1 <- reg_via_ols_cluster(data$el_emp_7d, data)
ols2 <- reg_via_ols_cluster_c(data$el_emp_earn_all_uncd_i, data)
ols3 <- reg_via_ols_cluster_c(data$el_emp_hour_all_uncd_i,data)
ols4 <- reg_via_ols_cluster_c(data$el_emp_wage_all_uncd_i,data)

# summary with cluster-robust SEs
summary(ols1, cluster="bl_date_baseline") #std: 0.015
summary(ols2, cluster="bl_date_baseline") #std: 0.096811
summary(ols3, cluster="bl_date_baseline") #std: 0.0590654
summary(ols4, cluster="bl_date_baseline") #std: 0.056860

# create table in stargazer // plus tard 
stargazer(ols1, se=list(coef(summary(ols1,cluster = c("bl_date_baseline")))[, 2]), type = "text") 

# ou de la modelisation ? 

#2. regression en panel 

# log transfo sur dummy
panel_data <- data %>% mutate(treat_c = log(treatment+sqrt((treatment^2) +1))) #appliquer log transfo sur la var traitement

# set df into panel form
panel_data <- pdata.frame(panel_data, index = c("anonid", "bl_block"))

#within pour un fixed effect model 
fixed_effect_model_c <- function(var_to_prompt, t_var, data_to_prompt){ 
  fe <- plm(var_to_prompt ~
                            factor(t_var)
                          +bl_ed_gr12
                          +bl_score_num
                          +bl_score_lit
                          +bl_score_cft
                          +bl_score_grit
                          +noncog_score 
                          +bl_belsco_num
                          +bl_belsco_lit_ter 
                          +bl_belsco_cft_ter 
                          +bl_belest_index
                          +bl_age
                          +bl_male
                          +bl_emp_7d
                          +bl_time_med
                          +bl_time_presentbias
                          +bl_risk_high, data = data_to_prompt, model = "within", effect = "time", vcov = "cluster", cluster = "bl_date_baseline")

summary(fe)

}

fixed_effect_model_c(panel_data$el_emp_earn_all_uncd_i,panel_data$treat_c,panel_data) #earnings, std: 0.096
fixed_effect_model_c(panel_data$el_emp_earn_all_uncd_i,panel_data$treat_c,panel_data)

### je pense que ca vient de ma transfo et de la leur, parce que mes reg en niveau, y a aucun probleme 

###########################
######## Table 2 ######## 
###########################

# pour trouver la table 2, on a juste a faire la difference avec l'intercept, qui est le groupe controle

ols1 <- ols1 %>% tidy() #transformer ses estimations en df

table2 <- function(var_to_prompt,data_to_prompt) {
  
  est <- reg_via_ols_cluster_c(data_to_prompt$var_to_prompt,data_to_prompt)
  table2_var_to_input <- est %>% tidy()
  
}

table2(data$el_emp_earn_all_uncd_i,data)


######################################
##### figure 2 plots // table D.2 online appendix
######################################

### cdf of earnings 

data_cdfplot_control <- data %>% filter(control==1)
CDF_control <- ecdf(data_cdfplot_control$el_emp_earn_all_uncd_i) 

data_cdfplot_public <- data %>% filter(public==1)
CDF_pub <- ecdf(data_cdfplot_public$el_emp_earn_all_uncd_i) 

data_cdfplot_private <- data %>% filter(private==1)
CDF_pri <- ecdf(data_cdfplot_private$el_emp_earn_all_uncd_i) 


plot(CDF_control, verticals=TRUE, do.points=FALSE,col='red')
plot(CDF_pub, verticals=TRUE, do.points=FALSE, add=TRUE, col='blue')
plot(CDF_pri, verticals=TRUE, do.points=FALSE, add=TRUE, col='black')


### quantile treatment effects
#https://cran.r-project.org/web/packages/qte/vignettes/R-QTEs.html#:~:text=QTEs%20are%20defined%20as%20the,the%20untreated%20potential%20outcome%20distribution. 

install.packages("qte")
library(qte)

jt.rand <- ci.qtet(el_emp_earn_all_uncd_i ~ private, data=data_endline_wage, probs=seq(0.6,1,0.025), se=T, iters=10)
jt.rand <- ci.qtet(el_emp_earn_all_uncd_i ~ public, data=data_endline_wage, probs=seq(0.6,1,0.025), se=T, iters=100)
ggqte(jt.rand)
summary(jt.rand)


### toutes les autres reg

######################################
#table 3 
######################################


reg_bl_cov(data$el_pctac, data) #skill beliefs , tout ok 
reg_bl_cov(data$el_selfest_med,data) # med self esteem // pas les memes // meme nb d'obs tout de meme

data_search <- data %>% filter(!is.na(bl_search_any)) #bon nb d'obs 
reg_bl_cov(data_search$el_search_target1, data_search) # targeted search // proches, 0.49% + meme std 

# match <- data %>% filter(!is.na(el_date) & !is.na(el_selfest_demean) & !is.na(el1_lag) & !is.na(el_selfest_med) & !is.na(el_index4) & !is.na(el_index5))
# toutes des vars apres traitement

data_since_treat <- data %>% filter(!is.na(el1_lag)) #bon nb d'obs 
reg_transfo(data_since_treat$el_report_use,data_since_treat) # a peu pres ca aussi 

interviews <- data %>% filter(!is.na(el1_lag) & !is.na(el_report_ints)) #bon nb d'obs 
reg_bl_cov(interviews$el_report_ints,interviews) # a peu pres ca aussi 

apps <- data %>% filter(!is.na(el1_lag) & !is.na(el_report_apps_i)) #bon nb d'obs 
reg_transfo(apps$el_report_apps_i,apps)

offers <- data %>% filter(!is.na(el1_lag) & !is.na(el_report_offs)) #bon nb d'obs 
reg_bl_cov(offers$el_report_offs,offers) # a peu pres ca aussi 

expoffer <- data %>% filter(!is.na(el_belsear_off_i))
reg_transfo(expoffer$el_belsear_off_i,expoffer)






# comprendre le cadre causal, comment randomisé
lm(formula = control ~ factor(bl_block), data = data)
