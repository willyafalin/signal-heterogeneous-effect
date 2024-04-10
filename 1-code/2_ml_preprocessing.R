
#####
#####
# Implement Lasso 
#####


library(tidyverse)
library(dplyr)
library(haven) 
library(plm)
library("labelled")
library("miceadds")
library(plm)
library(estimatr)
library(stargazer)

data <- read_dta("/Users/willylin/Desktop/ensae/S1/statapp/wsdata_cleaned_signalling.dta")

#################################################################################
#################################################################################
# Dictionary of variables 
#################################################################################
#################################################################################

dico <- function(dataset_to_prompt){
  
  # 1.1 Empty dataset + Empty list
  
  # Empty dataset 
  
  result_df2 <- data.frame(variable_name = character(0), 
                           na_count = numeric(0), 
                           not_na_count =numeric(0), 
                           unique_count = numeric(0))
  
  # Empty lists
  
  list_of_na <- list() 
  mean_var<-list()
  std_var<-list()
  first_pctile<-list()
  nine_pctile<-list()
  maximum <- list()
  minimum <- list()
  
  # 1.2 Loop: Fill in the empty dataset, row by row
  
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
  
  #1.3 Loop: Add labels + Prepare columns to fill inside the Empty lists
  
  for (i in 1:dim(dataset_to_prompt)[2]){
    
    col1 <- numeric(dim(dataset_to_prompt)[2])
    
    for (j in 1:dim(dataset_to_prompt)[2]){
      col1[j] <-ifelse(var_label((dataset_to_prompt)[i])=="NULL",colnames(dataset_to_prompt)[i],var_label((dataset_to_prompt)[i]))
    } #loop j, labels of variables
    
    col5 <- numeric(dim(dataset_to_prompt)[2])
    for (k in 1:dim(dataset_to_prompt)[2]){
      col5[k] <- ifelse(is.Date(dataset_to_prompt[[i]]),NA,mean(dataset_to_prompt[[i]],na.rm = TRUE)
      ) #mean
    }
    
    col6 <- numeric(dim(dataset_to_prompt)[2])
    for (t in 1:dim(dataset_to_prompt)[2]){
      col6[t] <- ifelse(is.Date(dataset_to_prompt[[i]]),NA,sd(dataset_to_prompt[[i]],na.rm = TRUE)
      ) #std_var
    }
    
    col7 <- numeric(dim(dataset_to_prompt)[2])
    for (y in 1:dim(dataset_to_prompt)[2]){
      col7[y] <- ifelse(is.Date(dataset_to_prompt[[i]]),NA,quantile(dataset_to_prompt[[i]],c(0.1),na.rm = TRUE)
      ) #1st percentile
    }
    
    col8 <- numeric(dim(dataset_to_prompt)[2])
    for (w in 1:dim(dataset_to_prompt)[2]){
      col8[w] <- ifelse(is.Date(dataset_to_prompt[[i]]),NA,quantile(dataset_to_prompt[[i]],c(0.9),na.rm = TRUE)
      ) #9th percentile
    }
    
    col9 <- numeric(dim(dataset_to_prompt)[2])
    for (p in 1:dim(dataset_to_prompt)[2]){
      col9[p] <- ifelse(is.Date(dataset_to_prompt[[i]]),NA,max(dataset_to_prompt[[i]],na.rm = TRUE)
      ) #max  
    }
    
    col10 <- numeric(dim(dataset_to_prompt)[2])
    for (o in 1:dim(dataset_to_prompt)[2]){
      col10[o] <- ifelse(is.Date(dataset_to_prompt[[i]]),NA,min(dataset_to_prompt[[i]],na.rm = TRUE)
      ) #min  
    }    
        
    
    
    list_of_na[i] <- col1
    mean_var[i] <- col5
    std_var[i] <- col6
    first_pctile[i] <- col7 
    nine_pctile[i] <- col8
    maximum[i] <- col9
    minimum[i] <- col10
    
    
  }
  
  
  # Bind the columns together from the prior Loop
  
  col1 <- rbind(list_of_na,mean_var,
                std_var,
                first_pctile,
                nine_pctile,
                minimum,
                maximum)
  col1 <- as.data.frame(col1)
  col1 <- t(col1)
  
  #1.4 Append 1.2 (initially Empty dataframe) and 1.3 (initially Empty list) 
  
  result_df3 <- cbind(result_df2, col1)
  
  #1.5 Get the total of observations count 
  
  result_df3 <- result_df3 %>% mutate(total_count = na_count  + not_na_count,
                                      obs_restantes = total_count-na_count) 
  
  dictionnaire_data <- result_df3
}

dicotest <- dico(data) #resulting dictionary of variables, without cross-sectioning the dataset

#################################################################################
#################################################################################
# Preprocessing before LASSO, GATE and CLAN analyses 
#################################################################################
#################################################################################


#1. remove duplicate variables, reduce the number of features ~ 60 variables retained eventually # let the lasso find the features for us
#################################################################################

var_names <- dico[1]

# use only unconditional variables _uncd, without transformations 
  # so we should drop duplicates of variables without _uncd and with _i

variables_to_remove <- grep("_uncd|_i\\b",var_names$variable_name) #find variables including _uncd and _i with \\b word boundary
variables_to_remove <- var_names$variable_name[variables_to_remove] #show the variables 
variables_to_remove <- gsub("_uncd", "", variables_to_remove) #remove _uncd 

var_names_bis <- var_names[!var_names$variable_name %in% variables_to_remove, ] #finds the indices of the variables_to_remove within var_names$variable_name, and takes them out

# test score variables
  # between two same variables, we want to keep the var that is the most populated and has the most accurate information
# keep: _nm (numerical). remove: _md, _ter, and the regular variable 
  # ex: noncog_score_nm is more populated than noncog_score and more accurate than noncog_score_ter (tercile) noncog_score_md (indicator for above median)  

variables_to_remove2 <- grep("_md|_ter",var_names_bis) # _md _ter variables
var_names_bis[variables_to_remove2] #show the variables 

var_names_bis <- var_names_bis[-variables_to_remove2]

# remove all non relevant features for the analysis. 
# block of rando, id, clean vars 

variables_to_remove3 <- grep("anonid|block|Edn_|date|randearly|treatment|_m1|_m2|_m3|attrit",var_names_bis) # _md _ter variables
var_names_bis[variables_to_remove3] #show the variables 
var_names_bis <- var_names_bis[-variables_to_remove3]

data_to_preprocess <- data[, names(data) %in% var_names_bis] #keep columns listed in var_names_bis

dico2 <- dico(data_to_preprocess) # get the var dictionary for all variables remaining, y and x

variables_to_remove4 <- grep("\\bel",var_names_bis)
var_names_bis[variables_to_remove4] #show the variables 
var_names_bis_x <- var_names_bis[-variables_to_remove4]

x_data_to_preprocess <- data[, names(data) %in% var_names_bis_x] #keep columns listed as features

dico3 <- dico(x_data_to_preprocess) # get the var dictionary for features x only 

#2. Preprocessing - returns to 89 features, by :
#################################################################################

#a. keeping dummies as they are 
#b. c. rescaling continuous variables 
#d. splitting categorical variables into dummies 

# a. first, we dissect the dataset into dummies, categorical vars, continuous vars 

# continuous variables to rescale 

continuous <- dico3 %>% subset(unique_count > 5)
add_to_continuous <- dico3 %>% filter(variable_name == "bl_score_plan") #add it as well, as bl_score_plan is not categorical
continuous <- bind_rows(continuous,add_to_continuous)
continuous <- continuous %>% select(c("variable_name")) #keep only the variable name to then select within the dataset 

# dummy vars, no need to be preprocessed

dummy <- dico3 %>% subset(unique_count < 3)
dummy <- dummy %>% select(c("variable_name")) %>% filter(!variable_name %in% c("control", "placebo", "private")) #keep only the variable name to then select within the dataset + keep only the public treatment category, i.e. remove categories private, placebo, control 

# categorical vars to bu cut into dummies 

categorical <- dico3 %>% subset(unique_count > 2 & unique_count < 6)
categorical <- categorical %>% filter(variable_name != "bl_score_plan")
categorical <- categorical %>% select(c("variable_name")) #keep only the variable name to then select within the dataset 


# b. create a rescale function. we want to normalise continuous variables between 0 and 1 

rescale <- function(x, new_min = 0, new_max = 1) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) *
    (new_max - new_min) + new_min
}

continuous <- as.character(continuous)

x_data_continuous <- x_data_to_preprocess[, names(x_data_to_preprocess) %in% continuous$variable_name] #keep columns listed as continuous vars
x_data_dummy <- x_data_to_preprocess[, names(x_data_to_preprocess) %in% dummy$variable_name] #keep columns listed as dummy vars
x_data_categorical <- x_data_to_preprocess[, names(x_data_to_preprocess) %in% categorical$variable_name] #keep columns listed as categorical vars

# c. now you rescale 

rescale_columns <- function(data) {
  apply(data, 2, function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

x_data_continuous$bl_age <- rescale(x_data_continuous$bl_age) #for one particular variable

x_data_continuous <- as.data.frame(rescale_columns(x_data_continuous)) # rescaling for each column

dico_x_continuous <- dico(x_data_continuous) # check if the rescaling worked - yes, max = 1, min = 0


# d. categorical vars into dummies

dico_x_categorical <- dico(x_data_categorical)

cat_var <- colnames(x_data_categorical)  # extracting column names from the data frame

install.packages("fastDummies")
library(fastDummies)
x_data_categorical$bl_race <- x_data_categorical$bl_race - 1 #so that the reference category for race becomes 0
x_categorical_NA <- dummy_cols(x_data_categorical,select_columns = c(cat_var)) #create dummies for each category, for each variable in the df 
x_categorical_NA_right_cols <- x_categorical_NA[,-which(names(x_categorical_NA) %in% cat_var)] #keep only the created dummies, remove the initial categorical variables 

var_categorical <- colnames(x_categorical_NA_right_cols) #list of variables in the categorical dataset. we now want to remove the reference dummies
var_cat_to_remove_index <- grep("_0$",var_categorical) #find dummies with ref category 0, ending exactly with _0
var_cat_to_remove <- var_categorical[var_cat_to_remove_index] #show the variables 
x_categorical_NA_right_cols <- x_categorical_NA_right_cols[, !var_categorical %in% var_cat_to_remove] #finds and removes the names of the variables ending with _0 within the list of colnames

# columns with NA remain though ! 

dico_x_categorical <- dico(x_categorical_NA_right_cols) # check if the transfo in dummies worked - yes, see unique_counts and min max

# e. append all 

x_data_preprocessed <- cbind(x_data_dummy, x_data_continuous, x_categorical_NA_right_cols) #append column-wise

dico_preprocessed_vars <- dico(x_data_preprocessed) # characteristics of all our 89 features 


# 3. preprocessing of our endline variables 
#################################################################################

y_data <- data %>% select(el_emp_7d, el_emp_hour_all,el_emp_earn_all,el_search_target1,el_search_target2,el_search_target3,el_search_target4) #keep our outcome variables of interest

# preprocessing of the two continuous variables 

y_data$el_emp_hour_all <- rescale(y_data$el_emp_hour_all) #rescale hours
y_data$el_emp_earn_all <- rescale(y_data$el_emp_earn_all) #rescale wages

dico_y <- dico(y_data)


#################################################################################
#################################################################################
# LASSO
#################################################################################
#################################################################################


