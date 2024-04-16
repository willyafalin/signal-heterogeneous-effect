
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

data <- read_dta("workspaces/signalling-heterogeneous-effect/0-dataset/wsdata_cleaned_signalling.dta")


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
                                      obs_restantes = total_count-na_count,
                                      percentage_not_na = obs_restantes/total_count*100) 
  
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

var_names <- dicotest[1]

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

#2. Preprocessing of X features - returns to 89 features, by :
#################################################################################

#a. keeping dummies as they are 
#b. c. rescaling continuous variables 
#d. splitting categorical variables into dummies 
# for each aforementioned variable categories : remove variables with too many NAs (see dictionary of variables associated)

# a. first, we dissect the dataset into dummies, categorical vars, continuous vars 

# continuous variables to rescale 

continuous <- dico3 %>% subset(unique_count > 5)
add_to_continuous <- dico3 %>% filter(variable_name == "bl_score_plan") #add it as well, as bl_score_plan is not categorical
continuous <- bind_rows(continuous,add_to_continuous)
continuous <- continuous %>% select(c("variable_name")) #keep only the variable name to then select within the dataset 

# dummy vars, no need to be preprocessed # although needs to remove vars with too many NAs

dummy_90 <- dico3 %>% subset(unique_count < 3 & percentage_not_na >80) # dummies with around 90% population minimum
dummy_80 <- dico3 %>% subset(unique_count < 3 & percentage_not_na >30) # dummies with around 70% population minimum

dummy_90 <- dummy_90 %>% select(c("variable_name")) %>% filter(!variable_name %in% c("control", "placebo", "private")) #keep only the variable name to then select within the dataset + keep only the public treatment category, i.e. remove categories private, placebo, control 
dummy_80 <- dummy_80 %>% select(c("variable_name")) %>% filter(!variable_name %in% c("control", "placebo", "private")) #keep only the variable name to then select within the dataset + keep only the public treatment category, i.e. remove categories private, placebo, control 


# categorical vars to bu cut into dummies 

categorical <- dico3 %>% subset(unique_count > 2 & unique_count < 6)
categorical <- categorical %>% filter(variable_name != "bl_score_plan")
categorical <- categorical %>% select(c("variable_name")) #keep only the variable name to then select within the dataset 


# b. create a rescale function. we want to normalise continuous variables between 0 and 1 

rescale <- function(x, new_min = 0, new_max = 1) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) *
    (new_max - new_min) + new_min
}


# return into dataframe, with selected vars 

x_data_continuous <- x_data_to_preprocess[, names(x_data_to_preprocess) %in% continuous$variable_name] #keep columns listed as continuous vars
x_data_categorical <- x_data_to_preprocess[, names(x_data_to_preprocess) %in% categorical$variable_name] #keep columns listed as categorical vars

x_data_dummy_90 <- x_data_to_preprocess[, names(x_data_to_preprocess) %in% dummy_90$variable_name] #keep columns listed as dummy vars, 90% pop
x_data_dummy_80 <- x_data_to_preprocess[, names(x_data_to_preprocess) %in% dummy_80$variable_name] #keep columns listed as dummy vars, 80% pop



# c. now you rescale 

rescale_columns <- function(data) {
  apply(data, 2, function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

x_data_continuous$bl_age <- rescale(x_data_continuous$bl_age) #for one particular variable

x_data_continuous <- as.data.frame(rescale_columns(x_data_continuous)) # rescaling for each column

dico_x_continuous <- dico(x_data_continuous) # check if the rescaling worked - yes, max = 1, min = 0

  # remove features with too many NAs 

    # keep vars name to remove below 80% 
continuous_vars_above_80 <- dico_x_continuous %>% filter(percentage_not_na > 80) %>% select(variable_name)
x_data_continuous_vars_above_80 <- x_data_continuous[,continuous_vars_above_80$variable_name] # dataset // ok, can append since same nb of total obs 

    # below 30%
continuous_vars_above_70 <- dico_x_continuous %>% filter(percentage_not_na > 30) %>% select(variable_name)
x_data_continuous_vars_above_70 <- x_data_continuous[,continuous_vars_above_70$variable_name] # dataset // ok, can append since same nb of total obs 


# d. categorical vars into dummies
##################################

dico_x_categorical <- dico(x_data_categorical)

x_data_categorical$bl_race <- x_data_categorical$bl_race - 1 #so that the reference category for race becomes 0 #careful, do it once only !

    # remove features with too many NAs // before creating dummies for each category ! because you create also Na indicators, which are fully populating the database - and not adding info whatsoever

        # keep vars name to remove below 80% 
categorical_vars_above_80 <- dico_x_categorical %>% filter(percentage_not_na > 80) %>% select(variable_name)
x_data_categorical_vars_above_80 <- x_data_categorical[,categorical_vars_above_80$variable_name] # dataset 

        # below 30%
categorical_vars_above_70 <- dico_x_categorical %>% filter(percentage_not_na > 30) %>% select(variable_name)
x_data_categorical_vars_above_70 <- x_data_categorical[,categorical_vars_above_70$variable_name] # dataset

    # now, create dummies for the two above datasets 

install.packages("fastDummies")
library(fastDummies)

    # dataset with 90% of features populated 
x_categorical_NA_90 <- dummy_cols(x_data_categorical_vars_above_80,select_columns = c(categorical_vars_above_80$variable_name)) #create dummies for each category, for each variable in the df 
x_categorical_NA_right_cols_90 <- x_categorical_NA_90[,-which(names(x_categorical_NA_90) %in% categorical_vars_above_80$variable_name)] #keep only the created dummies, remove the initial categorical variables 
# x_categorical_NA_right_cols_90 <- x_categorical_NA_90[,-which(names(x_categorical_NA_90) %in% colnames(x_data_categorical_vars_above_80))] #keep only the created dummies, remove the initial categorical variables 

          # we now want to remove the reference dummies
var_categorical_90 <- colnames(x_categorical_NA_right_cols_90) #list of variables in the categorical dataset
var_cat_to_remove_index_90 <- grep("_0$",var_categorical_90) #find dummies with ref category 0, ending exactly with _0
var_cat_to_remove_90 <- var_categorical_90[var_cat_to_remove_index_90] #show the variables 
x_categorical_NA_right_cols_90 <- x_categorical_NA_right_cols_90[, !var_categorical_90 %in% var_cat_to_remove_90] #dataset // finds and removes the names of the variables ending with _0 within the list of colnames

dico_x_categorical_90 <- dico(x_categorical_NA_right_cols_90) # check if the transfo in dummies worked - yes, see unique_counts and min max


    # dataset with 80% of features populated 
x_categorical_NA_80 <- dummy_cols(x_data_categorical_vars_above_70,select_columns = c(categorical_vars_above_70$variable_name)) #create dummies for each category, for each variable in the df 
x_categorical_NA_right_cols_80 <- x_categorical_NA_80[,-which(names(x_categorical_NA_80) %in% colnames(x_data_categorical_vars_above_70))] #keep only the created dummies, remove the initial categorical variables 

          # we now want to remove the reference dummies
var_categorical_80 <- colnames(x_categorical_NA_right_cols_80) #list of variables in the categorical dataset
var_cat_to_remove_index_80 <- grep("_0$",var_categorical_80) #find dummies with ref category 0, ending exactly with _0
var_cat_to_remove_80 <- var_categorical_80[var_cat_to_remove_index_80] #show the variables 
x_categorical_NA_right_cols_80 <- x_categorical_NA_right_cols_80[, !var_categorical_80 %in% var_cat_to_remove_80] #dataset // finds and removes the names of the variables ending with _0 within the list of colnames

dico_x_categorical_80 <- dico(x_categorical_NA_right_cols_80) # check if the transfo in dummies worked - yes, see unique_counts and min max




# columns with NA remain though ! 


# e. append all 

x_data_preprocessed_90 <- cbind(x_data_dummy_90, x_data_continuous_vars_above_80, x_categorical_NA_right_cols_90) #append column-wise
x_data_preprocessed_80 <- cbind(x_data_dummy_80, x_data_continuous_vars_above_70, x_categorical_NA_right_cols_80) #append column-wise

dico_preprocessed_vars_90 <- dico(x_data_preprocessed_90) # characteristics of all our 79 features 
dico_preprocessed_vars_80 <- dico(x_data_preprocessed_80) # characteristics of all our 78 features 

dico_preprocessed_vars_80$variable_name[!dico_preprocessed_vars_80$variable_name %in% dico_preprocessed_vars_90$variable_name]

# 3. preprocessing of our endline variables 
#################################################################################

y_data <- data %>% select(el_emp_7d, el_emp_hour_all,el_emp_earn_all,el_search_target1,el_search_target2,el_search_target3,el_search_target4) #keep our outcome variables of interest

# preprocessing of the two continuous variables 

y_data$el_emp_hour_all <- rescale(y_data$el_emp_hour_all) #rescale hours
y_data$el_emp_earn_all <- rescale(y_data$el_emp_earn_all) #rescale wages

dico_y <- dico(y_data)

y_data_wage <- y_data %>% select(el_emp_earn_all)

# 3.1 preprocessing test 
#################################################################################

wage_x <- cbind(x_data_preprocessed,y_data_wage)

wage_x_dico <- dico(wage_x)

# cut the dataset of wages into public and controls 
wage_x_control <- wage_x %>% subset(public==0)
wage_x_public <- wage_x %>% subset(public==1)

wage_x_c <- dico(wage_x_control) #30% of dataset not NA
wage_x_p <- dico(wage_x_public) #30% of dataset not NA

# take out NAs 
wage_x_control_clean <- wage_x_control %>% filter(!is.na(el_emp_earn_all))
wage_x_public_clean <- wage_x_public %>% filter(!is.na(el_emp_earn_all))

# look at features that have too many NAs => we take them out of the LASSO selection
wage_x_cc <- dico(wage_x_control_clean) 
wage_x_pc <- dico(wage_x_public_clean) 

dicotest <- dico(data)

#################################################################################
#################################################################################
# LASSO
#################################################################################
#################################################################################

###### avec des datas les plus populees 

y_data_wage <- y_data %>% select(el_emp_earn_all)

clean_90 <- cbind(x_data_preprocessed_90,y_data_wage)
clean_90 <- na.omit(clean_90)
clean_90_x <- clean_90 %>% subset(select(-c("clean_90$el_emp_earn_all"))) ################### bon reprends ici 
clean_90_y <- clean_90[,el_emp_earn_all]


# ok , il faut que je supprime les NA des Y !!!!!!!!!! sinon ca marche pas 


library(glmnet)
library(ggplot2)

# create training and test data

set.seed(42)
train_index_x <- sample(1:nrow(x_data_preprocessed), size = 0.8 * nrow(x_data_preprocessed))
train_index_y <- sample(1:nrow(y_data_wage), size = 0.8 * nrow(y_data_wage))

# # Ensure the sample size for y_data is within the range of its length
# sample_size_y <- min(0.8 * nrow(y_data), nrow(x_data_preprocessed))
# train_index_y <- sample(1:nrow(y_data), size = length(train_index_x))
# 
# install.packages("caret")
# library(caret)
# 
# train_index <- createDataPartition(y_data, p = 0.8, list = FALSE)

X_train <- x_data_preprocessed[train_index_x, ]
y_train <- y_data_wage[train_index_y, ]

X_test <- x_data_preprocessed[-train_index_x, ]
y_test <- y_data_wage[-train_index_y, ]

# Cross-validation
lasso_model <- cv.glmnet(as.matrix(X_train), y_train, alpha = 1, nfolds = 5)

# Plot optimal alpha
plot(lasso_model)

# Prediction and evaluation
y_pred <- predict(lasso_model, s = "lambda.min", newx = as.matrix(X_test))
mse <- mean((y_pred - y_test)^2)

cat("Mean Squared Error:", mse, "\n")

test_score <- cor(y_pred, y_test)^2

cat("R-squared:", test_score, "\n")

# Extract optimal alpha
optimal_alpha <- lasso_model$lambda.min
cat("Optimal Alpha:", optimal_alpha, "\n")

# Fit final Lasso model with optimal alpha
lasso_final <- glmnet(as.matrix(X_train), y_train, alpha = 1, lambda = optimal_alpha)


######################### poubelle de code below 
install.packages("dummies")
library(dummies)

create_dummy_variables <- function(column) {
  column <- factor(column, levels = unique(na.omit(column)))  # convert column to factor
  column[is.na(column)] <- "NA"   # eeplace NA values with a placeholder
  dummies <- model.matrix(~ column - 1, data = data.frame(column))  #create dummy variables for the column
  colnames(dummies) <- paste0(names(column), "_", colnames(dummies))
  return(dummies)
}

# apply the function to each column in dataset A
dummy_variables_list <- lapply(x_data_categorical, create_dummy_variables)

x_data_categorical_clean <- as.data.frame(do.call(cbind, dummy_variables_list))


dico_x_categorical <- dico(x_data_categorical)

# Assuming your dataset containing categorical variables is named x_data_categorical
cat_var <- colnames(x_data_categorical)  # extracting column names from the data frame

# Initialize an empty list to store dummy variables
dummy_vars <- list()

# Iterate over each categorical variable
for (var in cat_var) {
  # Create formula to specify the model matrix
  formula <- as.formula(paste0("~ -1 + ", var))
  
  # Create model matrix for the categorical variable
  cat_dummy <- model.matrix(formula, data = x_data_categorical)
  
  # Remove intercept column
  cat_dummy <- cat_dummy[, -1]
  
  # Add prefix to column names
  colnames(cat_dummy) <- paste0(var, "_", colnames(cat_dummy))
  
  # Add the dummy variables to the list
  dummy_vars[[var]] <- cat_dummy
}

# Combine the dummy variables into a single data frame
dummy_df <- do.call(cbind, dummy_vars)

# Append the dummy variables to the original data frame
x_data_categorical <- cbind(x_data_categorical, dummy_df)

# Remove the original categorical variables from the data frame
x_data_categorical <- x_data_categorical[, !(names(x_data_categorical) %in% cat_var)]

# Display the resulting data frame
print(x_data_categorical)


for (column in names(x_data_categorical)){
  data[[column]] <- factor(data[[column]])
  data[[column]][is.na(data[[column]])] <- "Missing"
  dummies <- model.matrix(~ data[[column]] - 1, data = data.frame(data[[column]]))  # create dummy variables for the column
  colnames(dummies) <- paste0(column, "_", colnames(dummies))   
  dummy_variables <- bind_cols(dummy_variables, as.data.frame(dummies)) # add the dummy variables to the dummy_variables dataframe
}





# finis le preprocessing. il faut mettre toutes les variables et laisser le lasso decider 

# Lasso = get Z variables 

# BLP analysis 



# CLAN analysis 



# GATES analysis









variables_to_preprocess <- data$variable_name[-indices_to_remove]

variables_y <- c(
  "el_emp_7d","el_emp_recruit_7d","el_emp_hour_all",        
  "el_search_7d","el_search_hour_7d","el_search_cost_7d",      
  "el_search_app","el_search_resp","el_search_off",          
  "el_belsear_app","el_belsear_mo","el_belsear_off" 
)


# we put the treatment variable to preprocess as well, hoping the lasso keeps it


variables_x <- c(
  "bl_dem_grant"           
  [4] "bl_emp_7d"               "bl_emp_earn_all"         "bl_search_cost"         
  [7] "bl_search_hour"          "bl_search_app"           "bl_search_off"          
  [10] "bl_belsco_num"           "bl_belsco_lit_ter"       "bl_belsco_cft_ter"      
  [13] "bl_belsear_app"          "bl_score_numter"         "bl_score_litter"        
  [16] "bl_score_cftter"         "bl_score_gritter"        "bl_score_planter"       
  [19] "bl_score_focuster"       "bl_score_flexter"        "bl_score_contter"       
  [22] "bl_race" "bl_male" "bl_age") 

# ignored 
# baseline : "bl_date_baseline" "bl_block"  "anonid"  
# endline :  "el_date"    "el_emp_m1"               "el_emp_m2"               "el_emp_m3"
# clean : "Edn_Matric"              "Edn_Cert"                "Edn_Degree"   

# added vars 
# bl_emp_earn_all, wage baseline, is not completely populated - may explain heterogeneous effect among public?
# bl_score_contter

# we should use unconditional data !!!! 

# transform categorical vars into dummies 

