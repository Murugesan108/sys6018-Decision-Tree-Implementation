###### Data Mining - Decision Trees and ROC Curves...from Scratch!
###### Murugesan Ramakrishnan
###### mr6rx
rm(list = ls())
library(dplyr)


fit.decision.tree <- function(input_data,min_observations = 3,max_split = 5){
  

  gini_index <- function(input_classes){
  ### Calculating the gini index after the split
  
  # Predictions in the given region
  classes_in_region <- input_classes
  
  ##Calculating the probability of each class k in the region m
  p_mk <- table(classes_in_region)/sum(table(classes_in_region))
  gini_index <- sum(p_mk * (1 - p_mk))
  return(gini_index)
  
  }
  
  ######## Function to split the tree
  
  tree_spilt <- function(input_tree_data, response){

    split_condition <- c()
    
    ##Initializing gini with a high value (larger than possible value)
    ##This has to be minimized by selecting the best split
    gini_min <- 10
    
    ## Selecting all the variables to loop except the response variable
    loop_vars <- names(input_tree_data)[!names(input_tree_data) %in% c(response,"Predict_region") ]
    
    
    ### Check if the region has already 100% correct predictions, if yes - we do not need to split further
    if( error_rate(input_tree_data,response, "Predict_region")[[1]] == 0){
      return(NULL)
    }
    
    ### Looping through every variable
    for(each in loop_vars){
      
      
      ###Looping through every 5% value of the column value
      split_values <- quantile(input_tree_data[[each]],seq(0,1,0.05))
      
      
      for(splits in split_values){
        
        
        input_tree_data$preds <- ifelse(input_tree_data[[each]] < splits,1,2)
        
        
        ### Now, checking if the observations per group is less than the minimum observation
        
        if(any(table(input_tree_data$preds) < min_observations  )) next
        
        ## Need to check if the split has resulted in two groups
        ## IF there is no split in groups, we skipt the process
        if(length(table(input_tree_data$preds)) < 2){
          next
          
        }
        
        
        total_rows <- nrow(input_tree_data)
        
        ### Getting the average of Gini index of the split groups
        ### Instead of simple average, we do a weighted average based on the size of the sample
        
        
        gini_score <- 
        
        ### Gini index of Split 1
        ( nrow(dplyr::filter(input_tree_data,preds == 1)) *
         gini_index(dplyr::filter(input_tree_data,preds == 1)[[response]]) +
        
        ### Gini index of Split 2
          nrow(dplyr::filter(input_tree_data,preds == 2)) *
          gini_index(dplyr::filter(input_tree_data,preds == 2)[[response]]) 
            
        )/ total_rows
        
        
      
        if(gini_score < gini_min){
          gini_min <- gini_score
          split_condition <- c(each, splits)
          
              }
            
          }
  
      }
        
  return(split_condition)  
  }
  
  input_data$Predict_region <- "" ## '' - None
  #### Predict_region will contain the region of every split
  #### This will also help me decide which region I should split next
  #### '' - will become 'L' and 'R'
  #### 'L' will become 'LL' and 'LR'
  #### 'LL' will further split to 'LLL' and 'LLR' and so on
  
  ### So, I decide the next split based on the length of this character 
  ### lesser the size, higher the priority to split

  
  ### Condition for stoppage
  delta_error <- 0.01 ## Atleast 1% change in error rate should be observated for further split
  #max_split 
  ### The decision tree stops based on the above condition, whichever comes first
  
  splits_done <- 0
  
  ##Varaible to store the name of the region where there can't be any more split (with respect to the split function)
  ## Conditions for no more split - 1) If the region has less than minimum observations
  ## 2) If the region already has 100% accuracy
  ## 3) The split does not give a difference in error rate of 1%
  
  no_more_split <- c()
  count <- 0
  response <- "Species"
  
  tree_splits_rules <- list()
  error_prev <- 1 #Setting the error to be 1 or 100% before starting the split process
  
  
  ### Doing a while loop with the stoppage condition
  while(splits_done < max_split){
    
    ## Getting backup of the data
    
    input_data_backup <- input_data
    ### Select the region to be split 
    ### Region is selected based on the region name size
    ### If we have 'LL' and 'R' - 'R' will be selected over 'LL' since 'R' is in level 2 of the tree
    
    ## Regions available
    #### Excluding the region with just 1 record in them
    regions <- names(table(input_data$Predict_region)[table(input_data$Predict_region) > min_observations])
    
    ### Removing the regions from 'no more split' variable
    
    regions <- regions[!regions %in% no_more_split]
    
    if(length(regions) == 0){
      break("No more splits can be performed")
    }
    
    region_in_consideration <- regions[which.min(nchar(regions))]
    
    
    ### Skip if the selected region has just one record
    
    
    ###Filtering the input based on the region
    
    selected_data <- dplyr::filter(input_data, Predict_region == region_in_consideration)
    rest_of_the_data <- dplyr::filter(input_data, Predict_region != region_in_consideration)
    
    
    
    ## Getting the split condition for the region from 
    split_condition <- tree_spilt(selected_data, response)
    
    tree_splits_rules[[region_in_consideration]] <- split_condition
    
    ### If the split is not possible, we skip to the next region
    if(is.null(split_condition)){
      no_more_split <- c(no_more_split,region_in_consideration)
      next
       
    }
    
    
    
    ### Renaming the region with the existing region name + "L" or "R"
    selected_data$Predict_region <- paste0(selected_data$Predict_region,
                      ifelse(selected_data[[split_condition[1]]] < split_condition[2],"L","R"))
    
    
    input_data <- rbind(selected_data,rest_of_the_data)
    
    
    
    error <- error_rate(input_data,response,"Predict_region")[[1]]
    error_change <- error_prev - error
    error_prev <- error
    
    ## If the error change is less thatn 1% we in-validate the current split getting back the
    ### original data without any split. Add the node to 'no_more_split' variable, so that
    ## we do not split the region any more
    if(error_change < delta_error){
      
      input_data <- input_data_backup
      no_more_split <- c(no_more_split,region_in_consideration)
      next  
    }
    
    splits_done <- splits_done + 1
    
    
  }


  return(tree_splits_rules)
    
}


# Function to calculate the Error rate

error_rate <- function(input_data, response, region){
  
  names(input_data)[names(input_data) == response] <- "response"
  
  input_data[["response"]] <- as.character(input_data[["response"]])
  
  ### Finding the probability of maximum class "prediction_prob" and the maximum class "predict_class"
  predict_prob <- input_data %>% group_by_(region) %>%
    summarize(prediction_prob = max(table(response))/sum(table(response)),
              predict_class = names(which.max(table(response))))
  
  ### Finding the overall error rate
  response_data <-  suppressMessages(left_join(input_data,predict_prob))
  
  
  ### Calculating the error rate based on the predicted class
  error_table <- table(response_data$predict_class == response_data$response)
  
  ## There are no FALSE value - return zero
  if(!"FALSE" %in% names(error_table)){
    return(0)
  }
  error_rate <- error_table[["FALSE"]]/nrow(response_data)
  
  return(list(error_rate,response_data))
  
}



### Prediction of new data based on the tree condition
identify_region <- function(current_record,tree_splits_rules){
  ##Creating a loop to enter the region based on the Tree rules
  next_node <- 1
  for(depth in 1:length(tree_splits_rules)){
    
    current_rule <- tree_splits_rules[[next_node]]
    
    ###Finding out the next direction from the current split
    next_direction <- ifelse(current_record[[current_rule[1]]] < current_rule[[2]],"L","R")
    
    ### Setting the direction with the current node
    next_node <- paste0(gsub("1","",next_node),next_direction)
    
    ### See if we have split for the next node
    
    
    if(!next_node %in% names(tree_splits_rules)) break
    
    
  }
  return(next_node)
  
  
}



#### Predict test
predict_test <- function(test_data, tree_rules, class_prob,
                         predict_for = "versicolor",actual = "Species"){
  
  
  region_predictions <- unique(class_prob[c("regions","prediction_prob","predict_class")])
  
  ##TEST Predictions for each region
  test_data$regions <- apply(test_data,1,identify_region,tree_rules)
  
  test_data <- suppressMessages(left_join(test_data,region_predictions))
  
  test_data$prediction_prob <- ifelse(test_data[[actual]] == predict_for,
                                      test_data$prediction_prob,
                                      1 - test_data$prediction_prob)
  
  return(test_data)
  
  
   
}

#########PREPARING THE DATA###############################
###### Splitting the iris dataset into train and test


iris_data <- iris[iris$Species != 'setosa',]
input_data <- iris_data

train_index <- sample(row.names(input_data),nrow(input_data) * 0.66)
test_index <- row.names(input_data)[!row.names(input_data) %in% train_index]


input_train <- input_data[train_index,]
input_test <- input_data[test_index,]


#################### FITTING THE DECISION TREE FOR TRAINING DATASET ##############

tree_rules <- fit.decision.tree(input_train,min_observations = 3)
tree_rules
##Predictions for each region
input_train$regions <- apply(input_train,1,identify_region,tree_rules)

err_rate_vals <- error_rate(input_train,"Species","regions")
## Evaluating the performance for the decision tree 
print("Training Error Rate")
print(err_rate_vals[[1]])

class_prob <- err_rate_vals[[2]]



################# PERFORMING PREDICTIONS ON THE TEST DATASET #####################

### Predicting the test data using the rules
test_with_preds <- predict_test(input_test,tree_rules,class_prob)

preds_table <- test_with_preds[c("Species","predict_class")] 

print("Testing Error Rate")
100 * (1-  table(preds_table[1] == preds_table[2])[["TRUE"]]/nrow(preds_table))




plot_roc_curve <- function(test_with_preds){
#### PLOTTING THE ROC CURVE #########

##Converting Veriscolor as positive class 
test_with_preds$class <- ifelse(test_with_preds$Species == "versicolor","P","N")

threshold_range = sort(seq(-0.1,1,0.01),decreasing = TRUE)
tp_rate <- c()
fp_rate <- c()

for(t in threshold_range){
  
  preds <- ifelse(test_with_preds$prediction_prob > t, "P","N")
  actual <- test_with_preds$class
  
  confusion_table <- table(preds,actual)
  
  ### Checking if we have got any positives at all
  
  if(!("P" %in% row.names(confusion_table))){
    
    tp_count = fp_count = 0

  }else{
    
    tp_count <- confusion_table["P","P"]
    fp_count <- confusion_table["P","N"]
  }
  
  
  
  tpr <- tp_count/sum(confusion_table[,"P"])
  fpr <- fp_count/sum(confusion_table[,"N"])
  
  fp_rate <- c(fp_rate,fpr)
  tp_rate <- c(tp_rate,tpr)
  
  
}


#### Plot for the ROC curve
plot(fp_rate,tp_rate,type = "l",xlim = c(0,1),ylim = c(0,1))
}




#### EXECUTING ROC CURVE FOR THE FIRST TIME #########

plot_roc_curve(test_with_preds)






############### REPEATING THE ROC PROCESS FOR DIFFERENT MODELS ################
####### RANDOM STOPPAGE CRITERIA FOR EACH ITERAETION IS GIVEN WITHIN THE LOOP ###

##### RUN THE FOLLOWING CODE AT ONCE TO SEE THE VALUE PRINTED ###########

for(repeats in 1:6){
  
cat("\n")  
print(paste0("Repeat No: ",repeats))

### Selecting the train and test data 
iris_data <- iris[iris$Species != 'setosa',]
input_data <- iris_data

train_index <- sample(row.names(input_data),nrow(input_data) * 0.66)
test_index <- row.names(input_data)[!row.names(input_data) %in% train_index]


input_train <- input_data[train_index,]
input_test <- input_data[test_index,]


## FITTING THE DECISION TREE FOR TRAINING DATASET ####


#### Randomly changing the stoppage criteria

min_obs_random <- sample(1:20,1)
max_split_random <- sample(1:10,1)

print("STOPPAGE CRITERIA")
print(paste0("Maximum number of nodes: ",max_split_random))
print(paste0("Minimum number of observations per split: ",min_obs_random))

tree_rules <- fit.decision.tree(input_train,min_observations = min_obs_random,max_split = max_split_random)
 
##Predictions for each region
input_train$regions <- apply(input_train,1,identify_region,tree_rules)

err_rate_vals <- error_rate(input_train,"Species","regions")
class_prob <- err_rate_vals[[2]]


print("Training Error Rate")
print(err_rate_vals[[1]] * 100)




### Predicting the test data using the rules
test_with_preds <- predict_test(input_test,tree_rules,class_prob)

preds_table <- test_with_preds[c("Species","predict_class")] 

print("Testing Error Rate")
print(100 * (1-  table(preds_table[1] == preds_table[2])[["TRUE"]]/nrow(preds_table)))


plot_roc_curve(test_with_preds)

}


###### The plot does not vary a lot for this dataset, while the difference can be seen
# for different datasets