################################# HEADER #########################

## Script created by Mihaly Garamvolgyi
## 2016/02/09
## R version 3.1.2 (2014-10-31) Pumpkin Helmet

################################# HEADER #########################

start.time <- Sys.time()

# Automatikus package telep?t?s
packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

# packages bet?lt?se
packages(dplyr)   
packages(ggplot2) # initial data exploration
packages(car)     # recode function
packages(xgboost) # modeling
packages(caret)
packages(caretEnsemble)
packages(AppliedPredictiveModeling)
packages(data.table)
packages(h2o)


# working directory
setwd('~/Vidhya/Bigmart')

Train <- read.csv("./Data/train.csv", na.strings = "")
Test <- read.csv("./Data/test.csv", na.strings = "")
# Sample <- read.csv("./Data/SampleSubmission.csv", na.strings = "")

Train <- mutate(Train, Segment="Train")
Test <- mutate(Test, Segment="Test")
Test <- mutate(Test, Item_Outlet_Sales="")

DT <- rbind(Train, Test)

# ===================== DATA PREPARATION ================

Mean_Item_Weight <- mean(DT$Item_Weight, na.rm=TRUE)

DT$Item_Weight <- replace(DT$Item_Weigh, is.na(DT$Item_Weight), 0)

# LF, Low Fat, low fat, Regular, reg to be corrected
# correct it to numeric later!
DT$Item_Fat_Content <- as.numeric(recode(DT$Item_Fat_Content, 
                                         "'reg'=0; 'Regular'=0; 
                                         'LF'=1; 'low fat'=1; 'Low Fat'=1; 
                                         else=0", 
                                         as.factor.result=FALSE))


# DT$Item_Visibility should probably be log()
DT$Item_Visibility <- replace(DT$Item_Visibility, is.na(DT$Item_Visibility), 0)

# !!!
DT$Item_Visibility <- log(DT$Item_Visibility * 1000)

DT$Item_Visibility <- replace(DT$Item_Visibility, is.infinite(DT$Item_Visibility), 0)

# recode DT$Item_type to numeric
# change this to numeric later! 
DT$Item_Type <- as.numeric(recode(DT$Item_Type, 
                                         "'Baking Goods'=1; 'Breads'=2; 'Breakfast'=3; 'Canned'=4;
                                         'Dairy'=5; 'Frozen Foods'=6; 'Fruits and Vegetables'=7; 'Hard Drinks'=8;
                                         'Health and Hygiene'=9; 'Household'=10; 'Meat'=11; 'Others'=12;
                                         'Seafood'=13; 'Snack Foods'=14; 'Soft Drinks'=15; 'Starchy Foods'=16;
                                         else=0", 
                                         as.factor.result=FALSE))
 
# recode DT$Item_MRP missing values with mean
Mean_Item_MRP <- mean(DT$Item_MRP, na.rm=TRUE)

DT$Item_MRP <- replace(DT$Item_MRP, is.na(DT$Item_MRP), Mean_Item_MRP)

# recode DT$Establishment_Year to numeric
# change this to numeric later! 
DT$Outlet_Establishment_Year <- as.factor(DT$Outlet_Establishment_Year) 

DT$Outlet_Establishment_Year <- as.numeric(recode(DT$Outlet_Establishment_Year, 
                                        "'1985'=1; '1987'=1; '1997'=2; '1998'=2;
                                         '1999'=2; '2002'=3; '2004'=3; 
                                          else=4", 
                                  as.factor.result=FALSE))

# recode DT$Outlet_Size
# change this to numeric later! 
DT$Outlet_Size[DT$Outlet_Type=="Grocery Store"] <- "Small"

DT$Outlet_Size[DT$Outlet_Type=="Supermarket Type1" & DT$Outlet_Location_Type=="Tier 2"] <- "Small"


DT$Outlet_Size <- as.numeric(recode(DT$Outlet_Size, 
                                 "'Small'=0; 'Medium'=1; 'High'=2; else=0", 
                                 as.factor.result=FALSE))

# recode DT$Outlet_Location_Type
DT$Outlet_Location_Type <- as.numeric(recode(DT$Outlet_Location_Type, 
                                   "'Tier 1'=0; 'Tier 2'=1; 'Tier 3'=2; else=0", 
                                   as.factor.result=FALSE))


# recode DT$Outlet_Type
DT$Outlet_Type <- as.numeric(recode(DT$Outlet_Type, 
                                            "'Grocery Store'=0; 'Supermarket Type1'=1; 'Supermarket Type2'=2; ; 'Supermarket Type3'=3 ; else=0", 
                                            as.factor.result=FALSE))

DT$Item_Outlet_Sales <- as.numeric(DT$Item_Outlet_Sales)

Mean_Item_Outlet_Sales <- mean(as.numeric(DT$Item_Outlet_Sales), na.rm=TRUE)

DT$Item_Outlet_Sales <- replace(DT$Item_Outlet_Sales, is.na(DT$Item_Outlet_Sales), 0)

# logarithm
DT$Item_Outlet_Sales <- log(DT$Item_Outlet_Sales)

DT$Item_Outlet_Sales <- replace(DT$Item_Outlet_Sale, is.infinite(DT$Item_Outlet_Sale), 0)

# ----- any engineered features should come here -----------------

DT$E_feature_1 <- as.factor(paste(as.character(DT$Outlet_Size), as.character(DT$Outlet_Location_Type), sep="_") )

DT$E_feature_2 <- as.factor(paste(as.character(DT$E_feature_1), as.character(DT$Outlet_Type), sep="_") )

DT$E_feature_3 <- as.factor(paste(as.character(DT$E_feature_2), as.character(DT$Outlet_Establishment_Year), sep="_") )
                     

# ----- Create final test and train data  ------------------------

Train <- DT[DT$Segment=='Train',]

# remove missing values 
#Train <- Train[Train$Item_Visibility > 0.00000,]

Test <- DT[DT$Segment=='Test',]

Train_original <- Train

Train_values <- Train$Item_Outlet_Sales

Test_ID_item <- Test$Item_Identifier

Test_ID_outlet <- Test$Outlet_Identifier

# recode outlet identifiers 

levels(Test$Outlet_Identifier) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

Test$Outlet_Identifier <- as.numeric(Test$Outlet_Identifier)

levels(Train$Outlet_Identifier) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

Train$Outlet_Identifier <- as.numeric(Train$Outlet_Identifier)

Columns <- c(colnames(Train)[12:13], 'Item_Identifier', 'Outlet_Identifier') # exclude identifiers, segment and value

Train <- Train[!colnames(Train) %in% Columns]

Test <- Test[!colnames(Test) %in% Columns]


# ============================== END DATA PREPARATION ====================

# ----- deep learining h2o -----------------

# h2o needs factor variables! 
Train$Item_Type <- as.factor(Train$Item_Type)
Train$Outlet_Establishment_Year <- as.factor(Train$Outlet_Establishment_Year)
Train$Outlet_Size <- as.factor(Train$Outlet_Size)
Train$Outlet_Location_Type <- as.factor(Train$Outlet_Location_Type)
Train$Outlet_Type <- as.factor(Train$Outlet_Type)

Test$Item_Type <- as.factor(Test$Item_Type)
Test$Outlet_Establishment_Year <- as.factor(Test$Outlet_Establishment_Year)
Test$Outlet_Size <- as.factor(Test$Outlet_Size)
Test$Outlet_Location_Type <- as.factor(Test$Outlet_Location_Type)
Test$Outlet_Type <- as.factor(Test$Outlet_Type)

Train <- cbind(Train, Train_values )

h2o.init()

h2o_Train <- as.h2o(Train)
h2o_Test <- as.h2o(Test)

h2o_Model <- h2o.deeplearning(
  x=c(1:12),
  y=13,
  train_samples_per_iteration= -2,
  training_frame=h2o_Train,
  hidden = c(100,100),
  epochs=16,
  # force_load_balance = TRUE,
  # use_all_factor_levels= TRUE,
  # Variable_importance = TRUE,
  nfolds=5,
  score_interval=5,
  score_training_samples = 0,
  stopping_rounds = 0,
  # single_node_mode = T,
  # signore_const_cols = TRUE,
  max_categorical_features =2147483647,
  distribution = 'AUTO',
  activation = 'TanhWithDropout',
  use_all_factor_levels = T,
  variable_importances = T,
  adaptive_rate = T,
  input_dropout_ratio = 0,
  loss = 'Automatic',
  epsilon = 1e-8,
  rho = 0.99,
  initial_weight_distribution = 'UniformAdaptive',
  single_node_mode = T,
  force_load_balance = T,
  fast_mode = T,
  diagnostics = T,
  fold_assignment= 'AUTO'
  # can be "AUTO", "Modulo", "Random" or "Stratified"
)

h2o_Prediction <- as.data.frame(h2o.predict(h2o_Model, h2o_Test))

h2o_Prediction$predict <- exp(h2o_Prediction$predict)

h2o_Prediction <- cbind(as.character(Test_ID_item), as.character(Test_ID_outlet), as.character(h2o_Prediction$predict))

colnames(h2o_Prediction) <- c("Item_Identifier", "Outlet_Identifier", "Item_Outlet_Sales") 

h2o_Train_Prediction <- as.data.frame(h2o.predict(h2o_Model, h2o_Train))

h2o_Error <- sqrt(sum(((h2o_Train_Prediction- Train_original$Item_Outlet_Sales)^2))/length(h2o_Train_Prediction))

h2o_Error_Total <- sum(((h2o_Train_Prediction- Train_original$Item_Outlet_Sales)^2))

print(paste("test error total: ", round(h2o_Error_Total, 3)))

print(paste("test error RMSE: ", round(h2o_Error, 3)))

write.table(h2o_Prediction, 
            file="submission_20_20160212.csv", 
            sep=",",
            row.names = FALSE,
            col.names = TRUE, 
            quote=FALSE
)

#  h2o.shutdown()

end.time <- Sys.time()

time.taken <- end.time - start.time

print(paste("elapsed time: ", round(time.taken, 2)))

# Changes vs benchmark:
# h2o algorithm
# output is logarithm
# excluded fat content
# AV score: 
#  [1] "test error total:  2647.383"
# [1] "test error RMSE:  51.453"
# [1] "elapsed time:  2.19"