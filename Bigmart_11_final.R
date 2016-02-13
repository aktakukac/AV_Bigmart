################################# HEADER #########################

## Script created by Mihaly Garamvolgyi
## 2016/02/09
## R version 3.1.2 (2014-10-31) Pumpkin Helmet

################################# HEADER #########################

start.time <- Sys.time()

# Automatikus package telepítés
packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

# packages betöltése
packages(dplyr)   
packages(ggplot2) # initial data exploration
packages(car)     # recode function
packages(xgboost) # modeling
packages(caret)
packages(AppliedPredictiveModeling)
packages(data.table)


# working directory
setwd("C:/Misi/Vidhya/Bigmart")

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





# ----- Create final test and train data  ------------------------

Train <- DT[DT$Segment=='Train',]

Test <- DT[DT$Segment=='Test',]

Train_original <- DT[DT$Segment=='Train',]

Train_values <- Train$Item_Outlet_Sales

Test_ID_item <- Test$Item_Identifier

Test_ID_outlet <- Test$Outlet_Identifier

Test_ID_outlet_type <- Test$Outlet_Type

Columns <- c(colnames(Train)[12:13], 'Item_Identifier' , 'Outlet_Identifier') # exclude identifiers, segment and value

Train <- Train[!colnames(Train) %in% Columns]

Test <- Test[!colnames(Test) %in% Columns]


# ============================== END DATA PREPARATION ====================

# ---- xgboost ---------

XG_Train <- xgb.DMatrix(as.matrix(Train),label=Train_values)

XG_Test = xgb.DMatrix(as.matrix(Test))

param <- list(
  objective = 'reg:linear',  # --linear regression / output: value
  eta = 0.1,
  gamma = 1,
  eval_metric = 'rmse' ,
  min_child_weight = 4,
  max_depth = 4,
  subsample = 0.85,
  colsample_bytree = 0.5,
  max_delta_step = 20
)

rounds <- 1500

XG_Model <- xgb.train(param, XG_Train,rounds)

XG_Prediction <- predict(XG_Model, XG_Test)

# convert back to original scale with exponential
# !!!
XG_Prediction <- exp(XG_Prediction)

XG_Prediction <- cbind(as.character(Test_ID_item), as.character(Test_ID_outlet), Test_ID_outlet_type , as.character(XG_Prediction))

colnames(XG_Prediction) <- c("Item_Identifier", "Outlet_Identifier", "Item_Outlet_type", "Item_Outlet_Sales") 

XG_Feature_names <- dimnames(Train)[[2]]

XG_importance_matrix <- xgb.importance(XG_Feature_names, model = XG_Model)

# print(XG_importance_matrix)

XG_Train_Prediction <- predict(XG_Model, XG_Train)

XG_Error <- sqrt(sum(((XG_Train_Prediction- Train_original$Item_Outlet_Sales)^2))/length(XG_Train_Prediction))

XG_Error_Total <- sum(((XG_Train_Prediction- Train_original$Item_Outlet_Sales)^2))

print(paste("test error total: ", round(XG_Error_Total, 3)))

print(paste("test error RMSE: ", round(XG_Error, 3)))

write.table(XG_Prediction, 
          file="submission_11_20160210_final.csv", 
          sep=",",
          row.names = FALSE,
          col.names = TRUE, 
          quote=FALSE
          )


end.time <- Sys.time()

time.taken <- end.time - start.time

print(paste("elapsed time: ", round(time.taken, 2)))

# Changes vs benchmark:
# visibility is logarithm
# output is logarithm
# excluded fat content
# AV score: 1174.49240846
# [1] "test error total:  1673.175"
# "test error RMSE:  0.443"
# "elapsed time:  20.63"
