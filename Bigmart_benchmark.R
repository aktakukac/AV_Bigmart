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


# working directory
setwd("C:/Misi/Vidhya/Bigmart")

Train <- read.csv("./Data/train.csv", na.strings = "")
Test <- read.csv("./Data/test.csv", na.strings = "")

Train <- mutate(Train, Segment="Train")
Test <- mutate(Test, Segment="Test")
Test <- mutate(Test, Item_Outlet_Sales="")

DT <- rbind(Train, Test)

# ===================== DATA PREPARATION ================

Mean_Item_Weight <- mean(DT$Item_Weight, na.rm=TRUE)

DT$Item_Weight <- replace(DT$Item_Weigh, is.na(DT$Item_Weight), Mean_Item_Weight)

DT$Item_Fat_Content <- as.numeric(recode(DT$Item_Fat_Content, 
                                         "'reg'=0; 'Regular'=0; 
                                         'LF'=1; 'low fat'=1; 'Low Fat'=1; 
                                         else=0", 
                                         as.factor.result=FALSE))

DT$Item_Visibility <- replace(DT$Item_Visibility, is.na(DT$Item_Visibility), 0)

DT$Item_Visibility <- replace(DT$Item_Visibility, is.infinite(DT$Item_Visibility), 0)

DT$Item_Type <- as.numeric(recode(DT$Item_Type, 
                                         "'Baking Goods'=1; 'Breads'=2; 'Breakfast'=3; 'Canned'=4;
                                         'Dairy'=5; 'Frozen Foods'=6; 'Fruits and Vegetables'=7; 'Hard Drinks'=8;
                                         'Health and Hygiene'=9; 'Household'=10; 'Meat'=11; 'Others'=12;
                                         'Seafood'=13; 'Snack Foods'=14; 'Soft Drinks'=15; 'Starchy Foods'=16;
                                         else=0", 
                                         as.factor.result=FALSE))
 
Mean_Item_MRP <- mean(DT$Item_MRP, na.rm=TRUE)

DT$Item_MRP <- replace(DT$Item_MRP, is.na(DT$Item_MRP), Mean_Item_MRP)

DT$Outlet_Establishment_Year <- as.factor(DT$Outlet_Establishment_Year) 

levels(DT$Outlet_Establishment_Year) <- c('0', '1', '2', '3', '4', '5', '6', '7', '8')

DT$Outlet_Establishment_Year <- as.numeric(DT$Outlet_Establishment_Year)

DT$Outlet_Size <- as.numeric(recode(DT$Outlet_Size, 
                                 "'Small'=0; 'Medium'=1; 'High'=2; else=0", 
                                 as.factor.result=FALSE))

DT$Outlet_Location_Type <- as.numeric(recode(DT$Outlet_Location_Type, 
                                   "'Tier 1'=0; 'Tier 2'=1; 'Tier 3'=2; else=0", 
                                   as.factor.result=FALSE))

DT$Outlet_Type <- as.numeric(recode(DT$Outlet_Type, 
                                            "'Grocery Store'=0; 'Supermarket Type1'=1; 'Supermarket Type2'=2; ; 'Supermarket Type3'=3 ; else=0", 
                                            as.factor.result=FALSE))

DT$Item_Outlet_Sales <- as.numeric(DT$Item_Outlet_Sales)

Mean_Item_Outlet_Sales <- mean(as.numeric(DT$Item_Outlet_Sales), na.rm=TRUE)

DT$Item_Outlet_Sales <- replace(DT$Item_Outlet_Sales, is.na(DT$Item_Outlet_Sales), 0)

DT$Item_Outlet_Sales <- replace(DT$Item_Outlet_Sale, is.infinite(DT$Item_Outlet_Sale), 0)

# ----- Create final test and train data  ------------------------

Train <- DT[DT$Segment=='Train',]

Test <- DT[DT$Segment=='Test',]

Train_values <- Train$Item_Outlet_Sales

Test_ID_item <- Test$Item_Identifier

Test_ID_outlet <- Test$Outlet_Identifier

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

XG_Prediction <- cbind(as.character(Test_ID_item), as.character(Test_ID_outlet), as.character(XG_Prediction))

colnames(XG_Prediction) <- c("Item_Identifier", "Outlet_Identifier", "Item_Outlet_Sales") 

write.table(XG_Prediction, 
          file="submission_1_20160210_benchmark.csv", 
          sep=",",
          row.names = FALSE,
          col.names = TRUE, 
          quote=FALSE
          )


end.time <- Sys.time()

time.taken <- end.time - start.time

print(paste("elapsed time: ", round(time.taken, 2)))

# score: 1338.12421642

