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
packages(h2o) # modeling
packages(caret)
packages(caretEnsemble)
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





# ----- Create final test and train data  ------------------------

Train <- DT[DT$Segment=='Train',]

# remove missing values 
#Train <- Train[Train$Item_Visibility > 0.00000,]

# save original train data for error calculations
Train_original <- Train

Test <- DT[DT$Segment=='Test',]

Train_values <- Train$Item_Outlet_Sales

Test_ID_item <- Test$Item_Identifier

Test_ID_outlet <- Test$Outlet_Identifier

Test_ID_outlet_type <- Test$Outlet_Type

# recode outlet identifiers 

    levels(Test$Outlet_Identifier) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    
    Test$Outlet_Identifier <- as.numeric(Test$Outlet_Identifier)
    
    levels(Train$Outlet_Identifier) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    
    Train$Outlet_Identifier <- as.numeric(Train$Outlet_Identifier)

Columns <- c(colnames(Train)[12:13], 'Item_Identifier') # exclude identifiers, segment and value

Train <- Train[!colnames(Train) %in% Columns]

Test <- Test[!colnames(Test) %in% Columns]


# ============================== END DATA PREPARATION ====================

# ---- caret package ---------

# methodList=c('glm', 'gbm', 'svmLinear', 'xgbLinear')

methodList=c('enpls')

fitControl <- trainControl(method="repeatedcv",
                           number=3,
                           repeats=3, 
                           savePredictions = TRUE)

Caret_Train <- cbind(data.table(Train),as.factor(Train_values))

Caret_Test <- data.table(Test)

formula <- Train_values ~ 
  Item_Weight +
  Item_Fat_Content +
  Item_Visibility +
  Item_Type +
  Item_MRP +
  Outlet_Identifier +
  Outlet_Establishment_Year +
  Outlet_Size + 
  Outlet_Location_Type +
  Outlet_Type


model_list <- caretList(formula, data=Caret_Train,
                        trControl=fitControl,
                        methodList=methodList)

ensemble <- caretEnsemble(model_list)

summary(ensemble)

Caret_Prediction <- predict(ensemble, newdata=Caret_Test)

Caret_Prediction <- exp(Caret_Prediction)

Caret_Prediction <- cbind(as.character(Test_ID_item), as.character(Test_ID_outlet), as.character(Caret_Prediction))

colnames(Caret_Prediction) <- c("Item_Identifier", "Outlet_Identifier", "Item_Outlet_Sales") 

# print predictions for submission

Caret_Train_Prediction <- predict(ensemble, newdata=Caret_Train)

Caret_Error <- sqrt(sum(((Caret_Train_Prediction- Train_original$Item_Outlet_Sales)^2))/length(Caret_Train_Prediction))

Caret_Error_Total <- sum(((Caret_Train_Prediction- Train_original$Item_Outlet_Sales)^2))

print(paste("test error total: ", round(Caret_Error_Total, 3)))

print(paste("test error RMSE: ", round(Caret_Error, 3)))

write.table(Caret_Prediction, 
          file="submission_18_20160211.csv", 
          sep=",",
          row.names = FALSE,
          col.names = TRUE, 
          quote=FALSE
          )

end.time <- Sys.time()

time.taken <- end.time - start.time

print(paste("elapsed time: ", round(time.taken, 2)))

# Changes vs benchmark:
# 
# AV score: 

