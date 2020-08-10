library(readr)
library(Amelia)
library(ggplot2)
library(moments)
library(mice) #assumes mar
library(modeest)
library(car)
library(coefplot)

test <- read_csv("Project1PropertyPricePrediction/Dataset/Property_Price_Test.csv")
View(test)

train <- read_csv("Project1PropertyPricePrediction/Dataset/Property_Price_Train.csv")
View(train)
View(train$Sale_Price)

dim(train)
dim(test)

colSums(is.na(train))
colSums(is.na(test))

summary(train)

df<-train[,c(2,5,6,8,12,13,16,17,19,20,22,30,31,33,34,39,40,41,42,43,44,45,54,57,63,68,72,77,78,79,81)]

dim(df)
View(df)

#handling missing values
sort(colSums(is.na(df)))
sum(is.na(df))
missmap(df)

class(df$Basement_Height)
as.factor(df$Basement_Height)
df$Basement_Height[which(is.na(df$Basement_Height))]<-"Gd"      
sum(is.na(df$Basement_Height))

as.factor(df$Exposure_Level)
mfv(df$Exposure_Level)
df$Exposure_Level[which(is.na(df$Exposure_Level))]=mfv(df$Exposure_Level,na_rm = TRUE)
sum(is.na(df$Exposure_Level))

class(df$BsmtFinType1)
as.factor(df$BsmtFinType1)
mfv(df$BsmtFinType1,na_rm = TRUE)
df$BsmtFinType1[which(is.na(df$BsmtFinType1))]=mfv(df$BsmtFinType1,na_rm = TRUE)
sum(is.na(df$BsmtFinType1))

class(df$Electrical_System)
as.factor(df$Electrical_System)
mfv(df$Electrical_System,na_rm = TRUE)
df$Electrical_System[which(is.na(df$Electrical_System))]=mfv(df$Electrical_System,na_rm = TRUE)
sum(is.na(df$Electrical_System))

sum(is.na(df))

#linear regression
str(df)
summary(df)
cor(df$Building_Class,df$Sale_Price)
df$Building_Class<-NULL  #OUT FROM MODEL

skewness(df$Lot_Size)
summary(df$Lot_Size)

#lotsize
hist(df$Lot_Size,xlim = c(0,20000),col="blue",breaks = 200)


#road
mfv(df$Road_Type)
barplot(table(df$Road_Type),col=c("red","green"))


#exposue
barplot(table(df$Exposure_Level),col=c("red","green","pink","blue","yellow"))

#shape
barplot(table(df$Property_Shape),col=c("red","green","pink","blue","yellow"))

#slope
barplot(table(df$Property_Slope)),col=c("red","green","pink","blue","yellow"))

#neighbour
table(df$Neighborhood)
mfv(df$Neighborhood)
sort(table(df$Neighborhood))
barplot(table(df$Neighborhood),col=("red"))


#house condition
table(df$House_Condition)

#construct year
range(df$Construction_Year)     #min-max
table(df$Construction_Year)
plot(table(df$Construction_Year),col="orange",lwd = 4)

#correlation between input variable and output
cor(df$Construction_Year,df$Sale_Price)
cor(df$Lot_Size,df$Sale_Price)

plot(log(df$Lot_Size,df$Sale_Price+1),col="red",xlim = c(0,2000))   #log transformation

barplot(df$Basement_Height,df$Sale_Price,xlim =c(0,3000))
cor(df$Basement_Height,df$Sale_Price)

cor(df$Total_Basement_Area,df$Sale_Price)
plot(df$Total_Basement_Area,df$Sale_Price,xlim = c(0,3000))

cor(df$First_Floor_Area,df$Sale_Price)
plot(df$First_Floor_Area,df$Sale_Price,col="blue")

cor(df$Garage_Area,df$Sale_Price)
plot(df$Garage_Area,df$Sale_Price)

cor(df$Second_Floor_Area,df$Sale_Price)
plot(df$Second_Floor_Area,df$Sale_Price)

dim(df)

#test data
df_test<-test[,c(2,5,6,8,12,13,16,17,19,20,22,30,31,33,34,39,40,41,42,43,44,45,54,57,63,68,72,77,78,79)]
colSums(is.na(df_test))
sort(colSums(is.na(df_test)))

df1$Exposure_Level[which(is.na(df1$Exposure_Level))]=mfv(df1$Exposure_Level,na_rm = TRUE)
df1$Basement_Height[which(is.na(df1$Basement_Height))]=mfv(df1$Basement_Height,na_rm=TRUE)
df1$BsmtFinType1[which(is.na(df1$BsmtFinType1))]=mfv(df1$BsmtFinType1,na_rm = TRUE)
df1$Sale_Type[which(is.na(df1$Sale_Type))]=mfv(df1$BsmtFinType1,na_rm = TRUE)
df1$Total_Basement_Area[which(is.na(df1$Total_Basement_Area))]=median(df1$Total_Basement_Area,na.rm = TRUE)
df1$Kitchen_Quality[which(is.na(df1$Kitchen_Quality))]=mfv(df1$Kitchen_Quality,na_rm = TRUE)
df1$Garage_Area[which(is.na(df1$Garage_Area))]=mfv(df1$Garage_Area,na_rm=TRUE)



colSums(is.na(df_test))

#building model
colnames(df)
mod_df<-lm(Sale_Price~.,data = df)
summary(mod_df)
str(df)
dim(df)
coefplot(mod_df)
mod_df$coefficients
sum(mod_df$residuals)

vif(mod_df)
sum(mod_df$residuals)
plot(mod_df,1)
plot(mod_df,2)
plot(mod_df,3)
plot(mod_df,4)
plot(mod_df,5)

df_train<-df[-c(524,1183,1299,186,589),] #outliars
mod_df2<-lm(Sale_Price~.,data=df_train)
summary(mod_df2)
plot(mod_df2,1)
plot(mod_df2,2)
plot(mod_df2,3)
plot(mod_df2,4)
plot(mod_df2,5)
sum(mod_df2$residuals)

#final prediction
house_price<-predict(mod_df2,newdata = df_test)
View(house_price)

levels(df_train$Condition1<-levels(df_test$Condition1))
levels(df_test$Road_Type)
levels(df_train$Road_Type)<-levels(df_test$Road_Type)
levels(df_train$Property_Slope)<-levels(df_test$Property_Slope)
levels(df_train$Condition1)<-levels(df_test$Condition1)
levels(df_train$House_Type)<-levels(df_test$House_Type)
levels(df_train$House_Design)<-levels(df_test$House_Design)
levels(df_test$Sale_Condition)<-NULL
levels(df_train$Sale_Condition)<-NULL
levels(df_train$Sale_Condition<-levels(df_test$Sale_Condition))

View(df_test$Sale_Condition)

table(df_test$Condition1)
table(df_train$Condition1)



mod_df3<-lm(Sale_Price~.,data=df_train)
summary(mod_df3)
plot(mod_df3,1)
plot(mod_df3,2)
plot(mod_df3,3)
plot(mod_df3,4)
plot(mod_df3,5)

df_test$predictedsaleprice<-predict(mod_df2,newdata = df_test)
dim(df_test)
View(df_test)

AIC(mod_df)
AIC(mod_df2)
AIC(mod_df3)
