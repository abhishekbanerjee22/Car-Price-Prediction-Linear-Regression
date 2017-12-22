#loading all the libraries for use
library(tidyr)
library(dplyr)
library(ggplot2)
library(MASS)
library(car)
#loading the data set into a data frame
#data preparation ,the carName variable removed and only carCompaby is used for analysis
car_Price_Data<- read.csv("CarPrice_Assignment.csv")
str(car_Price_Data)

car_Price_Data$CarName<-as.character(car_Price_Data$CarName)
#extracting carcomapny from carname column
CarCompany<-lapply(strsplit(car_Price_Data$CarName," "), function(x) x[1])
#converting column carCompany into character type
class(CarCompany)
CarCompany<-unlist(CarCompany)
class(CarCompany)
#adding column car company to data frame
car_Price_Data<-cbind(car_Price_Data,CarCompany)
car_Price_Data<-car_Price_Data[,-3]
car_Price_Data$CarCompany<- as.factor(car_Price_Data$CarCompany)

#data Cleaning
car_Price_Data[is.na(car_Price_Data)==T]
#no NAs
#no summary rows and repeated values
#one issue found, same car company names have different spelling and case
#converting these to consisent names and spellings for same category
#cleaning data in the next set of commands
summary(car_Price_Data$CarCompany)
car_Price_Data$CarCompany<-as.character(car_Price_Data$CarCompany)
car_Price_Data$CarCompany[which(car_Price_Data$CarCompany=='maxda')]<- 'mazda'
car_Price_Data$CarCompany[which(car_Price_Data$CarCompany=='Nissan')]<- 'nissan'
car_Price_Data$CarCompany[which(car_Price_Data$CarCompany=='toyouta')]<- 'toyota'
car_Price_Data$CarCompany[which(car_Price_Data$CarCompany=='porcshce')]<- 'porsche'
car_Price_Data$CarCompany[which(car_Price_Data$CarCompany=='vokswagen')]<- 'volkswagen'
car_Price_Data$CarCompany[which(car_Price_Data$CarCompany=='vw')]<- 'volkswagen'


car_Price_Data$CarCompany<-as.factor(car_Price_Data$CarCompany)
summary(car_Price_Data$CarCompany)
#removing car id from the data frame
car_Price_Data<-car_Price_Data[,-1]
#data cleaned

#converting varibales with two factor levels to 0 and 1
levels(car_Price_Data$fueltype)<-c(1,0)
car_Price_Data$fueltype<- as.numeric(levels(car_Price_Data$fueltype))[car_Price_Data$fueltype]
#0 for gas,1 for diesel

levels(car_Price_Data$aspiration)<-c(1,0)
car_Price_Data$aspiration<- as.numeric(levels(car_Price_Data$aspiration))[car_Price_Data$aspiration]
#1 for std,0 for turbo

levels(car_Price_Data$doornumber)<-c(1,0)
car_Price_Data$doornumber<- as.numeric(levels(car_Price_Data$doornumber))[car_Price_Data$doornumber]
#0 for two,1 for four

levels(car_Price_Data$enginelocation)<-c(1,0)
car_Price_Data$enginelocation<- as.numeric(levels(car_Price_Data$enginelocation))[car_Price_Data$enginelocation]
#1 for front,0 for rear


#creating dummy variables for columns with more than 2 factor levels
dummy_Company <- data.frame(model.matrix( ~CarCompany, data = car_Price_Data))
View(dummy_Company)
dummy_Company <- dummy_Company[,-1]

car_Price_Data <- cbind(car_Price_Data[,-25], dummy_Company)


dummy_carbody <- data.frame(model.matrix( ~carbody, data = car_Price_Data))
dummy_carbody <- dummy_carbody[,-1]

car_Price_Data <- cbind(car_Price_Data[,-5], dummy_carbody)


dummy_drivewheel <- data.frame(model.matrix( ~drivewheel, data = car_Price_Data))
dummy_drivewheel<- dummy_drivewheel[,-1]

car_Price_Data <- cbind(car_Price_Data[,-5], dummy_drivewheel)

dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = car_Price_Data))
dummy_enginetype<- dummy_enginetype[,-1]

car_Price_Data <- cbind(car_Price_Data[,-11], dummy_enginetype)

dummy_cylindernumber <- data.frame(model.matrix( ~cylindernumber, data = car_Price_Data))
dummy_cylindernumber<- dummy_cylindernumber[,-1]

car_Price_Data <- cbind(car_Price_Data[,-11], dummy_cylindernumber)


dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = car_Price_Data))
dummy_fuelsystem<- dummy_fuelsystem[,-1]

car_Price_Data <- cbind(car_Price_Data[,-12], dummy_fuelsystem)



# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(car_Price_Data), 0.7*nrow(car_Price_Data))
train = car_Price_Data[trainindices,]
test = car_Price_Data[-trainindices,]


# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)

#executing stepAIC to get rid of insignificant variables
step <- stepAIC(model_1, direction="both")

step
#using only the significant variables now after stepAIc

# executing the model
model_2 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + boreratio + stroke + peakrpm + 
                CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
                CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
                CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
                CarCompanyporsche + CarCompanyrenault + CarCompanysaab + 
                CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetyperotor + cylindernumberfive + fuelsystem2bbl + 
                fuelsystemmpfi + highwaympg + CarCompanymercury, data = train)
# Let us look at the summary of the model
summary(model_2)
vif(model_2)

#removing  curbweight due to high  vif
model_3 <- lm(formula = price ~ aspiration + enginelocation + carwidth 
                 + enginesize + boreratio + stroke + peakrpm + 
                CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
                CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
                CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
                CarCompanyporsche + CarCompanyrenault + CarCompanysaab + 
                CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetyperotor + cylindernumberfive + fuelsystem2bbl + 
                fuelsystemmpfi + highwaympg + CarCompanymercury, data = train)
summary(model_3)
vif(model_3)

#removin car company saab

model_4 <- lm(formula = price ~ aspiration + enginelocation + carwidth 
              + enginesize + boreratio + stroke + peakrpm + 
                CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
                CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
                CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
                CarCompanyporsche + CarCompanyrenault + 
                CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetyperotor + cylindernumberfive + fuelsystem2bbl + 
                fuelsystemmpfi + highwaympg + CarCompanymercury, data = train)

summary(model_4)
vif(model_4)
#removing enginesize due to high VIF
model_5 <- lm(formula = price ~ aspiration + enginelocation + carwidth 
              + boreratio + stroke + peakrpm + 
                CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
                CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
                CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
                CarCompanyporsche + CarCompanyrenault + 
                CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetyperotor + cylindernumberfive + fuelsystem2bbl + 
                fuelsystemmpfi + highwaympg + CarCompanymercury, data = train)

summary(model_5)
vif(model_5)
#removing carbodyhatchback
model_6 <-  lm(formula = price ~ aspiration + enginelocation + carwidth 
               + boreratio + stroke + peakrpm + 
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
                 CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
                 CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
                 CarCompanyporsche + CarCompanyrenault + 
                 CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                 carbodyhardtop  + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetyperotor + cylindernumberfive + fuelsystem2bbl + 
                 fuelsystemmpfi + highwaympg + CarCompanymercury, data = train)

summary(model_6)
vif(model_6)
#removing stroke due to high p value

model_7 <- lm(formula = price ~ aspiration + enginelocation + carwidth 
              + boreratio + peakrpm + 
                CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
                CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
                CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
                CarCompanyporsche + CarCompanyrenault + 
                CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                carbodyhardtop  + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetyperotor + cylindernumberfive + fuelsystem2bbl + 
                fuelsystemmpfi + highwaympg + CarCompanymercury, data = train)

summary(model_7)
vif(model_7)

#removing fuelsystemmmpfi due to Vif and high p value
model_8<-lm(formula = price ~ aspiration + enginelocation + carwidth 
            + boreratio + peakrpm + 
              CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
              CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
              CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
              CarCompanyporsche + CarCompanyrenault + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              carbodyhardtop  + carbodysedan + carbodywagon + 
              drivewheelrwd + enginetyperotor + cylindernumberfive + fuelsystem2bbl + 
               highwaympg + CarCompanymercury, data = train)


summary(model_8)
vif(model_8)

#removing enginetyperotor due to high p value

model_9<-lm(formula = price ~ aspiration + enginelocation + carwidth 
            + boreratio + peakrpm + 
              CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
              CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
              CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
              CarCompanyporsche + CarCompanyrenault + 
              CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
              carbodyhardtop  + carbodysedan + carbodywagon + 
              drivewheelrwd + cylindernumberfive + fuelsystem2bbl + 
              highwaympg + CarCompanymercury, data = train)

summary(model_9)
vif(model_9)
# removing boreratio due to high vif and p value

model_10<-lm(formula = price ~ aspiration + enginelocation + carwidth 
             + peakrpm + 
               CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
               CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
               CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
               CarCompanyporsche + CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
               carbodyhardtop  + carbodysedan + carbodywagon + 
               drivewheelrwd + cylindernumberfive + fuelsystem2bbl + 
               highwaympg + CarCompanymercury, data = train)

summary(model_10)
vif(model_10)
#removinf drivewheelrwd due to high vif and p value

model_11<-lm(formula = price ~ aspiration + enginelocation + carwidth 
             + peakrpm + 
               CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
               CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
               CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
               CarCompanyporsche + CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
               carbodyhardtop  + carbodysedan + carbodywagon + 
                cylindernumberfive + fuelsystem2bbl + 
               highwaympg + CarCompanymercury, data = train)

summary(model_11)
vif(model_11)
#removing aspiration due to high p  value

model_12<-lm(formula = price ~ enginelocation + carwidth 
             + peakrpm + 
               CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
               CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
               CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
               CarCompanyporsche + CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
               carbodyhardtop  + carbodysedan + carbodywagon + 
               cylindernumberfive + fuelsystem2bbl + 
               highwaympg + CarCompanymercury, data = train)

summary(model_12)
vif(model_12)
#removing peakrpm due to high p value

model_13<-lm(formula = price ~ enginelocation + carwidth + 
               CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
               CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
               CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
               CarCompanyporsche + CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
               carbodyhardtop  + carbodysedan + carbodywagon + 
               cylindernumberfive + fuelsystem2bbl + 
               highwaympg + CarCompanymercury, data = train)

summary(model_13)
vif(model_13)
#removing carbodyhardtop due to high p value


model_14<-lm(formula = price ~ enginelocation + carwidth + 
               CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
               CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
               CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
               CarCompanyporsche + CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                carbodysedan + carbodywagon + 
               cylindernumberfive + fuelsystem2bbl + 
               highwaympg + CarCompanymercury, data = train)

summary(model_14)
vif(model_14)


#removing carbody sedan due to high p value

model_15<-lm(formula = price ~ enginelocation + carwidth + 
               CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
               CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
               CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
               CarCompanyporsche + CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
                carbodywagon + 
               cylindernumberfive + fuelsystem2bbl + 
               highwaympg + CarCompanymercury, data = train)

summary(model_15)
vif(model_15)

#removing carbodywagon
model_16<-lm(formula = price ~ enginelocation + carwidth + 
               CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
               CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
               CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
               CarCompanyporsche + CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
               cylindernumberfive + fuelsystem2bbl + 
               highwaympg + CarCompanymercury, data = train)

summary(model_16)
vif(model_16)

#removing carCompany mercury due to high p value

model_17<-lm(formula = price ~ enginelocation + carwidth + 
               CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
               CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
               CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
               CarCompanyporsche + CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
               cylindernumberfive + fuelsystem2bbl + 
               highwaympg , data = train)

summary(model_17)
vif(model_17)

#remonving fuelsystem2bbl dut to high p value

model_18<-lm(formula = price ~ enginelocation + carwidth + 
               CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
               CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
               CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
               CarCompanyporsche + CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
               cylindernumberfive + 
               highwaympg , data = train)

summary(model_18)
vif(model_18)

#removing carCompany Porsche due to high vif
model_19<-lm(formula = price ~ enginelocation + carwidth + 
               CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
               CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
               CarCompanynissan + CarCompanypeugeot + CarCompanyplymouth + 
                CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
               cylindernumberfive + 
               highwaympg , data = train)

summary(model_19)
vif(model_19)

#removing carcompany nissan due to p value higher
model_20<-lm(formula = price ~ enginelocation + carwidth + 
               CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
               CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
              CarCompanypeugeot + CarCompanyplymouth + 
               CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota + CarCompanyvolkswagen + 
               cylindernumberfive + 
               highwaympg , data = train)

summary(model_20)
vif(model_20)

#removing carcomapny volkswagen

model_21<-lm(formula = price ~ enginelocation + carwidth + 
               CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
               CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
               CarCompanypeugeot + CarCompanyplymouth + 
               CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota +
               cylindernumberfive + 
               highwaympg , data = train)

summary(model_21)
vif(model_21)
#removing carcompany dodge

model_22<-lm(formula = price ~ enginelocation + carwidth + 
               CarCompanybmw + CarCompanybuick  + CarCompanyhonda + 
               CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
               CarCompanypeugeot + CarCompanyplymouth + 
               CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota +
               cylindernumberfive + 
               highwaympg , data = train)

summary(model_22)
vif(model_22)
#removing car company honda

model_23<-lm(formula = price ~ enginelocation + carwidth + 
               CarCompanybmw + CarCompanybuick + 
               CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
               CarCompanypeugeot + CarCompanyplymouth + 
               CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota +
               cylindernumberfive + 
               highwaympg , data = train)

summary(model_23)
vif(model_23)
#removing car company plymouth

model_24<-lm(formula = price ~ enginelocation + carwidth + 
               CarCompanybmw + CarCompanybuick + 
               CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
               CarCompanypeugeot + 
               CarCompanyrenault + 
               CarCompanysubaru + CarCompanytoyota +
               cylindernumberfive + 
               highwaympg , data = train)

summary(model_24)
vif(model_24)
#removing car company toyota

model_25<-lm(formula = price ~ enginelocation + carwidth + 
               CarCompanybmw + CarCompanybuick + 
               CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
               CarCompanypeugeot + 
               CarCompanyrenault + 
               CarCompanysubaru  +
               cylindernumberfive + 
               highwaympg , data = train)

summary(model_25)
vif(model_25)
#removing carCompany Subaru
model_26<-lm(formula = price ~ enginelocation + carwidth + 
               CarCompanybmw + CarCompanybuick + 
               CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
               CarCompanypeugeot + 
               CarCompanyrenault + 
               cylindernumberfive + 
               highwaympg , data = train)

summary(model_26)
vif(model_26)
#removing car company renault
model_27<-lm(formula = price ~ enginelocation + carwidth + 
               CarCompanybmw + CarCompanybuick + 
               CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
               CarCompanypeugeot + 
               cylindernumberfive + 
               highwaympg , data = train)

summary(model_27)
vif(model_27)

#removing car company mitsubishi
model_28<-lm(formula = price ~ enginelocation + carwidth + 
               CarCompanybmw + CarCompanybuick + 
               CarCompanyjaguar + CarCompanymazda +
               CarCompanypeugeot + 
               cylindernumberfive + 
               highwaympg , data = train)

summary(model_28)
vif(model_28)
#removing car company mazda

model_29<-lm(formula = price ~ enginelocation + carwidth + 
               CarCompanybmw + CarCompanybuick + 
               CarCompanyjaguar +
               CarCompanypeugeot + 
               cylindernumberfive + 
               highwaympg , data = train)

summary(model_29)
vif(model_29)
#removing car company peugeot
model_30<-lm(formula = price ~ enginelocation + carwidth + 
               CarCompanybmw + CarCompanybuick + 
               CarCompanyjaguar + cylindernumberfive + 
               highwaympg , data = train)

summary(model_30)
vif(model_30)
#removing cylinder number five
model_31<-lm(formula = price ~ enginelocation + carwidth + 
               CarCompanybmw + CarCompanybuick + 
               CarCompanyjaguar+
               highwaympg , data = train)

summary(model_31)
vif(model_31)

#prediction using the last and final model
Predict_1 <- predict(model_31,test[,-1])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared
#the rsquared is 0.829