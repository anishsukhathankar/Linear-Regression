library(stringr)
library(MASS)
library(car)

#Importing the Dataset
car_price <- read.csv("CarPrice_Assignment.csv")
View(car_price)
str(car_price)

#as described we need to consider just the company name not the model name so we
#would split the CarName column into 2 i.e Company Name and Model name
#By viewing the dataset we are able to see in some cases both are seperated by either "-" or Space
#so we would first replace "-" by space and then would split the column by space

car_price$CarName <- car_price$CarName %>% str_replace("-"," ")
car_price$CarName <- sub(" .*", "", car_price$CarName)

#now converting car name into factor
car_price$CarName <- as.factor(car_price$CarName)

#now lets see if there are any duplicate value in the dataset
unique(car_price)
#total no of observations are still 205 indicating that there are no duplicate rows

#lets check about the missing values
sum(is.na(car_price))
#There are no missing values

#now lets perform data conversion 
car_price$symboling <- as.factor(car_price$symboling)
car_price$curbweight <- as.numeric(car_price$curbweight)
car_price$enginesize <- as.numeric(car_price$enginesize)
car_price$horsepower <- as.numeric(car_price$horsepower)
car_price$peakrpm <- as.numeric(car_price$peakrpm)
car_price$citympg <- as.numeric(car_price$citympg)
car_price$highwaympg <- as.numeric(car_price$highwaympg)
str(car_price)


#now lets check if there are any outliears for the quantitative variables
#note that we wont cosnider car_id and price here as in price is the dependent variable which we are going to predict
#and car Id is not a quantitative variable even if cosnsist no, we can ignore it afterwords

#wheelbase
quantile(car_price$wheelbase,seq(0,1,0.01))

#carlength
quantile(car_price$carlength,seq(0,1,0.01))

#carwidth
quantile(car_price$carwidth,seq(0,1,0.01))

#carheight
quantile(car_price$carheight,seq(0,1,0.01))

#curbweight
quantile(car_price$curbweight,seq(0,1,0.01))
#its look like on 0 to 1 percentile so cap all values below 1819.72
car_price$curbweight[which(car_price$curbweight < 1819.72)] <- 1819.72

#enginesize
quantile(car_price$enginesize,seq(0,1,0.01))
#there are big jumps after 96% so cap all values above 209.00
car_price$curbweight[which(car_price$enginesize > 209.00)] <- 209.00

#boreratio
quantile(car_price$boreratio,seq(0,1,0.01))

#stroke
quantile(car_price$stroke,seq(0,1,0.01))

#compressionratio
quantile(car_price$compressionratio,seq(0,1,0.01))
#here there is jump after 90%, so caping all values above 10.9400
car_price$compressionratio[which(car_price$compressionratio > 10.9400)] <- 10.9400

#horsepower
quantile(car_price$horsepower,seq(0,1,0.01))
# there is a jump after 99%, so caping all values above 207.00
car_price$horsepower[which(car_price$horsepower > 207.00)] <- 207.00

#peakrpm
quantile(car_price$peakrpm,seq(0,1,0.01))

#citympg
quantile(car_price$citympg,seq(0,1,0.01))
#there is jump after 98%, so caping all the values above 38.00
car_price$citympg[which(car_price$citympg > 38.00)] <- 38.00

#highwaympg
quantile(car_price$highwaympg,seq(0,1,0.01))
#caping all values after 98% ie after 46.92
car_price$highwaympg[which(car_price$highwaympg > 46.92)] <- 46.92

#Now there are lots of categorical variable, we need to convert it into numeric to build the model
#so lets start building dummy variables and combine it with main data

str(car_price)
#######symboling#########
#there are 6 levels so diving those into 2 
#ie -2,-1,0 = "safe" and 1,2,3 = "risky"
summary(car_price$symboling)
levels(car_price$symboling)[1:3] <- "safe"
str(car_price)
levels(car_price$symboling)
levels(car_price$symboling)[2:4] <- "risky"
#creating dummy variable for it and converting it into numeric
dummy_symboling <- model.matrix(~symboling - 1,data = car_price)
dummy_symboling <- dummy_symboling[,-1]
car_price_v1 <- cbind(car_price[,-2],dummy_symboling)

#######fueltype#########
str(car_price_v1)
summary(car_price_v1$fueltype)
#0 = "diesel", 1= "gas"
levels(car_price_v1$fueltype)<-c(0,1)
car_price_v1$fueltype<- as.numeric(levels(car_price_v1$fueltype))[car_price_v1$fueltype]

#######aspiration#########
summary(car_price_v1$aspiration)
#0 = "std" and 1 = "turbo"
levels(car_price_v1$aspiration)<-c(0,1)
car_price_v1$aspiration<- as.numeric(levels(car_price_v1$aspiration))[car_price_v1$aspiration]

#######doornumber#########
summary(car_price_v1$doornumber)
#0 = "four" and 1 = "two"
levels(car_price_v1$doornumber)<-c(0,1)
car_price_v1$doornumber<- as.numeric(levels(car_price_v1$doornumber))[car_price_v1$doornumber]

#######carbody#########
summary(car_price_v1$carbody)
dummy_carbody <- model.matrix(~carbody - 1,data = car_price_v1)
dummy_carbody <- dummy_carbody[,-1]
car_price_v2 <- cbind(car_price_v1[,-6],dummy_carbody)

#######drivewheel#########
str(car_price_v2)
summary(car_price_v2$drivewheel)
dummy_drivewheel <- model.matrix(~drivewheel - 1,data = car_price_v2)
dummy_drivewheel <- dummy_drivewheel[,-1]
car_price_v3 <- cbind(car_price_v2[,-6],dummy_drivewheel)

#######enginelocation#########
str(car_price_v3)
summary(car_price_v3$enginelocation)
#0 = "front' and 1 = 'rear'
levels(car_price_v3$enginelocation)<-c(0,1)
car_price_v3$enginelocation<- as.numeric(levels(car_price_v3$enginelocation))[car_price_v3$enginelocation]

#######enginetype#########
summary(car_price_v3$enginetype)
dummy_enginetype <- model.matrix(~enginetype - 1,data = car_price_v3)
dummy_enginetype <- dummy_enginetype[,-1]
car_price_v4 <- cbind(car_price_v3[,-12],dummy_enginetype)

#######cylindernumber#########
str(car_price_v4)
summary(car_price_v4$cylindernumber)
dummy_cylindernumber <- model.matrix(~cylindernumber - 1,data = car_price_v4)
dummy_cylindernumber <- dummy_cylindernumber[,-1]
car_price_v5 <- cbind(car_price_v4[,-12],dummy_cylindernumber)

#######fuelsystem#########
str(car_price_v5)
summary(car_price_v5$fuelsystem)
dummy_fuelsystem <- model.matrix(~fuelsystem - 1,data = car_price_v5)
dummy_fuelsystem <- dummy_fuelsystem[,-1]
car_price_v6 <- cbind(car_price_v5[,-13],dummy_fuelsystem)

#######CarName#########
str(car_price_v6)
summary(car_price_v6$CarName)
dummy_carname <- model.matrix(~CarName - 1,data = car_price_v6)
dummy_carname <- dummy_carname[,-1]
car_price_final <- cbind(car_price_v6[,-2],dummy_carname)


##########################now lets start building model###########################

# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(car_price_final), 0.7*nrow(car_price_final))
train = car_price_final[trainindices,]
test = car_price_final[-trainindices,]

#now lets build a model containing all variable
model_1 <-lm(price~.,data=train)
summary(model_1)

#now lets get rid of unwanted variable by using stepAIC
step <- stepAIC(model_1, direction="both")
step

#so iteratively stepAIC have removed insignificant variable 
#we will now build the model on the last equation provided by step AIC

model_2 <- lm(formula = price ~ car_ID + fueltype + aspiration + enginelocation + 
                carwidth + curbweight + enginesize + stroke + compressionratio + 
                horsepower + peakrpm + citympg + dummy_symboling + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohcf + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + fuelsystem2bbl + 
                fuelsystemmpfi + CarNameaudi + CarNamebmw + CarNamebuick + 
                CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
                CarNamemaxda + CarNamemazda + CarNamemercury + CarNamemitsubishi + 
                CarNamenissan + CarNameNissan + CarNameplymouth + CarNameporcshce + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNametoyouta + 
                CarNamevokswagen + CarNamevolkswagen + CarNamevolvo + CarNamevw, 
              data = train)

summary(model_2)

#there are some variables which dont have * in the p-value, deleting those first
model_3 <- lm(formula = price ~ car_ID + aspiration + enginelocation + 
                carwidth + curbweight + enginesize + stroke + compressionratio  
                 + peakrpm + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohcf + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + fuelsystem2bbl + 
                fuelsystemmpfi + CarNamebmw + 
                CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
                CarNamemaxda + CarNamemazda + CarNamemercury + CarNamemitsubishi + 
                CarNamenissan + CarNameNissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNametoyouta + 
                CarNamevokswagen + CarNamevolkswagen + CarNamevolvo + CarNamevw, 
              data = train)

summary(model_3)

model_4 <- lm(formula = price ~ car_ID + aspiration + enginelocation + 
                carwidth + curbweight + enginesize + stroke   
              + peakrpm + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohcf + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + fuelsystem2bbl 
                 + CarNamebmw + 
                CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
                CarNamemaxda + CarNamemazda + CarNamemercury + CarNamemitsubishi + 
                CarNamenissan + CarNameNissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNametoyouta + 
                CarNamevokswagen + CarNamevolkswagen + CarNamevolvo + CarNamevw, 
              data = train)

summary(model_4)

#now lets check multicoleanirity as in all variable have atleast 1 star
vif(model_4)

#carwidth vif is 11.619683 and also the stars are 2 so lets remove it and check r^2

model_5 <- lm(formula = price ~ car_ID + aspiration + enginelocation  
                 + curbweight + enginesize + stroke   
              + peakrpm + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohcf + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + fuelsystem2bbl 
              + CarNamebmw + 
                CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
                CarNamemaxda + CarNamemazda + CarNamemercury + CarNamemitsubishi + 
                CarNamenissan + CarNameNissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNametoyouta + 
                CarNamevokswagen + CarNamevolkswagen + CarNamevolvo + CarNamevw, 
              data = train)

summary(model_5)
vif(model_5)

# as fuelsystem2bbl have no star, lets remove that
model_6 <- lm(formula = price ~ car_ID + aspiration + enginelocation  
              + curbweight + enginesize + stroke   
              + peakrpm + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohcf + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix  
              + CarNamebmw + 
                CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
                CarNamemaxda + CarNamemazda + CarNamemercury + CarNamemitsubishi + 
                CarNamenissan + CarNameNissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyota + CarNametoyouta + 
                CarNamevokswagen + CarNamevolkswagen + CarNamevolvo + CarNamevw, 
              data = train)

summary(model_6)

vif(model_6)

#now as in all independent variable have 3 star we will delete more variable depended upon there VIF
#lets delete soem of the top vif
#car_id, carnametoyota, carnamevolkswagen and carnamevolvo

model_7 <- lm(formula = price ~ aspiration + enginelocation  
              + curbweight + enginesize + stroke   
              + peakrpm + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohcf + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix  
              + CarNamebmw + 
                CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
                CarNamemaxda + CarNamemazda + CarNamemercury + CarNamemitsubishi + 
                CarNamenissan + CarNameNissan + CarNameplymouth + 
                CarNamerenault + CarNamesaab + CarNametoyouta + 
                CarNamevokswagen + CarNamevw, 
              data = train)

summary(model_7)
vif(model_7)

#now lets delete the one which dont have star
model_8 <- lm(formula = price ~ aspiration + enginelocation + curbweight + enginesize + stroke   
              + peakrpm + enginetypel + enginetypeohcf + cylindernumberfive +
                cylindernumberfour + cylindernumbersix + CarNamebmw +
                CarNamedodge + CarNamejaguar + CarNamemitsubishi , 
              data = train)
              
summary(model_8)                 

#lets remove enginetypel which dont have any star
model_9 <- lm(formula = price ~ aspiration + enginelocation + curbweight + enginesize + stroke   
              + peakrpm + enginetypeohcf + cylindernumberfive +
                cylindernumberfour + cylindernumbersix + CarNamebmw +
                CarNamedodge + CarNamejaguar + CarNamemitsubishi , 
              data = train)

summary(model_9)
vif(model_9)

#lets delete cylindernumberfive with 1 star and vif above 1
model_10 <- lm(formula = price ~ aspiration + enginelocation + curbweight + enginesize + stroke   
              + peakrpm + enginetypeohcf +cylindernumberfour + cylindernumbersix + CarNamebmw +
                CarNamedodge + CarNamejaguar + CarNamemitsubishi , 
              data = train)

summary(model_10)

#lets delete curbweight with no star

model_11 <- lm(formula = price ~ aspiration + enginelocation + enginesize + stroke   
               + peakrpm + enginetypeohcf +cylindernumberfour + cylindernumbersix + CarNamebmw +
                 CarNamedodge + CarNamejaguar + CarNamemitsubishi , 
               data = train)

summary(model_11)
vif(model_11)

#lets remove namedodge with 1 star and vif = 1.06
model_12 <- lm(formula = price ~ aspiration + enginelocation + enginesize + stroke   
               + peakrpm + enginetypeohcf +cylindernumberfour + cylindernumbersix + CarNamebmw 
               + CarNamejaguar + CarNamemitsubishi , 
               data = train)

summary(model_12)
vif(model_12)

#you can remove carnamemitsubishi as well
model_13 <- lm(formula = price ~ aspiration + enginelocation + enginesize + stroke   
               + peakrpm + enginetypeohcf +cylindernumberfour + cylindernumbersix + CarNamebmw 
               + CarNamejaguar , 
               data = train)
summary(model_13)
vif(model_13)

#remove peakrpm as well
model_14 <- lm(formula = price ~ aspiration + enginelocation + enginesize + stroke   
               + enginetypeohcf +cylindernumberfour + cylindernumbersix + CarNamebmw 
               + CarNamejaguar , 
               data = train)

summary(model_14)
vif(model_14)


#so this can be our final model with p value having 3 star and vif is also quite low

# now predicting the results in test dataset
str(test)
Predict_1 <- predict(model_14,test[,-19])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared