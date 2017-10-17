#Assignment: Multivariate Regression 
#######################################################################################################

##Question 1: Fit the best multiple regression model to the data in test.pollute.txt using pollution as the response variable

#######################################################################################################

library(readr)
test_pollute <- read_csv("C:/Users/awong/Downloads/zipped/test.pollute.csv")
View(test_pollute)


##EDA

summary(test_pollute)
pairs(test_pollute)
head(test_pollute)

#No NA's in dataset from looking at the table

##Create full model

full.lm <- lm(pollution ~., data = test_pollute)
summary(full.lm)
#Significant variables are temp, industry, population, and maybe wind)
#R squared is .6145
#F Stat: 11.63 on 6 and 34 DF,  p-value: 4.721e-07

##Create partial model with only those variables that were significant

part.lm <- lm(pollution ~ temp + industry + population + wind, data = test_pollute)
summary(part.lm)
#Significant variables are industry, population, and maybe temp
#R squard is .5958, which means we didn't loss a lot of information from taking out rain and wet.days
#F-statistic: 15.74 on 4 and 36 DF,  p-value: 1.553e-07



#Pairs plot for reduced model 
one <- c(rep(1,length(test_pollute$pollution)))
one
X2 <- cbind(pollution = test_pollute$pollution, temp = test_pollute$temp, population = test_pollute$population, industry = test_pollute$industry)
data1 <- cbind(test_pollute$pollution, X2[,-one])
pairs(data1)

##Or the same plot but faster
pairs(X2)



##Check for multicollinearity
cor(X2) ##Industry and population is highly corelated at .95

##So create a model where I only take one of them. I will keep industry instead of population because it's t test pvalue is a lot less than populations


#New full model
new.full.lm <- lm(pollution ~ temp + industry + wind + rain + wet.days, data = test_pollute)
summary(new.full.lm)
#Significant are temp, industry, and maybe wind and rain. 
#Rsquared is .54 
#F-statistic: 10.73 on 5 and 35 DF,  p-value: 2.621e-06

#New reduced model
new.part.lm <- lm(pollution ~ temp + industry + wind + rain , data = test_pollute)
summary(new.part.lm)
#Signifcant are temp, industry, wind, and rain
#R squared is .56 
#F-statistic: 13.73 on 4 and 36 DF,  p-value: 6.798e-07





##Extra sum of Squares (Check to see if partial model is better fit than full model)
anova(new.part.lm, new.full.lm) ##Since p value is above signifant we can say that partial model was better than full model
#Looks like rain and wet.days doesn't contribute significantly to pollution

##Now let's try to find the best model using AIC
library(MASS)
step <- stepAIC(new.full.lm)
step$anova #Same as my reduced model


######## Check residuals

res <- residuals(new.part.lm)
plot(res ~ test_pollute$temp) #Looks random but huge chance of outliers
plot(res ~ test_pollute$industry) #Looks random but huge chance of outliers
plot(res ~ test_pollute$wind) #Looks random but huge chance of outliers

fitted <- predict(new.part.lm)
plot(res~fitted) ##Again look randomly scatter with 2 outliers

qqplot(res, rnorm(length(res))) #Look normal with some crazy outlier 

#Let's try a transformation. 

new.part.lm2 <- lm(log(pollution) ~ temp + industry + wind + rain , data = test_pollute)
summary(new.part.lm2)

#R squared is .5902
#All metrics in model are significant
#F-statistic:  15.4 on 4 and 36 DF,  p-value: 1.971e-07


##Check residuals
res <- residuals(new.part.lm2)
plot(res ~ test_pollute$temp) #Looks random but huge chance of outliers
plot(res ~ test_pollute$industry) #Looks random but huge chance of outliers
plot(res ~ test_pollute$wind) #Looks random but huge chance of outliers

fitted <- predict(new.part.lm2)
plot(res~fitted) ##Again looks good with maybe some outliers

#Conclusion:
#After dealing with multicollinearity, my final model is: new.part.lm2 (Predictors = temp, industry, wind, rain and response is log(pollute))- Best R squared after dealing with multicollinearity
#and is built the same as model with best AIC score, except response variable has been transformed because of some outliers in residual plots.
#It's F test pvalue also says it's significant. 

###################################

#Question 2: Fit best multi regression model to data in Daphnia.txt using growth rate as response variable


##################################


library(readr)
daphnia <- read_csv("C:/Users/awong/Downloads/zipped/daphnia.csv")
View(daphnia)

attach(daphnia)

##EDA

summary(daphnia) ## Need to use dummy variables since we have nominal variables
head(daphnia)

##Dummy Variables

daphnia$Water <- as.numeric(Water == "Tyne") #Makes each Tyne = 1 and Wear = 0

brandD <- as.numeric(Detergent == "BrandD")
brandC <- as.numeric(Detergent == "BrandC")
brandB <- as.numeric(Detergent == "BrandB")
daphnia$Detergent <- brandB
daphnia <- cbind(daphnia, brandC, brandD)
names(daphnia)[names(daphnia) == "Detergent"] <- "brandB"


clone2 <- as.numeric(Daphnia == "Clone2")
clone3 <- as.numeric(Daphnia == "Clone3")
daphnia$Daphnia <- clone2
daphnia <- cbind(daphnia, clone3)
names(daphnia)[names(daphnia) == "Daphnia"] <- "clone2"


head(daphnia)
pairs(daphnia)

##Full model

full.lm.2 <- lm(Growth.rate ~., data = daphnia)
summary(full.lm.2)
## Significant were clone2 and clone3
## R squared is .3121
## F-statistic: 6.369 on 6 and 65 DF,  p-value: 2.608e-05


##Partial with only clone 2 and clone 3 since they were he ones significant from full model
p.lm.2 <- lm(Growth.rate ~ clone2 + clone3, data = daphnia)
summary(p.lm.2)
## Significant were clone2 and clone3
## R squared is .3151. Some how adjusted R squared increased
## F-statistic: 17.33 on 2 and 69 DF,  p-value: 7.958e-07



##Pairs plot for partial model
X2 <- cbind(Growth.rate = daphnia$Growth.rate,  clone2 = daphnia$clone2, clone3 = daphnia$clone3)
pairs(X2) ##Doesn't show much since categorical. 
## Growth Rate higher in clone2 when compared to others and Growth rate lower in clone 3 when compared to others


##Check multicollinearity
cor(X2) #Nothing seems collinear

##Extra sum of Squares (Check to see if partial model is better fit than full model)
anova(p.lm.2, full.lm.2) ##F test pvalue is not significant which means  partial is the better model


##Check non nested models
library(MASS)
stepAIC(full.lm.2) ## The Min AIC is the same model that we chose

##Residual analysis
res <- residuals(p.lm.2)
plot(res ~ daphnia$clone2) #Hard to tell since categorical
plot(res ~ daphnia$clone3) #Hard to tell since categorical

fitted <- predict(p.lm.2)
plot(res~fitted) ##Not sure if funnel since categorical. Doesn't look like it.

qqplot(res, rnorm(length(res))) #Looks normal

#Conclusion:
#After creating dummy variables. The best model is p.lm.2 model (predictors = clone2 , clone3 and response = growth rate).
#It has the lowest AIC score and best r squared value. This tells me that best indicator of growth rate is if
#daphnia is clone2 and/or clone3

##################################

#Question 3: Fit the best multiple regression model
#to the data in germination.txt with count as the 
#response variable.

######################################

library(readr)
germination <- read_csv("C:/Users/awong/Downloads/zipped/germination.csv")
View(germination)

data <- germination
attach(data)

##EDA
summary(data)
head(data) ## two columns with chrs which means we need dummy variables

data$Orobanche <- as.numeric(Orobanche == "a75") #Makes each a75 = 1 and a73 = 0
data$extract <- as.numeric(extract == "bean") #Makes each bean = 1 and cucumber = 0
head(data)
pairs(data)

#full model
full.lm.3 <- lm(count ~ ., data = data)
summary(full.lm.3)

##Significant are sample, extract
##R squared is .8408
##F-statistic:  36.2 on 3 and 17 DF,  p-value: 1.334e-07

##Partial Model
p.lm.3 <- lm(count ~ sample + extract, data = data)
summary(p.lm.3)

##Significant are sample, extract
## R squared is .8496
##F-statistic: 57.47 on 2 and 18 DF,  p-value: 1.53e-08

##Pairs plot of partial
X2 <- cbind(count = data$count, sample = data$sample, extract = data$extract)

##Check multicolinearity
cor(X2) ##Sample looks correlated to count, but that is probably good since count is our response variable

##Extra sum of Squares (Check to see if partial model is better fit than full model)
anova(p.lm.3, full.lm.3) #F test p value is above signifcant so partial model is better

##Non nexted models
stepAIC(full.lm.3)  ##Min AIC is the same model as I chose for partial model

##Residual analysis
res <- residuals(p.lm.3)
plot(res ~ data$sample) ##Looks random. Good
plot(res ~ data$extract) ##Can't tell it's categorical

fitted <- predict(p.lm.3)
plot(res ~ fitted) ##Look random, Good. No funnel shape
qqplot(res, rnorm(length(res))) ##Kind of looks normal with some crazy outlier point at second to bottom

#Conclusion:
#After creating dummy variables. The best model is p.lm.3 model (predictors = sample , extract and response = count).
#It has the lowest AIC score and best r squared value. Also pvalue for F test was significantly low. 
