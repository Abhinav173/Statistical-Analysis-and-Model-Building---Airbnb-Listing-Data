# ABHINAV SACHDEVA


###########################
#Load necessary Libraries:
###########################

library(car)
library(dplyr)
library(psych)
library(dplyr)
detach("package:plyr", unload=TRUE) 
library(leaflet)
library(gender)
library(rockchalk)
library(gdata)
library(plyr)
library(corrplot)
library(gmodels)
library(ggplot2)


#######################
# Load Data File: 
#######################

load("~/Desktop/Airbnb_data.RData")


#######################
# Exploring Data Set: 
#######################

airbnb <- Airbnb_Data_Clean

str(airbnb)


#######################
# Cleaning the Data Set:
######################

airbnb$price <- as.numeric(sub("\\$", "", airbnb$price)) # Remove $ sign from Price Variable

## Changing Variable Type: 
airbnb$bed_type <- as.factor(airbnb$bed_type)#Change Bed Type variable to factor
table(airbnb$bed_type)

airbnb$room_type <- as.factor(airbnb$room_type)#Change Room Type variable to factor
table(airbnb$room_type)

airbnb$property_type <- as.factor(airbnb$property_type)#Change Property Type variable to factor
table(airbnb$property_type)

airbnb$gender <- as.factor(airbnb$gender)

airbnb$Chicago_region <- as.factor(airbnb$Chicago_region)#Change Chicago Region variable to factor
table(airbnb$Chicago_region)

airbnb$zipcode <- as.factor(airbnb$zipcode)#Change Zipcode variable to factor
table(airbnb$zipcode)

airbnb$neighbourhood_cleansed <- as.factor(airbnb$neighbourhood_cleansed)#Change Neighborhood variable to factor
table(airbnb$neighbourhood_cleansed)

airbnb$cancellation_policy <- as.factor(airbnb$cancellation_policy)#Change Cancellation Policy variable to factor
table(airbnb$cancellation_policy)

airbnb$Multiple_Host <- as.factor(airbnb$Multiple_Host)#Change Multiple Host variable to factor
table(airbnb$Multiple_Host)

## Changing levels in Factor Variables:
### Minimum Nights(2 Levels: 1 and More than 1):
airbnb$minimum_nights<- as.numeric(airbnb$minimum_nights)
airbnb$minimum_nights[airbnb$minimum_nights==1 ] <- "1"
airbnb$minimum_nights[airbnb$minimum_nights > 1] <- "More than 1"
airbnb$minimum_nights <-as.factor(airbnb$minimum_nights)
table(airbnb$minimum_nights)

### accomodates(2 Levels: Atmost 2 and More than 2):
airbnb$accommodates<- as.character(airbnb$accommodates)
airbnb$accommodates<- as.factor(airbnb$accommodates)
airbnb$accommodates<-recode(airbnb$accommodates, `1` = "Atmost 2",`2` = "Atmost 2", .default ="More than 2")
table(airbnb$accommodates)

### Bathrooms(2 Levels: 1 and More than 1):
airbnb$bathrooms<- as.character(airbnb$bathrooms)
airbnb$bathrooms<- as.factor(airbnb$bathrooms)
airbnb$bathrooms<-recode(airbnb$bathrooms, `1` = "1", .default ="More than 1")
table(airbnb$bathrooms)

### Bedrooms(2 Levels: 1 and More than 1):
airbnb$bedrooms<- as.character(airbnb$bedrooms)
airbnb$bedrooms<- as.factor(airbnb$bedrooms)
airbnb$bedrooms<-recode(airbnb$bedrooms, `1` = "1", .default ="More than 1")
table(airbnb$bedrooms)

### Beds(2 Levels: 1 and More than 1):
airbnb$beds<- as.character(airbnb$beds)
airbnb$beds<- as.factor(airbnb$beds)
airbnb$beds<-recode(airbnb$beds, `1` = "1", .default ="More than 1")
table(airbnb$beds)

## Combining levels in Factor Variable:
### Cancellation Policy(Combining levels:super_strict_30 or strict to level: strict:
airbnb$cancellation_policy <-  combineLevels(airbnb$cancellation_policy,levs = c("strict", "super_strict_30"), newLabel = c("strict"))

str(airbnb)



#######################
# Univariate Analysis: 
######################

##Gender: 
ggplot(airbnb, aes(x=gender, fill= gender)) + 
  geom_bar(position="dodge") + 
  labs(title="Univariate Analysis: Gender", x="Gender", y="Count") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_discrete(name = "Gender")


##Room Type: 
ggplot(airbnb, aes(x=room_type, fill= room_type)) + 
  geom_bar(position="dodge") + 
  labs(title="Univariate Analysis: Property Type", x="Room Type", y="Count") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Room Type")


##Accomodates: 
ggplot(airbnb, aes(x=accommodates, fill= accommodates)) + 
  geom_bar(position="dodge") + 
  labs(title="Univariate Analysis: Accommodates", x="Accommodates", y="Count") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Accommodates")

##Bathrooms: 
ggplot(airbnb, aes(x=bathrooms, fill= bathrooms)) + 
  geom_bar(position="dodge") + 
  labs(title="Univariate Analysis: Bathrooms", x="Bathrooms", y="Count") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Bathrooms")

##Bedrooms:
ggplot(airbnb, aes(x=bedrooms, fill= bedrooms)) + 
  geom_bar(position="dodge") + 
  labs(title="Univariate Analysis: Bedrooms", x="Bedrooms", y="Count") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Bedrooms")

##Beds:
ggplot(airbnb, aes(x=beds, fill= beds)) + 
  geom_bar(position="dodge") + 
  labs(title="Univariate Analysis: Number of Beds", x="Number of Beds", y="Count") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Number of Beds")

##Minimum Nights:
ggplot(airbnb, aes(x=minimum_nights, fill= minimum_nights)) + 
  geom_bar(position="dodge") + 
  labs(title="Univariate Analysis: Minimum Nights", x="Minimum Nights", y="Count") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Minimum Nights")

##Cancellation Policy:
ggplot(airbnb, aes(x=cancellation_policy, fill= cancellation_policy)) + 
  geom_bar(position="dodge") + 
  labs(title="Univariate Analysis: Cancellation Policy", x="Cancellation Policy", y="Count") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Cancellation Policy")

##Chicago Region:
ggplot(airbnb, aes(x=Chicago_region, fill= Chicago_region)) + 
  geom_bar(position="dodge") + 
  labs(title="Univariate Analysis: Chicago Region", x="Chicago Region", y="Count") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Chicago Region")

##Multiple Hosts:
ggplot(airbnb, aes(x=Multiple_Host, fill= Multiple_Host)) + 
  geom_bar(position="dodge") + 
  labs(title="Univariate Analysis: Multiple Hosts", x="Multiple Hosts", y="Count") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Multiple Hosts")

##Zipcode:
ggplot(airbnb, aes(x=zipcode, fill= zipcode)) + 
  geom_bar(position="dodge") + 
  labs(title="Univariate Analysis: Zipcode", x="Zipcode", y="Count", main="horiz=FALSE (Default)" ) + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Zipcode")

##Number of Reviews:

hist(airbnb$number_of_reviews, main="Distribution of Number of Reviews", 
     col="steelblue", freq=F, ylim = c(0,0.07), xlab = "Number of Reviews")
lines(density(airbnb$number_of_reviews, na.rm = T), col="orange", lwd=3)

##Log Transformation-Number of Reviews:
hist(log(airbnb$number_of_reviews), main="Log Transformation: Distribution of Number of Reviews", 
     col="steelblue", freq=F, ylim = c(0,0.4), xlab = "Log(Number of Reviews)")
lines(density(log(airbnb$number_of_reviews), na.rm = T), col="orange", lwd=3)


##Price[DEPENDENT VARIABLE]:

hist(airbnb$price, main="Distribution of Price", 
     col="steelblue", freq=F, ylim = c(0,0.01), xlab = "Price")
lines(density(airbnb$price, na.rm = T), col="orange", lwd=3)

##Log Transformation-Price:
hist(log(airbnb$price), main="Log Transformation: Distribution of Price", 
     col="steelblue", freq=F, ylim = c(0,0.8), xlab = "Log(Price)")
lines(density(log(airbnb$price), na.rm = T), col="orange", lwd=3)


#####################
# Bivariate Analysis:
#####################

## Price Distribution by Chicago Regions:
ggplot(data=airbnb, aes(x=log(price), fill=Chicago_region))+geom_density(alpha=.8) +
  labs(title="Price Distribution by Chicago Regions", x="Log(Price)", y="Density") +  
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Chicago Region")


## Cancellation Policy Distribution by Region:
ggplot(airbnb, aes(x=Chicago_region, fill=cancellation_policy)) + 
  geom_bar(position="dodge") +
  labs(title="Cancellation Policy Distribution by Region", x="Chicago Region", y="Count(Cancellation Policy)") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Cancellation Policy")


## Distribution of Number of Reviews by Gender:
ggplot(data=airbnb, aes(x=log(number_of_reviews), fill=gender)) +
  geom_density(alpha=.8) +
  labs(title="Distribution of Number of Reviews by Gender", x="log(Number of Reviews)", y="Density") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Gender")


##Distribution of Number of Reviews by Cancellation Policy:
ggplot(data=airbnb, aes(x=log(number_of_reviews), fill=cancellation_policy)) +
  geom_density(alpha=.8) +
  labs(title="Distribution of Number of Reviews by Cancellation Policy", x="log(Number of Reviews)", y="Density") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Cancellation Policy")


## Distribution of Number of Reviews by Chicago Region
ggplot(data=airbnb, aes(x=log(number_of_reviews), fill=Chicago_region)) +
  geom_density(alpha=.8)+
  labs(title="Distribution of Number of Reviews by Chicago Region", x="log(Number of Reviews)", y="Density") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Chicago Region")


## Relationship between  Price and Number of Reviews in different Regions of Chicago: 
ggplot(data=airbnb, aes(x=log(number_of_reviews), y=log(price))) + 
  geom_point() + 
  facet_grid(.~Chicago_region) +
  stat_smooth(method="lm", se=F, lwd=2) + 
  labs(title="Relationship between Price and Number of Reviews in different Regions of Chicago", x="Log(Number of Reviews)", y="Log(Price)") +  
  theme(plot.title = element_text(hjust = 0.5))


## Relationship between Price & Number of Crimes reported by Chicago Regions:
ggplot(airbnb, aes(x=No_Crime_Reported, y=price, color=Chicago_region)) +
  geom_point() + geom_jitter()+
  labs(title="Relationship between Price & Number of Crimes reported by Chicago Regions", x="Number of Crimes Reported", y="Price") +  
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(name="Chicago Region")+
  facet_grid(.~airbnb$Chicago_region)


## Relationship between Price &  Number of Crimes reported by Cancellation Policy:
ggplot(airbnb, aes(x=No_Crime_Reported, y=price, color=cancellation_policy)) +
  geom_point() + geom_jitter() +
  labs(title="Relationship between Price &  Number of Crimes reported by Cancellation Policy", x="Number of Crimes Reported", y="Price") +  
  theme(plot.title = element_text(hjust = 0.5))+
  facet_grid(.~airbnb$cancellation_policy) + scale_color_discrete(name = "Cancellation Policy")


## Relationship between Price & Number of Reviews by Cancellation Policy among Different Chicago Regions
ggplot(data=airbnb, aes(x=number_of_reviews, y=price, color=cancellation_policy)) + 
  geom_point() + 
  facet_grid(.~Chicago_region) +
  labs(title="Relationship between Price & Number of Reviews by Cancellation Policy among Different Chicago Regions", x="Number of Reviews", y="Price") +  
  theme(plot.title = element_text(hjust = 0.5))+ scale_color_discrete(name = "Cancellation Policy")


## Relationship between Price & Walkscore by Cancellation Policy among Different Regions:
ggplot(data=airbnb, aes(x=Walkscore, y=(price), color=cancellation_policy)) + 
  geom_point() + 
  facet_grid(.~Chicago_region) +
  labs(title="Relationship between Price & Walkscore by Cancellation Policy among Different Regions", x="Walkscore", y="Price") + 
  theme(plot.title = element_text(hjust = 0.5))+ scale_color_discrete(name = "Cancellation Policy")


## Relationship between Price & Walkscore by Cancellation Policy among Different Genders:
ggplot(data=airbnb, aes(x=Walkscore, y=price, color=cancellation_policy)) + 
  geom_point() + 
  facet_grid(.~gender) +
  labs(title="Relationship between Price & Walkscore by Cancellation Policy among Different Genders", x="Walkscore", y="Price") +  
  theme(plot.title = element_text(hjust = 0.5)) + scale_color_discrete(name = "Cancellation Policy")


################
# ANOVA: 
###############

## Hypothesis 1: 
### Checking for conditions of ANOVA
airbnb %>% group_by(Chicago_region)%>%summarise(Average= mean(number_of_reviews, na.rm= TRUE), Median= median(number_of_reviews, na.rm=TRUE),SD= sd(number_of_reviews, na.rm=TRUE),Variance= var(number_of_reviews,na.rm = TRUE))
### ANOVA Test:
aov(log(airbnb$number_of_reviews)~airbnb$Chicago_region)
summary(aov(log(airbnb$number_of_reviews)~airbnb$Chicago_region))
TukeyHSD(aov(log(airbnb$number_of_reviews)~airbnb$Chicago_region),conf.level = 0.99)


#####################
# Correlation: 
#####################

##Correlation Matrix 1 (Hypothesis 2):
airbnb_test <- airbnb %>% filter(!is.na(airbnb$price))#Filter NA Values of Price
airbnbnum<- airbnb_test[,c(16,21,22,27,28)]
cormat <- cor(airbnbnum)
corrplot(cormat, addCoef.col = "black", method = "square", type="upper", diag=FALSE, title= "Correlation Matrix",mar=c(0,0,4,0))

##Correlation Test between price and number of reviews
cor.test(airbnb$price,airbnb$number_of_reviews)

##Correlation Matrix 2:
airbnb_avail<- airbnb_test[,c(18,19,20,21)]# Correlation Matrix of Availability
cormat_avail <- cor(airbnb_avail)
corrplot(cormat_avail, addCoef.col = "black", method = "square", type="upper", diag=FALSE, title= "Correlation Matrix: Availability",mar=c(0,0,4,0))


############
# Chisq Test:
############

## Hypothesis 3: 
airbnb_tab<- table(airbnb$cancellation_policy, airbnb$gender)
prop.table(airbnb_tab)*100
chisq.test(airbnb_tab)


#################################
# Checking for Multicollinearity:
################################

#Model 0 (Dependant Variable: Price): 
model0 <- lm(price ~  accommodates  + log(number_of_reviews) + cancellation_policy + Chicago_region + gender + bathrooms + No_Crime_Reported + Walkscore, data=airbnb)

## Variance Inflation Factor (VIF): 
vif(model0) 
sqrt(vif(model0)) > 2 # No Multicollinearity found


#####################
# Regression Models: 
#####################

## Changing the Reference Levels:
airbnb$Chicago_region <- relevel(airbnb$Chicago_region, ref=2) 

airbnb$gender <- relevel(airbnb$gender, ref=3) 

airbnb$bathrooms <- relevel(airbnb$bathrooms, ref=2)



options(scipen = 999)


## Model 1 (Dependant Variable: Price): 
model1 <- lm(price ~  accommodates  + log(number_of_reviews) + cancellation_policy + Chicago_region + gender + bathrooms + No_Crime_Reported + Walkscore, data=airbnb)

summary(model1)
hist(resid(model1), main="Distribution of Residuals", 
     col="steelblue", ylim = c(0,2500), xlab = "Residuals")
### Diagnostic Plots:
par(mfrow = c(2,2)) 
plot(model1)


## Model 2 (Dependant Variable: log(Price)):
model2 <- lm(log(price) ~  accommodates  + log(number_of_reviews) + cancellation_policy + Chicago_region + gender + bathrooms + No_Crime_Reported + Walkscore, data=airbnb)
summary(model2)
hist(resid(model2), main="Distribution of Residuals", 
     col="steelblue", ylim = c(0,2500), xlab = "Residuals")

### Diagnostic Plots: 
par(mfrow = c(2,2)) 
plot(model2)



## Model 3 (Dependant Variable: log(Price) after removing Outliers):

### Observing the Outliers:
plot(hatvalues(model2)) 
tail(sort(hatvalues(model2)), n =2)
airbnb[c(2390,3160),]

### Removing the Outliers:
outliers <- c(2390,3160)
airbnb1<-airbnb[-outliers,]

# Changing the reference level: North Region as reference
airbnb1$Chicago_region <- relevel(airbnb1$Chicago_region, ref=2) 

# Changing the referece level: Bathroom=1 as reference
airbnb1$bathrooms <- relevel(airbnb1$bathrooms, ref=2)

model3<-lm(log(price) ~  accommodates  + log(number_of_reviews) + cancellation_policy + Chicago_region + gender + bathrooms + No_Crime_Reported + Walkscore, data=airbnb1)
summary(model3) 
hist(resid(model3), main="Distribution of Residuals", 
     col="steelblue", ylim = c(0,2500), xlab = "Residuals")

### Diagnostic Plots: 
par(mfrow = c(2,2)) 
plot(model3)

##############################################################################################################################################################################################################










































