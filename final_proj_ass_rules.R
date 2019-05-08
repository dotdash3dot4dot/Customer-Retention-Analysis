#Saving the csv file to a dataframe

survey <- read.csv("C:\\Users\\aishu\\OneDrive\\Desktop\\IST 687 DS\\DS PROJECT\\Satisfaction Survey.csv") 
# survey <- read.csv("Satisfaction Survey.csv")

#Analyse the dataset
summary(survey)
str(survey)

#table(survey$Airline.Name)

#trim the dataset to remove unnecessary spaces
trimws(survey$Satisfaction, which = c("both"))
trimws(survey$Airline.Status, which = c("both"))
trimws(survey$Airline.Code, which = c("both"))
trimws(survey$Airline.Name, which = c("both"))
trimws(survey$Age, which = c("both"))
trimws(survey$Gender, which = c("both"))
trimws(survey$Price.Sensitivity, which = c("both"))
trimws(survey$Year.of.First.Flight, which = c("both"))
trimws(survey$No.of.Flights.p.a., which = c("both"))
trimws(survey$No..of.other.Loyalty.Cards, which = c("both"))
trimws(survey$X..of.Flight.with.other.Airlines, which = c("both"))
trimws(survey$Type.of.Travel, which = c("both"))
trimws(survey$Shopping.Amount.at.Airport, which = c("both"))
trimws(survey$Eating.and.Drinking.at.Airport, which = c("both"))
trimws(survey$Class, which = c("both"))
trimws(survey$Day.of.Month, which = c("both"))
trimws(survey$Flight.Distance, which = c("both"))
trimws(survey$Flight.date, which = c("both"))
trimws(survey$Flight.cancelled, which = c("both"))
trimws( survey$Flight.time.in.minutes,which = c("both"))
trimws(survey$Orgin.City, which = c("both"))
trimws(survey$Origin.State,which = c("both"))
trimws(survey$Destination.City, which = c("both"))
trimws(survey$Destination.State, which = c("both"))
trimws(survey$Scheduled.Departure.Hour, which = c("both"))
trimws(survey$Departure.Delay.in.Minutes, which = c("both"))
trimws(survey$Arrival.Delay.in.Minutes, which = c("both"))
trimws(survey$Arrival.Delay.greater.5.Mins, which = c("both"))



#Change the column names
colnames(survey)[colnames(survey)=="Airline.Status"] <- "Airline_Status"
colnames(survey)[colnames(survey)=="Price.Sensitivity"] <- "Price_Sensitivity"
colnames(survey)[colnames(survey)=="Year.of.First.Flight"] <- "Year_of_First_Flight"
colnames(survey)[colnames(survey)=="No.of.Flights.p.a."] <- "No_of_Flights_per_annum"
colnames(survey)[colnames(survey)=="X..of.Flight.with.other.Airlines"] <- "Percent_of_Flight_with_other_Airlines"
colnames(survey)[colnames(survey)=="Type.of.Travel"] <- "Type_of_Travel"
colnames(survey)[colnames(survey)=="No..of.other.Loyalty.Cards"] <- "No_of_other_Loyalty_Cards"
colnames(survey)[colnames(survey)=="Shopping.Amount.at.Airport"] <- "Shopping_Amount_at_Airport"
colnames(survey)[colnames(survey)=="Eating.and.Drinking.at.Airport"] <- "Eating_and_Drinking_at_Airport"
colnames(survey)[colnames(survey)=="Day.of.Month"] <- "Day_of_Month"
colnames(survey)[colnames(survey)=="Flight.date"] <- "Flight_date"
colnames(survey)[colnames(survey)=="Airline.Code"] <- "Airline_Code"
colnames(survey)[colnames(survey)=="Airline.Name"] <- "Airline_Name"
colnames(survey)[colnames(survey)=="Origin.City"] <- "Origin_City"
colnames(survey)[colnames(survey)=="Origin.State"] <- "Origin_State"
colnames(survey)[colnames(survey)=="Destination.City"] <- "Destination_City"
colnames(survey)[colnames(survey)=="Destination.State"] <- "Destination_State"
colnames(survey)[colnames(survey)=="Scheduled.Departure.Hour"] <- "Scheduled_Departure_Hour"
colnames(survey)[colnames(survey)=="Departure.Delay.in.Minutes"] <- "Departure_Delay_in_Minutes"
colnames(survey)[colnames(survey)=="Arrival.Delay.in.Minutes"] <- "Arrival_Delay_in_Minutes"
colnames(survey)[colnames(survey)=="Flight.cancelled"] <- "Flight_cancelled"
colnames(survey)[colnames(survey)=="Flight.time.in.minutes"] <- "Flight_time_in_minutes"
colnames(survey)[colnames(survey)=="Flight.Distance"] <- "Flight_Distance"
colnames(survey)[colnames(survey)=="Arrival.Delay.greater.5.Mins"] <- "Arrival_Delay_greater_5_Mins"


#Converting the NAs in the three columns to their mean values
survey$Flight_time_in_minutes[is.na(survey$Flight_time_in_minutes)] <- round(mean(survey$Flight_time_in_minutes, na.rm = TRUE))
survey$Arrival_Delay_in_Minutes[is.na(survey$Arrival_Delay_in_Minutes)] <- round(mean(survey$Arrival_Delay_in_Minutes, na.rm = TRUE))
survey$Departure_Delay_in_Minutes[is.na(survey$Departure_Delay_in_Minutes)] <- round(mean(survey$Departure_Delay_in_Minutes, na.rm = TRUE))
summary(survey)

#Converting the data types to numeric

numberize <- function(survey){
  for (i in 1:28){
    survey[,i] <- as.numeric(survey[,i])
    
    
  }
  return(survey)
}


#Verify
str(survey)

#association rules
#install.packages("dplyr")
library(dplyr)

#Data set for the Southeast Airlines Co.
Airline1_df <- filter(survey, Airline_Name == "Southeast Airlines Co. ")

#Data Set for the FlyFast Airways Inc
Airline2_df <- filter(survey, Airline_Name == "FlyFast Airways Inc. ")

Airline1_df$Year_of_First_Flight <- NULL
Airline1_df$Day_of_Month <- NULL
View(Airline1_df)
summary(Airline1_df)
str(Airline1_df)

summary(Airline1_df$Age)
CreateFeature_01 <- function(v) {
  vBucket <- replicate(length(v), "Average")
  vBucket[v>56] <- "High"
  vBucket[v<56] <- "Low"
  return(vBucket)
}
Airline1_df$Age <- CreateFeature_01(Airline1_df$Age)
Airline1_df$Age <- as.factor(Airline1_df$Age)

summary(Airline1_df$Price_Sensitivity)
CreateFeature_02 <- function(v) {
  vBucket <- replicate(length(v), "Average")
  vBucket[v>1] <- "High"
  vBucket[v<1] <- "Low"
  return(vBucket)
}
Airline1_df$Price_Sensitivity <- CreateFeature_02(Airline1_df$Price_Sensitivity)
Airline1_df$Price_Sensitivity <- as.factor(Airline1_df$Price_Sensitivity)

summary(Airline1_df$No_of_Flights_per_annum)
CreateFeature_03 <- function(v) {
  vBucket <- replicate(length(v), "Average")
  vBucket[v>24] <- "High"
  vBucket[v<24] <- "Low"
  return(vBucket)
}
Airline1_df$No_of_Flights_per_annum <- CreateFeature_03(Airline1_df$No_of_Flights_per_annum)
Airline1_df$No_of_Flights_per_annum <- as.factor(Airline1_df$No_of_Flights_per_annum)

summary(Airline1_df$Percent_of_Flight_with_other_Airlines)
CreateFeature_04 <- function(v) {
  vBucket <- replicate(length(v), "Average")
  vBucket[v>6] <- "High"
  vBucket[v<6] <- "Low"
  return(vBucket)
}
Airline1_df$Percent_of_Flight_with_other_Airlines <- CreateFeature_04(Airline1_df$Percent_of_Flight_with_other_Airlines)
Airline1_df$Percent_of_Flight_with_other_Airlines <- as.factor(Airline1_df$Percent_of_Flight_with_other_Airlines)

Airline1_df$No_of_other_Loyalty_Cards <- as.factor(Airline1_df$No_of_other_Loyalty_Cards)

summary(Airline1_df$Shopping_Amount_at_Airport)
CreateFeature <- function(u) {
  q <- quantile(u, c(0.3, 0.6))
  uCategories <- replicate(length(u), "Average")
  uCategories[u <= q[1]] <- "Low"
  uCategories[u > q[2]] <- "High"
  return(uCategories)
}
Airline1_df$Shopping_Amount_at_Airport <- CreateFeature(Airline1_df$Shopping_Amount_at_Airport)
Airline1_df$Shopping_Amount_at_Airport <- as.factor(Airline1_df$Shopping_Amount_at_Airport)

summary(Airline1_df$Satisfaction)
CreateFeature_09 <- function(v) {
  vBucket <- replicate(length(v), "Average")
  vBucket[v>3] <- "High"
  vBucket[v<3] <- "Low"
  return(vBucket)
}
Airline1_df$Satisfaction <- CreateFeature_09(Airline1_df$Satisfaction)
Airline1_df$Satisfaction <- as.factor(Airline1_df$Satisfaction)
  
summary(Airline1_df$Eating_and_Drinking_at_Airport)
CreateFeature <- function(u) {
  q <- quantile(u, c(0.3, 0.6))
  uCategories <- replicate(length(u), "Average")
  uCategories[u <= q[1]] <- "Low"
  uCategories[u > q[2]] <- "High"
  return(uCategories)
}
Airline1_df$Eating_and_Drinking_at_Airport <- CreateFeature(Airline1_df$Eating_and_Drinking_at_Airport)
Airline1_df$Eating_and_Drinking_at_Airport <- as.factor(Airline1_df$Eating_and_Drinking_at_Airport)

summary(Airline1_df$Scheduled_Departure_Hour)
CreateFeature <- function(u) {
  q <- quantile(u, c(0.25, 0.5, 0.75))
  uCategories <- replicate(length(u), "Morning")
  uCategories[u <= q[1]] <- "Evening"
  uCategories[u > q[2]] <- "Night"
  uCategories[u > q[3]] <- "Midnight"
  return(uCategories)
}
Airline1_df$Scheduled_Departure_Hour <- CreateFeature(Airline1_df$Scheduled_Departure_Hour)
Airline1_df$Scheduled_Departure_Hour <- as.factor(Airline1_df$Scheduled_Departure_Hour)

summary(Airline1_df$Departure_Delay_in_Minutes)
CreateFeature <- function(u) {
  q <- quantile(u, c(0.3, 0.6))
  uCategories <- replicate(length(u), "Average")
  uCategories[u <= q[1]] <- "Low"
  uCategories[u > q[2]] <- "High"
  return(uCategories)
}
Airline1_df$Departure_Delay_in_Minutes <- CreateFeature(Airline1_df$Departure_Delay_in_Minutes)
Airline1_df$Departure_Delay_in_Minutes <- as.factor(Airline1_df$Departure_Delay_in_Minutes)

summary(Airline1_df$Arrival_Delay_in_Minutes)
CreateFeature <- function(u) {
  q <- quantile(u, c(0.3, 0.6))
  uCategories <- replicate(length(u), "Average")
  uCategories[u <= q[1]] <- "Low"
  uCategories[u > q[2]] <- "High"
  return(uCategories)
}
Airline1_df$Arrival_Delay_in_Minutes <- CreateFeature(Airline1_df$Arrival_Delay_in_Minutes)
Airline1_df$Arrival_Delay_in_Minutes <- as.factor(Airline1_df$Arrival_Delay_in_Minutes)

summary(Airline1_df$Flight_time_in_minutes)
CreateFeature <- function(u) {
  q <- quantile(u, c(0.3, 0.6))
  uCategories <- replicate(length(u), "Average")
  uCategories[u <= q[1]] <- "Low"
  uCategories[u > q[2]] <- "High"
  return(uCategories)
}
Airline1_df$Flight_time_in_minutes <- CreateFeature(Airline1_df$Flight_time_in_minutes)
Airline1_df$Flight_time_in_minutes <- as.factor(Airline1_df$Flight_time_in_minutes)

summary(Airline1_df$Flight_Distance)
CreateFeature <- function(u) {
  q <- quantile(u, c(0.3, 0.6))
  uCategories <- replicate(length(u), "Average")
  uCategories[u <= q[1]] <- "Low"
  uCategories[u > q[2]] <- "High"
  return(uCategories)
}
Airline1_df$Flight_Distance <- CreateFeature(Airline1_df$Flight_Distance)
Airline1_df$Flight_Distance <- as.factor(Airline1_df$Flight_Distance)

str(Airline1_df)

View(Airline1_df)
install.packages("arules") 
library("arules") 


rules<-apriori(Airline1_df, parameter = list(support=0.1,confidence=0.5,minlen=2),appearance = list(rhs=("Satisfaction" = "1")))
View(Airline1_df)

# linear modelling

#single regression
# 
# install.packages("ggplot2")
# library("ggplot2")
# 
# survey1 <- filter(survey, Age > 55)
# 
# survey1 <- numberize(survey1)
# str(survey1)
# 
# 
# modelS1 <- lm(formula = Satisfaction ~Airline_Status, data = survey1)
# summary(modelS1)
# 
# Airline_Status <- jitter(survey1$Airline_Status)
# ggplot(data = survey1, aes(x=Airline_Status,y = Satisfaction )) + geom_point()+ggtitle("Airline_Status")
# 
# plot(x=Airline_Status, y= survey1$Satisfaction)
# 
# abline(modelS1)
# 
# modelS2 <- lm(formula = Satisfaction ~Age, data = survey1)
# summary(modelS2)
# 
# modelS3 <- lm(formula = Satisfaction ~Gender, data = survey1)
# summary(modelS3)
# 
# modelS4 <- lm(formula = Satisfaction ~Price_Sensitivity, data = survey1)
# summary(modelS4)
# 
# modelS5 <- lm(formula = Satisfaction ~Year_of_First_Flight, data = survey1)
# summary(modelS5)
# 
# modelS6 <- lm(formula = Satisfaction ~No_of_Flights_per_annum, data = survey1)
# summary(modelS6)
# 
# modelS7 <- lm(formula = Satisfaction ~Type_of_Travel, data = survey1)
# summary(modelS7)
# 
# modelS8 <- lm(formula = Satisfaction ~Eating_and_Drinking_at_Airport, data = survey1)
# summary(modelS8)
# 
# modelS9 <- lm(formula = Satisfaction ~Flight_cancelled, data = survey1)
# summary(modelS9)
# 
# modelS10 <- lm(formula = Satisfaction ~Arrival_Delay_greater_5_Mins, data = survey1)
# summary(modelS10)

