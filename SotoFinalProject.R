#Jada Soto
#Final Project - Stop, Question, and Frisk Data

#For my final project, I am aiming to answer the question - did NYC's Stop and Frisk program disproportionately target persons of color? 
#For the purpose of this project, I will refer to this program as stop and frisk, or SQF.
####################################################################
#I am going to begin by importing my dataset, titled "sqf.csv". 
stopandfriskdata<-read.csv("sqf.csv", stringsAsFactors = T)
#I am also going to create a dataframe, using the 2010 and 2020 census data from NYC examining the population by race from those years.
#I could not find reputable data for the years 2011 and 2018, so I will use 2010's census data to compare SQF data for 2011.
#I will use 2020's census population data to compare SQF data in 2018. 
TotalPopulation_2010<- c(8175133)
White_2010<- c(2722904)
Black_2010 <- c(1861295)
Asian_2010<-c(1028119)
HispanicOrigin_2010<-c(2336076)
SomeOtherRace_2010<-c(78063)
populationbyrace_2010<-data.frame(Asian_2010, SomeOtherRace_2010, TotalPopulation_2010,White_2010,Black_2010,HispanicOrigin_2010)
#Now, I am going to create a dataframe for 2020's census data. 
TotalPopulation_2020<-c(8804190)
White_2020<-c(2719856)
Black_2020<-c(1776891)
Asian_2020<-c(1373502)
SomeOtherRace_2020<-c(143632)
HispanicOrigin_2020<-c(2490350)
populationbyrace_2020<-data.frame(TotalPopulation_2020,White_2020,Black_2020,Asian_2020,SomeOtherRace_2020,HispanicOrigin_2020)
#Because the SQF data did not include individuals who were considered to be of two or more races, I did not include this data in the above datasets aside from the entire population. 
#It appears that American Indians, Alaska Natives, and Native Hawaiians were considered to be "some other race", so I am including this category in the dataset. 

#Now, I am going to determine the amount of stop and frisks that occurred in NYC during the years of 2011 and 2018 using nrow.
nrow(stopandfriskdata)
#There were 696,732 stop and frisks made in NYC in the years of 2011 and 2018. 
sqf2011<-subset(stopandfriskdata, year =="2011")
#In 2011, there were 685,724 stop and frisks made in NYC. 
sqf2018<-subset(stopandfriskdata,year=="2018")
#In 2018, there were 11,008 stop and frisks made in NYC. 
#############################

#Now, I am going to begin by subsetting the stop and frisk data and racial identities provided in the dataset:
#For those who were stopped as a part of the SQF program and were considered to be American Indians or Alaskan Natives:
amerindianalasnativesqf_2011<-subset(stopandfriskdata, race=="AMERICAN INDIAN/ALASKAN NATIVE" & year=="2011")
amerindianalasnativesqf_2018<-subset(stopandfriskdata, race=="AMERICAN INDIAN/ALASKAN NATIVE"& year=="2018")
#For those who were stopped as a part of the SQF program and were considered to be Black:
blacksqf_2011<-subset(stopandfriskdata, race== "BLACK" & year=="2011")
blacksqf_2018<-subset(stopandfriskdata, race== "BLACK" & year=="2018")
#For those who were stopped as a part of the SQF program and were considered to be White:
whitesqf_2011<-subset(stopandfriskdata, race=="WHITE" & year=="2011")
whitesqf_2018<-subset(stopandfriskdata, race=="WHITE"& year=="2018")
#For those who were stopped as a part of the SQF program and were considered to be Asian/Pacific Islanders:
apisqf_2011<-subset(stopandfriskdata,race=="ASIAN / PACIFIC ISLANDER" & year=="2011")
apisqf_2018<-subset(stopandfriskdata,race=="ASIAN / PACIFIC ISLANDER" & year=="2018")
#For those who were stopped as a part of the SQF program and were considered to be Black and White Hispanics:
BlackHispanicOriginSQF_2011<-subset(stopandfriskdata, race=="BLACK HISPANIC" & year=="2011")
WhiteHispanicOriginSQF_2011<-subset(stopandfriskdata,race=="WHITE HISPANIC" & year=="2011")
BlackHispanicOriginSQF_2018<-subset(stopandfriskdata, race=="BLACK HISPANIC" & year=="2018")
WhiteHispanicOriginSQF_2018<-subset(stopandfriskdata,race=="WHITE HISPANIC" & year=="2018")
################################################################################
#Now that I have subsetted all of my data, I am going to check how many racial groups were stopped and frisked in the years of 2011 and 2018. 
table(sqf2011$race)
#According to the table I created, in 2011, AMERICAN INDIAN/ALASKAN NATIVE persons were stopped and frisked 2,897 times.
#In 2011, BLACK persons were stopped and frisked 350,743 times. 
#In 2011, WHITE persons were stopped and frisked 61,805 times.
#In 2011, ASIAN / PACIFIC ISLANDERS were stopped and frisked 23,932 times. 
#In 2011, BLACK HISPANICS were stopped and frisked 48,438 times. 
#In 2011, WHITE HISPANICS were stopped and frisked 175,302 times. 

#Now, I am going to create a bargraph analyzing racial groups and the amount of times they were stopped and frisked in 2011.
barplot(table(sqf2011$race),
        ylim= c(0,350743),
        ylab="Number of Stop and Frisks in NYC in 2011", 
        col=c("red","white","blue"),
        main="Assessing Number of Stop and Frisks in NYC in 2011 by Race",
        xlab="Racial Groups Present in NYC in 2011 That Were Stopped and Frisked",
        cex.main=1,
        cex.names=0.25,
        cex.lab=0.8)
#In this barplot, I capped the ylim to be at 350,743, the amount of times Black people were stopped and frisked in NYC in 2011. 
#I made the ylim to be this height because I wanted the ylim to remain in whole numbers; if I would have put the total amount of stop and frisks in NYC in 2011 (685,724), it would have been too large of a number for other racial groups. 
#As displayed by this graph, Black people received the most amount of stop and frisks - 350,743, while American Indian and Alaska Natives received the smallest amount of stop and frisks - 2,897. 
########################
#Now, I am going to use the dataframe I created to see the total population in NYC in 2010 overall and also for each racial group.
populationbyrace_2010
#Now, I am going to create a barplot to show the total population in NYC in 2010 based off of census data. 
barplot(c(Asian_2010,
          Black_2010,
          White_2010,
          SomeOtherRace_2010,
          HispanicOrigin_2010),
        names=c("Asian","Black","White","Some Other Race","Hispanic Origin"),
        main="Population by Racial Groups in 2010, NYC",
        col=c("red","white","blue"),
        xlab="Racial Groups Present in NYC in 2010",
        ylab="Total Population Number in NYC in 2010", 
        cex.main=1,
        cex.names=0.50,
        cex.lab=0.8)
#In this barplot, it is clear that the White population in NYC is the most in comparison to minority groups.
#The White population in NYC was 2,722,904 in 2010. 
#In comparison, the Black population in NYC in 2010 was 1,861,295. 
#This is problematic due to the fact that Black people were stopped and frisked the most in NYC in 2011 - 350,743 times. 
#See paper for further analysis. 
##################################
#Now, I am going to create a table to determine the amount of stop and frisks by racial group in 2018 in NYC.
table(sqf2018$race)
#According to the table, AMERICAN INDIAN/ALASKAN NATIVE people were stopped and frisked 16 times.
#BLACK people were stopped and frisked 6,241 times.
#WHITE people were stopped and frisked 1,074 times. 
#ASIAN/PACIFIC ISLANDERS were stopped and frisked 221 times.
#BLACK HISPANICS were stopped and frisked 978 times. 
#WHITE HISPANICS were stopped and frisked 2,411 times. 
#Next, I will create a barplot illustrating the amount of stop and frisks by race in 2018. 
barplot(table(sqf2018$race),
        ylim= c(0,6241),
        ylab="Number of Stop and Frisks in NYC in 2018", 
        col=c("red","white","blue"),
        main="Assessing Number of Stop and Frisks in NYC in 2018 by Race",
        xlab="Racial Groups Present in NYC in 2018 That Were Stopped and Frisked",
        cex.main=1,
        cex.names=0.25,
        cex.lab=0.8)
#In this barplot, it is notable that the measurements on the Y axis are remarkably smaller than the measurements in 2011. 
#In 2018, there were only 11,008 stop and frisks in comparison to 2011's whopping 685,724 stop and frisks. 
#With that being said, I capped the measurements for the Y axis at 6,241, the largest amount of stop and frisks that occurred for a racial group, which was Black people. 
############################
#Now, I am going to use the dataframe I created to see the total population in NYC in 2010 overall and also for each racial group.
populationbyrace_2020
#######
barplot(c(Asian_2020,
          Black_2020,
          White_2020,
          SomeOtherRace_2020,
          HispanicOrigin_2020),
        names=c("Asian","Black","White","Some Other Race","Hispanic Origin"),
        main="Population by Racial Groups in 2020, NYC",
        col=c("red","white","blue"),
        xlab="Racial Groups Present in NYC in 2020",
        ylab="Total Population Number in NYC in 2020", 
        cex.main=1,
        cex.names=0.50,
        cex.lab=0.8)
#In this barplot, I decided to create a graph to utilize to examine NYC's population by race in 2020 in comparison to the amount of stop and frisks by racial groups in 2018. 
#Notably, White people are still the majority group in NYC, while Black people compose of a much lower percentage of the population in comparison to White people. 
#Again, this is problematic due to the fact that Black persons were stopped and frisked the most in both years of examination - 2011 and 2018 - despite composing of a smaller portion of the population in NYC.
#See paper for further analysis. 