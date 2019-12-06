MassDOT: Motor Vehicle Crash Analysis
================
Thejaswini Anupindi & Shriram Karthikeyan
November 8, 2019

Replace the first row for all the csv files below from the file Data/MassDOT\_IMPACT\_Template\_Fields\_Person.csv Import the year-wise person level crash dataa into separate data frames

``` r
PLCD_2019 <- read.csv("Data/2019_Person_Level_Crash_Details_.csv", stringsAsFactors=FALSE)
PLCD_2018 <- read.csv("Data/2018_Person_Level_Crash_Details_.csv", stringsAsFactors=FALSE)
PLCD_2017 <- read.csv("Data/2017_Person_Level_Crash_Details_.csv", stringsAsFactors=FALSE)
PLCD_2016 <- read.csv("Data/2016_Person_Level Crash_Details .csv", stringsAsFactors=FALSE)
PLCD_2015 <- read.csv("Data/2015_Person_Level_Crash_Details_.csv", stringsAsFactors=FALSE)
```

Delete the column TRVL\_DIRC\_DESCR from the 2019 and 2019 file

``` r
# PLCD_2019 <- PLCD_2019[ , -which(names(PLCD_2019) %in% c("TRVL_DIRC_DESCR"))]
# PLCD_2018 <- PLCD_2018[ , -which(names(PLCD_2018) %in% c("TRVL_DIRC_DESCR"))]
```

Combine the data for all years into a single data frame

``` r
PLCD<- rbind(PLCD_2015,PLCD_2016,PLCD_2017,PLCD_2018,PLCD_2019)
rm(PLCD_2015)
rm(PLCD_2016)
rm(PLCD_2017)
rm(PLCD_2018)
rm(PLCD_2019)
```

add libraries

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.5.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(magrittr)
library(stringr)
```

    ## Warning: package 'stringr' was built under R version 3.5.3

``` r
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 3.5.3

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.5.3

``` r
library(tidyr)
```

    ## 
    ## Attaching package: 'tidyr'

    ## The following object is masked from 'package:magrittr':
    ## 
    ##     extract

Remove unecessary columns

``` r
PLCD <- PLCD[ , -which(names(PLCD) %in% c("AADT_Station_Number.linked_RD","Milemarker","Opposing_Direction_Speed_Limit.linked_RD","Undivided_Left_Shoulder_Width.linked_RD","Number_of_Peak_Hour_Lanes.linked_RD","AADT_Derivation.linked_RD","Undivided_Left_Shoulder_Type.linked_RD","Truck_Exclusion_Type.linked_RD","Truck_Exclusion_Time.linked_RD","Left_Shoulder_Width.linked_RD ","Median_Width.linked_RD","Vehicle_Sequence_of_Events_.All_Vehicles.","Vehicle_Travel_Direction_.All_Vehicles.","Distance_and_Direction_from_Intersection","Distance_and_Direction_from_Milemarker","Vehicle_Configuration_.All_Vehicles.","RPA","Is_Geocoded","Geocoding_Method","Vehicle_Owner_City_Town"))]

#write.csv(PLCD,'PLCD.csv')
```

add Arial font family so that ggplot does not throw an error later

``` r
windowsFonts("Arial" = windowsFont("Arial"))
```

Heatmap of crash hour by month
==============================

``` r
PLCD$Crash_Date_YMD<- ymd(substr(PLCD$Crash_Date,1,10))
Temp_PLCD<-PLCD

#Extracting the required date format
Temp_PLCD$Crash_Month<-months(Temp_PLCD$Crash_Date_YMD)
Temp_PLCD$Crash_Date_YM<- format(PLCD$Crash_Date_YMD,"%Y %B")
Crash_Hr_Month1 <- dplyr::summarise(group_by(Temp_PLCD, Temp_PLCD$Crash_Hour, Temp_PLCD$Crash_Month), count = n())

#Renaming the columns
colnames(Crash_Hr_Month1)[colnames(Crash_Hr_Month1)=="Temp_PLCD$Crash_Month"] <- "Crash_Month"
colnames(Crash_Hr_Month1)[colnames(Crash_Hr_Month1)=="Temp_PLCD$Crash_Hour"] <- "Crash_Hour"
Crash_Hr_Month2 <- na.omit(Crash_Hr_Month1)
colnames(Crash_Hr_Month2)[colnames(Crash_Hr_Month2)=="Temp_PLCD$Crash_Month"] <- "Crash_Month"
colnames(Crash_Hr_Month2)[colnames(Crash_Hr_Month2)=="Temp_PLCD$Crash_Hour"] <- "Crash_Hour"

#Rearranging the Hour chronologically
hour <- read.csv("Data/hour.csv", na.strings="", stringsAsFactors=FALSE)


#Joining the arranged hour dataframe with the required dataframe
Final_Heatmap<-inner_join(Crash_Hr_Month2,hour)
```

    ## Joining, by = "Crash_Hour"

``` r
Final_Heatmap$Crash_Month <- substr(Final_Heatmap$Crash_Month, 0, 3)

#----------Graph - Heat Map-------------

heatmap<-ggplot(data=Final_Heatmap,
                mapping=aes(x=Final_Heatmap$Crash_Month,
                            y=reorder(Final_Heatmap$Crash_Hour,-Final_Heatmap$X), 
                            fill=Final_Heatmap$count))+
  geom_tile()  +
  xlab(label="Month")+
  ylab(label="Hour")+
  ggtitle("Crash Hour By Month")+
  theme(text=element_text(family="Arial"),
        plot.title = element_text(face="bold",size=18),
        legend.title = element_text( size=14, face="bold"),
        legend.text = element_text( size=10),
        axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))+
  scale_x_discrete(limits = month.abb)+
  geom_text(aes(label = Final_Heatmap$count),color="white")+
  scale_fill_continuous(low = "#B7DFD4", high = "#2C5985", name = "Number Of Crashes")+
  labs(fill = "Number Of Crashes")+
  theme(plot.title = element_text(hjust = 0.5))
heatmap
```

![](MVC_Mass_-_GitHub_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
#ggsave("Images/Crash_Time.jpeg", 
# plot = heatmap, 
# width = 10, height = 7, 
# units = "in",
# dpi = 600)
```

Timeseries of crash due to road surface condition
=================================================

``` r
#Extracting the required Date formats
PLCD$Crash_Date_YMD<- ymd(substr(PLCD$Crash_Date,1,10))
#months(PLCD$Crash_Date_YMD)
Temp_PLCD$Crash_Month<-months(Temp_PLCD$Crash_Date_YMD)
Temp_PLCD$Crash_Date_YM<- format(Temp_PLCD$Crash_Date_YMD,"%Y %B")
Temp_PLCD$Crash_Date_YrMonth<- format(Temp_PLCD$Crash_Date_YMD,"%Y-%m")
Temp_PLCD$Crash_Year<-format(as.Date(Temp_PLCD$Crash_Date_YMD, format="%Y-%m-%d"),"%Y")

#Subsetting the required columns
Road_Condition_Crash <- dplyr::summarise(group_by(Temp_PLCD,Temp_PLCD$Crash_Date_YrMonth, 
                                                  Temp_PLCD$Road_Surface_Condition, 
                                                  Temp_PLCD$Crash_Year, 
                                                  Temp_PLCD$Crash_Month), count = n())

#Renaming the columns with appropriate names
colnames(Road_Condition_Crash)[colnames(Road_Condition_Crash)=="Temp_PLCD$Crash_Date_YrMonth"] <- 
  "Crash_Year_Month"
colnames(Road_Condition_Crash)[colnames(Road_Condition_Crash)=="Temp_PLCD$Road_Surface_Condition"] <-
  "Road_Surface_Condition"
colnames(Road_Condition_Crash)[colnames(Road_Condition_Crash)=="Temp_PLCD$Crash_Year"] <- 
  "Crash_Year"
colnames(Road_Condition_Crash)[colnames(Road_Condition_Crash)=="Temp_PLCD$Crash_Month"] <- 
  "Crash_Month"

#Removing NAs if present and considering only the required conditions.
Road_Condition_Crash <- Road_Condition_Crash[!is.na(Road_Condition_Crash$Road_Surface_Condition), ]
Road_Condition_Crash <- Road_Condition_Crash[(Road_Condition_Crash$Road_Surface_Condition == "Dry" | Road_Condition_Crash$Road_Surface_Condition == "Ice" |
                                                Road_Condition_Crash$Road_Surface_Condition == "Slush" |
                                                Road_Condition_Crash$Road_Surface_Condition == "Snow" |
                                                Road_Condition_Crash$Road_Surface_Condition == "Wet"), ]
Road_Condition_Crash <- Road_Condition_Crash[!is.na(Road_Condition_Crash$Crash_Year_Month), ]

#----------Graph - Timeseries-------------

TimeSeries<-ggplot(Road_Condition_Crash, 
                   aes(Road_Condition_Crash$Crash_Year_Month,Road_Condition_Crash$count)) +
  geom_line(aes(color=Road_Condition_Crash$Road_Surface_Condition, 
                group=Road_Condition_Crash$Road_Surface_Condition ) )+
  geom_point(aes(color=Road_Condition_Crash$Road_Surface_Condition, 
                 group=Road_Condition_Crash$Road_Surface_Condition))+
  xlab(label="Year and Month of Crash")+
  ylab(label="Number Of Crashes")+
  scale_color_discrete(name = "Road Surface Condition")+
  ggtitle("Time Series of Road Surface condition During Crash")+
  guides(fill=guide_legend("Road Surface Condition"))+
  theme(text=element_text(family="Arial"), 
        plot.title = element_text(face="bold",size=18),
        legend.title = element_text( size=14, face="bold"),
        legend.text = element_text( size=12),
        axis.title.x = element_text(face="bold",size=14), 
        axis.title.y = element_text(face="bold",size=14),
        axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

TimeSeries
```

![](MVC_Mass_-_GitHub_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
#ggsave("TimeSeries.jpeg", 
# plot = TimeSeries, 
# width = 12, height = 8, 
# units = "in",
# dpi = 600)
```

Age group-Alcohol
=================

``` r
#Subsetting the dataframe, to consider only the required columns.
alcohol_suspected <- subset(PLCD,(Alcohol_Suspected == 'No, alcohol not used' | 
                      Alcohol_Suspected == 'Not reported' | 
                      Alcohol_Suspected == 'Reported but invalid' | 
                      Alcohol_Suspected == 'Yes, alcohol used') & 
                Driver_Age != 'NA', select = c(Driver_Age, Alcohol_Suspected))
  
agegroup_alcohol <- dplyr::summarise(group_by(alcohol_suspected,
                                              alcohol_suspected$Driver_Age, 
                                              alcohol_suspected$Alcohol_Suspected), count = n())

#Renaming the columns appropriately
colnames(agegroup_alcohol)[colnames(agegroup_alcohol)=="alcohol_suspected$Driver_Age"] <- "Driver_Age"
colnames(agegroup_alcohol)[colnames(agegroup_alcohol)=="alcohol_suspected$Alcohol_Suspected"] <- "Alcohol_Suspected"

agegroup_alcohol <- agegroup_alcohol[(agegroup_alcohol$Alcohol_Suspected == "No, alcohol not used" |
                                        agegroup_alcohol$Alcohol_Suspected == "Not reported" |
                                        agegroup_alcohol$Alcohol_Suspected == "Yes, alcohol used"),]
agegroup_alcohol$age_grp <- agegroup_alcohol$Driver_Age

#Grouping the ages of drivers
agegroup_alcohol$age_grp <- ifelse((agegroup_alcohol$Driver_Age<18) , 'UnderAge', agegroup_alcohol$age_grp)
agegroup_alcohol$age_grp <- ifelse((agegroup_alcohol$Driver_Age>=18 & agegroup_alcohol$Driver_Age<=21) , 
                                   '18-21', agegroup_alcohol$age_grp)
agegroup_alcohol$age_grp <- ifelse((agegroup_alcohol$Driver_Age>=22 & agegroup_alcohol$Driver_Age<=34) , 
                                   '22-34', agegroup_alcohol$age_grp)
agegroup_alcohol$age_grp <- ifelse((agegroup_alcohol$Driver_Age>=35 & agegroup_alcohol$Driver_Age<=50) , 
                                   '35-50', agegroup_alcohol$age_grp) 
agegroup_alcohol$age_grp <- ifelse((agegroup_alcohol$Driver_Age>=51 & agegroup_alcohol$Driver_Age<=65) , 
                                   '51-65', agegroup_alcohol$age_grp)
agegroup_alcohol$age_grp <- ifelse((agegroup_alcohol$Driver_Age>=66) , 
                                   'Above 66', agegroup_alcohol$age_grp)


#----------Graph Bar Plot-------------
Alcohol_AgeGroup <- ggplot(agegroup_alcohol, aes(x=reorder(agegroup_alcohol$Alcohol_Suspected,
                                                           agegroup_alcohol$count), 
                                                 y=agegroup_alcohol$count, fill=age_grp)) + 
  geom_bar(stat = "identity")+ 
  #ggtitle("Arrival Delay by Carrier Name, Fill By Year")+
  coord_flip() +
  guides(fill=guide_legend("Age Group"))+
  xlab(label="Number of Crashes")+
  ylab(label="Alcohol Suspected")+
  ggtitle("Crashes by Age Group and Alcohol")+
  theme(text=element_text(family="Arial"), 
        plot.title = element_text(face="bold",size=18),
        legend.title = element_text( size=14, face="bold"),
        legend.text = element_text( size=12),
        axis.title.x = element_text(face="bold",size=14), 
        axis.title.y = element_text(face="bold",size=14),
        axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12))+
  theme(plot.title = element_text(hjust = 0.5))

Alcohol_AgeGroup
```

![](MVC_Mass_-_GitHub_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
#ggsave("Alcohol_AgeGroup.jpeg", 
# plot = Alcohol_AgeGroup, 
# width = 10, height = 7, 
# units = "in",
# dpi = 600)
```

County Score
============

``` r
#Import County Wise Population
County_Pop <- read.csv("Data/County_Year_Pop_Mass.csv")
County_Pop <- gather(County_Pop, key = "Year", value = "Population" ,  2:5)
County_Pop$Year<- as.numeric(str_replace(County_Pop$Year,"X",""))

#importing 2019 population
Mass_Counties_Pop_2019 <- read.csv("Data/Mass_Counties_Pop_2019.csv")
County_Pop<-rbind(County_Pop,Mass_Counties_Pop_2019)
rm(Mass_Counties_Pop_2019)

PLCD$Crash_Year<-as.numeric(substr(PLCD$Crash_Date_YMD,1,4))

############################
#Total Number of Crashes
score_df<-PLCD%>%
  group_by(County_Name,Crash_Year)%>%
  summarise(Number_of_Crashes =n())

colnames(score_df)[colnames(score_df)=="County_Name"] <- "County"
colnames(score_df)[colnames(score_df)=="Crash_Year"] <- "Year"
Crash_Percentage_Vs_Pop <- inner_join(County_Pop,score_df)
```

    ## Joining, by = c("County", "Year")

    ## Warning: Column `County` joining factor and character vector, coercing into
    ## character vector

``` r
Crash_Percentage_Vs_Pop$Crash_Percentage<-Crash_Percentage_Vs_Pop$Number_of_Crashes/Crash_Percentage_Vs_Pop$Population

County_Crash_Percentage <-
  Crash_Percentage_Vs_Pop %>%
  group_by(County) %>%
  summarise(Crash_Percentage = mean(Crash_Percentage))


County_Crash_Percentage$Mean_Crash_Percent <- mean(County_Crash_Percentage$Crash_Percentage)

County_Crash_Percentage$Total_Crash_Score <- County_Crash_Percentage$Crash_Percentage/County_Crash_Percentage$Mean_Crash_Percent


#Traffic Device Functioning
TCDNF_score_df<-PLCD%>%
  filter(Traffic_Control_Device_Functioning == "No, device not functioning")%>%
  group_by(County_Name,Crash_Year)%>%
  summarise(TCDNF_Number_of_Crashes =n())


colnames(TCDNF_score_df)[colnames(TCDNF_score_df)=="County_Name"] <- "County"
colnames(TCDNF_score_df)[colnames(TCDNF_score_df)=="Crash_Year"] <- "Year"
TCDNF_Crash_Percentage_Vs_Pop <- inner_join(County_Pop,TCDNF_score_df)
```

    ## Joining, by = c("County", "Year")

    ## Warning: Column `County` joining factor and character vector, coercing into
    ## character vector

``` r
TCDNF_Crash_Percentage_Vs_Pop$Crash_Percentage<-
  TCDNF_Crash_Percentage_Vs_Pop$TCDNF_Number_of_Crashes/TCDNF_Crash_Percentage_Vs_Pop$Population

TCDNF_County_Crash_Percentage <-
  TCDNF_Crash_Percentage_Vs_Pop %>%
  group_by(County) %>%
  summarise(Crash_Percentage = mean(Crash_Percentage))


#Add the mean
TCDNF_County_Crash_Percentage$Mean_Crash_Percent <- mean(TCDNF_County_Crash_Percentage$Crash_Percentage)

TCDNF_County_Crash_Percentage$TCDNF_Crash_Score <- TCDNF_County_Crash_Percentage$Crash_Percentage/TCDNF_County_Crash_Percentage$Mean_Crash_Percent

###########################################
#Protective System Used
unique(PLCD$Protective_System_Used)
```

    ##  [1] ""                             "Shoulder and lap belt used"  
    ##  [3] "Unknown"                      "Helmet used"                 
    ##  [5] "Child safety seat used"       "Shoulder belt only used"     
    ##  [7] "Not reported"                 "None used - vehicle occupant"
    ##  [9] "Lap belt only used"           "Reported but invalid"

``` r
PSU_score_df<-PLCD%>%
  filter(Protective_System_Used == "None used - vehicle occupant")%>%
  group_by(County_Name,Crash_Year)%>%
  summarise(PSU_Number_of_Crashes =n())


colnames(PSU_score_df)[colnames(PSU_score_df)=="County_Name"] <- "County"
colnames(PSU_score_df)[colnames(PSU_score_df)=="Crash_Year"] <- "Year"
PSU_Crash_Percentage_Vs_Pop <- inner_join(County_Pop,PSU_score_df)
```

    ## Joining, by = c("County", "Year")

    ## Warning: Column `County` joining factor and character vector, coercing into
    ## character vector

``` r
PSU_Crash_Percentage_Vs_Pop$Crash_Percentage<-
  PSU_Crash_Percentage_Vs_Pop$PSU_Number_of_Crashes/PSU_Crash_Percentage_Vs_Pop$Population

PSU_County_Crash_Percentage <-
  PSU_Crash_Percentage_Vs_Pop %>%
  group_by(County) %>%
  summarise(Crash_Percentage = mean(Crash_Percentage))



#Add the mean
PSU_County_Crash_Percentage$Mean_Crash_Percent <- mean(PSU_County_Crash_Percentage$Crash_Percentage)

PSU_County_Crash_Percentage$PSU_Crash_Score <- PSU_County_Crash_Percentage$Crash_Percentage/PSU_County_Crash_Percentage$Mean_Crash_Percent


#Alcohol Suspected
unique(PLCD$Alcohol_Suspected)
```

    ## [1] ""                     "Not reported"         NA                    
    ## [4] "No, alcohol not used" "Yes, alcohol used"    "Unknown"             
    ## [7] "Reported but invalid"

``` r
AS_score_df<-PLCD%>%
  filter(Alcohol_Suspected == "Yes, alcohol used")%>%
  group_by(County_Name,Crash_Year)%>%
  summarise(AS_Number_of_Crashes =n())


colnames(AS_score_df)[colnames(AS_score_df)=="County_Name"] <- "County"
colnames(AS_score_df)[colnames(AS_score_df)=="Crash_Year"] <- "Year"
AS_Crash_Percentage_Vs_Pop <- inner_join(County_Pop,AS_score_df)
```

    ## Joining, by = c("County", "Year")

    ## Warning: Column `County` joining factor and character vector, coercing into
    ## character vector

``` r
AS_Crash_Percentage_Vs_Pop$Crash_Percentage<-
  AS_Crash_Percentage_Vs_Pop$AS_Number_of_Crashes/AS_Crash_Percentage_Vs_Pop$Population

AS_County_Crash_Percentage <-
  AS_Crash_Percentage_Vs_Pop %>%
  group_by(County) %>%
  summarise(Crash_Percentage = mean(Crash_Percentage))

#Add the mean
AS_County_Crash_Percentage$Mean_Crash_Percent <- mean(AS_County_Crash_Percentage$Crash_Percentage)

AS_County_Crash_Percentage$AS_Crash_Score <- AS_County_Crash_Percentage$Crash_Percentage/AS_County_Crash_Percentage$Mean_Crash_Percent



#Drugs Suspected

unique(PLCD$Drugs_Suspected)
```

    ## [1] ""                     "Not reported"         NA                    
    ## [4] "No, drug not used"    "Yes, drug used"       "Unknown"             
    ## [7] "Reported but invalid"

``` r
DS_score_df<-PLCD%>%
  filter(Drugs_Suspected == "Yes, drug used")%>%
  group_by(County_Name,Crash_Year)%>%
  summarise(DS_Number_of_Crashes =n())


colnames(DS_score_df)[colnames(DS_score_df)=="County_Name"] <- "County"
colnames(DS_score_df)[colnames(DS_score_df)=="Crash_Year"] <- "Year"
DS_Crash_Percentage_Vs_Pop <- inner_join(County_Pop,DS_score_df)
```

    ## Joining, by = c("County", "Year")

    ## Warning: Column `County` joining factor and character vector, coercing into
    ## character vector

``` r
# #checking PLCD for NA in Crash_Year
# PLCD%>%
#   filter(is.na(Crash_Year))

DS_Crash_Percentage_Vs_Pop$Crash_Percentage <-
  DS_Crash_Percentage_Vs_Pop$DS_Number_of_Crashes/DS_Crash_Percentage_Vs_Pop$Population

DS_County_Crash_Percentage <-
  DS_Crash_Percentage_Vs_Pop %>%
  group_by(County) %>%
  summarise(Crash_Percentage = mean(Crash_Percentage))



#Add the mean
DS_County_Crash_Percentage$Mean_Crash_Percent <- mean(DS_County_Crash_Percentage$Crash_Percentage)

DS_County_Crash_Percentage$DS_Crash_Score <- DS_County_Crash_Percentage$Crash_Percentage/DS_County_Crash_Percentage$Mean_Crash_Percent
```

Combine Scores and create Heat Map
==================================

``` r
#----------Combining the scores-------------
County_Scores<-as.data.frame(cbind(County_Crash_Percentage$County
                                   ,County_Crash_Percentage$Total_Crash_Score
                                   ,TCDNF_County_Crash_Percentage$TCDNF_Crash_Score
                                   ,PSU_County_Crash_Percentage$PSU_Crash_Score
                                   ,AS_County_Crash_Percentage$AS_Crash_Score
                                   ,DS_County_Crash_Percentage$DS_Crash_Score)
                             )

#Renaming the columns appropriately.
colnames(County_Scores)[colnames(County_Scores)=="V1"] <- "County"
colnames(County_Scores)[colnames(County_Scores)=="V2"] <- "Total_Crash_Score"
colnames(County_Scores)[colnames(County_Scores)=="V3"] <- "Malfunctioning_Traffic_Device"
colnames(County_Scores)[colnames(County_Scores)=="V4"] <- "Protective_System_Used"
colnames(County_Scores)[colnames(County_Scores)=="V5"] <- "Alcohol_Suspected"
colnames(County_Scores)[colnames(County_Scores)=="V6"] <- "Drugs_Suspected"


County_Scores$Total_Crash_Score<-
  round(as.numeric(as.character(County_Scores$Total_Crash_Score)),digits=2)
County_Scores$Malfunctioning_Traffic_Device<-
  round(as.numeric(as.character(County_Scores$Malfunctioning_Traffic_Device)),digits=2)
County_Scores$Protective_System_Used<-
  round(as.numeric(as.character(County_Scores$Protective_System_Used)),digits=2)
County_Scores$Alcohol_Suspected<-
  round(as.numeric(as.character(County_Scores$Alcohol_Suspected)),digits=2)
County_Scores$Drugs_Suspected<-
  round(as.numeric(as.character(County_Scores$Drugs_Suspected)),digits=2)


County_Scores <- as.data.frame(gather(County_Scores, key = "Score_Type", value = "Score" ,  2:6))
County_Scores$Score_Type <- str_replace_all(County_Scores$Score_Type,"_"," ")

#----------Graph - County Score Heat Map-------------

CountyScore <- ggplot(data=County_Scores,
                      mapping=aes(x=County_Scores$Score_Type,
                                  y=reorder(County_Scores$County,-County_Scores$Score), 
                                  fill=County_Scores$Score))+
  geom_tile()  +
  xlab(label="Score Type")+
  ylab(label="County")+
  ggtitle("County Score")+
  theme(text=element_text(family="Arial"), 
        plot.title = element_text(face="bold",size=18),
        legend.title = element_text( size=14, face="bold"),
        legend.text = element_text( size=12),
        axis.title.x = element_text(face="bold",size=12), 
        axis.title.y = element_text(face="bold",size=14),
        axis.text.x = element_text(size=12), 
        axis.text.y = element_text(size=12))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label = County_Scores$Score),color="Black")+
  scale_fill_gradient2(low ="#1EAA1C", mid = "white", high = "#D30003", midpoint = 1)+
  labs(fill = "Score")+
     labs(caption = "Formula:
                      Crash Percent = Number of Crashes / Population of County (At Year and County Level)
                      Crash Score = Crash Percentage / Mean Crash Percent(At County Level)") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.caption = element_text(hjust = 0.5,  face="italic", color="red", size=12))
 

CountyScore
```

![](MVC_Mass_-_GitHub_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
# ggsave("Images/CountyScore.jpeg",
# plot = CountyScore,
# width = 14, height = 8,
# units = "in",
# dpi = 600)
```

The above scores tell us that Nantucket is the worst county compared to the rest and it is especially has a bad "Alcohol Suspected" score because it is a beach town where a lot of people come to party. Suffolk on the other hand, seems to be the best county in the state possibly due to the high population and a significant of those who use public transportation in a dense city such as Boston as opposed to driving a car.
