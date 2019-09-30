library(tidyverse)
library(readr)
library(ggplot2)
library(lubridate)
##data import##
UMD=read_tsv("../data/UMD_Services_Provided_20190719.tsv")
metadata=read_tsv("../data/UMD_Services_Provided_metadata_20190719.tsv")

##data cleaning##
UMD_selected=UMD %>% 
  select(`Date`, `Client File Number`, `Food Pounds`, `Clothing Items`, `Food Provided for`) %>%#select variables
  rename(CFN=`Client File Number`, food=`Food Pounds`, clothing=`Clothing Items`, number=`Food Provided for`) %>%#simplify name
  drop_na()#drop NAs
head(UMD_selected)

##question1##
#Food Pounds
ggplot(data = UMD_selected, aes(food)) +
  geom_histogram() +
  labs(x="Food Pounds", title = "Histogram of Food Pound")
  

#Clothing Items
ggplot(data = UMD_selected, aes(clothing)) +
  geom_histogram() +
  labs(x="Clothing Items", title = "Histogram of Clothing Items")

#Number of people in the family for which food was provided
ggplot(data = UMD_selected, aes(number)) +
  geom_histogram() +
  labs(x="Food Provided for", title = "Histogram of Food Provided for")

UMD_selected=UMD_selected %>% filter((food<75) & (clothing<40) & (number<10))

#Food Pounds
ggplot(data = UMD_selected, aes(food)) +
  theme_linedraw()+
  geom_histogram() +  
  labs(x="Food Pounds", title = "Histogram of Food Pound without outliers")

#Clothing Items
ggplot(data = UMD_selected, aes(clothing)) +
  theme_linedraw() +
  geom_histogram() +
  labs(x="Clothing Items", title = "Histogram of Clothing Items without outliers")

#Number of people in the family for which food was provided
ggplot(data = UMD_selected, aes(number)) +
  theme_linedraw() +
  geom_histogram() +
  labs(x="Food Provided for", title = "Histogram of Food Provided for without outliers")

#number vs food
ggplot(data=UMD_selected,aes(x=number, y=food)) +
  geom_point() +
  geom_smooth() +
  labs(x="Food Provided for",
       y="Food Pounds",
       title = "Food Pounds vs Food Provided for")

#number vs clothing
ggplot(data=UMD_selected,aes(x=number, y=clothing)) +
  geom_point() +
  geom_smooth() +
  labs(x="Food Provided  for",
       y="Clothing Items",
       title = "Clothing Items vs Food Provided for")

#food vs clothing
ggplot(data=UMD_selected,aes(x=food, y=clothing)) +
  geom_point() +
  geom_smooth() +
  labs(x="Food Pounds",
       y="Clothing Items",
       title="Food Pounds vs Clothing Items")

#subset1
UMD_subset1=UMD_selected %>% 
  group_by(CFN) %>%
  summarize(number=round(mean(number)), food=sum(food), clothing=sum(clothing), freq=n()) %>%
  mutate(food_mean=food/freq, clothing_mean=clothing/freq)
  
#number vs food_mean
ggplot(data=UMD_subset1,aes(x=number, y=food_mean)) +
  theme_linedraw() +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x="Food Provided for",
       y="Average Food Pounds",
       title = "Average Food Pounds vs Food Provided for")

#number vs clothing_mean
ggplot(data=UMD_subset1,aes(x=number, y=clothing_mean)) +
  theme_linedraw() +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) + 
  labs(x="Food Provided for",
       y="Average Clothing Items",
       title = "Average Clothing Items vs Food Provided for")

#food vs clothing
ggplot(data=UMD_subset1,aes(x=food, y=clothing)) +
  theme_linedraw() +
  geom_point() +
  geom_smooth(aes(colour="gam")) +
  geom_smooth(method = "lm", aes(colour="lm")) +
  labs(x="Food Pounds",
       y="Clothing Items",
       title="Clothing Items vs Food Pounds")

##question2##
#subset2
UMD_subset2=UMD_selected %>% 
  mutate(Date=as.Date(Date, format="%m/%d/%Y")) %>%
  mutate(year=year(Date), month=month(Date), day=day(Date)) %>%
  mutate(month=as.factor(month), day=as.factor(day)) %>%
  filter((year>2004) & (year<2019))

ggplot(data=UMD_subset2,aes(year)) +
  geom_bar(aes(fill=month)) +
  labs(x="Year",
       title = "Bar chart of Year")

ggplot(data = UMD_subset2, aes(x=month, y=clothing)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  labs(x="Month",
       y="Clothing Items",
       title = "Boxplot of Month and Clothing Items")

ggplot(data = UMD_subset2, aes(x=month, y=food)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  labs(x="Month",
       y="Clothing Items",
       title = "Boxplot of Month and Food Pounds")

UMD_subset3=UMD_subset2 %>% 
  group_by(year) %>%
  summarize(food=sum(food), clothing=sum(clothing), number=sum(number), freq=n())

ggplot(data = UMD_subset3, aes(x=year, y=food)) +
  geom_point() +
  geom_smooth(method = "lm")
