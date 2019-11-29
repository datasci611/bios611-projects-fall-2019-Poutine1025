install.packages('reticulate', repo="http://cran.us.r-project.org")
library(reticulate)
library(tidyverse)
Income=read_csv("./scripts/Income.csv")
Disability=read_csv("./scripts/Disability.csv")
Client=read_csv("./scripts/Client.csv")
#Demographics
Client$Gender=ifelse(Client$Gender=="Trans Female (MTF or Male to Female)","TransF",Client$Gender)
Client$Race=ifelse(Client$Race=="White (HUD)","White",Client$Race)
Client$Race=ifelse(Client$Race=="Asian (HUD)","Asian",Client$Race)
Client$Race=ifelse(Client$Race=="Black or African American (HUD)","African",Client$Race)
Client$Race=ifelse(Client$Race=="American Indian or Alaska Native (HUD)","Indian",Client$Race)
Client$Race=ifelse(Client$Race=="Native Hawaiian or Other Pacific Islander (HUD)","Hawaiian",Client$Race)
Client$Ethnicity=ifelse(Client$Ethnicity=="Hispanic/Latino (HUD)","Latino","Non-Latino")

#Age vs Gender filled by Race
ggplot(data = Client, aes(x=Gender, y=Age)) +
  geom_boxplot(aes(fill=Race)) +
  labs(title="Age vs Gender filled by Race") +
  theme_light()
ggsave(filename = "Age_vs_Gender_filled_by_Race.png",path = "./results")
#Age vs Gender filled by Ethnicity
ggplot(data = Client, aes(x=Gender, y=Age)) +
  geom_boxplot(aes(fill=Ethnicity)) +
  labs(title="Age vs Gender filled by Ethnicity") +
  theme_light()
ggsave(filename = "Age_vs_Gender_filled_by_Ethnicity.png",path = "./results")
#Race
ggplot(data = Client, aes(x=Race)) +
  geom_bar(aes(fill=Gender)) +
  labs(title = "Sample sizes of different races") +
  theme_linedraw()
ggsave(filename = "Sample_sizes_of_different_races.png",path = "./results")
#Ethinity
ggplot(data = Client, aes(x=Ethnicity)) +
  geom_bar(aes(fill=Gender)) +
  labs(title = "Sample sizes of different ethnicities") +
  theme_linedraw()
ggsave(filename = "Sample_sizes_of_different_ethnicities.png",path = "./results")

#Disability
#logistic regression
Disability$Disability=ifelse(Disability$Disability=="Yes (HUD)",1,0)
logistic_model=glm(Disability~Gender+Race+Ethnicity+Age,
                   data = Disability,
                   family = binomial(link = "logit"))
summary(logistic_model)

#Income
ggplot(data = Income, aes(x=Income)) +
  geom_histogram() +
  theme_light()

#inverse gaussian regression
invgaussain_model=glm(Income~Gender+Race+Ethnicity+Age,
                      data = Income,
                      family = inverse.gaussian(link = "1/mu^2"))
summary(invgaussain_model)

#creat Rmarkdown file
rmarkdown::render("./results/project3_report.Rmd", "html_document")
