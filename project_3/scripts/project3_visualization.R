library(tidyverse)
client=read_csv("Client.csv")
disability=read_csv("Disability.csv")

ggplot(data = client, aes(x=Gender)) + 
  geom_bar(aes(fill=Race))

ggplot(data = client, aes(x=Gender)) + 
  geom_bar(aes(fill=Ethnicity))

ggplot(data = client, aes(x=Age)) +
  geom_histogram() +
  theme_linedraw()

ggplot(data = disability, aes(x=DisaType)) + 
  geom_bar(aes(fill=as.factor(Year)))

ggplot(data = disability, aes(x=Date)) + 
  geom_bar()
