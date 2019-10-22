
clean_data <- function(data){
  data_selected=data %>% 
    select(`Date`, `Client File Number`, `Food Pounds`, `Clothing Items`, `Food Provided for`) %>%
    rename(`Number of people per family`=`Food Provided for`) %>%
    drop_na() %>%
    filter((`Food Pounds`<75) & (`Clothing Items`<40) & (`Number of people per family`<10))
  
  return(data_selected)
}

clean_data2 <- function(data){
  data_selected=data %>% 
    mutate(Date=as.Date(Date, format="%m/%d/%Y")) %>%
    mutate(year=year(Date), month=month(Date), day=day(Date)) %>%
    mutate(month=as.factor(month), day=as.factor(day)) %>%
    filter((year>2004) & (year<2019))
  
  return(data_selected)
}

clean_data3 <- function(data){
  data_selected=data %>% 
    group_by(year) %>%
    summarize(`Food Pounds`=sum(`Food Pounds`)) %>%
    rename(Year=year)

  return(data_selected)
}

Vrelation <- function(data, Xvariable, Yvariable){
  ggplot(data, aes(x=get(Xvariable), y=get(Yvariable))) +
    geom_point() +
    geom_smooth() +
    labs(x = Xvariable,
         y = Yvariable,
         title = paste(Yvariable, " vs ", Xvariable))
}

Timeplot <- function(data, Y, M){
  dataS = data %>%
    filter(year>=Y[1] & year<=Y[2])%>%
    filter(month%in%M)
  
  ggplot(dataS,aes(year)) +
    geom_bar(aes(fill=month)) +
    labs(x="Year",
         title = paste("The number of transactions from ", Y[1]," to ",Y[2]))
}

linearmodel <- function(data, Y){
  model_FoodvsYear=lm(`Food Pounds`~Year, data)
  Food=predict(model_FoodvsYear, newdata = data.frame(Year=Y))
  dataN=data %>%
    mutate(Group="Old")
  dataN=rbind(dataN, c(Y, Food, "New"))
  dataN$Year=as.numeric(dataN$Year)
  dataN$`Food Pounds`=as.numeric(dataN$`Food Pounds`)
  
  ggplot(dataN, aes(x=Year, y=`Food Pounds`)) +
    geom_point(aes(color=Group), size = 3) +
    geom_smooth(method = "lm") +
    labs(x="Year",
         y="Food Pounds",
         title=paste("Food amount prediction for year ", Y))
}