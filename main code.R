userprofile <- read.csv("C:/Users/GALAXY AUTOMATION/Desktop/data sci/Datasets/userprofile.csv")
plot_missing(userprofile)
library(tidyverse)
userprofile1<-userprofile[ ,c(-1,-2,-3)]
Zomato.Chennai.Listing.2020 <- read.csv("C:/Users/GALAXY AUTOMATION/Desktop/data sci/Datasets/Zomato Chennai Listing 2020.csv")
sum(is.na(userprofile1))
str(userprofile1)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(dplyr)
ggplot(data=userprofile1)+geom_bar(mapping=aes(x=smoker),position="dodge")+facet_wrap(~drink_level)+ggtitle("Smoker Vs Drink_level")
ggplot(data=userprofile1)+geom_bar(mapping=aes(x=smoker,fill=drink_level),position="dodge")+ggtitle("Smoker Vs Drink_level")      
ggplot(data=userprofile1)+geom_bar(mapping=aes(x=drink_level,fill=smoker))+ggtitle("Smoker & Drinkers in One Graph")
ggplot(data=userprofile1)+geom_bar( mapping = aes(x = marital_status, fill =transport),position="dodge")+ggtitle("Marital status & trasport")
grid.arrange(marital_status_plot, marital_status_last, nrow = 6) 
Z<-Zomato.Chennai.Listing.2020[1:138,]
A<-cbind(Z,userprofile1)
main<-A[,c(2,4,5,6,8,9,10,11,16,17,18,27)]
Q<-table(main$marital_status)
install.packages("plotrix")
library(plotrix)
lbs=paste(c("widow","Married","singal"),"%",sep=" ")
pie3D(Q,labels=lbs,main="Pie Chart Showing Ratio of Female and Male")
ggplot(data=main)+ geom_bar(mapping=aes(x=Dining.Rating,fill=Delivery.Rating))
ggplot(data=main)+geom_bar(mapping=aes(x=Delivery.Rating))
W<-main[1:6,]
#required aseding order
#barplot(decreasing="T"(table(W$Location)))
names(main)
top_rest_type <- main %>% select(Dining.Rating) %>% group_by(Dining.Rating) %>% count() %>% arrange(desc(n))
top_rest_type <- top_rest_type[1:10,]
top_rest_type %>%ggplot(aes(x=reorder(Dining.Rating,n),y=n))+ geom_bar(stat = "identity")
barplot(sort(table(main$Dining.Rating)))

#barplot(order(table(main$Location)))
#????want to sort by top 5 ???????
#### if we want to plot mumbai ind vs all other ream performancs then
#### ncr_names <- c("New Delhi", "Ghaziabad", "Noida", "Gurgaon", "Faridabad")
new <- data
new$City <- as.character(new$City)
suppressWarnings(new$City[new$City != ncr_names] <- "Rest of India")
suppressWarnings(new$City[new$City == ncr_names] <- "Delhi-NCR")
new$City <- as.factor(new$City)
data <- new

######################################################
### Country Wise Restaurant data visualization #####
count <- as.data.frame(table(Zomato$Country))
ggplot(count,aes(x=Var1,y=Freq)) + geom_bar(stat='identity', fill='blue') +
  labs(title='Country wise restaurant count') + 
  xlab('Country')+ylab('Restaurant Count') + coord_flip()

 #######################################################
main$Name.of.Restaurant<-as.factor(main$Name.of.Restaurant)
main$Location<-as.factor(main$Location)
main$Cuisine<-as.factor(main$Cuisine)

#### location Wise Restaurant data visualization###
location_count <- as.data.frame(table(main$Location))
ggplot(subset(location_count,Freq>1),aes(x=Var1,y=Freq))+geom_bar(stat='identity', fill='blue')+coord_flip()

###################################################
#Out top 20 cuisines offered across restaurants
###################################################

cuisine_count <- as.data.frame(table(main$Cuisine))
colnames(cuisine_count) <- c('Cuisine','Count')
cuisine_count <- cuisine_count[order(cuisine_count$Count,decreasing = T),]
cuisine_count$Cuisine <- factor(cuisine_count$Cuisine, level =cuisine_count$Cuisine[order(cuisine_count$Count,decreasing = T)])
ggplot(data = head(cuisine_count,20), aes(x = Cuisine,y =Count,fill=Count))+ geom_col() +
  geom_bar(stat = 'identity') + ggtitle("Top 20  Cousines Offered Across Indian Restaurants") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Cuisine") + ylab("Restaurant Count") + 
  scale_fill_gradient(low = "#5EDA9E", high = "#00611C")+coord_flip()

###################################################
#budget distribution
###################################################
ggplot(data=main)+ geom_bar(mapping=aes(x=budget,fill='red'))
ggplot(data=main)+ geom_bar(mapping=aes(x=marital_status))+scale_fill_viridis_d()
#main$Dining.Rating<-ifelse(main$Dining.Rating=="=4.0","High","Low")
ggplot(data=main)+ geom_bar(mapping=aes(x=Dining.Rating))
ggplot(data=main)+ geom_bar(mapping=aes(x=budget))
ggplot(data=main)+ geom_bar(mapping=aes(x=Location))+coord_flip()+scale_fill_viridis_d()


########## colour plot #######
ggplot(data = main, aes(factor(budget), fill=factor(budget))) + geom_bar() + ggtitle("Price Range Distribution") + 
  theme(plot.title = element_text(hjust = 0.6)) +xlab('Price Range') + ylab('Number of Restaurants')
ggplot(data=main) + geom_bar(mapping=aes(x=ambience),fill=c('red','blue','pink','orange'))

############################
#colour plot new datasets
############################

Diabetes <- read.csv("C:/Users/GALAXY AUTOMATION/Desktop/data sci/Datasets/Diabetes.csv")
T<-head(Diabetes,138)
main <- cbind(main, T = T$Is_Diabetic )
ggplot(data=main) + geom_bar(mapping=aes(x=ambience),fill=c('red','blue','pink','orange'))

############################  
 # columb name replacement
##############################
names(main)[13]<-"Is_Diabetic"
ggplot(data=head(main,10))+geom_bar(mapping=aes(x=marital_status, fill=Delivery.Rating.Count), position="dodge")+facet_wrap(~budget)

####################################################
@MAKING PLOT FOR ONE HOTEL
#####################################################
S<-main[4,]
S$Delivery.Rating.Count<-as.numeric(S$Delivery.Rating.Count)
hist(S$Delivery.Rating.Count)
ggplot(data=S)+geom_bar(mapping = aes(x=Delivery.Rating.Count,fill=budget),position="dodge")
main$Dining.Rating<-as.numeric(main$Dining.Rating)
hist(main$Dining.Rating)

####################################################
 Data cleaning or ? remove_missing
###################################################
main$Dining.Rating.Count[main$Dining.Rating.Count == "Does not offer Dining"] <- "0"
main <- droplevels(main)
main$Delivery.Rating [main$Delivery.Rating  == "None"] <- "0"
main <- droplevels(main)
main$Delivery.Rating.Count[main$Delivery.Rating.Count  == "Does not offer Delivery"] <- "0"
main <- droplevels(main)
main$ambience[main$ambience=="?"]<-"family"
main$transport[main$transport=="?"]<-"public"
main <- droplevels(main)

############################
Range selection and counting
############################
library(dplyr)
main$Dining.Rating.Count<-as.numeric(main$Dining.Rating.Count)
main %>% mutate(cuts = cut(Dining.Rating.Count, c(0, 1000, 10000))) %>% 
  group_by(cuts) %>% 
  summarize(n=n())

##############################
Grouping data
##############################
main$Dining.Rating[main$Dining.Rating == 2 | main$Dining.Rating == 9] = "Low"
main$Dining.Rating[main$Dining.Rating == 11| main$Dining.Rating == 10] = "Medium"
main$Dining.Rating[main$Dining.Rating == 15| main$Dining.Rating == 18] = "High"
main

#############################
grouping
data$AGE<-cut(data$AGE,breaks=c(0,35,48,100),labels=c("Less than 35","35-48","49+"))























































































                                
        
        