
#NAME : Thet Pyae Sone Aung
#Task : 3 (EDA on Retail)
#Subject : Data Science and Business Analytics

install.packages("caret")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("DataExplorer")
install.packages("tidyverse")
install.packages("ClusterR")
install.packages("cluster")
library(ClusterR)
library(cluster)
library(caret)
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(tidyverse)
SampleSuperstore <- read_csv("C:/Users/DELL/Downloads/SampleSuperstore.csv")
summary(SampleSuperstore)
#Ship Mode           Segment            Country              City          
#Length:9994        Length:9994        Length:9994        Length:9994       
#Class :character   Class :character   Class :character   Class :character  
##Mode  :character   Mode  :character   Mode  :character   Mode  :character  



#State            Postal Code       Region            Category        
#Length:9994        Min.   : 1040   Length:9994        Length:9994       
#Class :character   1st Qu.:23223   Class :character   Class :character  
##Mode  :character   Median :56431   Mode  :character   Mode  :character  
#Mean   :55190                                        
#3rd Qu.:90008                                        
#Max.   :99301                                        
#Sub-Category           Sales              Quantity        Discount     
#Length:9994        Min.   :    0.444   Min.   : 1.00   Min.   :0.0000  
#Class :character   1st Qu.:   17.280   1st Qu.: 2.00   1st Qu.:0.0000  
#Mode  :character   Median :   54.490   Median : 3.00   Median :0.2000  
#Mean   :  229.858   Mean   : 3.79   Mean   :0.1562  
#3rd Qu.:  209.940   3rd Qu.: 5.00   3rd Qu.:0.2000  
#Max.   :22638.480   Max.   :14.00   Max.   :0.8000  
#Profit         
#Min.   :-6599.978  
#1st Qu.:    1.729  
#Median :    8.666  
#Mean   :   28.657  
#3rd Qu.:   29.364  
#Max.   : 8399.976  

SampleSuperStore<-distinct_all(SampleSuperstore)
SampleSuperStore<-select(SampleSuperStore,-c(Country,City,'Postal Code'))

dim(SampleSuperstore)
#[1] 9994   13

#After choosing distict values from dataset
dim(SampleSuperStore)
#[1] 9977   13

# Total Profit and Sales
sum(SampleSuperStore$Profit)
#[1] 286241.4
sum(SampleSuperStore$Sales)
#[1] 2296196

plot_bar(SampleSuperStore$State)
plot(SampleSuperStore$Sales,SampleSuperStore$Profit)
unique(SampleSuperStore$State)
plot_histogram(SampleSuperStore$Sales)
plot_bar(SampleSuperStore$Category,title = "Category")

qplot(Sales,Profit,data=SampleSuperStore,geom = c("point","smooth"),color=State)

qplot(Sales,Profit,data=SampleSuperStore,geom ="boxplot" ,color=State)
qplot(Sales,Discount,data=SampleSuperStore,col=Region)

qplot(Category,Sales,data=SampleSuperStore,geom="boxplot",col=Region,main="Sales by Category")
qplot(Profit,data=SampleSuperStore,geom="boxplot",col=Category,main="Category by Profit")
qplot(Profit,Sales,data=SampleSuperStore,geom="boxplot",col=Category)
qplot(Sales,Discount,data=SampleSuperStore,facets=.~Category)
boxplot(Sales~Profit,SampleSuperStore)

plot_bar(SampleSuperStore$`Sub-Category`)
plot_bar(SampleSuperStore$Category)
plot_bar(SampleSuperStore$Region)
plot_bar(SampleSuperStore$`Ship Mode`)
plot_bar(SampleSuperStore$Segment)
qplot(Sales,Discount,data=SampleSuperStore,col=Category,geom=c("point","smooth"))

boxplot(Sales~Category,SampleSuperStore,xlab="Sales",ylab = "Category",main="Sales by Category")


#	Exploratory Data Analysis on Retail

	#Category of Technology is the highest sales in South Region.
	#Technological Accessories are the highest profit in total sales amount.
	#Sales and Profits are positively correlated and make more sales and make more profits.
	#Technology things makes most profit among them and also make loss.
	#Furniture make the lowest in sale and profit.
	#Furniture can get discount in every time they sell.
	#SUb Category of Office supplies are the most frequent pattern in buying process.
	#Florida is the highest sales in State.
	#Indiana is the highest profit and Ohio is the lowest.


