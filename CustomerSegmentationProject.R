############################################################
# Customer Segmentation Project (Capstone CYO Project)
# By Richard Jonyo
# Goal: The goal of this project is to identify segments of customers based on common characteristics or patterns. 
#          In this machine learning project, we will make use of K-means clustering.
###########################################################
# Note: this process could take a couple of minutes
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(DataExplorer)) install.packages("DataExplorer", repos = "http://cran.us.r-project.org")
if(!require(heatmaply)) install.packages("heatmaply", repos = "http://cran.us.r-project.org")
if(!require(dlookr)) install.packages("dlookr", repos = "http://cran.us.r-project.org")
if(!require(highcharter)) install.packages("highcharter", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")

library(ggplot2)
library(dplyr)
library(tidyr)
library(DataExplorer)
library(lubridate)
library(heatmaply)
library(dlookr)
library(highcharter)
library(factoextra)
library(scales)

#E-Commerce Dataset:
#https://www.kaggle.com/fabiendaniel/customer-segmentation/data

#Load dataset locally
customer_data <- read.csv("dataset/data.csv")


head(customer_data) #Preview customer_data
dim(customer_data) #541,909 observations and 8 columns
str(customer_data) #view datatypes
names(customer_data)#view column names
length(unique(customer_data$CustomerID))#4,373 unique customers
anyNA(customer_data) #check missing values - results to TRUE
customer_data[!complete.cases(customer_data),] #lots of missing CustomerIDs

summary(customer_data$Quantity)#Quantity summary
summary(customer_data$UnitPrice)#UnitPrice summary

#We plot missing values
plot_missing(customer_data)

length(unique(customer_data$CustomerID)) #4,373 unique customerIds
length(unique(customer_data$InvoiceNo)) #25,900 unique invoice no.s

#We have a outliers on UnitPrice & Quantity using the dlookr package
#some items are more expensive than others, and some quanties ordered are larger than others
plot_outlier(customer_data, Quantity, col = "#FF3399")
plot_outlier(customer_data, UnitPrice, col = "#FF3399")

#Plot missing values using the DataExplorer package
#Looking at the size of the dataset and the missing value plot, it seems as if we can remove the missing values and still have a good-sized set of data to work on
customer_data <- subset(customer_data, !is.na(customer_data$CustomerID))
dim(customer_data) #We remain with 406,829 observations without NAs

#Negative Quantities reflect cancelled orders, indicated with the C letter in front of the Invoice Number
#Checking Quantities with negative values arranged in descending order
quantityCheck <- customer_data %>% 
  filter(Quantity < 0) %>% 
  arrange(Quantity)
head(quantityCheck, 5)

#Replace all negative Quantity and Price with NA
customer_data <- customer_data %>% 
  mutate(Quantity = replace(Quantity, Quantity<=0, NA),
         UnitPrice = replace(UnitPrice, UnitPrice<=0, NA))
customer_data <- customer_data %>%
  drop_na() #Delete any customerID with NA
dim(customer_data) #we remain with 397,884 observations

#We check again the presence of outliers
#Most of the products that are being sold are mostly a low priced
plot_outlier(customer_data, Quantity, col = "#FF3399")
plot_outlier(customer_data, UnitPrice, col = "#FF3399")

#Create date variables
#We separate date and time components of invoice date
customer_data$date <- sapply(customer_data$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][1]})
customer_data$time <- sapply(customer_data$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][2]})

#we create month, year and hour of day columns
customer_data$month <- sapply(customer_data$date, FUN = function(x) {strsplit(x, split = '[/]')[[1]][1]})
customer_data$year <- sapply(customer_data$date, FUN = function(x) {strsplit(x, split = '[/]')[[1]][3]})
customer_data$hourOfDay <- sapply(customer_data$time, FUN = function(x) {strsplit(x, split = '[:]')[[1]][1]})
tmp <- customer_data %>% select(CustomerID, Country, date, time, month, year, hourOfDay)
head(tmp)

#We convert date column to date format
#We can create day of the week column
customer_data$date <- as.Date(customer_data$date, "%m/%d/%Y")
str(customer_data)
customer_data$dayOfWeek <- wday(customer_data$date, label=TRUE)
tmp <- customer_data %>% select(CustomerID, Country, date, time, month, year, hourOfDay, dayOfWeek)
head(tmp)

#We convert date column to date format
#We can create a new column for day of the week, using the wday function from the lubridate package.
customer_data$date <- as.Date(customer_data$date, "%m/%d/%Y")
str(customer_data)
customer_data$dayOfWeek <- wday(customer_data$date, label=TRUE)
tmp <- customer_data %>% select(CustomerID, Country, date, time, month, year, hourOfDay, dayOfWeek)
head(tmp)

#add a TotalCost column
customer_data <- customer_data %>% mutate(TotalCost = Quantity * UnitPrice)

#we turn the appropriate variables into factors
customer_data$month <- as.factor(customer_data$month)
customer_data$year <- as.factor(customer_data$year)
levels(customer_data$year) <- c(2010,2011)
customer_data$hourOfDay <- as.factor(customer_data$hourOfDay)
customer_data$dayOfWeek <- as.factor(customer_data$dayOfWeek)
customer_data$Country <- as.factor(customer_data$Country)
str(customer_data)

### EXPLORATORY ANALYSIS ###
#We employ Exploratory Data Analysis (EDA) to conduct an initial investigation inside the dataset and observe common patterns, spot anomalies and retrieve useful information about the data in a graphical way.
#We have a good dataset to start performing some summary analyses

## Revenue Summaries
#Plot revenues over time
options(repr.plot.width=8, repr.plot.height=3)
customer_data %>%
  group_by(date) %>%
  summarise(revenue = sum(TotalCost)) %>%
  ggplot(aes(x = date, y = revenue)) + 
  geom_line() + 
  geom_smooth(method = 'auto', se = FALSE) + 
  labs(x = 'Date', y = 'Revenue (£)', title = 'Revenue by Date')

# Plot Revenue by Day of Week
customer_data %>%
  group_by(dayOfWeek) %>%
  summarise(revenue = sum(TotalCost)) %>%
  ggplot(aes(x = dayOfWeek, y = revenue)) + 
  geom_col() + 
  labs(x = 'Day of Week', y = 'Revenue (£)', title = 'Revenue by Day of Week')

#Summary of revenue generated on each particular weekday
weekdaySummary <- customer_data %>%
  group_by(date, dayOfWeek) %>%
  summarise(revenue = sum(TotalCost), transactions = n_distinct(InvoiceNo)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  ungroup()

#Plot of Revenue by Day of the Week
ggplot(weekdaySummary, aes(x = dayOfWeek, y = revenue)) + 
  geom_boxplot() + 
  labs(x = 'Day of the Week', y = 'Revenue', title = 'Revenue by Day of the Week')

#Plot of Transactions by Day of the Week
ggplot(weekdaySummary, aes(x = dayOfWeek, y = transactions)) + 
  geom_boxplot() + labs(x = 'Day of the Week', y = 'Daily Transactions', title = 'Transactions by Day of the Week')

#Plot of Average Order Value by Day of the Week
ggplot(weekdaySummary, aes(x = dayOfWeek, y = aveOrdVal)) + 
  geom_boxplot() + labs(x = 'Day of the Week', y = 'Average Order Value', title = 'Average Order Value by Day of the Week')

#The differences in the amount of revenue on each day of the week is driven by a difference in the no. of transactions, rather than the average order value
 
#There is skewness in our distributions
#Data suggest people are more ready to buy on Thursdays and fewer transactions on sundays
ggplot(weekdaySummary, aes(transactions, fill = dayOfWeek)) + 
  geom_density(alpha = 0.2)

## Country Summary
countrySummary <- customer_data %>%
  group_by(Country) %>%
  summarise(revenue = sum(TotalCost), transactions = n_distinct(InvoiceNo)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  ungroup() %>%
  arrange(desc(revenue))
head(countrySummary, n = 10)
#tail(countrySummary, n = 10)

#Top five countries by revenue
top5Countries <- customer_data %>%
  filter(Country == 'United Kingdom' | Country == 'Netherlands' | Country == 'EIRE' | Country == 'Germany' | Country == 'France' | Country == 'Australia')

#dataframe of top 5 countries by revenue
top_5 <- top5Countries %>%
  group_by(Country) %>%
  dplyr::summarise(revenue = sum(TotalCost), transactions = n_distinct(InvoiceNo), 
                   customers = n_distinct(CustomerID)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  arrange(desc(revenue))

#Plot countries vs. revenue
top_5 %>% 
  ggplot(aes(x=Country, y=revenue))+
  geom_bar(stat = 'identity', fill = '#FF9933') +
  ggtitle('Top 5 Countries by Revenue') +
  xlab('Countries') +
  ylab('Revenue')+
  scale_y_continuous(labels = comma)


#Plot top 5 country revenue summary (Without United Kingdom)
#Netherlands and EIRE are significant sources of revenue
#Germany and France also represent significant opportunities
#we repeat the above step without United Kingdom
#Top five countries in terms of revenue contribution
top5Countries <- customer_data %>%
  filter(Country == 'Netherlands' | Country == 'EIRE' | Country == 'Germany' | Country == 'France' | Country == 'Australia')
top_5 <- top5Countries %>%
  group_by(Country) %>%
  dplyr::summarise(revenue = sum(TotalCost), transactions = n_distinct(InvoiceNo), 
                   customers = n_distinct(CustomerID)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  arrange(desc(revenue))

top_5 %>% 
  group_by(Country) %>%
  dplyr::summarise(revenue = sum(revenue)) %>% 
  hchart('treemap', hcaes(x = 'Country', value = 'revenue', color = 'revenue')) %>%
  hc_title(text=" Top 5 Countries by Revenue (excluding United Kingdom)")


# Customer segmentation
#We Customer ID to look for differences between customers
custSummary_1 <- customer_data %>%
  group_by(CustomerID) %>%
  summarise(revenue = sum(TotalCost), transactions = n_distinct(InvoiceNo)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  ungroup() %>%
  arrange(desc(revenue))

head(custSummary_1)

#summarize customers with high revenues/sales
custSummary_2 <- customer_data %>%
  group_by(CustomerID, InvoiceNo) %>%
  summarise(revenue = sum(TotalCost), transactions = n_distinct(InvoiceNo)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  ungroup() %>%
  arrange(revenue) %>%
  mutate(cumsum=cumsum(revenue))

head(custSummary_2)

#It seems many of the large transactions are refunded
#we sum the revenue
custSummary_2 <- customer_data %>%
  group_by(InvoiceNo, CustomerID, Country, date, month, year, hourOfDay, dayOfWeek) %>%
  summarise(orderVal = sum(TotalCost)) %>%
  mutate(recent = Sys.Date() - date) %>%
  ungroup()

custSummary_2$recent <- as.character(custSummary_2$recent)
custSummary_2$recentDays <- sapply(custSummary_2$recent, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][1]})
custSummary_2$recentDays <- as.integer(custSummary_2$recentDays)
head(custSummary_2, n = 5)

#The dataframe can provide us with the order value and date & time information for each transaction, that can be groupped by customer
customerSummary_3 <- custSummary_2 %>%
  group_by(CustomerID, Country) %>%
  summarise(orders = n_distinct(InvoiceNo), revenue = sum(orderVal), meanRevenue = round(mean(orderVal), 2), medianRevenue = median(orderVal), 
            mostDay = names(which.max(table(dayOfWeek))), mostHour = names(which.max(table(hourOfDay))),
            recency = min(recentDays))%>%
  ungroup()
head(customerSummary_3)

#We filter oders greater than 1 and revenue greater than 50 pounds
#Our dataframe that gives us a list of repeat customers and tells us their country, how many orders they have made, total revenue and average order value as well as the day of the week and the time of the day they most frequently place orders.
customerSummary_3Sum <- customerSummary_3 %>%
  filter(orders > 1, revenue > 50)
head(customerSummary_3Sum)
dim(customerSummary_3Sum) # We remain with a small subset (2845)

#From this, we're in a better position to answer a number of questions about our customers that we could use to target specific marketing materials, promotions and offers.
custTargets <- customerSummary_3Sum %>%
  select(recency, revenue, meanRevenue, medianRevenue, orders) %>%
  as.matrix()
rownames(custTargets) <- customerSummary_3Sum$CustomerID
head(custTargets)

#Generate a heatmap
#By analysing how customers cluster, we discover groups of customers that behave in similar ways. 
#This level of customer segmentation is useful in marketing to these groups of customers appropriately. 
#A marketing campaign that works for a group of customers that places low value orders frequently may not be appropriate for customers who place sporadic, high value orders for example.
options(repr.plot.width=20, repr.plot.height=14)
heatmap(scale(custTargets), cexCol = 0.7)
#Recency: It refers to the number of days before the reference date when a customer made the last purchase. Lesser the value of recency, higher is the customer visit to a store.

#Clusturing using K-Means
set.seed(1, sample.kind="Rounding") #using R version 4.0.4`

#We plot optimal number of clusters using factoextra package
fviz_nbclust(custTargets, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

clusters <- kmeans(scale(custTargets[,1:5]), 4, nstart = 1) # Performing kmeans with 4 clusters. nstart > 1 is often recommended.
custTargets$Cluster <- as.factor(clusters$cluster) # Attaching the results to CustomersID to identify each customer's cluster

#cluster sizes
clusters$size

#cluster means
clusters$centers

#We plot the clusters
fviz_cluster(clusters, data=as.data.frame(custTargets)[, -6], ellipse.type = "norm")



