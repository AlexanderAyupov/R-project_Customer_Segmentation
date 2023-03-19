install.packages("tidyverse", repos = "http://cran.us.r-project.org")
install.packages("skimr", repos = "http://cran.us.r-project.org")
install.packages("janitor", repos = "http://cran.us.r-project.org")
install.packages("DT", repos = "http://cran.us.r-project.org")
install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
install.packages("knitr", repos = "http://cran.us.r-project.org")
install.packages("plyr", repos = "http://cran.us.r-project.org")
library(DT)
library(tidyverse)
library(readr)
library(skimr)
library(janitor)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(rmarkdown)
library(lubridate)
library(rmarkdown)
library(knitr)
#export csv

wolt_dataset <- read_csv("dataset_for_analyst_assignment_20201120.csv")


#data preparation, understanding dataset, data cleaning
glimpse(wolt_dataset)
str(wolt_dataset)
as_tibble(wolt_dataset) %>% 
  print(n=50)

colnames(wolt_dataset)
head(wolt_dataset)
summary(wolt_dataset)

any(is.na(wolt_dataset))      #Check if there are null values
nrow(na.omit(wolt_dataset))
nrow(wolt_dataset)            # Check total number of observations


sum(is.na(wolt_dataset$USER_ID))        # Check for null values in a "key"/important for analysis columns
sum(is.na(wolt_dataset$PURCHASE_COUNT))
length(unique(wolt_dataset$USER_ID)) == nrow(wolt_dataset) #Check for duplicates in a "key" column.

wolt_dataset_cleaned <- wolt_dataset %>% filter(PURCHASE_COUNT>0)   #Filter dataset to eliminate null values in the column

length(unique(wolt_dataset$REGISTRATION_COUNTRY))     #Check unique values in REGISTRATION_COUNTRY

wolt_dataset_cleaned <- wolt_dataset_cleaned %>%        #Filter dataset for countries which have enough representative of population for analysis
  filter(REGISTRATION_COUNTRY=="FIN" |
           REGISTRATION_COUNTRY=="DNK" |
           REGISTRATION_COUNTRY=="GRC")

#Group by Registration_country, Country vs Income

viz_by_country <- wolt_dataset_cleaned %>%
  group_by(REGISTRATION_COUNTRY) %>%                             
  summarize(customers = n()) %>%
  arrange(customers) %>%                                
  mutate(REGISTRATION_COUNTRY = factor(REGISTRATION_COUNTRY, REGISTRATION_COUNTRY)) %>%     
  ggplot(aes(x=REGISTRATION_COUNTRY, y=customers, fill=REGISTRATION_COUNTRY)) +                  
  geom_bar(stat="identity") +                      
  coord_flip() +
  theme(legend.position="none") +
  geom_text(aes(label = round(customers))) +
  labs(title ="Number of Customers/Country", caption = "By Alexander Ayupov", x="Country", y="Customers")
       
viz_country_vs_income <- wolt_dataset_cleaned %>%
  group_by(REGISTRATION_COUNTRY) %>% 
  summarize(pcount = sum(TOTAL_PURCHASES_EUR)) %>% 
  ggplot(aes(x = REGISTRATION_COUNTRY, y = pcount, fill = REGISTRATION_COUNTRY)) +
  geom_col() + 
  coord_flip() +
  theme(legend.position="none") +
  geom_text(aes(label = round(pcount))) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title ="Total purchases by Country", caption = "By Alexander Ayupov", x="Country", y="Total Purchases")

#Group by Registration country by Preferred Device

viz_prefdevice_by_country <- wolt_dataset_cleaned %>% 
  filter(!is.na(PREFERRED_DEVICE)) %>% 
  ggplot(aes(PREFERRED_DEVICE, fill=PREFERRED_DEVICE)) +
  geom_bar() +
  facet_wrap(~REGISTRATION_COUNTRY) +
  theme(legend.position="none") +
  labs(title ="Preferred Device by Country", caption = "By Alexander Ayupov", x="Device", y="Customers")

#User Recency
user_recency <- wolt_dataset_cleaned %>% 
  select(USER_ID, MEDIAN_DAYS_BETWEEN_PURCHASES) %>% 
  rename(recency = MEDIAN_DAYS_BETWEEN_PURCHASES) %>% 
  group_by(USER_ID)

user_recency[is.na(user_recency)] <- 0   #replace na with 0

print(summary(user_recency$recency)) 

user_recency_filter <- user_recency %>%     #filter outliers
  filter(recency <= 100)

viz_recency <- ggplot(user_recency_filter, aes(recency)) +
  geom_histogram(bins = 20, color=4, fill="gray") +
  ylab('Number of Customers') +
  theme_minimal() +
  labs(title ="Day of Customers Inactivity", subtitle = "Order recency", caption = "By Alexander Ayupov", x="Days of Inactivity", y="Customers") +
  annotate("text", x=50,y=4000,label="50% of Users have ~9 days of Inactivity\nAverage is one month without making a single purchase", color="blue")

#User frequency

user_frequency <- wolt_dataset_cleaned %>% 
  select(USER_ID, PURCHASE_COUNT) %>% 
  rename(Frequency = PURCHASE_COUNT) %>% 
  group_by(USER_ID)

print(summary(user_frequency$Frequency))

user_frequency_filter <- user_frequency %>%     #filter outliers
  filter(Frequency <= 30)

viz_frequency <- ggplot(user_frequency_filter, aes(USER_ID, Frequency)) +
  geom_boxplot(color=4) +
  theme(axis.ticks.x = element_blank()) +
  theme_minimal() +
  labs(title ="Purchase Frequency", caption = "By Alexander Ayupov", x="Customers", y="Number of Purchases per Customer") +
  annotate("text", size=3.3, x=16700,y=25,label="Average 6 transaction per Customer\n50% of customers have ~3 transactions", color="blue")

#User monetary value
user_monetary_value <- wolt_dataset_cleaned %>% 
  select(USER_ID, TOTAL_PURCHASES_EUR) %>% 
  rename(monetary_value = TOTAL_PURCHASES_EUR) %>% 
  group_by(USER_ID)

print(summary(user_monetary_value$monetary_value))

MV_filter_1000 <- user_monetary_value %>% 
  filter(monetary_value <= 1000)

print(summary(MV_filter_1000$monetary_value))

MV_filter_over1000 <- user_monetary_value %>% 
  filter(monetary_value > 1000)

print(summary(MV_filter_over1000$monetary_value))

viz_monetary_1000 <- ggplot(MV_filter_1000, aes(monetary_value)) +
  geom_histogram(bins=10, color=4,fill="grey") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::comma) +
  labs(title ="Revenue of Users - Below €1K", caption = "By Alexander Ayupov", x="Revenue", y="Number of Customers") +
  annotate("text", x=650,y=4000,label="50% of Users brings 70€ revenue\nAverage revenue per user is 150€", color="blue")

viz_monetary_ov1000 <- ggplot(MV_filter_over1000, aes(monetary_value)) +
  geom_histogram(bins=10, color=4,fill="grey") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::comma) + 
  labs(title ="Revenue of Users - Over €1K", caption = "By Alexander Ayupov", x="Revenue", y="Number of Customers") +
  annotate("text", x=6000,y=100,label="50% of Users brings 1350€ revenue\nAverage revenue per user is 1600€", color="blue")

#Merging recency, frequence, monetary value

Users_RFM <- merge(user_recency, user_frequency) 
Users_RFM <- merge(Users_RFM, user_monetary_value) 
DT::datatable((Users_RFM),
              rownames = FALSE,
              options = list(
                pageLength = 10))

#KMeans Cluster - Break customers by clusters

set.seed(415)
clusters <- kmeans(scale(Users_RFM[,2:4]), 3, nstart = 1)
Users_RFM$Cluster <- as.factor(clusters$cluster)

KMeans_Results <- Users_RFM %>%
  group_by(Cluster) %>%
  summarise('Number of Users' = n(),
            'Recency Mean' = round(mean(recency)),
            'Frequency Mean' = scales::comma(round(mean(Frequency))),
            'Monetary Value Mean' = scales::dollar(round(mean(monetary_value))),
            'Cluster Revenue' = scales::dollar(sum(monetary_value))
  )

table_clusters <- DT::datatable((KMeans_Results),
              rownames = FALSE)

#Clusters viz

Cluster_size_visz <- ggplot(KMeans_Results, aes(Cluster, `Number of Users`)) +
  geom_text(aes(label = `Number of Users`), vjust = -0.3) +
  geom_bar(aes(fill=Cluster), stat='identity') +
  theme_classic() +
  labs(title ="Number of Users per Cluster", caption = "By Alexander Ayupov", x="Clusters", y="Number of Customers")

print(Cluster_size_visz)

#Breakdown Users data by Daytime/Weekdays

wolt_dataset_cleaned <- wolt_dataset_cleaned %>%
  mutate(MOST_COMMON_HOUR_OF_THE_DAY_TO_PURCHASE = parse_date_time(MOST_COMMON_HOUR_OF_THE_DAY_TO_PURCHASE, 'H:'))   #format as date

wolt_dataset_cleaned <- wolt_dataset_cleaned %>%
  mutate(MOST_COMMON_HOUR_OF_THE_DAY_TO_PURCHASE = format(strptime(MOST_COMMON_HOUR_OF_THE_DAY_TO_PURCHASE, "%Y-%m-%d %H:%M:%S"),'%H:%M'))   #Leave H:M format

glimpse(wolt_dataset_cleaned)

#Weekdays wrangling
unique(wolt_dataset_cleaned$MOST_COMMON_WEEKDAY_TO_PURCHASE)

wolt_dataset_cleaned$MOST_COMMON_WEEKDAY <- factor(wolt_dataset_cleaned$MOST_COMMON_WEEKDAY_TO_PURCHASE, levels=1:7,
                                                   labels=c("Monday", "Tuesday", "Wednesday",
                                                            "Thursday", "Friday", "Saturday", "Sunday"))
#plotting Purchase count by daytime/weekdays by Country

viz_hours_fin <- wolt_dataset_cleaned %>%
  filter(REGISTRATION_COUNTRY=="FIN") %>%  
  group_by(MOST_COMMON_HOUR_OF_THE_DAY_TO_PURCHASE) %>% 
  summarize(pcount = sum(PURCHASE_COUNT)) %>% 
  ggplot(aes(x=MOST_COMMON_HOUR_OF_THE_DAY_TO_PURCHASE, y = pcount, fill=pcount)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(legend.position="none") +
  labs(title ="Number of orders by day time FIN", caption = "By Alexander Ayupov", x="Hours of Purchase", y="Purchase count")


viz_hours_dnk <- wolt_dataset_cleaned %>%
  filter(REGISTRATION_COUNTRY=="DNK") %>%  
  group_by(MOST_COMMON_HOUR_OF_THE_DAY_TO_PURCHASE) %>% 
  summarize(pcount = sum(PURCHASE_COUNT)) %>% 
  ggplot(aes(x=MOST_COMMON_HOUR_OF_THE_DAY_TO_PURCHASE, y = pcount, fill=pcount)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(legend.position="none") +
  labs(title ="Number of orders by day time DNK", caption = "By Alexander Ayupov", x="Hours of Purchase", y="Purchase count")

viz_hours_grc <- wolt_dataset_cleaned %>%
  filter(REGISTRATION_COUNTRY=="GRC") %>%  
  group_by(MOST_COMMON_HOUR_OF_THE_DAY_TO_PURCHASE) %>% 
  summarize(pcount = sum(PURCHASE_COUNT)) %>% 
  ggplot(aes(x=MOST_COMMON_HOUR_OF_THE_DAY_TO_PURCHASE, y = pcount, fill=pcount)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(legend.position="none") +
  labs(title ="Number of orders by day time GRC", caption = "By Alexander Ayupov", x="Hours of Purchase", y="Purchase count")

viz_days_fin <- wolt_dataset_cleaned %>%
  filter(REGISTRATION_COUNTRY=="FIN") %>% 
  group_by(MOST_COMMON_WEEKDAY) %>% 
  summarize(pcount = sum(PURCHASE_COUNT)) %>% 
  ggplot(aes(x = MOST_COMMON_WEEKDAY, y = pcount, fill=pcount)) +
  geom_col() +
  labs(title ="Number of orders by Weekdays FIN", caption = "By Alexander Ayupov", x="Days of Purchase", y="Purchase count")

viz_days_dnk <- wolt_dataset_cleaned %>%
  filter(REGISTRATION_COUNTRY=="DNK") %>% 
  group_by(MOST_COMMON_WEEKDAY) %>% 
  summarize(pcount = sum(PURCHASE_COUNT)) %>% 
  ggplot(aes(x = MOST_COMMON_WEEKDAY, y = pcount, fill=pcount)) +
  geom_col() +
  labs(title ="Number of orders by Weekdays DNK", caption = "By Alexander Ayupov", x="Days of Purchase", y="Purchase count")

viz_days_grc <- wolt_dataset_cleaned %>%
  filter(REGISTRATION_COUNTRY=="GRC") %>% 
  group_by(MOST_COMMON_WEEKDAY) %>% 
  summarize(pcount = sum(PURCHASE_COUNT)) %>% 
  ggplot(aes(x = MOST_COMMON_WEEKDAY, y = pcount, fill=pcount)) +
  geom_col() +
  labs(title ="Number of orders by Weekdays GRC", caption = "By Alexander Ayupov", x="Days of Purchase", y="Purchase count")

#count mean

wolt_dataset_cleaned %>% 
  filter(REGISTRATION_COUNTRY=="FIN") %>% 
  group_by(MOST_COMMON_WEEKDAY) %>% 
  mutate(pcount = sum(PURCHASE_COUNT)) %>% 
  summarise(mean(pcount))

wolt_dataset_cleaned %>% 
  filter(REGISTRATION_COUNTRY=="DNK") %>% 
  group_by(MOST_COMMON_WEEKDAY) %>% 
  mutate(pcount = sum(PURCHASE_COUNT)) %>% 
  summarise(mean(pcount))

wolt_dataset_cleaned %>% 
  filter(REGISTRATION_COUNTRY=="GRC") %>% 
  group_by(MOST_COMMON_WEEKDAY) %>% 
  mutate(pcount = sum(PURCHASE_COUNT)) %>% 
  summarise(mean(pcount))


  

