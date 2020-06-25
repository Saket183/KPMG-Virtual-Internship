library(readxl)
transactions<- read_excel("C:/Users/Sunil Jha/Desktop/KPMG Virtual Internships/KPMG.xlsx",sheet =  'Transactions')
NewCustomerList<- read_excel("C:/Users/Sunil Jha/Desktop/KPMG Virtual Internships/KPMG.xlsx",sheet =  'NewCustomerList')
CustomerDemographic<- read_excel("C:/Users/Sunil Jha/Desktop/KPMG Virtual Internships/KPMG.xlsx",sheet =  'CustomerDemographic')
CustomerAddress<- read_excel("C:/Users/Sunil Jha/Desktop/KPMG Virtual Internships/KPMG.xlsx",sheet =  'CustomerAddress')

transactions <- na.omit(transactions)
NewCustomerList <- na.omit(NewCustomerList)
CustomerDemographic <- na.omit(CustomerDemographic)
CustomerAddress <- na.omit(CustomerAddress)


#working with the transation data to clean and remove the unwanted data 
transactions$product_first_sold_date <- as.Date(transactions$product_first_sold_date , format = "%m/%d/%y")
library(dplyr)
transactions <- filter(transactions, order_status != "Cancelled")

transactions$day <- weekdays(transactions$product_first_sold_date)

#working with the newcustomer data to clean and remove the unwanted data 
NewCustomerList$...21 <- NULL

NewCustomerList$DOB <- as.Date(NewCustomerList$DOB == "" , format = "%m/%d/%y")

NewCustomerList <- filter(NewCustomerList, job_industry_category != "n/a")

# age from dob
NewCustomerList$Current_age = as.numeric(difftime(Sys.Date(),NewCustomerList$DOB, units = "weeks"))/52.25




transactions$online_order <- as.factor(transactions$online_order)


#Run the model with all the variables
m_full <- glm(online_order ~ transaction_id + product_id + customer_id + list_price + standard_cost , data = transactions,
              family = binomial())
summary(m_full)


m1 <- glm(online_order ~ product_id + customer_id + list_price + standard_cost , data = transactions,
          family = binomial())
summary(m1)

anova(m1,m_full,test="Chisq")

m2 <- glm(online_order ~ product_id + list_price + standard_cost , data = transactions,
          family = binomial())
summary(m2)

anova(m2,m1,test="Chisq")

m3 <- glm(online_order ~ list_price + standard_cost , data = transactions,
          family = binomial())
summary(m3)

anova(m3,m2,test="Chisq")


m4 <- glm(online_order ~ standard_cost , data = transactions,
          family = binomial())
summary(m4)

anova(m3,m4,test="Chisq")
