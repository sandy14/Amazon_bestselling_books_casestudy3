# Amazon_bestselling_books_casestudy3 #

This Case study is part of Google Professional Business Analyst Certificate ,done by Sandhya Verma.

As part of 3rdcase study ,I have choosen Amazon bestselling books dataset.
Dataset on Amazon's Top 50 bestselling books from 2009 to 2019. Contains 550 books, data has been categorized into fiction and non-fiction using Goodreads.
We are going to do exploratory data analysis in R.


#Install packages#

install.packages("tidyverse")
library(tidyverse)
install.packages("tidyr")
library(tidyr)
install.packages("skimr")
library(skimr)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("janitor")
library(janitor)

#Import CSV file#

books <- read.csv("bestsellers with categories.csv")


#Preview of dataset#

head(books)
str(books)


#Explore to find distinct values#

n_distinct(books$Name)
nrow(books)
n_distinct(books$Aurthor)
n_distinct(books$Genre)

#find Duplicate values#
sum(duplicated(books$Name))

#because same book sold next year as well ,so duplicated.#

#Removing leading n ending trails from string#

books <- books%>%
  mutate(name= trimws(name),
         author= trimws(author),
         genre =trimws(genre))

#changing fromat of datatype #

books$Price <- as.numeric(books$Price)
books$genre <- as.factor(books$genre)
 
str(books)

#categorize Rating ,Price and Reviews  for analysis #

#rating : 3.0 to 3.5 = Good,3.5 to 4.0 = Very Good,4.0 to 4.5= Excellent,4.5 to 5 =Outstanding#

#Price :  Free books     = price equal to 0,
         #low price      = Price between 0 to 10 Dollar,
         #Moderate price = price between 10 to 20 dollar
         # High  price   = price between 20 to 30 dollar
         # Very high price = price more than 30 dollar


#Reviews    very low review   < than 3000
        #   Low Reviews       = Reviews between  3000 to  7000
        #   Good Reviews     = reviews between 7000 to 15000
        #   Very good reviews = Reviews between 15000 to 30000
        #  Excellent reviews  = Reviews more than 30000

#Creating dataframe for Rating#
 Rating <- books%>%
   mutate(Rating_category= case_when(User.Rating >=3.0 & User.Rating <=3.5 ~ "Good",
                                     User.Rating >=3.5 & User.Rating <=4.0 ~ "Very Good",
                                     User.Rating >=4.0 & User.Rating <=4.5 ~ "Excellent",
                                     User.Rating >=4.5 & User.Rating <=5 ~ "Outstanding"))
View(Rating)   



#Creating dataframe for Price#
Price <- books%>%
  mutate(Price_category= case_when( Price < 1 ~ "Free books",
                                    Price >= 1 & Price <= 10 ~ "Low_price" ,
                                    Price >=10 & Price <= 20 ~ "Moderate_price",
                                    Price >=20 & Price <= 30 ~"High_price",
                                    Price > 30 ~ "Very_high_price"))


#Creating dataframe for Reviews#                                    

Reviews <- books %>%
  mutate(Review_category =case_when( Reviews <  3000 ~    "Very_low_Reviews",
                                     Reviews >= 3000 & Reviews <= 7000 ~ "Low_Reviews",
                                     Reviews >=7000 & Reviews <= 15000 ~ "Good_Reviews",
                                     Reviews >= 15000 & Reviews <= 30000 ~ "Very_good_Reviews",
                                     Reviews > 30000 ~ "Excellent_Reviews"))


#Explore data#
summary_yearly <- books%>%
  group_by(Year)%>%
  summarise(Book_count=n(),Avg_rating= mean(User.Rating), Avg_price= mean(Price)
            ,Avg_Reviews= mean(Reviews))

View(summary_yearly)

#creating visualization#


#Calculating % for rating to create pei chart#
  
Rating_percent <- Rating%>%
  count(Rating_category)%>%
  mutate(Percentage= scales::percent((n/sum(n))))
view(Rating_percent)


#Create a pie chart for Rating visualization#

  
  ggplot(Rating_percent,aes(x="",y=Percentage,fill=Rating_category))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y")+
  ggtitle("Pie chart based on Book Ratings")+
  theme_minimal()+
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
        axis.text.x = element_blank())+
  geom_text(aes(label = Percentage),position = position_stack(vjust = 0.5))


    
#calculating % for price categories#
  
  
  Price_percent <- Price%>%
    count(Price_category)%>%
    mutate(Percentage= scales::percent((n/sum(n))))
#Creating Pei chart for price categories#
  
  ggplot(Price_percent,aes(x="",y=Percentage,fill=Price_category))+
    geom_bar(stat = "identity",width = 1)+
    coord_polar("y")+
    ggtitle("Pie chart based on Book Price")+
    theme_minimal()+
    theme(axis.title.x= element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(), 
          plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
          axis.text.x = element_blank())+
    geom_text(aes(label = Percentage),position = position_stack(vjust = 0.5))
  
  
  #calculating % for Reviews categories#
  
  
  Review_percent <- Reviews%>%
    count(Review_category)%>%
    mutate(Percentage= scales::percent((n/sum(n))))
  
  
  #Creating Pei chart for Reviews categories#
  
  ggplot(Review_percent,aes(x="",y=Percentage,fill=Review_category))+
    geom_bar(stat = "identity",width = 1)+
    coord_polar("y")+
    ggtitle("Pie chart based on Book reviews")+
    theme_minimal()+
    theme(axis.title.x= element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(), 
          plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
          axis.text.x = element_blank())+
    geom_text(aes(label = Percentage),position = position_stack(vjust = 0.5))
  
  


  
